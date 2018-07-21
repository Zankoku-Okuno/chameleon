module Language.Chameleon.Syntax
    ( Nest(..)
    , parseNest
    , SyntaxError(..)
    , quietShow
    ) where

import Data.Foldable
import Data.List
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), viewl, ViewR(..), viewr)
import qualified Data.Sequence as Seq

import Control.Arrow
import Control.Monad
import Control.Monad.WarnErrs

import qualified Language.Chameleon.Token.Cooked as Cooked
import qualified Language.Chameleon.Token as Token
import Language.Chameleon.Token hiding (Atom)

{-
A nest is a extension of s-expressions.

An s-expression is either an atom, or a combination, which is a list of s-expressions.
S-expressions often take a form such as `(foo (a a1 a2) b (c c1 ...) ...)`.

In nests, a combination is a non-empty list of segments, which are lists of s-expressions.
This allows nests to take a more concise form, such as `foo(a a1 a2; b; c c1 ...; ...)`.
Unlike s-expressions, combinations in nests always have a leading element.

This implementation carries additional information:
    * where the nest was parsed, for use in later error reporting or debugging
    * the delimiters and segment separators used, for use in re-rendering
-}
data Nest atom
    = Atom
        { atom :: atom
        , location :: Location
        }
    | Comb
        { id :: IdType
        , segments :: [[Nest atom]] -- Note location of each segment can be determined from the locations of the first and last children of the segment
        , delim :: DelimiterType
        , sep :: Maybe SeparatorType
        , location :: Location -- NOTE overall location
        , openAt :: Location -- NOTE location of open delimiter
        , closeAt :: Location -- NOTE location of close delimiter
        }
    deriving(Read, Show)


parseNest :: [Token atom] -> WarnErrs SyntaxError (Nest atom)
parseNest [] = error "internal error"
parseNest tokens = execParser (loop tokens) >>= drainStack
    where
    loop :: [Token atom] -> Parser atom ()
    loop [] = pure ()
    loop (token : rest) = go token >> loop rest
    drainStack :: [StackItem atom] -> WarnErrs SyntaxError (Nest atom)
    drainStack [] = error "internal error"
    drainStack [Done it] = pure it
    drainStack stack = do
        errs $ toErr <$> stack
        pure undefined
        where
        toErr Item{..} = UnmatchedDelimiter
            { expectedBefore = Cooked.theLocation $ last tokens
            , ..
            }
    go :: Token atom -> Parser atom ()
    go (Tok (Token.Atom atom) location) = bufferChild Atom{..}
    go (Tok (Token.Open id delim) openLoc) = push (id, delim, openLoc)
    go (Tok (Token.Separate sep) loc) = bufferSegment (sep, loc)
    go (Tok (Token.Close File) closeAt) = close closeAt
    go (Tok (Token.Close delim) closeAt) = do
        child <- pop (delim, closeAt)
        bufferChild child


data SyntaxError
    = UnmatchedDelimiter
        { delim :: DelimiterType
        , openAt :: Location
        , expectedBefore :: Location
        }
    | UnexpectedSeparator
        { sep :: SeparatorType
        , unexpectedAt :: Location
        , expectedFrom :: Location
        }
    deriving(Read, Show)


newtype Parser atom a = P { unParser :: [StackItem atom] -> WarnErrs SyntaxError (a, [StackItem atom]) }

data StackItem atom
    = Item
        { id :: IdType
        , delim :: DelimiterType
        , sep :: Maybe (SeparatorType, Location)
        , segments :: Seq (Seq (Nest atom))
        , openAt :: Location
        }
    | Done (Nest atom)

instance Functor (Parser atom) where
    fmap f (P action) = P $ \stack -> first f <$> action stack

instance Applicative (Parser atom) where
    pure x = P $ \stack -> pure (x, stack)
    (<*>) = ap

instance Monad (Parser atom) where
    return = pure
    (P action) >>= k = P $ \stack -> do
        (x, stack) <- action stack
        unParser (k x) stack

runParser :: Parser atom a -> WarnErrs SyntaxError (a, [StackItem atom])
runParser (P action) = action []

evalParser :: Parser atom a -> WarnErrs SyntaxError a
evalParser = (fst <$>) . runParser

execParser :: Parser atom a -> WarnErrs SyntaxError [StackItem atom]
execParser = (snd <$>) . runParser


push :: (IdType, DelimiterType, Location) -> Parser atom ()
push (id, delim, openAt) = P $ \stack -> pure ((), top' : stack)
    where
    top' = Item{sep = Nothing, segments = Seq.singleton Seq.empty, ..}

pop :: (DelimiterType, Location) -> Parser atom (Nest atom)
pop (delim', closeAt) = P $ \(top@Item{..} : stack') ->
    if delim == delim'
    then pure (combine top, stack')
    else err $ UnmatchedDelimiter
        { openAt = openAt
        , expectedBefore = closeAt
        , ..
        }
    where
    combine Item{..} = Comb
        { segments = unSeq segments
        , sep = fst <$> sep
        , location = openAt `mergeLoc` closeAt
        , ..
        }

bufferChild :: Nest atom -> Parser atom ()
bufferChild child' = P $ \(top : stack) ->
    pure ((), alter top : stack)
    where
    alter Item{segments = viewr -> priorSegments :> children, ..} =
        Item{segments = priorSegments |> (children |> child'), ..}

bufferSegment :: forall atom. (SeparatorType, Location) -> Parser atom ()
bufferSegment (sep', loc) = P $ \(top : stack) -> case sep (top :: StackItem atom) of
    Nothing -> pure ((), (addSep . alter) top : stack)
    Just (sep0, loc0) -> if sep0 == sep'
        then pure ((), alter top : stack)
        else err UnexpectedSeparator
            { sep = sep'
            , unexpectedAt = loc
            , expectedFrom = loc0
            }
    where
    alter Item{segments = viewr -> priorSegments :> lastSegment, ..} =
        Item{segments = priorSegments |> lastSegment |> Seq.empty, ..}
    addSep Item{..} = Item{sep = Just (sep', loc), ..}

close :: Location -> Parser atom ()
close closeAt = P $ \[top] -> pure ((), [Done $ combine top])
    where
    combine Item{..} = Comb
        { segments = unSeq segments
        , location = openAt `mergeLoc` closeAt
        , sep = fst <$> sep
        , ..
        }


{- Show the syntax, but without the location data. -}
quietShow :: Show atom => Nest atom -> String -- TODO use a pretty-printer
quietShow Atom{..} = show atom
quietShow Comb{..} = concat [open delim, join sep qShowSegments, close delim]
    where
    open Paren = unintern id ++ "("
    open Bracket = unintern id ++ "["
    open Brace = unintern id ++ "{"
    open (Indent _) = unintern id ++ ":"
    open File = ""
    join :: Maybe SeparatorType -> [String] -> String
    join Nothing [] = ""
    join Nothing [x] = x
    join (Just Comma) xs = concatMap (++ ", ") xs
    join (Just Semicolon) xs = concatMap (++ "; ") xs
    join (Just (Newline _)) xs = case delim of
        Indent level -> concatMap (('\n' : replicate level ' ') ++) xs
        File -> concatMap (++ "\n") xs
    join Nothing xs = error $ show xs
    close Paren = ")"
    close Bracket = "]"
    close Brace = "}"
    close (Indent _) = ""
    close File = ""
    qShowSegments = toList $ qShowChildren <$> segments
    qShowChildren children = case delim of
        Indent _ -> intercalate "\n" . toList $ quietShow <$> children
        File -> intercalate "\n" . toList $ quietShow <$> children
        _ -> intercalate " " . toList $ quietShow <$> children

unSeq = toList . (toList <$>) . Seq.filter (not . Seq.null)