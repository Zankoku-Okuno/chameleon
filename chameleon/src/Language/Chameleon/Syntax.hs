module Language.Chameleon.Syntax where

import Data.Foldable
import Data.List
import Data.Sequence (Seq, (<|), (|>), (><), ViewL(..), viewl, ViewR(..), viewr)
import qualified Data.Sequence as Seq

import qualified Language.Chameleon.Token as Token
import Language.Chameleon.Token hiding (Atom)

data Syntax atom
    = Atom
        { atom :: atom
        , location :: Location
        }
    | Combs
        { id :: IdType
        , delim :: DelimiterType
        , sep :: Maybe SeparatorType
        , segments :: [[Syntax atom]] -- Note location of each segment can be determined from the locations of the first and last children of the segment
        , location :: Location -- NOTE overall location
        , openLoc :: Location -- NOTE location of open delimiter
        , closeLoc :: Location -- NOTE location of close delimiter
        }
    deriving(Read, Show)


data StackItem atom
    = Item
        { id :: IdType
        , delim :: DelimiterType
        , sep :: Maybe SeparatorType
        , segments :: Seq (Seq (Syntax atom))
        , openLoc :: Location
        }
    | Done (Syntax atom)

parse :: [Token atom] -> Syntax atom
parse tokens = extract (loop [] tokens)
    where
    loop stack [] = stack
    loop stack (token : rest) = loop (go token stack) rest
    extract [] = error "internal error"
    extract [Done it] = it
    extract stack = error "missing close delimiters"


go :: Token atom -> [StackItem atom] -> [StackItem atom]
go (Tok (Token.Atom atom) location) stack = bufferChild Atom{..} stack
go (Tok (Token.Open id delim) openLoc) stack = push (id, delim, openLoc) stack
go (Tok (Token.Separate sep) _) stack = bufferSegment sep stack
go (Tok (Token.Close File) closeLoc) [Item{..}] = [Done Combs{segments = unSeq segments, location = (fst openLoc, snd closeLoc), ..}]
go (Tok (Token.Close delim) closeLoc) stack =
    let (child, stack') = pop (delim, closeLoc) stack
    in bufferChild child stack'


push :: (IdType, DelimiterType, Location) -> [StackItem atom] -> [StackItem atom]
push (id, delim, openLoc) stack = (Item{sep = Nothing, segments = Seq.singleton Seq.empty, ..}) : stack

pop :: (DelimiterType, Location) -> [StackItem atom] -> (Syntax atom, [StackItem atom])
pop (delim', closeLoc) (Item{..} : stack') =
    if delim == delim'
    then (Combs{segments = unSeq segments, location = (fst openLoc, snd closeLoc), ..}, stack')
    else error "wrong close delimiter"

bufferChild :: Syntax atom -> [StackItem atom] -> [StackItem atom]
bufferChild child' (Item{segments = viewr -> priorSegments :> children, ..} : stack) =
    Item{segments = priorSegments |> (children |> child'), ..} : stack

bufferSegment :: SeparatorType -> [StackItem atom] -> [StackItem atom]
bufferSegment sep' (Item{sep = Nothing, segments = viewr -> priorSegments :> lastSegment, ..} : stack) =
    Item{sep = Just sep', segments = priorSegments |> lastSegment |> Seq.empty, ..} : stack
bufferSegment sep' (Item{sep = sep@(Just sep0), segments = viewr -> priorSegments :> lastSegment,..} : stack) =
    if sep0 == sep' then Item{segments = priorSegments |> lastSegment |> Seq.empty, ..} : stack
        else error "mixed separators" -- FIXME use a real error


{- Show the syntax, but without the location data. -}
quietShow :: Show atom => Syntax atom -> String -- TODO use a pretty-printer
quietShow Atom{..} = show atom
quietShow Combs{..} = concat [open delim, join sep qShowSegments, close delim]
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