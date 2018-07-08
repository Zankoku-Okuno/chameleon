module Language.Chameleon.Token.Cooked
    ( Token(..)
    , thePayload
    , TokenType(..), DelimiterType(..), SeparatorType(..)
    , TokenError(..), TokenErrorType(..)
    , theErrType, theErrLoc
    , Config(..)
    , clean
    ) where

import Data.Maybe
import Data.List

import Control.Arrow
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T

import Language.Chameleon.Token.Common
import Language.Chameleon.Token.Raw (Raw(..), RawToken, RawTokenType)
import qualified Language.Chameleon.Token.Raw as Raw


data Token atom = Tok (TokenType atom) Location
    deriving(Read, Show)

thePayload :: Token atom -> TokenType atom
thePayload (Tok it _) = it


data TokenType atom
    = Atom atom
    | Open IdType DelimiterType
    | Separate SeparatorType
    | Close DelimiterType
    deriving(Read, Show)

data DelimiterType
    = Paren
    | Bracket
    | Brace
    | Indent Int
    | File
    deriving(Eq, Read, Show)

data SeparatorType
    = Comma
    | Semicolon
    | Newline Int
    deriving(Eq, Read, Show)


data TokenError = TokErr TokenErrorType Location
    deriving(Eq, Read, Show)
data TokenErrorType
    = UnknownInput
    | NoLebensraum
    | LeadingAlign
    | TrailingSpace
    | NoNewlineAtEof
    deriving(Eq, Read, Show)

theErrType :: TokenError -> TokenErrorType
theErrType (TokErr t _) = t

theErrLoc :: TokenError -> Location
theErrLoc (TokErr _ loc) = loc


data Config atom raw = Config
    { makeAtom :: [raw] -> atom
    , openFileAs :: Symbol
    }


clean :: Config atom raw -> [RawToken raw] -> ([Token atom], [TokenError])
clean config allRaw =
    let (rawErrs, raw) = partition Raw.isError allRaw
        lines = lineTokens raw
        sofLoc = Raw.theLocation (head $ snd $ head lines)
        eofLoc = Raw.theLocation (last $ snd $ last lines)
        ---- Cook Tokens
        minized = (unlineTokens . removeBoring) lines
        cooked = runClean config $ do
            main <- makeClean minized
            endDedents <- drainDedents 0 eofLoc
            pure $ concat [[Tok (Open (openFileAs config) File) sofLoc], main, endDedents, [Tok (Close File) eofLoc]]
        ---- Cook Errors
        charErrs = TokErr UnknownInput <$> mergeRawErrors rawErrs
        lebensraumErrs = TokErr NoLebensraum <$> concatMap (warnNoLebensraum . snd) lines
        alignErrs = TokErr LeadingAlign <$> concatMap (warnLeadingAlign . snd) lines
        trailingSpaceErrs = TokErr TrailingSpace <$> warnTrailingSpace raw
        eofNlErr = TokErr NoNewlineAtEof <$> warnNoNewlineAtEof (snd $ last lines)
        allErrs = charErrs ++ lebensraumErrs ++ alignErrs ++ trailingSpaceErrs ++ eofNlErr
        cookedErrs = (sortOn theErrLoc) allErrs
    in (cooked, cookedErrs)


type Indent = (Int, Location)
type Line raw = (Indent, [RawToken raw])
lineTokens :: [RawToken raw] -> [Line raw]
lineTokens input = case break isNewline input of
        (preLine, nl : rest) -> let line = preLine ++ [nl]
            in mkLine line : lineTokens rest
        (line, []) -> [mkLine line]
    where
    mkLine (Raw (Raw.Indentation level) loc : line) = ((level, loc), line)
    mkLine line@(Raw _ (loc, _) : _) = ((0, (loc, loc)), line)
    mkLine [] = error "internal error"
    isNewline (Raw Raw.Newline _) = True
    isNewline _ = False

-- after lineTokens, there should be no more indent tokens




mergeRawErrors :: [RawToken raw] -> [Location]
mergeRawErrors [] = []
mergeRawErrors [Raw Raw.Error loc] = [loc]
mergeRawErrors (Raw Raw.Error loc : rest) =
    let (merged, continue) = chainErrs loc rest
    in merged : mergeRawErrors continue
    where
    chainErrs :: Location -> [RawToken raw] -> (Location, [RawToken raw])
    chainErrs (startLoc, midLoc1) (Raw Raw.Error (midLoc2, endLoc) : rest)
        | midLoc1 == midLoc2 = chainErrs (startLoc, endLoc) rest
    chainErrs loc rest = (loc, rest)

warnLeadingAlign :: [RawToken raw] -> [Location]
warnLeadingAlign (Raw Raw.AlignMark loc : _) = [loc]
warnLeadingAlign _ = []

warnTrailingSpace :: [RawToken raw] -> [Location]
warnTrailingSpace raw = Raw.theLocation <$> filter Raw.isTrailingSpace raw

warnNoNewlineAtEof :: [RawToken raw] -> [Location]
warnNoNewlineAtEof [Raw Raw.EndFile _] = []
warnNoNewlineAtEof line = [Raw.theLocation $ last line]

warnNoLebensraum :: [RawToken raw] -> [Location]
warnNoLebensraum (before : Raw (Raw.Id _) loc2 : rest)
    | isCrowdedBefore before = (fst $ Raw.theLocation before, snd loc2) : rec
    | otherwise = rec
    where rec = warnNoLebensraum rest
warnNoLebensraum (Raw (Raw.Separate _) loc1 : after : rest)
    | isCrowdedAfter after = (fst loc1, snd $ Raw.theLocation after) : rec
    | otherwise = rec
    where rec = warnNoLebensraum (after : rest)
warnNoLebensraum (Raw (Raw.Close _) loc1 : after : rest)
    | isCrowdedAfter after = (fst loc1, snd $ Raw.theLocation after) : rec
    | otherwise = rec
    where rec = warnNoLebensraum (after : rest)
warnNoLebensraum (before : Raw (Raw.Atom _) loc2 : rest)
    | isCrowdedBefore before = (fst $ Raw.theLocation before, snd $ loc2) : rec
    | otherwise = rec
    where rec = warnNoLebensraum rest
warnNoLebensraum (Raw (Raw.Atom _) loc1 : after : rest)
    | isCrowdedAfter after = (fst loc1, snd $ Raw.theLocation after) : rec
    | otherwise = rec
    where rec = warnNoLebensraum (after : rest)
warnNoLebensraum (_ : rest) = warnNoLebensraum rest
warnNoLebensraum [] = []

isCrowdedAfter :: RawToken raw -> Bool
isCrowdedAfter (Raw (Raw.Atom _) _) = True
isCrowdedAfter (Raw (Raw.Id _) _) = True
isCrowdedAfter _ = False

isCrowdedBefore :: RawToken raw -> Bool
isCrowdedBefore (Raw (Raw.Atom _) _) = True
isCrowdedBefore (Raw (Raw.Close _) _) = True
isCrowdedBefore _ = False


removeBoring :: [Line raw] -> [Line raw]
removeBoring lines = exclude (null . snd) $ second (exclude isIgnorable) <$> lines

isIgnorable :: RawToken raw -> Bool
isIgnorable (Raw Raw.Newline _) = True
isIgnorable (Raw Raw.InlineSpace _) = True
-- NOTE indentation isn't boring: it can be a sign of an internal error
isIgnorable (Raw Raw.LineContinue _) = True
isIgnorable (Raw Raw.TrailingSpace _) = True
isIgnorable (Raw Raw.AlignMark _) = True
isIgnorable (Raw (Raw.SectionMark _ _) _) = True
isIgnorable (Raw Raw.EndFile _) = True
isIgnorable (Raw Raw.CommentText _) = True
isIgnorable (Raw (Raw.Open Raw.Comment) _) = True
isIgnorable (Raw (Raw.Close Raw.Comment) _) = True
isIgnorable _ = False


unlineTokens :: [Line raw] -> [RawToken raw]
unlineTokens = concatMap restoreIndentation
    where
    restoreIndentation ((level, loc), line) = Raw (Raw.Indentation level) loc : line


makeClean :: [RawToken raw] -> Clean atom raw [Token atom]
makeClean input@(LeadingOpen id (Indent level) loc rest) = do
    diff <- compareIndent level
    if diff == GT
    then do
        newIndent level
        let tok = Tok (Open id $ Indent level) loc
        (tok:) <$> makeClean rest
    else do
        let (backtrack, backloc) = restOfBadIndent input
        toks <- drainDedents level backloc
        (toks++) <$> makeClean backtrack
makeClean (LeadingIndent level loc rest backtrack) = do
    diff <- compareIndent level
    case diff of
        GT -> makeClean rest
        EQ -> do
            let tok = Tok (Separate $ Newline level) loc
            (tok:) <$> makeClean rest
        LT -> do
            level' <- fromMaybe (error "internal error") <$> popIndent
            let tok = Tok (Close $ Indent level') loc
            (tok:) <$> makeClean backtrack
makeClean (LeadingOpen id delim loc rest) = do
    let tok = Tok (Open id delim) loc
    (tok:) <$> makeClean rest
makeClean (LeadingSeparate delim loc rest) = do
    let tok = Tok (Separate delim) loc
    (tok:) <$> makeClean rest
makeClean (LeadingClose delim loc rest) = do
    let tok = Tok (Close delim) loc
    (tok:) <$> makeClean rest
makeClean (LeadingAtom parts loc rest) = do
    makeAtom <- gets makeAtom
    let tok = Tok (Atom $ makeAtom (Raw.thePayload <$> parts)) loc
    (tok:) <$> makeClean rest
makeClean [] = pure []

drainDedents :: Int -> Location -> Clean atom raw [Token atom]
drainDedents level loc = loop
    where
    loop = popIndent >>= \case
        Just level' | level' > level ->
            let tok = Tok (Close $ Indent level') loc
            in (tok:) <$> loop
        _ -> pure []

pattern LeadingOpen :: IdType -> DelimiterType -> Location -> [RawToken raw] -> [RawToken raw]
pattern LeadingOpen id delim loc rest <- (fromOpen -> Just (id, delim, loc, rest))
fromOpen (Raw (Raw.Id id) (startLoc, _)
        : Raw (Raw.Open Raw.Indent) _
        : Raw (Raw.Indentation level) (_, endLoc)
        : rest) = Just (id, Indent level, (startLoc, endLoc), rest)
fromOpen (Raw (Raw.Id id) (startLoc, _)
        : Raw (Raw.Open (xlateDelim -> Just delim)) (_, endLoc)
        : rest) = Just (id, delim, (startLoc, endLoc), rest)
fromOpen _ = Nothing

restOfBadIndent :: [RawToken raw] -> ([RawToken raw], Location)
restOfBadIndent (Raw (Raw.Id _) _ : Raw (Raw.Open Raw.Indent) _ : rest@(Raw (Raw.Indentation _) loc : _)) = (rest, loc)
restOfBadIndent _ = error "internal error"

pattern LeadingSeparate :: SeparatorType -> Location -> [RawToken raw] -> [RawToken raw]
pattern LeadingSeparate sep loc rest <- (fromSep -> Just (sep, loc, rest))
fromSep (Raw (Raw.Separate (xlateSep -> sep)) loc : rest) = Just (sep, loc, rest)
fromSep _ = Nothing

pattern LeadingClose :: DelimiterType -> Location -> [RawToken raw] -> [RawToken raw]
pattern LeadingClose delim loc rest <- (fromClose -> Just (delim, loc, rest))
fromClose (Raw (Raw.Close (xlateDelim -> Just delim)) loc : rest) = Just (delim, loc, rest)
fromClose _ = Nothing

pattern LeadingIndent :: Int -> Location -> [RawToken raw] -> [RawToken raw] -> [RawToken raw]
pattern LeadingIndent level loc rest backtrack <- (fromIndent -> Just (level, loc, rest, backtrack))
fromIndent backtrack@(Raw (Raw.Indentation level) loc : rest) = Just (level, loc, rest, backtrack)
fromIndent _ = Nothing

pattern LeadingAtom :: [Raw raw] -> Location -> [RawToken raw] -> [RawToken raw]
pattern LeadingAtom parts loc rest <- (fromAtom -> Just (parts, loc, rest))
fromAtom (Raw (Raw.Atom parts) loc : rest) = Just (parts, loc, rest)
fromAtom _ = Nothing

xlateDelim :: Raw.DelimiterType -> Maybe DelimiterType
xlateDelim Raw.Paren = Just Paren
xlateDelim Raw.Bracket = Just Bracket
xlateDelim Raw.Brace = Just Brace
xlateDelim _ = Nothing

xlateSep :: Raw.SeparatorType -> SeparatorType
xlateSep Raw.Comma = Comma
xlateSep Raw.Semicolon = Semicolon


newtype Clean atom raw a = Clean { unClean :: (Config atom raw, [Int]) -> (a, [Int]) }
instance Functor (Clean atom raw) where
    fmap f (Clean action) = Clean $ \input -> first f $ action input
instance Applicative (Clean atom raw) where
    pure x = Clean $ \(_, stack) -> (x, stack)
    (<*>) = ap
instance Monad (Clean atom raw) where
    Clean action >>= k = Clean $ \(config, stack) ->
        case action (config, stack) of
            (x, stack') -> unClean (k x) (config, stack')

runClean :: Config atom raw -> Clean atom raw a -> a
runClean config (Clean action) = fst $ action (config, []) -- TODO check the stack is emptied

gets :: (Config atom raw -> b) -> Clean atom raw b
gets f = Clean $ \(config, stack) -> (f config, stack)

newIndent :: Int -> Clean atom raw ()
newIndent top' = Clean $ \(_, stack) -> ((), top':stack)

compareIndent :: Int -> Clean atom raw Ordering
compareIndent x = Clean $ \(_, stack) -> 
    let result = case stack of
                    [] -> x `compare` 0
                    (y : _) -> x `compare` y
    in (result, stack)

popIndent :: Clean atom raw (Maybe Int)
popIndent = Clean $ \(_, stack) -> case stack of
    [] -> (Nothing, stack)
    top : stack' -> (Just top, stack')



exclude p = filter (not . p)
