-- FIXME sometimes, I'm using '\n' instead of a platform-agnostic newline
module Language.Chameleon.Token.Raw
    ( Raw(..)
    , thePayload, theLocation
    , RawToken
    , RawTokenType(..), DelimiterType(..), SeparatorType(..)
    , isTrailingSpace, isError
    , Config(..)
    , tokenize
    , Parser, TokenError, wrap
    ) where

import Data.Void
import Data.Maybe
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set

import Text.Megaparsec
import Text.Megaparsec.Char

import Language.Chameleon.Token.Common


type Parser = Parsec Void Text
type TokenError = ParseError Char Void


data Raw a = Raw a Location -- NOTE I'm not keeping the original text around; it can be looked up in the original file if needed.
    deriving(Read, Show)

thePayload :: Raw a -> a
thePayload (Raw it _) = it

theLocation :: Raw a -> Location
theLocation (Raw _ it) = it

type RawToken atom = Raw (RawTokenType atom)

data RawTokenType atom
    -- Whitespace
    = Newline
    | Indentation Int
    | InlineSpace
    | LineContinue
    | TrailingSpace
    | AlignMark
    | SectionMark Int Text
    | EndFile
    -- Punctuation
    | Open DelimiterType
    | Close DelimiterType
    | Separate SeparatorType
    -- Content
    | CommentText
    | Id IdType
    | Atom [Raw atom]
    -- Error Token
    | Error
    deriving(Read, Show)

data DelimiterType
    = Paren
    | Bracket
    | Brace
    | Indent
    | Comment
    deriving(Eq, Read, Show)

data SeparatorType
    = Comma
    | Semicolon
    deriving(Eq, Read, Show)

isTrailingSpace :: RawToken atom -> Bool
isTrailingSpace (Raw TrailingSpace _) = True
isTrailingSpace _ = False

isError :: RawToken atom -> Bool
isError (Raw Error _) = True
isError _ = False



data Config atom = Config
    { parseId :: Parser IdType
    , parseAtoms :: Parser [Raw atom]
    }

tokenize :: Maybe FilePath -> Config atom -> Text -> [RawToken atom]
tokenize filepath_m config input = case parse parser filepath input of
        Left err -> error "internal error"
        Right toks -> toks
    where
    parser = parseFile config
    filepath = fromMaybe "" filepath_m


parseFile :: Config atom -> Parser [RawToken atom]
parseFile config = do
    tokenss <- many (parseTokens config)
    end <- singleton <$> wrap (EndFile <$ eof)
    pure $ concat tokenss ++ end

parseTokens :: Config atom -> Parser [RawToken atom]
parseTokens Config{..} =
        parseWhitespace
    <|> parseComment
    <|> parseOpen parseId
    <|> parseClose
    <|> parseSeparate
    <|> singleton <$> wrap (Atom <$> parseAtoms)
    <|> singleton <$> wrap (Error <$ anyChar) -- NOTE must be last


parseOpen :: Parser IdType -> Parser [RawToken atom]
parseOpen parseId = try $ do
    id <- wrap $ Id <$> parseId
    delim <- wrap open
    pure [id, delim]
    where
    open = Open <$> choice
        [ Paren <$ char '('
        , Bracket <$ char '['
        , Brace <$ char '{'
        , Indent <$ char ':'
        ]

parseClose :: Parser [RawToken atom]
parseClose = singleton <$> wrap close
    where
    close = Close <$> choice
        [ Paren <$ char ')'
        , Bracket <$ char ']'
        , Brace <$ char '}'
        ]

parseSeparate :: Parser [RawToken atom]
parseSeparate = singleton <$> wrap sep
    where
    sep = Separate <$> choice
        [ Comma <$ char ','
        , Semicolon <$ char ';'
        ]

parseComment :: Parser [RawToken atom]
parseComment = blockComment <|> lineComment -- NOTE the order is important here
    where
    lineComment = do
        open <- wrap $ Open Comment <$ char '#'
        text <- wrap $ CommentText <$ takeWhileP Nothing (/= '\n')
        close <- wrap $ pure (Close Comment)
        pure [open, text, close]
    blockComment = do
        open <- singleton <$> (wrap $ Open Comment <$ string "#{")
        inner <- concat <$> many (innerText <|> blockComment)
        close <- singleton <$> (wrap $ Close Comment <$ string "}#")
        pure $ concat [open, inner, close]
        where
        innerText = singleton <$> wrap (CommentText <$ some textElems)
        textElems = choice
            [ void $ takeWhile1P Nothing (`notElem` ['#', '}'])
            , void . try $ char '#' >> notFollowedBy (char '{')
            , void . try $ char '}' >> notFollowedBy (char '#')
            ]

parseWhitespace :: Parser [RawToken atom]
parseWhitespace = choice $
    [ trailingSpace
    , indentation
    , sectionMark
    , singleton <$> wrap (InlineSpace <$ inlineSpace)
    , singleton <$> wrap (Newline <$ newline)
    , singleton <$> wrap (LineContinue <$ lineContinue)
    , singleton <$> wrap (AlignMark <$ alignMark)
    ]
    where
    inlineSpace = takeWhile1P (Just "space") (== ' ')
    lineContinue = try $ char '\\' >> newline >> optional inlineSpace
    alignMark = char '\\' >> lookAhead inlineSpace
    indentation = try $ do
        atStartLine
        singleton <$> wrap (Indentation . T.length <$> inlineSpace)
    sectionMark = (singleton <$>) . wrap $ do
        atStartLine
        char '\\'
        level <- T.length <$> takeWhile1P (Just "space") (== '#')
        text <- takeWhileP Nothing (/= '\n')
        pure $ SectionMark level text -- FIXME I may as well trim the text here
    trailingSpace = try $ do
        space <- wrap (TrailingSpace <$ inlineSpace)
        atLineEnd
        pure [space]


wrap :: Parser a -> Parser (Raw a)
wrap payload = try $ do
    startPos <- getPosition
    payload <- payload
    endPos <- getPosition
    pure $ Raw payload (startPos, endPos)

-- wrapAtom :: Raw atom -> RawToken atom
-- wrapAtom (Raw atom loc) = Raw (Atom atom) loc


singleton :: a -> [a]
singleton = (:[])

atStartLine = do
    startCol <- sourceColumn <$> getPosition
    when (startCol /= pos1) $ failure Nothing Set.empty
    pure ()

atLineEnd = lookAhead . try $ void eol <|> eof
