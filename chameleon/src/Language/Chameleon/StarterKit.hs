module Language.Chameleon.StarterKit
    ( NumberPart(..), StringPart(..)
    , StarterRaw(..), StarterAtom(..)
    , makeStarterAtom
    , NumberConfig(..), RadixConfig(..)

    , basicConfig, basicNumberConfig
    , basicId
    , basicVariable, basicInteger, basicReal, basicString
    , basicCharEscapes, basicEscapes, basicStringLegal

    , fancyConfig, fancyNumberConfig
    , fancyVariable, fancyNumber, fancyString, fancyChar
    , docstring
    , fancyCharEscapes, fancyEscapes

    , mkToken, mk1Token

    , buildNumber
    , binaryRadix, octalRadix, decimalRadix, hexRadix

    , bindigit, digit, octdigit, hexdigit
    , lowAlpha, highAlpha, alpha
    , alphaNum
    , opSymbol, sepSymbol, trailSymbol

    ) where

import Numeric
import Data.Ratio
import Data.Symbol
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe

import Data.Monoid
import Control.Arrow

import qualified Language.Chameleon.Token.Raw as Raw
import Language.Chameleon.Token (TokenErrorType(..))
import qualified Language.Chameleon.Token as Token
import qualified Language.Chameleon.Syntax as Syntax

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Control.Monad.WarnErrs as WarnErrs


mkToken = Raw.wrap
mk1Token = ((:[]) <$>) . mkToken


data NumberPart
    = RawSign Integer -- NOTE either one or negative one
    | RawRadix Integer
    | RawNumberDelim
    | RawInteger Text
    | RawSeparatrix
    | RawMantissa Text
    | RawExpMark
    | RawExpSign Integer -- NOTE either one or negative one
    | RawExponent Text
    deriving(Eq)

data NumberConfig = NumberConfig
    { radices :: [RadixConfig]
    , defaultRadix :: RadixConfig
    , separatrixChar :: Char
    , delimiterChar :: Char
    }
data RadixConfig = RadixConfig
    { radixMark :: Char
    , radixBase :: Integer
    , radixIsDigit :: Char -> Bool
    , radixFromDigit :: Char -> Integer
    , exponentChar :: Char -- TODO allow multiple chars, each with a different base
    , exponentScale :: Integer
    }

data StringPart
    = RawStringQuote
    | RawStringLiteral Text
    | RawStringEscape Text
    | RawStringLineContinue
    deriving(Eq)


data StarterRaw
    = RawNumber NumberPart
    | RawString StringPart
    | RawChar StringPart
    | RawVariable Symbol

data StarterAtom
    = Integer Integer
    | Real Rational
    | String Text
    | Char Char
    | Variable Symbol
instance Show StarterAtom where
    -- TODO check that this matches the basicConfig
    show (Integer i) = show i
    show (Real r) = show r
    show (String t) = show t
    show (Char c) = show c
    show (Variable x) = unintern x

-- TODO I think these fucntions might need to be in WarnErrs
-- TODO with more atoms, update makeStarterAtom
makeStarterAtom :: [StarterRaw] -> StarterAtom
makeStarterAtom xs@(RawNumber _ : _) = case buildNumber basicNumberConfig (fromRaw <$> xs) of
    Left real -> Real real
    Right integer -> Integer integer
    where fromRaw (RawNumber x) = x
makeStarterAtom xs@(RawString _ : _) = String $ T.concat (fromRaw <$> xs)
    where
    fromRaw (RawString RawStringQuote) = ""
    fromRaw (RawString (RawStringLiteral x)) = x
    fromRaw (RawString (RawStringEscape x)) = x
    fromRaw (RawString RawStringLineContinue) = ""
makeStarterAtom [RawChar RawStringQuote, RawChar x, RawChar RawStringQuote] = Char (theChar x)
    where
    theChar (RawStringLiteral (T.unpack -> [c])) = c
    theChar (RawStringEscape (T.unpack -> [c])) = c
makeStarterAtom [RawVariable x] = Variable x


------ Dealing with Numbers ------

binaryRadix :: RadixConfig
binaryRadix = RadixConfig
        { radixMark = 'b'
        , radixBase = 2
        , radixIsDigit = bindigit
        , radixFromDigit = \c -> fromIntegral $ ord c - ord '0'
        , exponentChar = 'p'
        , exponentScale = 2
        }

octalRadix :: RadixConfig
octalRadix = RadixConfig
        { radixMark = 'o'
        , radixBase = 8
        , radixIsDigit = octdigit
        , radixFromDigit = \c -> fromIntegral $ ord c - ord '0'
        , exponentChar = 'p'
        , exponentScale = 2
        }

decimalRadix :: RadixConfig
decimalRadix = RadixConfig
        { radixMark = 'd'
        , radixBase = 10
        , radixIsDigit = digit
        , radixFromDigit = \c -> fromIntegral $ ord c - ord '0'
        , exponentChar = 'e'
        , exponentScale = 10
        }

hexRadix :: RadixConfig
hexRadix = RadixConfig
        { radixMark = 'x'
        , radixBase = 16
        , radixIsDigit = hexdigit
        , radixFromDigit = fromIntegral . \case
            c   | c `elem` ['0' .. '9'] -> ord c - ord '0'
                | c `elem` ['A' .. 'B'] -> 10 + ord c - ord 'A'
                | c `elem` ['a' .. 'b'] -> 10 + ord c - ord 'a'
        , exponentChar = 'p'
        , exponentScale = 2
        }

-- TODO more base configs (12 (TE, XZ, AB, ↊↋), 60?, 64?)


buildNumber :: NumberConfig -> [NumberPart] -> Either Rational Integer
buildNumber NumberConfig{..} xs =
    let sign = case signParts of { [] -> 1; [sign] -> sign }
        radix = fromMaybe defaultRadix $ case radixParts of { [] -> Nothing; [radix] -> radices `lookupRadix` radix }
        integer = parse radix integerParts
        mantissa = 
            let numer = parse radix mantissaParts
                denom = radixBase radix * fromIntegral (T.length mantissaParts)
            in numer % denom
        expSign = case expSignParts of { [] -> 1; [sign] -> sign }
        exponent = (read . T.unpack) exponentParts -- TODO parse other bases
        isIntegral = not $ RawSeparatrix `elem` xs || expSign < 0
    in if isIntegral
        then Right $ sign * integer
        else Left $
            let signMult = toRational sign
                baseMult = toRational integer + mantissa
                magMult = toRational $ (exponentScale radix % 1) ^^ (expSign * exponent)
            in  product [signMult, baseMult, magMult]
    where
    signParts = concatMap grab xs
        where
        grab (RawSign sign) = [sign]
        grab _ = []
    radixParts = concatMap grab xs
        where
        grab (RawRadix radix) = [radix]
        grab _ = []
    integerParts = T.concat $ grab <$> xs
        where
        grab (RawInteger text) = text
        grab _ = ""
    mantissaParts = T.concat $ grab <$> xs
        where
        grab (RawMantissa text) = text
        grab _ = ""
    expSignParts = concatMap grab xs
        where
        grab (RawExpSign sign) = [sign]
        grab _ = []
    exponentParts = T.concat $ grab <$> xs
        where
        grab (RawExponent text) = text
        grab _ = ""
    parse RadixConfig{..} (T.unpack -> text) =
        let parser = readInt radixBase radixIsDigit (fromIntegral . radixFromDigit)
        in case parser text of { (n, _) : _ -> n}
    [] `lookupRadix` r = Nothing
    (conf : rest) `lookupRadix` r =
        if r == radixBase conf then Just conf else rest `lookupRadix` r


------ Basic Syntax ------

basicConfig :: Token.Config StarterRaw StarterAtom
basicConfig = Token.Config
    { parseId = basicId
    , parseAtoms = P.choice
        [ ((first RawNumber <$>) <$>) <$> basicInteger
        , ((first RawNumber <$>) <$>) <$> basicReal
        , ((first RawString <$>) <$>) <$> basicString '\''
        , ((first RawVariable <$>) <$>) <$> basicVariable
        ]
    , makeAtom = makeStarterAtom
    , openFileAs = "__FILE__"
    , errorOn = [ UnknownInput, NoLebensraum, LeadingAlign ]
    , warnAbout = [ TrailingSpace, NoNewlineAtEof ]
    }

basicNumberConfig :: NumberConfig
basicNumberConfig = NumberConfig
    { radices = []
    , defaultRadix = decimalRadix
    , separatrixChar = '.'
    , delimiterChar = '_'
    }


basicId :: Raw.Parser Token.IdType
basicId = intern . T.unpack <$> (firstLetter <$$> T.cons <*> moreLetters)
    where
    firstLetter = P.satisfy (disj alpha (== '_'))
    moreLetters :: Raw.Parser Text
    moreLetters = P.takeWhileP Nothing (disj alphaNum (== '_'))


basicVariable :: Raw.Parser [Raw.Raw (Symbol, Token.AtomType)]
basicVariable = mk1Token (basicId <$$> (, "variable"))


basicInteger :: Raw.Parser [Raw.Raw (NumberPart, Token.AtomType)]
basicInteger = mk1Token $ (RawInteger <$> P.takeWhile1P Nothing digit) <$$> (, "number")

basicReal :: Raw.Parser [Raw.Raw (NumberPart, Token.AtomType)]
basicReal = do
    whole <- mkToken $ (RawInteger <$> P.takeWhile1P Nothing digit) <$$> (, "number")
    separatrix <- mkToken $ (RawSeparatrix <$ P.string ".") <$$> (, "number")
    mantissa <- mkToken $ (RawMantissa <$> P.takeWhile1P Nothing digit) <$$> (, "number")
    pure [whole, separatrix, mantissa]


basicString :: Char -> Raw.Parser [Raw.Raw (StringPart, Token.AtomType)]
basicString quote = do
    open <- mk1Token $ (RawStringQuote <$ P.char quote) <$$> (, "string-quote")
    texts <- P.many $ mkToken $ P.choice
        [ normalChar <$$> (, "string")
        , doubleChar <$$> (, "string-escape")
        ]
    close <- mk1Token $ (RawStringQuote <$ P.char quote) <$$> (, "string-quote")
    pure $ concat [open, texts, close]
    where
    normalChar, doubleChar :: Raw.Parser StringPart
    normalChar = (RawStringLiteral <$>) $ P.takeWhile1P Nothing (/= quote)
    doubleChar = (RawStringEscape <$>) $ P.try $ P.char quote >> (T.singleton <$> P.char quote)


------ Fancy Syntax ------

fancyConfig :: Token.Config StarterRaw StarterAtom
fancyConfig = Token.Config
    { parseId = basicId
    , parseAtoms = P.choice
        [ ((first RawNumber <$>) <$>) <$> fancyNumber fancyNumberConfig
        , ((first RawString <$>) <$>) <$> fancyString '\"' fancyEscapes basicStringLegal
        , ((first RawString <$>) <$>) <$> docstring ("<<<", ">>>")
        , ((first RawChar <$>) <$>) <$> fancyChar '\'' fancyCharEscapes basicStringLegal
        , ((first RawVariable <$>) <$>) <$> fancyVariable
        ]
    , makeAtom = makeStarterAtom
    , openFileAs = "__FILE__"
    , errorOn = [ UnknownInput, NoLebensraum, LeadingAlign ]
    , warnAbout = [ TrailingSpace, NoNewlineAtEof ]
    }

fancyNumberConfig :: NumberConfig
fancyNumberConfig = NumberConfig
    { radices = [binaryRadix, octalRadix, decimalRadix, hexRadix] -- TODO all the other bases
    , defaultRadix = decimalRadix
    , separatrixChar = '.'
    , delimiterChar = '_'
    }


fancyVariable :: Raw.Parser [Raw.Raw (Symbol, Token.AtomType)]
fancyVariable = mk1Token $ P.choice
    [ (intern . T.unpack <$> parseVariable) <$$> (, "variable")
    , (intern . T.unpack <$> parseOperator) <$$> (, "operator")
    ]
    where
    parseVariable = do
        lead <- T.singleton <$> P.satisfy (\(c :: Char) -> alpha c || c == '_')
        body <- P.takeWhileP Nothing (\(c :: Char) -> alphaNum c || c == '_' || trailSymbol c)
        pure $ lead <> body
    parseOperator = do
        lead <- T.singleton <$> P.satisfy opSymbol
        body <- P.takeWhileP Nothing (\(c :: Char) -> opSymbol c || alphaNum c || trailSymbol c)
        pure $ lead <> body


fancyNumber :: NumberConfig -> Raw.Parser [Raw.Raw (NumberPart, Token.AtomType)]
fancyNumber NumberConfig{..} = P.try $ do
    sign <- P.option [] $ mk1Token $ parseSign RawSign <$$> (, "number-sign")
    (radix, radixConfig@RadixConfig{..}) <- do
        radix_m <- P.optional $ mkToken $ P.choice (aRadix <$> radices)
        pure $ maybe ([], defaultRadix) radixFromTok radix_m
    whole <- parseNumberText RawInteger radixConfig
    mantissa <- P.option [] $ P.try $ do
        separatrix <- mkToken $ (RawSeparatrix <$ P.char separatrixChar) <$$> (, "number-separatrix")
        mantissa <- parseNumberText RawMantissa radixConfig
        pure (separatrix : mantissa)
    exponent <- P.option [] $ do
        mark <- mk1Token $ (RawExpMark <$ P.char exponentChar) <$$> (, "number-exponent-mark")
        sign <- P.option [] $ mk1Token $ parseSign RawExpSign <$$> (, "number-exponent-sign")
        -- TODO exponent radix
        exp <- parseNumberText RawExponent decimalRadix
        pure $ concat [mark, sign, exp]
    pure $ concat [sign, radix, whole, mantissa, exponent]
    where
    parseSign :: (Integer -> NumberPart) -> Raw.Parser NumberPart
    parseSign f = f <$> P.choice [1 <$ P.char '+', -1 <$ P.char '-']
    aRadix :: RadixConfig -> Raw.Parser RadixConfig
    aRadix conf@RadixConfig{..} = conf <$ P.string (T.pack ['0', radixMark])
    radixFromTok :: Raw.Raw RadixConfig -> ([Raw.Raw (NumberPart, Token.AtomType)], RadixConfig)
    radixFromTok (Raw.Raw conf@RadixConfig{..} loc text) = ([Raw.Raw (RawRadix radixBase, "number-radix") loc text], conf)
    parseNumberText :: (Text -> NumberPart) -> RadixConfig -> Raw.Parser [Raw.Raw (NumberPart, Token.AtomType)]
    parseNumberText ctor RadixConfig{..} = P.some $ mkToken $ P.choice
        [ (ctor <$> P.takeWhile1P Nothing radixIsDigit) <$$> (, "number")
        , (RawNumberDelim <$ P.string (T.pack [delimiterChar])) <$$> (, "number-delimiter")
        ]


fancyString :: Char -> [(Char, Text)] -> (Char -> Bool) -> Raw.Parser [Raw.Raw (StringPart, Token.AtomType)]
fancyString quote escapes isLegal = do
    open <- mk1Token $ (RawStringQuote <$ P.char quote) <$$> (, "string-quote")
    texts <- P.many $ mkToken $ P.choice
        [ normalChar <$$> (, "string")
        , lineContinue <$$> (, "string-linecontinue")
        -- TODO ascii escapes
        -- TODO unicode escapes
        , normalEscape <$$> (, "string-escape")
        ]
    close <- mk1Token $ (RawStringQuote <$ P.char quote) <$$> (, "string-quote")
    pure $ concat [open, texts, close]
    where
    normalChar, lineContinue, normalEscape :: Raw.Parser StringPart
    normalChar = (RawStringLiteral <$>) $ P.takeWhile1P Nothing (\c -> c `notElem` [quote, '\\'] && isLegal c)
    lineContinue = do
        P.string "\\\n"
        P.takeWhileP Nothing (== ' ')
        P.char '\\'
        pure RawStringLineContinue
    normalEscape = do
        P.char '\\'
        P.choice $ (\(char, text) -> RawStringEscape text <$ P.char char) <$> escapes

docstring :: (Text, Text) -> Raw.Parser [Raw.Raw (StringPart, Token.AtomType)]
docstring (open, close) = P.try $ do
    (open, quoteText) <- P.try $ do
        openTok <- mkToken $ (RawStringQuote <$ P.string open) <$$> (, "string-quote")
        nameTok <- mkToken $ (RawStringQuote <$ P.takeWhile1P Nothing alphaNum) <$$> (, "string-quote-docstrname")
        pure ([openTok, nameTok], Raw.theText nameTok)
    (indent, spacesText) <- P.choice
        [ do
            tok <- mkToken $ (RawStringQuote <$ P.char '\n') <$$> (, "string-linecontinue")
            pure ([tok], "")
        , P.try $ do
            startTok <- mkToken $ (RawStringQuote <$ P.string "\\\n") <$$> (, "string-linecontinue")
            spacesTok <- mkToken $ (RawStringQuote <$ P.takeWhileP Nothing (== ' ')) <$$> (, "string-linecontinue")
            endTok <- mkToken $ (RawStringQuote <$ P.char '\\') <$$> (, "string-linecontinue")
            pure ([startTok, spacesTok, endTok], (Raw.theText spacesTok <> " "))
        ]
    let parseClose = P.try $ do
            nl <- mkToken $ (RawStringLiteral . T.init <$> P.takeWhile1P Nothing (== '\n')) <$$> (, "string")
            lc <- mkToken $ (RawStringLineContinue <$ (P.string spacesText)) <$$> (, "string-linecontinue")
            nameTok <- mkToken $ (RawStringQuote <$ P.string quoteText) <$$> (, "string-quote-docstrname")
            closeTok <- mkToken $ (RawStringQuote <$ P.string close) <$$> (, "string-quote")
            pure [nl, lc, nameTok, closeTok]
        parseLine = P.try $ do
            nl <- mkToken $ (RawStringLiteral <$> P.takeWhile1P Nothing (== '\n')) <$$> (, "string")
            lc <- mkToken $ (RawStringLineContinue <$ (P.string spacesText)) <$$> (, "string-linecontinue")
            text <- mkToken $ (RawStringLiteral <$> P.takeWhileP Nothing (/= '\n')) <$$> (, "string")
            pure [text, nl, lc]
    let loop = P.optional parseClose >>= \case
            Just closeToks -> pure closeToks
            Nothing -> do
                line <- parseLine
                rest <- loop
                pure $ line ++ rest
    let oddballDocstr = P.try $ do -- NOTE in case the docstring is ended on the very next line
            nameTok <- mk1Token $ (RawStringQuote <$ P.string quoteText) <$$> (, "string-quote-docstrname")
            closeTok <- mk1Token $ (RawStringQuote <$ P.string close) <$$> (, "string-quote")
            pure $ concat [open, indent, nameTok, closeTok]
        normalDocstr = do
            firstLine <- mk1Token $ (RawStringLiteral <$> P.takeWhileP Nothing (/= '\n')) <$$> (, "string")
            moreLines <- loop
            pure $ concat [open, indent, firstLine, moreLines]
    P.choice [oddballDocstr, normalDocstr]


fancyChar :: Char -> [(Char, Char)] -> (Char -> Bool) -> Raw.Parser [Raw.Raw (StringPart, Token.AtomType)]
fancyChar quote escapes isLegal = do
    open <- mkToken $ (RawStringQuote <$ P.char quote) <$$> (, "char-quote")
    theChar <- mkToken $ P.choice
        [ normalChar <$$> (, "char")
        -- TODO ascii escapes
        -- TODO unicode escapes
        , normalEscape <$$> (, "char-escape")
        ]
    close <- mkToken $ (RawStringQuote <$ P.char quote) <$$> (, "char-quote")
    pure [open, theChar, close]
    where
    normalChar, normalEscape :: Raw.Parser StringPart
    normalChar = (RawStringLiteral . T.singleton <$>) $ P.satisfy (\c -> c `notElem` [quote, '\\'] && isLegal c)
    normalEscape = do
        P.char '\\'
        P.choice $ (\(char, text) -> RawStringEscape (T.singleton text) <$ P.char char) <$> escapes

-- TODO docstrings, indented docstrings


basicCharEscapes :: [(Char, Char)]
basicCharEscapes =
    [ ('0', '\x00')
    , ('a', '\x07')
    , ('b', '\x08')
    , ('f', '\x0C')
    , ('n', '\x0A')
    , ('r', '\x0D')
    , ('t', '\x09')
    , ('v', '\x0B')
    , ('\'', '\'')
    , ('\"', '\"')
    , ('\\', '\\')
    ]

basicEscapes :: [(Char, Text)]
basicEscapes = second T.singleton <$> basicCharEscapes

fancyCharEscapes :: [(Char, Char)]
fancyCharEscapes = basicCharEscapes ++
    [ ('e', '\x1B')
    ]

fancyEscapes :: [(Char, Text)]
fancyEscapes = (second T.singleton <$> basicCharEscapes) ++
    [ ('&', "")
    ]

basicStringLegal :: Char -> Bool
basicStringLegal = (/= '\n') -- TODO any whitespace chars other than space are illegal


------ Utilities ------

--FIXME all of these are quite slow tests
bindigit :: Char -> Bool
bindigit = (`elem` ['0', '1'])

digit :: Char -> Bool
digit = (`elem` ['0' .. '9'])

octdigit :: Char -> Bool
octdigit = (`elem` ['0' .. '7'])

hexdigit :: Char -> Bool
hexdigit c = c `elem` ['0' .. '9'] || c `elem` ['A' .. 'F'] || c `elem` ['a' .. 'f']


lowAlpha :: Char -> Bool
lowAlpha = (`elem` ['a' .. 'z'])

highAlpha :: Char -> Bool
highAlpha = (`elem` ['A' .. 'Z'])

alpha :: Char -> Bool
alpha = disj lowAlpha highAlpha

alphaNum :: Char -> Bool
alphaNum = disj alpha digit

opSymbol :: Char -> Bool
opSymbol = (`elem` ("~!$%^&*-=+|<>/?" :: [Char])) -- NOTE everything on my keeb that isn't a letter, number, or ,.;:`'"_@#\()[]{}

sepSymbol :: Char -> Bool
sepSymbol = (`elem` ("-_" :: [Char]))

trailSymbol :: Char -> Bool
trailSymbol = (`elem` ("!?\'" :: [Char]))


disj :: (a -> Bool) -> (a -> Bool) -> a -> Bool
disj p q x = p x || q x

(<$$>) = flip (<$>)
