module Experiment where

import Data.Symbol
import Control.Arrow
import Control.Monad.WarnErrs

import Text.Megaparsec (some)
import Text.Megaparsec.Char
import qualified Language.Chameleon.Token.Raw as Raw
import qualified Language.Chameleon.Token.Cooked as Cooked
import Language.Chameleon.Token
import Language.Chameleon.Syntax

conf :: Config Int Int
conf = Config
    { parseId = intern <$> some letterChar
    , parseAtoms = ((:[]) <$>) . Raw.wrap $ 0 <$ string "0"
    , makeAtom = \_ -> 0
    , openFileAs = "__file__"
    , errorOn = [Cooked.UnknownInput, Cooked.NoLebensraum]
    , warnAbout = [Cooked.LeadingAlign]
    }

test input = case runWarnErrs $ tokenize conf Nothing input of
    Left err -> error $ show err
    Right (toks, warns) -> do
        let syntax = parse toks
        putStrLn $ quietShow syntax
        putStrLn $ show syntax
