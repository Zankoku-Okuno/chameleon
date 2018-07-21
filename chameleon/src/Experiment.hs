-- TODO delete me
module Experiment where

import Control.Arrow
import Control.Monad.WarnErrs

import Language.Chameleon.Token
import Language.Chameleon.Syntax
import Language.Chameleon.StarterKit


test input = case runWarnErrs $ tokenize basicConfig Nothing input of
    Left err -> error $ show err
    Right (toks, warns) -> do
        print `mapM_` warns
        case runWarnErrs $ parseNest toks of
            Left err -> error $ show err
            Right (syntax, warns) -> do
                print `mapM_` warns
                putStrLn $ quietShow syntax
                -- putStrLn $ show syntax
