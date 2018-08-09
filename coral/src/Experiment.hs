module Experiment where -- TODO delete me

import Data.Symbol

import Data.Compos

import Control.Monad.WarnErrs

import Language.Chameleon.Token (Location)
import Language.Coral.Syntax.Abstract
import Language.Coral.Syntax.Parse


test input = case runWarnErrs $ parse Nothing input of
    Left err -> error $ show err
    Right (decls, warns) -> do
        print `mapM_` warns
        (putStrLn . renderBack) `mapM_` decls
