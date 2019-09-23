module Experiment where -- TODO delete me

-- import Data.Symbol

import Control.Monad.WarnErrs

import Language.Chameleon.Token (Location)
import Language.Coral.Syntax.Abstract
import Language.Coral.Syntax.Parse
import Language.Coral.Syntax.Show


test input = case runWarnErrs $ parse Nothing input of
    Left err -> error $ show err
    Right (decls, warns) -> do
        print `mapM_` warns
        (putStrLn . quietShow) `mapM_` decls
