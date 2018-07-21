module Experiment where -- TODO delete me

import Data.Symbol

import Data.Compos

import Control.Monad.WarnErrs

import Language.Coral.Syntax.Abstract
import Language.Coral.Syntax.Parse


test input = case runWarnErrs $ parse Nothing input of
    Left err -> error $ show err
    Right (tree, warns) -> do
        print `mapM_` warns
        putStrLn $ renderBack tree
        print $ decldVals tree
        print $ docdVals tree


decldVals :: Ast c -> [Symbol]
decldVals (DeclVal x _) = [x]
decldVals tree = composFold decldVals tree

docdVals :: Ast c -> [Symbol]
docdVals (DeclValDoc x _) = [x]
docdVals tree = composFold docdVals tree
