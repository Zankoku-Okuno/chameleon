module Main where

import qualified Data.Text as T
import Experiment

main :: IO ()
main = test $ T.unlines
    [ "val three 3"
    --, "doc val x \"Mesdames, messiurres, bon soir!\""
    , "val eight ap(op(add) three 5)"
    , "val add1 let:"
    , "  val add fun(a b, ap(op(add) a b))"
    , "  val add1 fun(x, ap(add x 1))"
    , "  in"
    , "  add1"
    , "let:"
    , "  val add fun(a b, ap(op(add) a b))"
    , "  in"
    , "  val incr fun(a, ap(add a 1))"
    , "  val decr fun(a, ap(add a -1))"
    , ""
    ]