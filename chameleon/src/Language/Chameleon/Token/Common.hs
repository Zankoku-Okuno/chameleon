module Language.Chameleon.Token.Common
    ( module Data.Symbol
    , module Text.Megaparsec.Pos
    , IdType
    , Location
    ) where

import Data.Symbol
import Text.Megaparsec.Pos

type IdType = Symbol
type Location = (SourcePos, SourcePos)
