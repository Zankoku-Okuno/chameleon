module Language.Chameleon.Token.Common
    ( module Data.Symbol
    , module Text.Megaparsec.Pos
    , IdType
    , AtomType
    , Location(..)
    , mergeLoc
    , humanLoc
    ) where

import Data.Symbol
import Text.Megaparsec.Pos

type IdType = Symbol
type AtomType = Symbol

type Line = Int
type Col = Int
data Location = Location
    { loc_file :: FilePath
    , loc_start :: (Line, Col)
    , loc_end :: (Line, Col)
    }
    deriving(Eq, Read, Show)

instance Ord Location where
    a `compare` b = (loc_start a, loc_end a) `compare` (loc_start b, loc_end b)

mergeLoc :: Location -> Location -> Location
mergeLoc Location{loc_file=loc_file, loc_start=loc_start} Location{loc_end=loc_end} = Location{..}

humanLoc :: Location -> String
humanLoc Location{..} = concat ["file ", loc_file, " line:col ", humanSpot loc_start, "-", humanSpot loc_end]
    where
    humanSpot (line, col) = concat [show line, ":", show col]
