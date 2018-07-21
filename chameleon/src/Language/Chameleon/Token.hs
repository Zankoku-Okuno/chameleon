module Language.Chameleon.Token
    ( module Language.Chameleon.Token.Common
    , Token(..), TokenType(..), Cooked.DelimiterType(..), Cooked.SeparatorType(..)
    , TokenError(..), TokenErrorType(..)
    , Config(..)
    , tokenize
    ) where

import Data.List

import Control.Monad.WarnErrs

import Data.Text (Text)

import Language.Chameleon.Token.Common
import Language.Chameleon.Token.Raw (Raw)
import qualified Language.Chameleon.Token.Raw as Raw
import Language.Chameleon.Token.Cooked (Token(..), TokenType(..), TokenError(..), TokenErrorType(..))
import qualified Language.Chameleon.Token.Cooked as Cooked

data Config raw atom = Config
    { parseId :: Raw.Parser IdType
    , parseAtoms :: Raw.Parser [Raw (raw, AtomType)]
    , makeAtom :: [raw] -> atom
    , openFileAs :: Symbol
    , errorOn :: [TokenErrorType]
    , warnAbout :: [TokenErrorType]
    }

tokenize :: Config raw atom -> Maybe FilePath -> Text -> WarnErrs TokenError [Token atom]
tokenize Config{..} filepath input = do
    let (errors, nonErrors) = partition ((`elem` errorOn) . Cooked.theErrType) issues
        (warnings, ignoredIssues) = partition ((`elem` warnAbout) . Cooked.theErrType) nonErrors
    warns warnings
    errs errors
    pure cooked
    where
    (cooked, issues) = Cooked.clean Cooked.Config{Cooked.makeAtom = makeAtom, Cooked.openFileAs = openFileAs} raw
    raw = Raw.tokenize filepath Raw.Config{Raw.parseId = parseId, Raw.parseAtoms = parseAtoms} input