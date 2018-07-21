module Language.Coral.Syntax.Parse where

import Data.Symbol
import Data.Text (Text)
import qualified Data.Text as T

import Data.List

import Control.Monad.WarnErrs

import Language.Coral.Syntax.Abstract
import Language.Chameleon.Token (tokenize, TokenError)
import Language.Chameleon.Syntax
import Language.Chameleon.StarterKit

data CompileError -- FIXME move this to an error module
    = TokenError TokenError
    | SyntaxError SyntaxError
    | GrammarError GrammarError
    -- TODO type error
    deriving(Read, Show)

data GrammarError = TODO
  deriving(Read, Show)

parse :: Maybe FilePath -> Text -> WarnErrs CompileError File
parse filepath input = do
    tokens <- tokenize fancyConfig filepath input `mapErrs` TokenError
    nest <- parseNest tokens `mapErrs` SyntaxError
    parseFile nest `mapErrs` GrammarError


parseFile :: Nest StarterAtom -> WarnErrs GrammarError File
parseFile Comb{id = "__FILE__", segments = decls} = File <$> parseDecl `mapM` decls

parseDecl :: [Nest StarterAtom] -> WarnErrs GrammarError Decl
parseDecl [ Atom{atom = Variable "val"}
          , Atom{atom = Variable x}
          , body
          ] = DeclVal x <$> parseExpr body
parseDecl [ Atom{atom = Variable "doc"}
          , Atom{atom = Variable x}
          , Atom{atom = String docstr}
          ] = pure $ DeclValDoc x docstr
parseDecl [ Atom{atom = Variable "doc"}
          , Atom{atom = Variable "val"}
          , Atom{atom = Variable x}
          , Atom{atom = String docstr}
          ] = pure $ DeclValDoc x docstr

parseExpr :: Nest StarterAtom -> WarnErrs GrammarError Expr
parseExpr Atom{atom = Integer i} = pure $ ExprConst (IntConst i)

renderBack :: Ast cat -> String
renderBack (File decls) = "\n" `intercalate` (renderBack <$> decls)
renderBack (DeclVal x e) = concat ["val ", unintern x, " ", renderBack e]
renderBack (DeclValDoc x text) = concat ["doc val ", unintern x, " ", show text]
renderBack (ExprConst c) = case c of
    IntConst i -> show i
