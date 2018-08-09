module Language.Coral.Syntax.Parse where

import Data.Symbol
import Data.Text (Text)
import qualified Data.Text as T

import Data.List

import Control.Monad.WarnErrs

import Language.Coral.Syntax.Abstract hiding (Ast, Decl, Expr)
import qualified Language.Coral.Syntax.Abstract as Ast
import Language.Chameleon.Token (tokenize, Location(..), TokenError)
import Language.Chameleon.Syntax
import Language.Chameleon.StarterKit


type Ann = Location

type Ast cat = Ast.Ast Ann cat
type Expr = Ast Expr_
type Decl = Ast Decl_


data CompileError -- FIXME move this to an error module
    = TokenError TokenError
    | SyntaxError SyntaxError
    | GrammarError GrammarError
    -- TODO scope error
    -- TODO type error
    deriving(Read, Show)

data GrammarError = TODO
  deriving(Read, Show)


parse :: Maybe FilePath -> Text -> WarnErrs CompileError [Decl]
parse filepath input = do
    tokens <- tokenize fancyConfig filepath input `mapErrs` TokenError
    nest <- parseNest tokens `mapErrs` SyntaxError
    parseFile nest `mapErrs` GrammarError


parseFile :: Nest StarterAtom -> WarnErrs GrammarError [Decl]
parseFile Comb{id = "__FILE__", segments = decls, ..} = parseDecl `mapM` decls

parseDecl :: [Nest StarterAtom] -> WarnErrs GrammarError Decl
parseDecl [ Atom{atom = Variable "val", ..}
          , Atom{atom = Variable x, location = Location{loc_end = end}}
          , body
          ] = DeclVal location{ loc_end = end } x <$> parseExpr body
-- parseDecl [ Atom{atom = Variable "doc", ..}
--           , Atom{atom = Variable "val"}
--           , Atom{atom = Variable x}
--           , Atom{atom = String docstr, location = Location{loc_end = end} }
--           ] = pure $ DeclValDoc location{ loc_end = end } x docstr
parseDecl [Comb{id = "let", segments = break (isKeyword "in") -> (local, tail -> ds), ..}]
    = DeclLet location <$> parseDecl `traverse` local <*> parseDecl `traverse` ds

parseExpr :: Nest StarterAtom -> WarnErrs GrammarError Expr
parseExpr Atom{atom = Integer i, ..}
    = pure $ ExprConst location (IntConst i)
parseExpr Comb{id = "op", segments = [[Atom {atom = opname}]], ..}
    = ExprPrim location <$> parseOpname opname
    where
    parseOpname (String name) = pure $ (intern . T.unpack) name
    parseOpname (Variable name) = pure name
    parseOpname _ = err $ TODO
parseExpr Atom{atom = Variable x, ..}
    = pure $ ExprVar location x
parseExpr Comb{id = "ap", segments = [e : es], ..} | notNull es
    = ExprApp location <$> parseExpr e <*> parseExpr `traverse` es
parseExpr Comb{id = "fun", segments = [xs, [e]], ..} | notNull xs
    = ExprLam location <$> parseVar `traverse` xs <*> parseExpr e
parseExpr it@Comb{id = "fun"} = error $ "bad function: " ++ show it
parseExpr Comb{id = "let", segments = break (isKeyword "in") -> (ds, tail -> [[e]]), ..}
    = ExprLet location <$> parseDecl `traverse` ds <*> parseExpr e
parseExpr it = error $ "parseExpr: " ++ show it

parseVar :: Nest StarterAtom -> WarnErrs GrammarError Name
parseVar Atom{atom = Variable x} = pure x
parseVar _ = err $ TODO


renderBack :: Ast.Ast cat ann -> String
renderBack (DeclVal _ x e) = concat ["val ", unintern x, " ", renderBack e]
-- renderBack (DeclValDoc _ x text) = concat ["doc val ", unintern x, " ", show text]
renderBack (DeclLet _ local ds) = concat
    [ "let{"
    , intercalate "; " $ renderBack <$> local
    , " ; in ; "
    , intercalate "; " $ renderBack <$> ds
    , "}"
    ]
renderBack (ExprConst _ c) = case c of
    IntConst i -> show i
renderBack (ExprPrim _ op) = concat ["op(", show op, ")"]
renderBack (ExprVar _ x) = unintern x
renderBack (ExprApp _ e es) = concat ["ap(", intercalate " " $ renderBack <$> e:es, ")"]
renderBack (ExprLam _ xs e) = concat ["fun(", intercalate " " $ unintern <$> xs, ", ", renderBack e, ")"]
renderBack (ExprLet _ ds e) = concat
    [ "let{"
    , intercalate "; " $ renderBack <$> ds
    , " ; in ; "
    , renderBack e
    , "}"
    ]





isKeyword :: Symbol -> [Nest StarterAtom] -> Bool
isKeyword kw [Atom{atom = Variable id}] | id == kw = True
isKeyword _ _ = False

notNull = not . null
