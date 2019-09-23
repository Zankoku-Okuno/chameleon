module Language.Coral.Syntax.Parse where

import Data.Symbol
import Data.Text (Text)
import qualified Data.Text as T

import Language.Coral.Frontend.Error
import Language.Coral.Syntax.Abstract
import Language.Chameleon.Token (tokenize, Location(..), TokenError)
import Language.Chameleon.Syntax
import Language.Chameleon.StarterKit


data L
instance Lang L where
    type Ann L = Location
    type Var L = SourceName


parse :: Maybe FilePath -> Text -> WarnErrs CompileError [Decl L]
parse filepath input = do
    tokens <- TokenError `mapErrs` tokenize fancyConfig filepath input
    nest <- SyntaxError `mapErrs` parseNest tokens
    GrammarError `mapErrs` parseFile nest


parseFile :: Nest StarterAtom -> WarnErrs GrammarError [Decl L]
parseFile Comb{id = "__FILE__", segments = decls, ..} = parseDecl `mapM` decls

parseDecl :: [Nest StarterAtom] -> WarnErrs GrammarError (Decl L)
parseDecl [ Atom{atom = Variable "val", ..}
          , x@Atom{atom = Variable _, location = Location{loc_end = end}}
          , body
          ] = DeclVal location{ loc_end = end } <$> parseVar x <*> parseExpr body
-- parseDecl [ Atom{atom = Variable "doc", ..}
--           , Atom{atom = Variable "val"}
--           , Atom{atom = Variable x}
--           , Atom{atom = String docstr, location = Location{loc_end = end} }
--           ] = pure $ DeclValDoc location{ loc_end = end } x docstr
parseDecl [Comb{id = "let", segments = break (isKeyword "in") -> (local, tail -> ds), ..}]
    = DeclLet location <$> parseDecl `traverse` local <*> parseDecl `traverse` ds
parseDecl it = error $ "parseDecl: " ++ show it

parseExpr :: Nest StarterAtom -> WarnErrs GrammarError (Expr L)
parseExpr Atom{atom = Integer i, ..}
    = pure $ ExprConst location (IntConst i)
parseExpr Comb{id = "op", segments = [[Atom {atom = opname}]], ..}
    = ExprPrim location <$> parseOpname opname
    where
    parseOpname (String name) = pure $ (intern . T.unpack) name
    parseOpname (Variable name) = pure name
    parseOpname _ = fatal $ TODO_Grammar -- FIXME error recovery
parseExpr x@Atom{atom = Variable _, ..}
    = ExprVar location <$> parseVar x
parseExpr Comb{id = "ap", segments = [e : es], ..} | notNull es
    = ExprApp location <$> parseExpr e <*> parseExpr `traverse` es
parseExpr Comb{id = "fun", segments = [xs, [e]], ..} | notNull xs
    = ExprLam location <$> parseVar `traverse` xs <*> parseExpr e
parseExpr it@Comb{id = "fun"} = error $ "bad function: " ++ show it
parseExpr Comb{id = "let", segments = break (isKeyword "in") -> (ds, tail -> [[e]]), ..}
    = ExprLet location <$> parseDecl `traverse` ds <*> parseExpr e
parseExpr it = error $ "parseExpr: " ++ show it

parseVar :: Nest StarterAtom -> WarnErrs GrammarError SourceName
parseVar Atom{atom = Variable x} = pure $ ShortName x
parseVar _ = fatal $ TODO_Grammar -- FIXME error recovery





isKeyword :: Symbol -> [Nest StarterAtom] -> Bool
isKeyword kw [Atom{atom = Variable id}] | id == kw = True
isKeyword _ _ = False

notNull = not . null
