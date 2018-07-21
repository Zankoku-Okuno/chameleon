module Language.Coral.Syntax.Abstract where

import Data.Symbol
import Data.Text (Text)

import Data.Compos


type Name = Symbol -- FIXME parameterizable name --FIXME different categories of name

-- FIXME location data, or other annotations
data Ast :: AstCat -> * where
    File :: [Decl] -> File
    DeclVal :: Name -> Expr -> Decl
    DeclValDoc :: Name -> Text -> Decl
    ExprConst :: Const -> Expr
    -- TODO: ExprPrim (apply primitive operation) -- FIXME parameterizable primitives
    -- TODO ExprVar (use variable)

data Const -- FIXME parameterizable constant type
    = IntConst Integer
    -- TODO rational constant
    -- | StrConst Text -- TODO


data AstCat
    = File_
    | Decl_
    | Expr_

type File = Ast File_
type Decl = Ast Decl_
type Expr = Ast Expr_


instance Compos Ast where
    compos f (File decls) = File <$> traverse f decls
    compos f (DeclVal x e) = DeclVal x <$> f e
    compos f (DeclValDoc x text) = pure $ DeclValDoc x text
    compos f (ExprConst c) = pure $ ExprConst c