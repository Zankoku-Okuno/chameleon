module Language.Coral.Syntax.Abstract where

import Data.Symbol
import Data.Text (Text)

import Data.Compos


type Name = Symbol -- FIXME parameterizable name --FIXME different categories of name

-- FIXME location data, or other annotations
data Ast ann (cat :: AstCat) where
    --- Declarations ---
    DeclVal    :: ann -> Name -> Expr ann -> Decl ann
    -- DeclValDoc :: ann -> Name -> Text -> Decl ann
    DeclLet :: ann -> [Decl ann] -> [Decl ann] -> Decl ann
    --- Expressions ---
    ExprConst  :: ann -> Const -> Expr ann
    ExprPrim   :: ann -> Symbol -> Expr ann -- FIXME parameterizable primitive operations
    ExprVar    :: ann -> Symbol -> Expr ann
    ExprApp    :: ann -> Expr ann -> [Expr ann] -> Expr ann
    ExprLam    :: ann -> [Name] -> Expr ann -> Expr ann
    ExprLet    :: ann -> [Decl ann] -> Expr ann -> Expr ann

data Const -- FIXME parameterizable constant type
    = IntConst Integer
    -- TODO rational constant
    -- | StrConst Text -- TODO




data AstCat
    = Decl_
    | Expr_

type Decl ann = Ast ann Decl_
type Expr ann = Ast ann Expr_


instance Compos (Ast ann) where
    compos f (DeclVal ann x e) = DeclVal ann x <$> f e
    -- compos f (DeclValDoc ann x text) = pure $ DeclValDoc ann x text
    compos f (DeclLet ann local ds) = DeclLet ann <$> f `traverse` local <*> f `traverse` ds
    compos f (ExprConst ann c) = pure $ ExprConst ann c
    compos f (ExprPrim ann opname) = pure $ ExprPrim ann opname
    compos f (ExprVar ann x) = pure $ ExprVar ann x
    compos f (ExprApp ann e es) = ExprApp ann <$> f e <*> f `traverse` es
    compos f (ExprLam ann x e) = ExprLam ann x <$> f e
    compos f (ExprLet ann ds e) = ExprLet ann <$> f `traverse` ds <*> f e
