module Language.Coral.Syntax.Abstract where

import Data.Symbol
import Data.Text (Text)


class Lang l where
    type Ann l :: *
    type Var l :: *
    -- type Const l :: AstCat -> * -- TODO


data Decl l
    = DeclVal (Ann l) (Var l) (Expr l)
    -- DeclValDoc :: ann -> var OfExpr -> Text -> Decl ann var
    | DeclLet (Ann l) [Decl l] [Decl l]

data Expr l
    = ExprVar    (Ann l) (Var l)
    | ExprApp    (Ann l) (Expr l) [Expr l]
    | ExprLam    (Ann l) [Var l] (Expr l)
    | ExprLet    (Ann l) [Decl l] (Expr l)
    | ExprConst  (Ann l) Const
    | ExprPrim   (Ann l) Symbol -- FIXME parameterizable primitive operations


data Const -- FIXME parameterizable constant type
    = IntConst Integer
    -- TODO rational constant
    -- | StrConst Text -- TODO


{- In a normal multiplate, each field of the record is a coalgebra `A -> k A`.
    Here, the type A is replaced by a (n-)functor `A x`, and the coalgebras become functorially-related families of co-algebras: `A x -> k (A x')`.
    Perhaps it makes sense to call this technique a "Family Dinner"? Unless I still have co-algebras and this is a multiplate already.
-}
data Plate l l' t = P
    { decl :: Decl l -> t (Decl l')
    , expr :: Expr l -> t (Expr l')
    , ann :: Ann l -> t (Ann l')
    , var :: Var l -> t (Var l')
    }

multiplate :: Applicative t => Plate l l' t -> Plate l l' t
multiplate super = P
    { decl = \case
        DeclVal a x e -> DeclVal <$> ann super a <*> var super x <*> expr super e
        DeclLet a local ds -> DeclLet <$> ann super a <*> decl super `traverse` local <*> decl super `traverse` ds
    , expr = \case
        ExprVar a x -> ExprVar <$> ann super a <*> var super x
        ExprApp a f es -> ExprApp <$> ann super a <*> expr super f <*> expr super `traverse` es
        ExprLam a xs e -> ExprLam <$> ann super a <*> var super `traverse` xs <*> expr super e
        ExprLet a ds e -> ExprLet <$> ann super a <*> decl super `traverse` ds <*> expr super e
        ExprConst a c -> ExprConst <$> ann super a <*> pure c
        ExprPrim a c -> ExprPrim <$> ann super a <*> pure c
    , ann = ann super
    , var = var super
    }

purePlate :: Applicative t => Plate l l t
purePlate = P
    { decl = pure
    , expr = pure
    , ann = pure
    , var = pure
    }


------ Representations of names used throughout compiler ------

data SourceName
    = ShortName Symbol
    -- | LongName -- TODO long name
    deriving(Eq, Ord, Show)


data NamePart
    = SourceName Symbol
    | GenName Symbol
    -- | Synonym QualName -- TODO maybe I need this?
    deriving(Show)

data QualName = QName
    { sourceName :: SourceName
    , qualName :: [NamePart] -- TODO use Seq
    }
    deriving(Show)