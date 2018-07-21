module Data.Compos where

import Data.Functor.Identity
import Data.Functor.Const


class Compos (f :: k -> *) where
    compos :: Applicative t => (forall a. f a -> t (f a)) -> f a -> t (f a)


composOp :: Compos f => (forall a. f a -> f a) -> f a -> f a
composOp f = runIdentity . compos (Identity . f)

composFold :: (Monoid m, Compos f) => (forall a. f a -> m) -> f a -> m
composFold f = getConst . compos (Const . f)
