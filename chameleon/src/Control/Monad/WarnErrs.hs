{-
Multiple errors may be reported at once
If there are any errors, all further computation halts.
Reporting an empty list of errors performs no action.

Many warnings may be reported.
Warnings are collected during computation.
The warnings are append-only, and cannot even be inspected during computation.

At the end of computation, either the result or the errors are reported, alongside the collected warnings.
Errors and warnings are reported in the error they were raised.
-}
module Control.Monad.WarnErrs
    ( WarnErrs, runWarnErrs
    , warn, warns
    , err, errs
    , mapErrs
    ) where

import Data.Foldable
import Data.Sequence (Seq, (<|), (|>), (><))
import qualified Data.Sequence as Seq

import Control.Monad


newtype WarnErrs report a = WE { unWE :: Seq report -> (Either [report] a, Seq report) }

runWarnErrs :: WarnErrs report a -> Either ([report], [report]) (a, [report])
runWarnErrs (WE action) = case action Seq.empty of
    (Right x, warns) -> Right (x, toList warns)
    (Left errs, warns) -> Left (errs, toList warns)

warn :: report -> WarnErrs report ()
warn warn' = WE $ \warns -> (Right (), warns |> warn')

warns :: [report] -> WarnErrs report ()
warns warns' = WE $ \warns -> (Right (), warns >< Seq.fromList warns')

err :: report -> WarnErrs report a
err err = WE $ \warns -> (Left [err], warns)

errs :: [report] -> WarnErrs report ()
errs [] = WE $ \warns -> (Right (), warns)
errs errs = WE $ \warns -> (Left errs, warns)

mapErrs :: WarnErrs report a -> (report -> report') -> WarnErrs report' a
mapErrs (WE action) f = WE $ \warns -> case action Seq.empty of
    (Left errs, warns') -> (Left (f <$> errs), warns >< (f <$> warns'))
    (Right x, warns') -> (Right x, warns >< (f <$> warns'))


instance Functor (WarnErrs report) where
    fmap f (WE action) = WE $ \warns -> case action warns of
        (Left errs, warns') -> (Left errs, warns')
        (Right x, warns') -> (Right $ f x, warns')

instance Applicative (WarnErrs report) where
    pure x = WE $ \warns -> (Right x, warns)
    (<*>) = ap

instance Monad (WarnErrs report) where
    WE action >>= k = WE $ \warns -> case action warns of
        (Left errs, warns') -> (Left errs, warns')
        (Right x, warns') -> case k x of
            WE action' -> action' warns'
