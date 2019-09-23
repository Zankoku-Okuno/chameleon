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
    , fatal, fatals
    , fatalize
    , mapErrs
    ) where

import Data.Monoid
import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Control.Monad

data St report = St { st_warns :: Seq report, st_errs :: Seq report }
newtype WarnErrs report a = WE { unWE :: St report -> (Either (Seq report) a, St report) }

runWarnErrs :: WarnErrs report a -> Either ([report], [report]) (a, [report])
runWarnErrs (WE action) = case action mempty of
    (Right x, St{..}) | Seq.null st_errs -> Right (x, toList st_warns)
    (Left errs, St{..}) -> Left (toList $ st_errs <> errs, toList st_warns)

warn :: report -> WarnErrs report ()
warn warn' = WE $ \st -> (Right (), st <> mempty{st_warns = Seq.singleton warn'})

warns :: [report] -> WarnErrs report ()
warns warns' = WE $ \st -> (Right (), st <> mempty{st_warns = Seq.fromList warns'})

err :: a -> report -> WarnErrs report a
err onError err' = WE $ \st -> (Right onError, st <> mempty{st_errs = Seq.singleton err'})

errs :: a -> [report] -> WarnErrs report a
errs onError errs' = WE $ \st -> (Right onError, st <> mempty{st_errs = Seq.fromList errs'})

fatal :: report -> WarnErrs report a
fatal err = WE $ \st -> (Left $ Seq.singleton err, st)

fatals :: [report] -> WarnErrs report ()
fatals [] = WE $ \st -> (Right (), st)
fatals errs = WE $ \st -> (Left $ Seq.fromList errs, st)

fatalize :: WarnErrs report a -> WarnErrs report a
fatalize (WE action) = WE $ \st0 -> case action mempty of
    (Left errs, st@St{..}) -> (Left $ errs <> st_errs, st{st_errs = mempty})
    (Right x, st@St{..}) -> if null st_errs
        then (Right x, St{..})
        else (Left st_errs, st{st_errs = mempty})

mapErrs :: (report -> report') -> WarnErrs report a -> WarnErrs report' a
mapErrs f (WE action) = WE $ \st0 -> case action mempty of
    (Left errs, st') -> (Left (f <$> errs), st0 <> (f <$> st'))
    (Right x, st') -> (Right x, st0 <> (f <$> st'))


instance Functor St where
    fmap f St{..} = St (f <$> st_warns) (f <$> st_errs)

instance Monoid (St report) where
    mempty = St mempty mempty
    mappend a b = St (st_warns a <> st_warns b) (st_errs a <> st_errs b)


instance Functor (WarnErrs report) where
    fmap f (WE action) = WE $ \st -> case action st of
        (Left errs, st') -> (Left errs, st')
        (Right x, st') -> (Right $ f x, st')

instance Applicative (WarnErrs report) where
    pure x = WE $ \st -> (Right x, st)
    (<*>) = ap

instance Monad (WarnErrs report) where
    WE action >>= k = WE $ \st -> case action st of
        (Left errs, st') -> (Left errs, st')
        (Right x, st') -> case k x of
            WE action' -> action' st'
