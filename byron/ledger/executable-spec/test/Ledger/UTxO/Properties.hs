{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ledger.UTxO.Properties where

import Data.String (fromString)
import Data.Foldable (traverse_)
import Hedgehog
  ( Property
  , PropertyT
  , (===)
  , assert
  , classify
  , forAll
  , property
  , success
  , withTests
  )

import Control.State.Transition.Generator (trace)
import Control.State.Transition.Trace (firstAndLastState, traceLength, Trace)

import Cardano.Ledger.Spec.STS.UTXO (reserves, utxo)
import Cardano.Ledger.Spec.STS.UTXOW (UTXOW)
import Ledger.UTxO (balance, TxId)


-- | Check that the money is constant in the system.
moneyIsConstant :: Property
moneyIsConstant = withTests 200 . property $ do
  (st0, st) <- firstAndLastState <$> forAll (trace @(UTXOW TxId) 500)
  reserves st0 + balance (utxo st0) === reserves st + balance (utxo st)

-- To test the performance of the integrated shrinker for UTxO traces one could replace
-- the return statement of the UTXO @transitionRule@ by:
--
-- >>> let xs =
-- >>>       if 2 < length (txins tx)
-- >>>       then drop 1 (txins tx)
-- >>>       else txins tx
-- >>> return $ UTxOState { utxo     = (xs ⋪ utxo) ∪ txouts tx
-- >>>                    , reserves = reserves + fee
-- >>>                    }
--
-- This should give a minimal counterexample of a trace with a signal
-- containing exactly 3 inputs, and only one output.

-- | Classify the traces.
prop_classifyTraces :: Property
-- TODO: we might want to run this only while developing, and not on CI.
prop_classifyTraces = withTests 200 . property $ do
  classifyTraces 500 50
  success

classifyTraces :: Int -> Int -> PropertyT IO ()
classifyTraces ub step = do
  tr <- forAll (trace @(UTXOW TxId) ub)
  classify "empty"      $ traceLength tr == 0
  classify "singleton"  $ traceLength tr == 1
  traverse_ (classifyInterval tr) $ mkIntervals 2 (ub - 1) step
  classify ubL $ traceLength tr == 1
  where
    ubL = fromString $ show ub

-- | Given a lower bound @low@,  an upper bound @high@ and a step size @step@
-- (both of which must be positive), divide the interval @[0, ub]@ into
-- sub-intervals of @step@ size.
--
-- If any of these values is negative the empty list will be returned.
mkIntervals
  :: Int
  -- ^ Interval lower bound
  -> Int
  -- ^ Interval upper bound
  -> Int
  -- ^ Step size, used to divide the interval in sub-intervals of the same
  -- length.
  -> [(Int, Int)]
mkIntervals low high step
  | 0 <= low && low <= high && 0 < step =
    [(low + i * step, high `min` (low + (i + 1) * step)) | i <- [0 .. n - 1]]
  | otherwise = []
  where
    highNorm = high - low
    n = highNorm `div` step + 1 `min` highNorm `mod` step

classifyInterval :: Trace s -> (Int, Int) -> PropertyT IO ()
classifyInterval tr (low, high) =
  classify desc $! low <= traceLength tr && traceLength tr < high
  where
    -- Hedgehog's LabelName doesn't have a monoid instance at the moment.
    desc = fromString $ "[" <> show low <> ", " <> show high <> ")"
