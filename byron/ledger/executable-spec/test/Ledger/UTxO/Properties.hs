{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ledger.UTxO.Properties where

import Control.Monad (when)
import Hedgehog
  ( Property
  , (===)
  , classify
  , forAll
  , property
  , success
  , withTests
  )

import Control.State.Transition.Generator (classifyTraceLength, trace)
import Control.State.Transition.Trace
  ( TraceOrder(OldestFirst)
  , firstAndLastState
  , traceLength
  , traceSignals
  )

import Cardano.Ledger.Spec.STS.UTXO (reserves, utxo)
import Cardano.Ledger.Spec.STS.UTXOW (UTXOW)
import Ledger.UTxO (TxId, balance, body, inputs, outputs)

-- | Check that the money is constant in the system.
moneyIsConstant :: Property
moneyIsConstant = withTests 200 . property $ do
  (st0, st) <- firstAndLastState <$> forAll (trace @(UTXOW TxId) 500)
  reserves st0 + balance (utxo st0) === reserves st + balance (utxo st)

-- To test the performance of the integrated shrinker for UTxO traces one could
-- replace the return statement of the 'UTXO' @transitionRule@ by:
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

tracesAreClassified :: Property
-- TODO: we might want to run this only while developing, and not on CI.
tracesAreClassified = withTests 200 . property $ do
  let (tl, step) = (500, 50)
  tr <- forAll (trace @(UTXOW TxId) tl)
  classifyTraceLength tr tl step
  -- Classify the average number of inputs and outputs. Note that the intervals
  -- were arbitrarily determined, since in order to have a good partition of
  -- the interval [0, maximum possible number of inputs/outputs] we'd need to
  -- know how many addresses are being used in the trace generation.
  let actualTl = traceLength tr
  when (0 < actualTl) $ do
    let txs = body <$> traceSignals OldestFirst tr
        nrInputs = fromIntegral $ sum (length . inputs <$> txs)
        nrOutputs = fromIntegral $ sum (length . outputs <$> txs)
        avgInputs = nrInputs / fromIntegral actualTl
        avgOutputs = nrOutputs / fromIntegral actualTl
    -- Classify the average number of inputs
    classify "avg nr. inputs == 0"      $ (0 :: Double) == avgInputs
    classify "avg nr. inputs == 1"      $ 1 == avgInputs
    classify "avg nr. inputs [2, 5)"    $ 2 <= avgInputs && avgInputs < 5
    classify "avg nr. inputs [5, 10)"   $ 5 <= avgInputs && avgInputs < 10
    classify "avg nr. inputs [10, 25)"  $ 10 <= avgInputs && avgInputs < 25
    classify "avg nr. inputs [25, 100)" $ 25 <= avgInputs && avgInputs < 100
    classify ">= 100"                   $ 100 <= avgInputs
    -- Classify the average number of outputs
    classify "avg nr. outputs == 0"      $ (0 :: Double) == avgOutputs
    classify "avg nr. outputs == 1"      $ 1 == avgOutputs
    classify "avg nr. outputs [2, 5)"    $ 2 <= avgOutputs && avgOutputs < 5
    classify "avg nr. outputs [5, 10)"   $ 5 <= avgOutputs && avgOutputs < 10
    classify "avg nr. outputs [10, 25)"  $ 10 <= avgOutputs && avgOutputs < 25
    classify "avg nr. outputs [25, 100)" $ 25 <= avgOutputs && avgOutputs < 100
    classify ">= 100"                    $ 100 <= avgOutputs
  success
