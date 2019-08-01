{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ledger.UTxO.Properties where

import           Control.Arrow ((***))
import           Control.Lens (view, (&), (^.), _2)
import           Control.Monad (when)
import           Data.Foldable (foldl', traverse_)
import           Hedgehog (MonadTest, Property, classify, cover, forAll, property, success,
                     withTests, (===))

import           Control.State.Transition.Generator (classifyTraceLength, trace, traceOfLength)
import           Control.State.Transition.Trace (Trace, TraceOrder (OldestFirst), firstAndLastState,
                     preStatesAndSignals, traceEnv, traceLength, traceSignals, _traceInitState)

import           Cardano.Ledger.Spec.STS.UTXO (UTxOState (UTxOState), pps, reserves, utxo)
import           Cardano.Ledger.Spec.STS.UTXOW (UTXOW)
import qualified Data.Map.Strict as Map
import           Data.Set (Set, empty, fromList, union)
import           Ledger.Core (Lovelace, dom, unLovelace, (∩), (∪), (⋪), (◁))
import           Ledger.UTxO (Tx (Tx), TxIn (TxIn), TxOut (TxOut), TxWits, UTxO (UTxO), balance,
                     body, inputs, outputs, pcMinFee, txins, txouts)

--------------------------------------------------------------------------------
-- UTxO Properties
--------------------------------------------------------------------------------

-- | Check that the money is constant in the system.
moneyIsConstant :: Property
moneyIsConstant = withTests 200 . property $ do
  (st0, st) <- firstAndLastState <$> forAll (trace @UTXOW 500)
  reserves st0 + balance (utxo st0) === reserves st + balance (utxo st)

-- | Check that there is no double spending
noDoubleSpending :: Property
noDoubleSpending = property $ do
  t <- forAll (trace @UTXOW 300)
  let
    UTxOState {utxo = utxo0} = _traceInitState t
    txs = body <$> traceSignals OldestFirst t
  when (all (\ti -> dom (txouts ti) ∩ dom utxo0 == empty) txs) $
    traverse_ (noCommonInputsTxs txs) (zip txs [0 .. ])
   where
    noCommonInputsTxs :: MonadTest m => [Tx] -> (Tx, Int) -> m ()
    noCommonInputsTxs txs (tx, i) =
      traverse_ (\txj -> txins' txj ∩ txins' tx === empty) (take i txs)

    txins' :: Tx -> Set TxIn
    txins' = fromList . txins

-- | Check that UTxO is outputs minus inputs
utxoDiff :: Property
utxoDiff = property $ do
  t <- forAll (trace @UTXOW 500)
  let
    (utxo0, utxoSt) = (utxo *** utxo) . firstAndLastState $ t
    txs = body <$> traceSignals OldestFirst t
  when (all (\ti -> dom (txouts ti) ∩ dom utxo0 == empty) txs) $
    foldl' union' empty txs ⋪ (utxo0 ∪ allTxOuts txs) === utxoSt
 where
  union' :: Set TxIn -> Tx -> Set TxIn
  union' s tx = s `union` fromList (txins tx)

  allTxOuts :: [Tx] -> UTxO
  allTxOuts txs = foldl' (∪) (UTxO Map.empty) (map txouts txs)

--------------------------------------------------------------------------------
-- Coverage guarantees for UTxO traces
--------------------------------------------------------------------------------

relevantCasesAreCovered :: Property
relevantCasesAreCovered = withTests 400 $ property $ do
  let tl = 300
  tr <- forAll (traceOfLength @UTXOW tl)
  let n :: Integer
      n = fromIntegral $ traceLength tr

  when (n > 0) $ do
    let
        ss = preStatesAndSignals OldestFirst tr
        txs = (body . view _2) <$> ss
        (avgInputs, avgOutputs) = avgInputsOutputs txs

    cover 20 "all txs have zero fee surplus" (avgFeeSurplus tr n == 0)
    cover 20 "avg. tx fee surplus (0,10]" (0 < avgFeeSurplus tr n && avgFeeSurplus tr n <= 10)
    cover 20 "avg. tx fee surplus (10,30]" (10 < avgFeeSurplus tr n && avgFeeSurplus tr n <= 30)
    cover 1 "avg. tx fee surplus (30,...)" (30 < avgFeeSurplus tr n)

    cover 20 "avg. nr. of tx inputs (1,5]" (1 <= avgInputs && avgInputs <= 5)
    cover 20 "avg. nr. of tx inputs (5,10]" (5 < avgInputs && avgInputs <= 10)

    cover 20 "avg. nr. of tx outputs (1,5]" (1 <= avgOutputs && avgOutputs <= 5)
    cover 20 "avg. nr. of tx outputs (5,10]" (5 < avgOutputs && avgOutputs <= 10)

    cover 80 "starting UTxO has no future inputs" (all (== empty) (futureInputs tr))
  where
    -- | The average "fee surplus" for transactions in the trace.
    --   Could be zero if all the transactions had zero surplus fee.
    avgFeeSurplus :: Trace UTXOW -> Integer -> Int
    avgFeeSurplus tr n
      = preStatesAndSignals OldestFirst tr
      & map (txFeeSurplus (pcMinFee pps_))
      & sum
      & (`div` n)
      & fromIntegral
      where
        pps_ = pps (tr ^. traceEnv)

    -- | The difference between the Tx Fee and the Min Fee
    txFeeSurplus :: (Tx -> Lovelace) -> (UTxOState, TxWits) -> Integer
    txFeeSurplus txMinFee (st, txw)
      = fee - minFee
      where
          tx_ = body txw
          utxo_ = utxo st
          fee = unLovelace $ balance (txins tx_ ◁ utxo_) - balance (txouts tx_)
          minFee = unLovelace $ txMinFee tx_

    -- | The intersection of the starting UTxO and each transaction in
    -- a trace
    futureInputs :: Trace UTXOW -> [Set TxIn]
    futureInputs tr =
      let
        UTxOState {utxo = utxo0} = _traceInitState tr
        txs = body <$> traceSignals OldestFirst tr
      in
        (\ti -> dom (txouts ti) ∩ dom utxo0) <$> txs

-- | Returns the average number of inputs and outputs for a list of transactions.
avgInputsOutputs :: [Tx] -> (Double, Double)
avgInputsOutputs txs
  = case length txs of
      0 -> (0,0)
      n -> ( nrInputs  / (fromIntegral n)
           , nrOutputs / (fromIntegral n))
  where
    nrInputs = fromIntegral $ sum (length . inputs <$> txs)
    nrOutputs = fromIntegral $ sum (length . outputs <$> txs)

--------------------------------------------------------------------------------
-- Classified Traces (not included in CI test runs, but useful for development)
--------------------------------------------------------------------------------

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
tracesAreClassified = withTests 200 . property $ do
  let (tl, step) = (500, 50)
  tr <- forAll (trace @UTXOW tl)
  classifyTraceLength tr tl step

  let
    pparams = pps (tr ^. traceEnv)
    -- Transaction with one input and one output
    unitTx = Tx [TxIn undefined 0] [TxOut undefined 100]
    unitTxFee = pcMinFee pparams unitTx
  classify "Unit transaction cost == 0" $ unitTxFee == 0
  classify "Unit transaction cost == 1" $ unitTxFee == 1
  classify "Unit transaction cost [2, 5)" $ 2 <= unitTxFee && unitTxFee < 5
  classify "Unit transaction cost [5, 10)" $ 5 <= unitTxFee && unitTxFee < 10
  classify "Unit transaction cost [10, 25)" $ 10 <= unitTxFee && unitTxFee < 25
  classify "Unit transaction cost [25, 100)" $ 25 <= unitTxFee && unitTxFee < 100
  classify "Unit transaction cost >= 100" $ 100 <= unitTxFee

  -- Classify the average number of inputs and outputs. Note that the intervals
  -- were arbitrarily determined, since in order to have a good partition of
  -- the interval [0, maximum possible number of inputs/outputs] we'd need to
  -- know how many addresses are being used in the trace generation.
  let actualTl = traceLength tr
  when (0 < actualTl) $ do
    let txs = body <$> traceSignals OldestFirst tr
        (avgInputs, avgOutputs) = avgInputsOutputs txs

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
