{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.Examples
  ( testMaryNoDelegLEDGER,
  )
where

import Cardano.Ledger.ShelleyMA.Rules.Utxow ()
-- obtaining orphan STS (UTXOW (ShelleyMAEra ma c))
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import GHC.Records
import Shelley.Spec.Ledger.API (LEDGER, LedgerEnv (..))
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    UTxOState (..),
    emptyDPState,
    emptyUTxOState,
  )
import Shelley.Spec.Ledger.Tx (Tx (..))
import Shelley.Spec.Ledger.UTxO (UTxO)
import Test.Cardano.Ledger.EraBuffet (MaryTest)
import Test.Shelley.Spec.Ledger.Utils (applySTSTest, runShelleyBase)
import Test.Tasty.HUnit (Assertion, (@?=))

ignoreAllButUTxO ::
  Either [[PredicateFailure (LEDGER MaryTest)]] (UTxOState MaryTest, DPState MaryTest) ->
  Either [[PredicateFailure (LEDGER MaryTest)]] (UTxO MaryTest)
ignoreAllButUTxO (Left e) = Left e
ignoreAllButUTxO (Right (UTxOState utxo _ _ _, _)) = Right utxo

testMaryNoDelegLEDGER ::
  UTxO MaryTest ->
  Tx MaryTest ->
  LedgerEnv MaryTest ->
  Either [[PredicateFailure (LEDGER MaryTest)]] (UTxO MaryTest) ->
  Assertion
testMaryNoDelegLEDGER utxo tx env (Right expectedUTxO) = do
  checkTrace @(LEDGER MaryTest) runShelleyBase env $
    pure (emptyUTxOState {_utxo = utxo}, emptyDPState) .- tx .-> expectedSt'
  where
    txFee = getField @"txfee" (_body tx)
    expectedSt' = (emptyUTxOState {_utxo = expectedUTxO, _fees = txFee}, emptyDPState)
testMaryNoDelegLEDGER utxo tx env predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTSTest @(LEDGER MaryTest) (TRC (env, (emptyUTxOState {_utxo = utxo}, emptyDPState), tx))
  (ignoreAllButUTxO st) @?= predicateFailure
