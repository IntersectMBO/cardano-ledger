{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.Examples (
  testMaryNoDelegLEDGER,
) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley.API (LedgerEnv (..), ShelleyLEDGER)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), UTxOState (..), smartUTxOState)
import Cardano.Ledger.State (UTxO)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty)
import GHC.Stack
import Lens.Micro
import Test.Cardano.Ledger.Mary.TreeDiff ()
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase)
import Test.Control.State.Transition.Trace (checkTrace, (.-), (.->>))
import Test.Tasty.HUnit (Assertion, (@?=))

ignoreAllButUTxO ::
  Either (NonEmpty (PredicateFailure (ShelleyLEDGER MaryEra))) (LedgerState MaryEra) ->
  Either (NonEmpty (PredicateFailure (ShelleyLEDGER MaryEra))) (UTxO MaryEra)
ignoreAllButUTxO = fmap (\(LedgerState (UTxOState utxo _ _ _ _ _) _) -> utxo)

testMaryNoDelegLEDGER ::
  HasCallStack =>
  UTxO MaryEra ->
  Tx MaryEra ->
  LedgerEnv MaryEra ->
  Either (NonEmpty (PredicateFailure (ShelleyLEDGER MaryEra))) (UTxO MaryEra) ->
  Assertion
testMaryNoDelegLEDGER utxo tx env (Right expectedUTxO) = do
  checkTrace @(ShelleyLEDGER MaryEra) runShelleyBase env $
    pure (LedgerState (smartUTxOState (ledgerPp env) utxo (Coin 0) (Coin 0) def mempty) def)
      .- tx
      .->> expectedSt'
  where
    txFee = tx ^. bodyTxL . feeTxBodyL
    expectedSt' = LedgerState (smartUTxOState (ledgerPp env) expectedUTxO (Coin 0) txFee def mempty) def
testMaryNoDelegLEDGER utxo tx env predicateFailure@(Left _) = do
  let st =
        runShelleyBase $
          applySTSTest @(ShelleyLEDGER MaryEra)
            ( TRC
                ( env
                , LedgerState (smartUTxOState (ledgerPp env) utxo (Coin 0) (Coin 0) def mempty) def
                , tx
                )
            )
  ignoreAllButUTxO st @?= predicateFailure
