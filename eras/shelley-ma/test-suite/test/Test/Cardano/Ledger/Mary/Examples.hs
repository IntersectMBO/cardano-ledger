{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.Examples
  ( testMaryNoDelegLEDGER,
  )
where

-- obtaining orphan STS (UTXOW (ShelleyMAEra ma c))

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.Shelley.API (LedgerEnv (..), ShelleyLEDGER)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), UTxOState (..), smartUTxOState)
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.UTxO (UTxO)
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->>))
import Data.Default.Class (def)
import GHC.Stack
import Lens.Micro
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase)
import Test.Tasty.HUnit (Assertion, (@?=))

ignoreAllButUTxO ::
  Either [PredicateFailure (ShelleyLEDGER Mary)] (LedgerState Mary) ->
  Either [PredicateFailure (ShelleyLEDGER Mary)] (UTxO Mary)
ignoreAllButUTxO = fmap (\(LedgerState (UTxOState utxo _ _ _ _) _) -> utxo)

testMaryNoDelegLEDGER ::
  HasCallStack =>
  UTxO Mary ->
  ShelleyTx Mary ->
  LedgerEnv Mary ->
  Either [PredicateFailure (ShelleyLEDGER Mary)] (UTxO Mary) ->
  Assertion
testMaryNoDelegLEDGER utxo tx env (Right expectedUTxO) = do
  checkTrace @(ShelleyLEDGER Mary) runShelleyBase env $
    pure (LedgerState (smartUTxOState utxo (Coin 0) (Coin 0) def) def) .- tx .->> expectedSt'
  where
    txFee = tx ^. bodyTxL . feeTxBodyL
    expectedSt' = LedgerState (smartUTxOState expectedUTxO (Coin 0) txFee def) def
testMaryNoDelegLEDGER utxo tx env predicateFailure@(Left _) = do
  let st =
        runShelleyBase $
          applySTSTest @(ShelleyLEDGER Mary)
            (TRC (env, LedgerState (smartUTxOState utxo (Coin 0) (Coin 0) def) def, tx))
  ignoreAllButUTxO st @?= predicateFailure
