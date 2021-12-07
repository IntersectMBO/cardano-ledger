{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.Examples
  ( testMaryNoDelegLEDGER,
  )
where

-- obtaining orphan STS (UTXOW (ShelleyMAEra ma c))

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley.API (LEDGER, LedgerEnv (..))
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    UTxOState (..),
    smartUTxOState,
  )
import Cardano.Ledger.Shelley.PParams (PParams' (..))
import Cardano.Ledger.Shelley.Tx (Tx (..))
import Cardano.Ledger.Shelley.UTxO (UTxO)
import Cardano.Ledger.ShelleyMA.Rules.Utxow ()
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import Data.Default.Class (def)
import GHC.Records
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import Test.Cardano.Ledger.Shelley.Utils (applySTSTest, runShelleyBase)
import Test.Tasty.HUnit (Assertion, (@?=))

type MaryTest = MaryEra TestCrypto

ignoreAllButUTxO ::
  Either [PredicateFailure (LEDGER MaryTest)] (UTxOState MaryTest, DPState TestCrypto) ->
  Either [PredicateFailure (LEDGER MaryTest)] (UTxO MaryTest)
ignoreAllButUTxO = fmap (\(UTxOState utxo _ _ _ _, _) -> utxo)

testMaryNoDelegLEDGER ::
  UTxO MaryTest ->
  Tx MaryTest ->
  LedgerEnv MaryTest ->
  Either [PredicateFailure (LEDGER MaryTest)] (UTxO MaryTest) ->
  Assertion
testMaryNoDelegLEDGER utxo tx env (Right expectedUTxO) = do
  checkTrace @(LEDGER MaryTest) runShelleyBase env $
    pure (smartUTxOState utxo (Coin 0) (Coin 0) def, def) .- tx .-> expectedSt'
  where
    txFee = getField @"txfee" (getField @"body" tx)
    expectedSt' = (smartUTxOState expectedUTxO (Coin 0) txFee def, def)
testMaryNoDelegLEDGER utxo tx env predicateFailure@(Left _) = do
  let st =
        runShelleyBase $
          applySTSTest @(LEDGER MaryTest)
            (TRC (env, (smartUTxOState utxo (Coin 0) (Coin 0) def, def), tx))
  ignoreAllButUTxO st @?= predicateFailure
