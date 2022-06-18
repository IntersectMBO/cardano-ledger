{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Generic.AggPropTests where

import qualified Cardano.Ledger.Alonzo.PParams as Alonzo (PParams' (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.Shelley.LedgerState
  ( EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    UTxOState (..),
  )
import qualified Cardano.Ledger.Shelley.PParams as Shelley (PParams' (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Control.State.Transition (STS (..))
import Control.State.Transition.Trace (Trace (..), TraceOrder (..), firstAndLastState, traceSignals)
import Control.State.Transition.Trace.Generator.QuickCheck (HasTrace (..))
import Data.Default.Class (Default (def))
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Cardano.Ledger.Generic.Functions
  ( getBody,
    getCollateralInputs,
    getCollateralOutputs,
    getInputs,
    getOutputs,
    isValid',
  )
import Test.Cardano.Ledger.Generic.GenState (GenSize (..), initStableFields)
import Test.Cardano.Ledger.Generic.MockChain (MOCKCHAIN, MockBlock (..), MockChainState (..))
import Test.Cardano.Ledger.Generic.Proof (Evidence (..), Proof (..), Reflect (..))
import Test.Cardano.Ledger.Generic.Trace (Gen1, genTrace)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

-- ============================================================

aggProp :: agg -> (agg -> Signal sts -> agg) -> (State sts -> State sts -> agg -> prop) -> Trace sts -> prop
aggProp agg0 aggregate test trace = test firstState lastState (foldl' aggregate agg0 sigs)
  where
    sigs = traceSignals OldestFirst trace
    (firstState, lastState) = firstAndLastState trace

-- | The aggregate sizes of (outputs - inputs) is consistent with the change in size of the UTxO.
--   Be carefull to choose the correct outputs and inputs, depending on if the Tx validates.
consistentUtxoSizeProp :: Proof era -> Trace (MOCKCHAIN era) -> Property
consistentUtxoSizeProp proof trace = aggProp agg0 aggregate makeprop trace
  where
    agg0 = 0
    aggregate count (MockBlock _ _ txs) = foldl' aggTx count txs
    aggTx count tx =
      count
        + ( if valid
              then length (getOutputs proof body) - Set.size (getInputs proof body)
              else length (getCollateralOutputs proof body) - Set.size (getCollateralInputs proof body)
          )
      where
        body = getBody proof tx
        IsValid valid = isValid' proof tx
    makeprop firstSt lastSt n = getUtxoSize firstSt === getUtxoSize lastSt - n
    getUtxoSize :: MockChainState era -> Int
    getUtxoSize state = (Map.size . unUTxO . _utxo . lsUTxOState . esLState . nesEs . mcsNes) state

aggUTxO ::
  forall era.
  ( HasTrace (MOCKCHAIN era) (Gen1 era),
    Reflect era
  ) =>
  Proof era ->
  Gen Property
aggUTxO proof = do
  trace1 <- genTrace proof 100 (def {blocksizeMax = 4, slotDelta = (6, 12)}) (initStableFields proof)
  pure $ consistentUtxoSizeProp proof trace1

aggTests :: TestTree
aggTests =
  testGroup
    "tests, aggregating Tx's over a Trace."
    [ testProperty "UTxO size in Babbage" (aggUTxO (Babbage Mock)),
      testProperty "UTxO size in Alonzo" (aggUTxO (Alonzo Mock)),
      testProperty "UTxO size in Mary" (aggUTxO (Mary Mock))
    ]

-- ===============================================================

-- TODO. An analog of   Test.Cardano.Ledger.Shelley.Rules.TestChain(forAllChainTrace)
-- We will add additional analogs (ledgerTraceFromBlock, poolTraceFromBlock) soon,
-- and then redo the tests in that module in the Generic fashion
forAllChainTrace :: (Testable prop, Reflect era) => Proof era -> Int -> (Trace (MOCKCHAIN era) -> prop) -> Property
forAllChainTrace p@(Babbage _) n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) (initStableFields p)
forAllChainTrace p@(Alonzo _) n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) (initStableFields p)
forAllChainTrace p@(Mary _) n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) (initStableFields p)
forAllChainTrace p@(Allegra _) n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) (initStableFields p)
forAllChainTrace p@(Shelley _) n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) (initStableFields p)
