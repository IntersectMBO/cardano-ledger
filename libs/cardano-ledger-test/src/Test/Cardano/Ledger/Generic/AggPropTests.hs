{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Generic.AggPropTests where

import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState (
  DPState (..),
  DState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  PState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.Rules.Reports (synopsisCoinMap)
import Cardano.Ledger.TreeDiff (diffExpr)
import Cardano.Ledger.UMapCompact (View (RewardDeposits), depositView, domain, fromCompact, sumDepositView)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val ((<+>))
import Control.State.Transition (STS (..))
import Control.State.Transition.Trace (
  SourceSignalTarget (..),
  Trace (..),
  TraceOrder (..),
  firstAndLastState,
  sourceSignalTargets,
  traceSignals,
 )
import Control.State.Transition.Trace.Generator.QuickCheck (HasTrace (..))
import Data.Default.Class (Default (def))
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Cardano.Ledger.Generic.Functions (
  getBody,
  getCollateralInputs,
  getCollateralOutputs,
  getInputs,
  getOutputs,
  isValid',
 )
import Test.Cardano.Ledger.Generic.GenState (GenSize (..), initStableFields)
import Test.Cardano.Ledger.Generic.MockChain (MOCKCHAIN, MockBlock (..), MockChainState (..))
import Test.Cardano.Ledger.Generic.Proof (Evidence (..), Proof (..), Reflect (..), Some (..), preBabbage, unReflect)
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
consistentUtxoSizeProp :: EraTx era => Proof era -> Trace (MOCKCHAIN era) -> Property
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
    getUtxoSize state = (Map.size . unUTxO . utxosUtxo . lsUTxOState . esLState . nesEs . mcsNes) state

aggUTxO ::
  forall era.
  ( HasTrace (MOCKCHAIN era) (Gen1 era)
  , Reflect era
  ) =>
  Proof era ->
  Gen Property
aggUTxO proof = do
  trace1 <- genTrace proof 100 (def {blocksizeMax = 4, slotDelta = (6, 12)}) initStableFields
  pure $ consistentUtxoSizeProp proof trace1

aggTests :: TestTree
aggTests =
  testGroup
    "tests, aggregating Tx's over a Trace."
    [ testProperty "UTxO size in Babbage" (aggUTxO (Babbage Mock))
    , testProperty "UTxO size in Alonzo" (aggUTxO (Alonzo Mock))
    , testProperty "UTxO size in Mary" (aggUTxO (Mary Mock))
    ]

-- ===============================================================

-- TODO. An analog of   Test.Cardano.Ledger.Shelley.Rules.TestChain(forAllChainTrace)
-- We will add additional analogs (ledgerTraceFromBlock, poolTraceFromBlock) soon,
-- and then redo the tests in that module in the Generic fashion
forAllChainTrace :: (Testable prop, Reflect era) => Proof era -> Int -> (Trace (MOCKCHAIN era) -> prop) -> Property
-- TODO re-enable this once we have added all the new rules to Conway
-- forAllChainTrace p@(Conway _) n propf =
-- property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) initStableFields
forAllChainTrace (Conway _) _ _ = undefined
forAllChainTrace p@(Babbage _) n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) initStableFields
forAllChainTrace p@(Alonzo _) n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) initStableFields
forAllChainTrace p@(Mary _) n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) initStableFields
forAllChainTrace p@(Allegra _) n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) initStableFields
forAllChainTrace p@(Shelley _) n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) initStableFields

-- ===========================================================

-- | Check that the sum of Key Deposits and the Pool Depoits are equal to the utxosDeposits
depositInvariant ::
  SourceSignalTarget (MOCKCHAIN era) ->
  Property
depositInvariant SourceSignalTarget {source = mockChainSt} =
  let LedgerState {lsUTxOState = utxost, lsDPState = DPState dstate pstate} = (esLState . nesEs . mcsNes) mockChainSt
      allDeposits = utxosDeposited utxost
      sumCoin m = Map.foldl' (<+>) (Coin 0) m
      keyDeposits = fromCompact $ sumDepositView (RewardDeposits (dsUnified dstate))
      poolDeposits = sumCoin (psDeposits pstate)
   in counterexample
        ( unlines
            [ "Deposit invariant fails"
            , "All deposits = " ++ show allDeposits
            , "Key deposits = " ++ synopsisCoinMap (Just (depositView (dsUnified dstate)))
            , "Pool deposits = " ++ synopsisCoinMap (Just (psDeposits pstate))
            ]
        )
        (allDeposits === keyDeposits <+> poolDeposits)

rewardDepositDomainInvariant ::
  SourceSignalTarget (MOCKCHAIN era) ->
  Property
rewardDepositDomainInvariant SourceSignalTarget {source = mockChainSt} =
  let LedgerState {lsDPState = DPState dstate _} = (esLState . nesEs . mcsNes) mockChainSt
      rewardDomain = domain (RewardDeposits (dsUnified dstate))
      depositDomain = Map.keysSet (depositView (dsUnified dstate))
   in counterexample
        ( unlines
            [ "Reward-Deposit domain invariant fails"
            , diffExpr rewardDomain depositDomain
            ]
        )
        (rewardDomain === depositDomain)

itemPropToTraceProp :: (SourceSignalTarget (MOCKCHAIN era) -> Property) -> Trace (MOCKCHAIN era) -> Property
itemPropToTraceProp f trace1 = conjoin (map f (sourceSignalTargets trace1))

depositEra :: forall era. Reflect era => Proof era -> TestTree
depositEra proof =
  testGroup
    (show proof)
    [ testProperty "Deposits = KeyDeposits + PoolDeposits" (forAllChainTrace proof 10 (itemPropToTraceProp (depositInvariant @era)))
    , testProperty "Reward domain = Deposit domain" (forAllChainTrace proof 10 (itemPropToTraceProp (rewardDepositDomainInvariant @era)))
    ]

-- | Build a TestTree that tests 'f' at all the Eras listed in 'ps'
testEras :: String -> [Some Proof] -> (forall era. Reflect era => Proof era -> TestTree) -> TestTree
testEras message ps f = testGroup message (applyF ps)
  where
    applyF [] = []
    applyF (Some e : more) = unReflect f e : applyF more

depositTests :: TestTree
depositTests = testEras "deposit invariants" preBabbage depositEra
