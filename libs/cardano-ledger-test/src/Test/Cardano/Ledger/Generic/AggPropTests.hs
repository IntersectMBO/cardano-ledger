{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Generic.AggPropTests where

import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState (
  DState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  PState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.Rules.Reports (synopsisCoinMap)
import Cardano.Ledger.State (EraCertState (..), UTxO (..))
import Cardano.Ledger.UMap (UView (RewDepUView), depositMap, domain, fromCompact, sumDepositUView)
import Cardano.Ledger.Val ((<+>))
import Control.State.Transition (STS (..))
import Data.Default (Default (def))
import Data.Foldable as F (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lens.Micro ((^.))
import qualified Prettyprinter as Pretty
import Test.Cardano.Ledger.Binary.TreeDiff (ansiDocToString, diffExpr)
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
import Test.Cardano.Ledger.Generic.Proof (
  Proof (..),
  Reflect (..),
  Some (..),
  preBabbage,
  unReflect,
 )
import Test.Cardano.Ledger.Generic.Trace (Gen1, genTrace, testPropMax)
import Test.Control.State.Transition.Trace (
  SourceSignalTarget (..),
  Trace (..),
  TraceOrder (..),
  firstAndLastState,
  sourceSignalTargets,
  traceSignals,
 )
import Test.Control.State.Transition.Trace.Generator.QuickCheck (HasTrace (..))
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

-- ============================================================

aggProp ::
  agg -> (agg -> Signal sts -> agg) -> (State sts -> State sts -> agg -> prop) -> Trace sts -> prop
aggProp agg0 aggregate test trace = test firstState lastState (F.foldl' aggregate agg0 sigs)
  where
    sigs = traceSignals OldestFirst trace
    (firstState, lastState) = firstAndLastState trace

-- | The aggregate sizes of (outputs - inputs) is consistent with the change in size of the UTxO.
--   Be carefull to choose the correct outputs and inputs, depending on if the Tx validates.
consistentUtxoSizeProp :: EraTx era => Proof era -> Trace (MOCKCHAIN era) -> Property
consistentUtxoSizeProp proof trace = aggProp agg0 aggregate makeprop trace
  where
    agg0 = 0
    aggregate count (MockBlock _ _ txs) = F.foldl' aggTx count txs
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
    [ testPropMax 30 "UTxO size in Babbage" (aggUTxO Babbage)
    , testPropMax 30 "UTxO size in Alonzo" (aggUTxO Alonzo)
    , testPropMax 30 "UTxO size in Mary" (aggUTxO Mary)
    ]

-- ===============================================================

-- TODO. An analog of   Test.Cardano.Ledger.Shelley.Rules.TestChain(forAllChainTrace)
-- We will add additional analogs (ledgerTraceFromBlock, poolTraceFromBlock) soon,
-- and then redo the tests in that module in the Generic fashion
forAllChainTrace ::
  (Testable prop, Reflect era) => Proof era -> Int -> (Trace (MOCKCHAIN era) -> prop) -> Property
forAllChainTrace p@Conway n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) initStableFields
forAllChainTrace p@Babbage n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) initStableFields
forAllChainTrace p@Alonzo n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) initStableFields
forAllChainTrace p@Mary n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) initStableFields
forAllChainTrace p@Allegra n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) initStableFields
forAllChainTrace p@Shelley n propf =
  property $ propf <$> genTrace p n (def {blocksizeMax = 4, slotDelta = (6, 12)}) initStableFields

-- ===========================================================

-- | Check that the sum of Key Deposits and the Pool Depoits are equal to the utxosDeposits
depositInvariant ::
  EraCertState era =>
  SourceSignalTarget (MOCKCHAIN era) ->
  Property
depositInvariant SourceSignalTarget {source = mockChainSt} =
  let LedgerState {lsUTxOState = utxost, lsCertState = certState} = (esLState . nesEs . mcsNes) mockChainSt
      -- TODO handle VState
      pstate = certState ^. certPStateL
      dstate = certState ^. certDStateL
      allDeposits = utxosDeposited utxost
      sumCoin m = Map.foldl' (<+>) (Coin 0) m
      keyDeposits = fromCompact $ sumDepositUView (RewDepUView (dsUnified dstate))
      poolDeposits = sumCoin (psDeposits pstate)
   in counterexample
        ( ansiDocToString . Pretty.vsep $
            [ "Deposit invariant fails:"
            , Pretty.indent 2 . Pretty.vsep . map Pretty.pretty $
                [ "All deposits = " ++ show allDeposits
                , "Key deposits = " ++ synopsisCoinMap (Just (depositMap (dsUnified dstate)))
                , "Pool deposits = " ++ synopsisCoinMap (Just (psDeposits pstate))
                ]
            ]
        )
        (allDeposits === keyDeposits <+> poolDeposits)

rewardDepositDomainInvariant ::
  EraCertState era =>
  SourceSignalTarget (MOCKCHAIN era) ->
  Property
rewardDepositDomainInvariant SourceSignalTarget {source = mockChainSt} =
  let LedgerState {lsCertState = certState} = (esLState . nesEs . mcsNes) mockChainSt
      -- TODO VState
      dstate = certState ^. certDStateL
      rewardDomain = domain (RewDepUView (dsUnified dstate))
      depositDomain = Map.keysSet (depositMap (dsUnified dstate))
   in counterexample
        ( ansiDocToString . Pretty.vsep $
            [ "Reward-Deposit domain invariant fails:"
            , Pretty.indent 2 $ diffExpr rewardDomain depositDomain
            ]
        )
        (rewardDomain === depositDomain)

itemPropToTraceProp ::
  (SourceSignalTarget (MOCKCHAIN era) -> Property) -> Trace (MOCKCHAIN era) -> Property
itemPropToTraceProp f trace1 = conjoin (map f (sourceSignalTargets trace1))

depositEra :: forall era. Reflect era => Proof era -> TestTree
depositEra proof =
  testGroup
    (show proof)
    [ testProperty
        "Deposits = KeyDeposits + PoolDeposits"
        (forAllChainTrace proof 10 (itemPropToTraceProp (depositInvariant @era)))
    , testProperty
        "Reward domain = Deposit domain"
        (forAllChainTrace proof 10 (itemPropToTraceProp (rewardDepositDomainInvariant @era)))
    ]

-- | Build a TestTree that tests 'f' at all the Eras listed in 'ps'
testEras :: String -> [Some Proof] -> (forall era. Reflect era => Proof era -> TestTree) -> TestTree
testEras message ps f = testGroup message (applyF ps)
  where
    applyF [] = []
    applyF (Some e : more) = unReflect f e : applyF more

depositTests :: TestTree
depositTests = testEras "deposit invariants" preBabbage depositEra
