{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Rules.IncrementalStake (
  incrStakeComputationTest,
  incrStakeComparisonTest,
  stakeDistr,
  aggregateUtxoCoinByCredential,
) where

import Test.Cardano.Ledger.Shelley.Rules.TestChain (
  TestingLedger,
  forAllChainTrace,
  ledgerTraceFromBlock,
  longTraceLen,
  traceLen,
 )

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (natVersion)
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), Ptr, StakeReference (StakeRefBase, StakeRefPtr))
import Cardano.Ledger.EpochBoundary (SnapShot (..), Stake (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  DState (..),
  EpochState (..),
  IncrementalStake (..),
  LedgerState (..),
  NewEpochState (..),
  PState (..),
  UTxOState (..),
  credMap,
  curPParamsEpochStateL,
  incrementalStakeDistr,
  ptrsMap,
 )
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.UTxO (UTxO (..), coinBalance)
import Control.SetAlgebra (dom, eval, (▷), (◁))
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.VMap as VMap
import Lens.Micro hiding (ix)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants, maxMajorPV)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState (..))
import Test.Cardano.Ledger.Shelley.Utils (
  ChainProperty,
 )
import Test.Cardano.Ledger.TerseTools (tersemapdiffs)
import Test.Control.State.Transition.Trace (
  SourceSignalTarget (..),
  sourceSignalTargets,
 )
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (
  Property,
  conjoin,
  counterexample,
  (===),
 )
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

incrStakeComputationTest ::
  forall era ledger.
  ( EraGen era
  , TestingLedger era ledger
  , ChainProperty era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  ) =>
  TestTree
incrStakeComputationTest =
  testProperty "incremental stake calc" $
    forAllChainTrace @era longTraceLen defaultConstants {maxMajorPV = natVersion @8} $ \tr -> do
      let ssts = sourceSignalTargets tr

      conjoin . concat $
        [ -- preservation properties
          map (incrStakeComp @era @ledger) ssts
        ]

incrStakeComp ::
  forall era ledger.
  (EraSegWits era, ChainProperty era, TestingLedger era ledger) =>
  SourceSignalTarget (CHAIN era) ->
  Property
incrStakeComp SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map checkIncrStakeComp $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    checkIncrStakeComp :: SourceSignalTarget ledger -> Property
    checkIncrStakeComp
      SourceSignalTarget
        { source = LedgerState UTxOState {utxosUtxo = u, utxosStakeDistr = sd} dp
        , signal = tx
        , target = LedgerState UTxOState {utxosUtxo = u', utxosStakeDistr = sd'} dp'
        } =
        counterexample
          ( mconcat
              ( [ "\nDetails:\n"
                , "\ntx\n"
                , show tx
                , "\nsize original utxo\n"
                , show (Map.size $ unUTxO u)
                , "\noriginal utxo\n"
                , show u
                , "\noriginal sd\n"
                , show sd
                , "\nfinal utxo\n"
                , show u'
                , "\nfinal sd\n"
                , show sd'
                , "\noriginal ptrs\n"
                , show ptrs
                , "\nfinal ptrs\n"
                , show ptrs'
                ]
              )
          )
          $ utxoBal === fromCompact incrStakeBal
        where
          utxoBal = coinBalance u'
          incrStakeBal = fold (credMap sd') <> fold (ptrMap sd')
          ptrs = ptrsMap $ certDState dp
          ptrs' = ptrsMap $ certDState dp'

incrStakeComparisonTest ::
  forall era.
  ( EraGen era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , EraGov era
  ) =>
  Proxy era ->
  TestTree
incrStakeComparisonTest Proxy =
  testProperty "Incremental stake distribution at epoch boundaries agrees" $
    forAllChainTrace traceLen (defaultConstants {maxMajorPV = natVersion @8}) $ \tr ->
      conjoin $
        map (\(SourceSignalTarget _ target _) -> checkIncrementalStake @era ((nesEs . chainNes) target)) $
          filter (not . sameEpoch) (sourceSignalTargets tr)
  where
    sameEpoch SourceSignalTarget {source, target} = epoch source == epoch target
    epoch = nesEL . chainNes

checkIncrementalStake ::
  forall era.
  (EraTxOut era, EraGov era) =>
  EpochState era ->
  Property
checkIncrementalStake es =
  let
    (LedgerState (UTxOState utxo _ _ _ incStake _) (CertState _vstate pstate dstate)) = esLState es
    stake = stakeDistr @era utxo dstate pstate
    istake = incrementalStakeDistr (es ^. curPParamsEpochStateL) incStake dstate pstate
   in
    counterexample
      ( "\nIncremental stake distribution does not match old style stake distribution"
          ++ tersediffincremental "differences: Old vs Incremental" (ssStake stake) (ssStake istake)
      )
      (stake === istake)

tersediffincremental :: String -> Stake c -> Stake c -> String
tersediffincremental message (Stake a) (Stake c) =
  tersemapdiffs (message ++ " " ++ "hashes") (mp a) (mp c)
  where
    mp = Map.map fromCompact . VMap.toMap

-- | Compute the current Stake Distribution. This was called at the Epoch boundary in the Snap Rule.
--   Now it is called in the tests to see that its incremental analog 'incrementalStakeDistr' agrees.
stakeDistr ::
  forall era.
  EraTxOut era =>
  UTxO era ->
  DState era ->
  PState era ->
  SnapShot (EraCrypto era)
stakeDistr u ds ps =
  SnapShot
    (Stake $ VMap.fromMap (UM.compactCoinOrError <$> eval (dom activeDelegs ◁ stakeRelation)))
    (VMap.fromMap delegs)
    (VMap.fromMap poolParams)
  where
    rewards' :: Map.Map (Credential 'Staking (EraCrypto era)) Coin
    rewards' = UM.rewardMap (dsUnified ds)
    delegs :: Map.Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
    delegs = UM.sPoolMap (dsUnified ds)
    ptrs' = ptrsMap ds
    PState {psStakePoolParams = poolParams} = ps
    stakeRelation :: Map (Credential 'Staking (EraCrypto era)) Coin
    stakeRelation = aggregateUtxoCoinByCredential ptrs' u rewards'
    activeDelegs :: Map.Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
    activeDelegs = eval ((dom rewards' ◁ delegs) ▷ dom poolParams)

-- | Sum up all the Coin for each staking Credential. This function has an
--   incremental analog. See 'incrementalAggregateUtxoCoinByCredential'
aggregateUtxoCoinByCredential ::
  forall era.
  EraTxOut era =>
  Map Ptr (Credential 'Staking (EraCrypto era)) ->
  UTxO era ->
  Map (Credential 'Staking (EraCrypto era)) Coin ->
  Map (Credential 'Staking (EraCrypto era)) Coin
aggregateUtxoCoinByCredential ptrs (UTxO u) initial =
  Map.foldl' accum initial u
  where
    accum ans out =
      let c = out ^. coinTxOutL
       in case out ^. addrTxOutL of
            Addr _ _ (StakeRefPtr p)
              | Just cred <- Map.lookup p ptrs -> Map.insertWith (<>) cred c ans
            Addr _ _ (StakeRefBase hk) -> Map.insertWith (<>) hk c ans
            _other -> ans
