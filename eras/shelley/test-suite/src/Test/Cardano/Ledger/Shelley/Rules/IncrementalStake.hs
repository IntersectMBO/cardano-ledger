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
  incrStakeUnitTest,
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
import Test.Cardano.Ledger.UnitTestTools

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Ptr, StakeReference (StakeRefBase, StakeRefPtr))
import Cardano.Ledger.EpochBoundary (SnapShot (..), Stake (..))
import Cardano.Ledger.Shelley (ShelleyEra)
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
  updateStakeDistribution,
 )
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.UTxO (UTxO (..), coinBalance)
import Control.SetAlgebra (dom, eval, (▷), (◁))
import Data.Default.Class (Default (..))
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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
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
    (VMap.fromMap poolPs)
  where
    rewards' :: Map.Map (Credential 'Staking (EraCrypto era)) Coin
    rewards' = UM.rewardMap (dsUnified ds)
    delegs :: Map.Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
    delegs = UM.sPoolMap (dsUnified ds)
    ptrs' = ptrsMap ds
    PState {psStakePoolParams = poolPs} = ps
    stakeRelation :: Map (Credential 'Staking (EraCrypto era)) Coin
    stakeRelation = aggregateUtxoCoinByCredential ptrs' u rewards'
    activeDelegs :: Map.Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
    activeDelegs = eval ((dom rewards' ◁ delegs) ▷ dom poolPs)

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

-- =================================================================
-- Incremental Stake Unit test

-- There are 2 Pools
pool1, pool2 :: KeyHash 'StakePool StandardCrypto
pool1 = poolHash 30
pool2 = poolHash 31

-- There are 5 wallets, each with its own Credential
tom, ann, ron, john, mary :: Credential 'Staking StandardCrypto
tom = stakeCred 1
ann = stakeCred 2
ron = stakeCred 3
john = stakeCred 4
mary = stakeCred 5

-- They each own an address
tomAddr, annAddr, ronAddr, johnAddr, maryAddr :: Addr StandardCrypto
tomAddr = addr 1 0 -- 0 means tomAddr  does not have a StakeReference
annAddr = addr 2 2
ronAddr = addr 3 3
johnAddr = addr 4 4
maryAddr = addr 5 0 -- 0 means maryAddr  does not have a StakeReference

-- Each wallet has registered its credential,
rewards :: Map (Credential 'Staking StandardCrypto) UM.RDPair
rewards =
  Map.fromList -- (rdpair reward deposit)
    [ (tom, rdpair 5 6) -- only rewards should be distributed
    , (ann, rdpair 7 6) -- so we should see 53 distributed
    , (ron, rdpair 11 6)
    , (john, rdpair 13 6)
    , (mary, rdpair 17 6)
    ]

rdpair :: Word64 -> Word64 -> UM.RDPair
rdpair x y = UM.RDPair (CompactCoin x) (CompactCoin y)

-- Each wallet delegates to one of the pools
delegations :: Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto)
delegations =
  Map.fromList
    [ (tom, pool1) -- since every one is delegated
    , (ann, pool2) -- no ones stake should be left out
    , (ron, pool1)
    , (john, pool2)
    , (mary, pool2)
    ]

-- Each wallet has one or more UTxO entries
-- Since tom and mary use a StakeRefNull those entries will not be distributed
utxo1 :: UTxO (ShelleyEra StandardCrypto)
utxo1 =
  UTxO
    ( Map.fromList
        [ (txIn 40 1, txOut tomAddr (Coin 23)) -- Not distrubuted 23
        , (txIn 40 2, txOut tomAddr (Coin 29)) -- Not distributed 29
        , (txIn 42 4, txOut annAddr (Coin 31))
        , (txIn 43 1, txOut ronAddr (Coin 33))
        , (txIn 44 2, txOut johnAddr (Coin 41))
        , (txIn 45 1, txOut maryAddr (Coin 43)) -- Not distributed 43
        ] -- total 200 - 95  105 should be distributed
    )

pparams :: PParams (ShelleyEra StandardCrypto)
pparams = emptyPParams & ppProtocolVersionL .~ ProtVer (natVersion @2) 0 -- Shelley

incrementalStake :: IncrementalStake StandardCrypto
incrementalStake = updateStakeDistribution pparams (IStake mempty mempty) mempty utxo1

umap :: UM.UMap StandardCrypto
umap = UM.unify rewards Map.empty delegations Map.empty

dState :: DState (ShelleyEra StandardCrypto)
dState = def {dsUnified = umap}

pState :: PState (ShelleyEra StandardCrypto)
pState =
  def
    { psStakePoolParams =
        Map.fromList [(pool1, poolParams pool1 30), (pool2, poolParams pool2 31)]
    }

testStakeDistr :: Map (Credential 'Staking StandardCrypto) Coin
testStakeDistr = Map.map fromCompact (VMap.toMap (unStake (ssStake snap)))
  where
    snap = incrementalStakeDistr pparams incrementalStake dState pState

expected :: Map (Credential 'Staking StandardCrypto) Coin
expected =
  Map.fromList -- Coin Part is (utxoCoin <> rewardCoin)
    [ (tom, Coin 0 <> Coin 5) -- tom uxtxoCoin is zero because address has StakeRefNull
    , (ann, Coin 31 <> Coin 7)
    , (ron, Coin 33 <> Coin 11)
    , (john, Coin 41 <> Coin 13)
    , (mary, Coin 0 <> Coin 17) -- mary uxtxoCoin is zero because address has StakeRefNull
    ]

incrStakeUnitTest :: TestTree
incrStakeUnitTest =
  testGroup
    "IncrementalStake unit tests"
    [ testCase
        "Reward part is counted if delegated"
        (assertEqual "manual and computed naps disagree" expected testStakeDistr)
    , testCase
        "Correct sum is computed"
        (assertEqual "sum is not expected value of 158" (Coin 158) (fold testStakeDistr))
    ]
