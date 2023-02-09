{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Rules.TestChain (
  -- TestPoolReap
  removedAfterPoolreap,
  -- TestNewEpoch
  adaPreservationChain,
  collisionFreeComplete,
  -- Test Pool
  poolProperties,
  -- Test Delegation
  delegProperties,
  forAllChainTrace,
  -- Helper Functions
  ledgerTraceFromBlock,
  -- Helper Constraints
  TestingLedger,
  splitTrace,
  forEachEpochTrace,
  depositTests,
  minimal,
  traceLen,
  longTraceLen,
) where

import Cardano.Ledger.BaseTypes (Globals, StrictMaybe (..))
import Cardano.Ledger.Block (
  Block (..),
  bbody,
  bheader,
  neededTxInsForBlock,
 )
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Witness))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (ApplyBlock, ShelleyDELEG)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Internal (compareAdaPots)
import Cardano.Ledger.Shelley.LedgerState (
  DPState (..),
  DState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  PState (..),
  UTxOState (..),
  completeRupd,
  deltaF,
  deltaR,
  deltaT,
  iRReserves,
  iRTreasury,
  keyTxRefunds,
  rewards,
  rs,
  totalTxDeposits,
 )
import Cardano.Ledger.Shelley.Rewards (sumRewards)
import Cardano.Ledger.Shelley.Rules (
  DelegEnv (..),
  LedgerEnv (..),
  PoolEnv (..),
  ShelleyPOOL,
  votedValue,
 )
import Cardano.Ledger.Shelley.Rules.Reports (
  showCred,
  showIR,
  showKeyHash,
  showListy,
  showMap,
  showWithdrawal,
  synopsisCert,
  synopsisCoinMap,
  trim,
 )
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.TreeDiff (diffExpr, ediffEq)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UMapCompact (depositView, sumRewardsView)
import qualified Cardano.Ledger.UMapCompact as UM
import Cardano.Ledger.UTxO (UTxO (..), coinBalance, txins, txouts)
import Cardano.Ledger.Val ((<+>), (<->))
import Cardano.Protocol.TPraos.API (GetLedgerView)
import Cardano.Protocol.TPraos.BHeader (
  BHeader (..),
  bhbody,
  bheaderSlotNo,
 )
import Control.Monad.Trans.Reader (ReaderT)
import Control.SetAlgebra (eval, (∩))
import Control.State.Transition
import Control.State.Transition.Trace (
  SourceSignalTarget (..),
  Trace (..),
  TraceOrder (OldestFirst),
  sourceSignalTargets,
  splitTrace,
  traceStates,
 )
import qualified Control.State.Transition.Trace as Trace
import Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import Data.Foldable (fold, foldl', toList)
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import Lens.Micro hiding (ix)
import Lens.Micro.Extras (view)
import Test.Cardano.Ledger.Shelley.Generator.Block (tickChainState)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import qualified Test.Cardano.Ledger.Shelley.Generator.Presets as Preset (genEnv)
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass (scriptKeyCombinations)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Generator.Trace.Chain (mkGenesisChainState)
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState (..), totalAda, totalAdaPots)
import qualified Test.Cardano.Ledger.Shelley.Rules.Deleg as TestDeleg (
  checkInstantaneousRewards,
  keyDeRegistration,
  keyDelegation,
  keyRegistration,
  rewardsSumInvariant,
 )
import qualified Test.Cardano.Ledger.Shelley.Rules.Pool as TestPool (
  poolRegistration,
  poolRetirement,
  poolStateIsInternallyConsistent,
 )
import qualified Test.Cardano.Ledger.Shelley.Rules.PoolReap as TestPoolreap
import Test.Cardano.Ledger.Shelley.Utils (
  ChainProperty,
  epochFromSlotNo,
  runShelleyBase,
  testGlobals,
 )
import Test.QuickCheck (
  Property,
  Testable (..),
  conjoin,
  counterexample,
  withMaxSuccess,
  (.&&.),
  (.||.),
  (===),
 )
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as TQC

-- ==========================================

adaIsPreserved ::
  forall era.
  ( EraGen era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , ProtVerAtMost era 8
  , GovernanceState era ~ ShelleyPPUPState era
  , EraGovernance era
  ) =>
  Property
adaIsPreserved =
  forAllChainTrace @era longTraceLen $ \tr -> do
    let ssts :: [SourceSignalTarget (CHAIN era)]
        -- Signal(CHAIN era) = Block (BHeader (EraCrypto era)) era
        ssts = sourceSignalTargets tr
        -- noEpochBoundarySsts = filter sameEpoch ssts
        justBoundarySsts = filter (not . sameEpoch) ssts

    conjoin (map (checkPreservation @era) (zip justBoundarySsts [0 ..]))

minimal ::
  forall era.
  ( EraGen era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , ProtVerAtMost era 8
  , GovernanceState era ~ ShelleyPPUPState era
  , EraGovernance era
  ) =>
  TestTree
minimal =
  testGroup
    "Minimal Property Tests"
    [ TQC.testProperty "total amount of Ada is preserved (Chain)" (adaIsPreserved @era)
    ]

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Word64
numberOfTests = 300

traceLen :: Word64
traceLen = 100

longTraceLen :: Word64
longTraceLen = 150

type TestingLedger era ledger =
  ( BaseM ledger ~ ReaderT Globals Identity
  , Environment ledger ~ LedgerEnv era
  , State ledger ~ LedgerState era
  , Signal ledger ~ Tx era
  , Embed (EraRule "DELEGS" era) ledger
  , Embed (EraRule "UTXOW" era) ledger
  , STS ledger
  )

----------------------------------------------------------------------
-- Properties for Chain
---------------------------------------------------------------------

-- | Tx inputs are eliminated, outputs added to utxo and TxIds are unique
collisionFreeComplete ::
  forall era ledger.
  ( EraGen era
  , EraGovernance era
  , ChainProperty era
  , TestingLedger era ledger
  , QC.HasTrace (CHAIN era) (GenEnv era)
  ) =>
  Property
collisionFreeComplete =
  forAllChainTrace @era traceLen $ \tr -> do
    let ssts = sourceSignalTargets tr
    conjoin . concat $
      [ -- collision freeness
        map (eliminateTxInputs @era @ledger) ssts
      , map (newEntriesAndUniqueTxIns @era @ledger) ssts
      , -- no double spend
        map noDoubleSpend ssts
      , -- tx signatures
        map (requiredMSigSignaturesSubset @era @ledger) ssts
      ]

-- | Various preservation properties
adaPreservationChain ::
  forall era ledger.
  ( EraGen era
  , EraGovernance era
  , TestingLedger era ledger
  , ChainProperty era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , ProtVerAtMost era 8
  , GovernanceState era ~ ShelleyPPUPState era
  ) =>
  Property
adaPreservationChain =
  forAllChainTrace @era longTraceLen $ \tr -> do
    let ssts :: [SourceSignalTarget (CHAIN era)]
        -- In this test, the STS Signal has this definition
        -- Signal(CHAIN era) = Block (BHeader (EraCrypto era)) era
        ssts = sourceSignalTargets tr
        noEpochBoundarySsts = filter sameEpoch ssts
        justBoundarySsts = filter (not . sameEpoch) ssts

    conjoin . concat $
      [ -- preservation properties
        map (checkPreservation @era) (zip justBoundarySsts [0 ..])
      , map (potsSumIncreaseWithdrawalsPerTx @era @ledger) ssts
      , map (potsSumIncreaseByRewardsPerTx @era @ledger) ssts
      , map (preserveBalance @era @ledger) ssts
      , map (preserveBalanceRestricted @era @ledger) ssts
      , map (preserveOutputsTx @era @ledger) ssts
      , map (potsRewardsDecreaseByWithdrawalsPerTx @era @ledger) ssts
      , map (canRestrictUTxO @era @ledger) ssts
      , -- well formed deposits
        map nonNegativeDeposits ssts
      , -- non-epoch-boundary preservation properties
        map checkWithdrawlBound noEpochBoundarySsts
      , map (utxoDepositsIncreaseByFeesWithdrawals @era @ledger) noEpochBoundarySsts
      , map potsSumIncreaseWithdrawalsPerBlock noEpochBoundarySsts
      , map feesNonDecreasing noEpochBoundarySsts
      ]

infoRetire :: Map (KeyHash 'StakePool c) Coin -> KeyHash 'StakePool c -> String
infoRetire deposits keyhash = showKeyHash keyhash ++ extra
  where
    extra = case Map.lookup keyhash deposits of
      Nothing -> " (?)"
      Just coin -> " (" ++ show coin ++ ")"

-- ADA should be preserved for all state transitions in the generated trace
checkPreservation ::
  forall era.
  ( EraSegWits era
  , ShelleyEraTxBody era
  , ProtVerAtMost era 8
  , GovernanceState era ~ ShelleyPPUPState era
  ) =>
  (SourceSignalTarget (CHAIN era), Int) ->
  Property
checkPreservation (SourceSignalTarget {source, target, signal}, count) =
  counterexample
    ( mconcat
        ( [ "\ncount = " ++ show count ++ "\n"
          , compareAdaPots "before" (totalAdaPots source) "after" (totalAdaPots target)
          , "\n\nTotal lovelace before block\n"
          , show sourceTotal
          , "\n\nTotal lovelace after block\n"
          , show targetTotal
          , "\n\nEpoch before block\n"
          , show (nesEL . chainNes $ source)
          , "\n\nEpoch after block\n"
          , show (nesEL . chainNes $ target)
          , "\n\nCurrent protocol parameters\n"
          , show currPP
          , "\nReward Accounts before update\n"
          , showMap (trim 10 . showCred) show (UM.unUnify oldRAs)
          , "\nReward Accounts after update\n"
          , showMap (trim 10 . showCred) show (UM.unUnify newRAs)
          , "\nRetiring pools before update\n"
          , showMap (infoRetire oldPoolDeposit) show oldRetire
          , "\nRetiring pools after update\n"
          , showMap (infoRetire newPoolDeposit) show newRetire
          , "\nMIR\n"
          , showIR mir
          , "\n\nRegistered Reserves MIR total "
          , show (fold regMirRes)
          , "\n\nUnregistered Reserves MIR total "
          , show (fold unRegMirRes)
          , "\n\nRegistered Treasury MIR total "
          , show (fold regMirTre)
          , "\n\nUnregistered Treasury MIR total "
          , show (fold unRegMirTre)
          , "\n\nPools Retiring This epoch\n"
          , showMap (infoRetire oldPoolDeposit) show (Map.filter (\e -> e == (nesEL . chainNes $ target)) oldRetire)
          ]
            ++ obligationMsgs
            ++ rewardUpdateMsgs
            ++ ["\n\ntransactions"]
            ++ txs
        )
    )
    $ sourceTotal === targetTotal
  where
    sourceTotal = totalAda source
    targetTotal = totalAda target

    currPP = esPp . nesEs . chainNes $ source
    prevPP = view ppProtocolVersionL . esPrevPp . nesEs . chainNes $ source

    ru' = nesRu . chainNes $ source
    lsOld = esLState . nesEs . chainNes $ source
    lsNew = esLState . nesEs . chainNes $ target
    oldRAs = rewards . dpsDState . lsDPState $ lsOld
    newRAs = rewards . dpsDState . lsDPState $ lsNew
    oldDPState = lsDPState $ lsOld
    oldRetire = psRetiring . dpsPState . lsDPState $ lsOld
    newRetire = psRetiring . dpsPState . lsDPState $ lsNew
    oldPoolDeposit = psDeposits . dpsPState . lsDPState $ lsOld
    newPoolDeposit = psDeposits . dpsPState . lsDPState $ lsNew

    proposal = votedValue (proposals . utxosGovernance . lsUTxOState $ lsOld) currPP 5
    obligationMsgs = case proposal of
      Nothing -> []
      Just proposal' ->
        [ "\n\nProposed protocol parameter update\n"
        , show proposal'
        ]

    mir = dsIRewards . dpsDState . lsDPState $ lsOld
    isRegistered kh _ = UM.member kh oldRAs
    (regMirRes, unRegMirRes) = Map.partitionWithKey isRegistered (iRReserves mir)
    (regMirTre, unRegMirTre) = Map.partitionWithKey isRegistered (iRTreasury mir)

    rewardUpdateMsgs = case ru' of
      SNothing -> []
      SJust ru'' ->
        let (ru, _rewevent) = runShelleyBase (completeRupd ru'')
            regRewards = Map.filterWithKey (\kh _ -> UM.member kh oldRAs) (rs ru)
         in [ "\n\nSum of new rewards "
            , show (sumRewards prevPP (rs ru))
            , "\n\nNew rewards "
            , show (rs ru)
            , "\n\nSum of new registered rewards "
            , show (sumRewards prevPP regRewards)
            , "\n\nChange in Fees "
            , show (deltaF ru)
            , "\n\nChange in Treasury "
            , show (deltaT ru)
            , "\n\nChange in Reserves "
            , show (deltaR ru)
            , "\n\nNet effect of reward update "
            , show $
                deltaT ru
                  <> deltaF ru
                  <> deltaR ru
                  <> toDeltaCoin (sumRewards prevPP (rs ru))
            ]

    txs' = toList $ (fromTxSeq @era . bbody) signal
    txs = map dispTx (zip txs' [0 :: Int ..])

    dispTx (tx, ix) =
      "\n\n******** Transaction "
        ++ show ix
        ++ " "
        ++ show (hashAnnotated (tx ^. bodyTxL))
        ++ "\nfee :"
        ++ show (tx ^. bodyTxL . feeTxBodyL)
        ++ "\nwithdrawals:"
        ++ showWithdrawal (tx ^. bodyTxL . withdrawalsTxBodyL)
        ++ "\ncerts:"
        ++ showListy (("   " ++) . synopsisCert) (toList $ tx ^. bodyTxL . certsTxBodyL)
        ++ "total deposits "
        ++ show (totalTxDeposits currPP oldDPState (tx ^. bodyTxL))
        ++ "\ntotal refunds "
        ++ show (keyTxRefunds currPP oldDPState (tx ^. bodyTxL))

-- If we are not at an Epoch Boundary (i.e. epoch source == epoch target)
-- then the total rewards should change only by withdrawals
checkWithdrawlBound ::
  EraGen era => SourceSignalTarget (CHAIN era) -> Property
checkWithdrawlBound SourceSignalTarget {source, signal, target} =
  counterexample "checkWithdrawlBound" $
    rewardDelta === withdrawals signal
  where
    rewardDelta :: Coin
    rewardDelta =
      fromCompact (sumRewardsView (rewards . dpsDState . lsDPState . esLState . nesEs . chainNes $ source))
        <-> fromCompact (sumRewardsView (rewards . dpsDState . lsDPState . esLState . nesEs . chainNes $ target))

-- | If we are not at an Epoch Boundary, then (Utxo + Deposits)
-- increases by Withdrawals minus Fees (for all transactions in a block)
utxoDepositsIncreaseByFeesWithdrawals ::
  forall era ledger.
  ( ChainProperty era
  , EraGen era
  , TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
utxoDepositsIncreaseByFeesWithdrawals SourceSignalTarget {source, signal, target} =
  counterexample "utxoDepositsIncreaseByFeesWithdrawals" $
    circulation target
      <-> circulation source
      === withdrawals signal
      <-> txFees ledgerTr
  where
    us = lsUTxOState . esLState . nesEs . chainNes
    circulation chainSt =
      let UTxOState {utxosUtxo = u, utxosDeposited = d} = us chainSt
       in coinBalance u <+> d
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger source signal

-- | If we are not at an Epoch Boundary, then (Utxo + Deposits + Fees)
-- increases by sum of withdrawals for all transactions in a block
potsSumIncreaseWithdrawalsPerBlock ::
  (ChainProperty era, EraGen era) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsSumIncreaseWithdrawalsPerBlock SourceSignalTarget {source, signal, target} =
  counterexample
    "potsSumIncreaseWithdrawalsPerBlock"
    $ potsSum target <-> potsSum source === withdrawals signal
  where
    potsSum chainSt =
      let UTxOState {utxosUtxo = u, utxosDeposited = d, utxosFees = f} =
            lsUTxOState . esLState . nesEs . chainNes $ chainSt
       in coinBalance u <+> d <+> f

-- | If we are not at an Epoch Boundary, then (Utxo + Deposits + Fees)
-- increases by sum of withdrawals in a transaction
potsSumIncreaseWithdrawalsPerTx ::
  forall era ledger.
  ( ChainProperty era
  , EraGen era
  , TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsSumIncreaseWithdrawalsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "potsSumIncreaseWithdrawalsPerTx" $
    conjoin $
      map sumIncreaseWithdrawals $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    sumIncreaseWithdrawals :: SourceSignalTarget ledger -> Property
    sumIncreaseWithdrawals
      SourceSignalTarget
        { source = LedgerState UTxOState {utxosUtxo = u, utxosDeposited = d, utxosFees = f} _
        , signal = tx
        , target = LedgerState UTxOState {utxosUtxo = u', utxosDeposited = d', utxosFees = f'} _
        } =
        property (hasFailedScripts tx)
          .||. (coinBalance u' <+> d' <+> f')
            <-> (coinBalance u <+> d <+> f)
            === fold (unWithdrawals (tx ^. bodyTxL . withdrawalsTxBodyL))

-- | (Utxo + Deposits + Fees) increases by the reward delta
potsSumIncreaseByRewardsPerTx ::
  forall era ledger.
  ( ChainProperty era
  , EraSegWits era
  , TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsSumIncreaseByRewardsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "potsSumIncreaseByRewardsPerTx" $
    conjoin $
      map sumIncreaseRewards $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    sumIncreaseRewards
      SourceSignalTarget
        { source =
          LedgerState
            UTxOState {utxosUtxo = u, utxosDeposited = d, utxosFees = f}
            DPState {dpsDState = DState {dsUnified = umap1}}
        , target =
          LedgerState
            UTxOState {utxosUtxo = u', utxosDeposited = d', utxosFees = f'}
            DPState {dpsDState = DState {dsUnified = umap2}}
        } =
        (coinBalance u' <+> d' <+> f')
          <-> (coinBalance u <+> d <+> f)
          === (UM.fromCompact (sumRewardsView (UM.RewardDeposits umap1)))
          <-> (UM.fromCompact (sumRewardsView (UM.RewardDeposits umap2)))

-- | The Rewards pot decreases by the sum of withdrawals in a transaction
potsRewardsDecreaseByWithdrawalsPerTx ::
  forall era ledger.
  ( ChainProperty era
  , EraGen era
  , TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsRewardsDecreaseByWithdrawalsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "potsRewardsDecreaseByWithdrawalsPerTx" $
    conjoin $
      map rewardsDecreaseByWithdrawals $
        sourceSignalTargets ledgerTr
  where
    rewardsSum = UM.fromCompact . sumRewardsView . rewards . dpsDState
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    rewardsDecreaseByWithdrawals
      SourceSignalTarget
        { source = LedgerState _ dpstate
        , signal = tx
        , target = LedgerState _ dpstate'
        } =
        let totalRewards = rewardsSum dpstate
            totalRewards' = rewardsSum dpstate'
            txWithdrawals = fold (unWithdrawals (tx ^. bodyTxL . withdrawalsTxBodyL))
         in conjoin
              [ counterexample
                  "A transaction should not increase the Rewards pot"
                  (totalRewards >= totalRewards')
              , counterexample
                  "Withdrawals should be non-negative"
                  (txWithdrawals >= Coin 0)
              , counterexample
                  "Rewards should increase by withdrawals"
                  (hasFailedScripts tx || totalRewards <-> totalRewards' == txWithdrawals)
              ]

-- | Preserve the balance in a transaction, i.e., the sum of the consumed value
-- equals the sum of the created value.
preserveBalance ::
  forall era ledger.
  ( ChainProperty era
  , EraGen era
  , TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
preserveBalance SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "preserveBalance" $
    conjoin $
      map createdIsConsumed $
        sourceSignalTargets ledgerTr
  where
    (tickedChainSt, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    pp_ = (esPp . nesEs . chainNes) tickedChainSt

    createdIsConsumed SourceSignalTarget {source = ledgerSt, signal = tx, target = ledgerSt'} =
      counterexample
        "preserveBalance created /= consumed ... "
        (failedScripts .||. ediffEq created consumed_)
      where
        failedScripts = property $ hasFailedScripts tx
        LedgerState (UTxOState {utxosUtxo = u}) dpstate = ledgerSt
        LedgerState (UTxOState {utxosUtxo = u'}) _ = ledgerSt'
        txb = tx ^. bodyTxL
        created =
          coinBalance u'
            <+> txb
              ^. feeTxBodyL
            <+> totalTxDeposits pp_ dpstate txb
        consumed_ =
          coinBalance u
            <+> keyTxRefunds pp_ dpstate txb
            <+> fold (unWithdrawals (txb ^. withdrawalsTxBodyL))

-- | Preserve balance restricted to TxIns and TxOuts of the Tx
preserveBalanceRestricted ::
  forall era ledger.
  ( ChainProperty era
  , TestingLedger era ledger
  , ShelleyEraTxBody era
  , EraSegWits era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
preserveBalanceRestricted SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "preserveBalanceRestricted" $
    conjoin $
      map createdIsConsumed $
        sourceSignalTargets ledgerTr
  where
    (tickedChainSt, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    pp_ = (esPp . nesEs . chainNes) tickedChainSt

    createdIsConsumed
      SourceSignalTarget
        { source = LedgerState (UTxOState {utxosUtxo = UTxO u}) dpstate
        , signal = tx
        } =
        inps === outs
        where
          txb = tx ^. bodyTxL
          inps =
            coinBalance @era (UTxO (Map.restrictKeys u (txb ^. inputsTxBodyL)))
              <> keyTxRefunds pp_ dpstate txb
              <> fold (unWithdrawals (txb ^. withdrawalsTxBodyL))
          outs =
            coinBalance (txouts @era txb)
              <> txb
                ^. feeTxBodyL
              <> totalTxDeposits pp_ dpstate txb

preserveOutputsTx ::
  forall era ledger.
  ( ChainProperty era
  , EraGen era
  , TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
preserveOutputsTx SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "preserveOutputsTx" $
    conjoin $
      map outputPreserved $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    outputPreserved
      SourceSignalTarget
        { target = LedgerState (UTxOState {utxosUtxo = UTxO utxo}) _12
        , signal = tx
        } =
        let UTxO outs = txouts @era (tx ^. bodyTxL)
         in property $
              hasFailedScripts tx
                .||. counterexample "TxOuts are not a subset of UTxO" (outs `Map.isSubmapOf` utxo)

canRestrictUTxO ::
  forall era ledger.
  ( ChainProperty era
  , EraSegWits era
  , TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
canRestrictUTxO SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "canRestrictUTxO" $
    conjoin $
      zipWith
        outputPreserved
        (sourceSignalTargets ledgerTrFull)
        (sourceSignalTargets ledgerTrRestr)
  where
    (_, ledgerTrFull) = ledgerTraceFromBlock @era @ledger chainSt block
    (UTxO irrelevantUTxO, ledgerTrRestr) =
      ledgerTraceFromBlockWithRestrictedUTxO @era @ledger chainSt block
    outputPreserved
      SourceSignalTarget {target = LedgerState (UTxOState {utxosUtxo = UTxO uFull}) _}
      SourceSignalTarget {target = LedgerState (UTxOState {utxosUtxo = UTxO uRestr}) _} =
        counterexample
          (unlines ["non-disjoint:", show uRestr, show irrelevantUTxO])
          (uRestr `Map.disjoint` irrelevantUTxO)
          .&&. uFull
            === (uRestr `Map.union` irrelevantUTxO)

-- | Check that consumed inputs are eliminated from the resulting UTxO
eliminateTxInputs ::
  forall era ledger.
  ( ChainProperty era
  , EraGen era
  , TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
eliminateTxInputs SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "eliminateTxInputs" $
    conjoin $
      map inputsEliminated $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    inputsEliminated
      SourceSignalTarget
        { target = LedgerState (UTxOState {utxosUtxo = (UTxO u')}) _
        , signal = tx
        } =
        property $
          hasFailedScripts tx
            || Set.null (eval (txins @era (tx ^. bodyTxL) ∩ Map.keysSet u'))

-- | Collision-Freeness of new TxIds - checks that all new outputs of a Tx are
-- included in the new UTxO and that all TxIds are new.
newEntriesAndUniqueTxIns ::
  forall era ledger.
  ( ChainProperty era
  , EraGen era
  , TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
newEntriesAndUniqueTxIns SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "newEntriesAndUniqueTxIns" $
    conjoin $
      map newEntryPresent $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    newEntryPresent
      SourceSignalTarget
        { source = LedgerState (UTxOState {utxosUtxo = UTxO u}) _
        , signal = tx
        , target = LedgerState (UTxOState {utxosUtxo = UTxO u'}) _
        } =
        let UTxO outs = txouts @era (tx ^. bodyTxL)
            outIds = Set.map (\(TxIn _id _) -> _id) (Map.keysSet outs)
            oldIds = Set.map (\(TxIn _id _) -> _id) (Map.keysSet u)
         in property $
              hasFailedScripts tx
                || ((outIds `Set.disjoint` oldIds) && (outs `Map.isSubmapOf` u'))

-- | Check for required signatures in case of Multi-Sig. There has to be one set
-- of possible signatures for a multi-sig script which is a sub-set of the
-- signatures of the tansaction.
requiredMSigSignaturesSubset ::
  forall era ledger.
  ( ChainProperty era
  , EraGen era
  , TestingLedger era ledger
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
requiredMSigSignaturesSubset SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "requiredMSigSignaturesSubset" $
    conjoin $
      map signaturesSubset $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    signaturesSubset :: SourceSignalTarget ledger -> Property
    signaturesSubset SourceSignalTarget {signal = tx} =
      let khs = keyHashSet tx
       in property $
            all (existsReqKeyComb khs) (tx ^. witsTxL . scriptTxWitsL)

    existsReqKeyComb keyHashes msig =
      any (\kl -> Set.fromList kl `Set.isSubsetOf` keyHashes) (scriptKeyCombinations (Proxy @era) msig)
    keyHashSet :: Tx era -> Set (KeyHash 'Witness (EraCrypto era))
    keyHashSet tx_ =
      Set.map witVKeyHash (tx_ ^. witsTxL . addrTxWitsL)

--- | Check for absence of double spend in a block
noDoubleSpend ::
  forall era.
  (ChainProperty era, EraGen era) =>
  SourceSignalTarget (CHAIN era) ->
  Property
noDoubleSpend SourceSignalTarget {signal} =
  counterexample "noDoubleSpend" $
    [] === getDoubleInputs txs
  where
    txs = toList $ (fromTxSeq @era . bbody) signal

    getDoubleInputs :: [Tx era] -> [(Tx era, [Tx era])]
    getDoubleInputs [] = []
    getDoubleInputs (t : ts) = lookForDoubleSpends t ts ++ getDoubleInputs ts
    lookForDoubleSpends :: Tx era -> [Tx era] -> [(Tx era, [Tx era])]
    lookForDoubleSpends _ [] = []
    lookForDoubleSpends tx_j ts =
      [(tx_j, doubles) | not (null doubles)]
      where
        doubles =
          if hasFailedScripts tx_j
            then []
            else
              filter
                ( \tx_i ->
                    not
                      ( hasFailedScripts tx_i
                          || Set.disjoint inps_j (tx_i ^. bodyTxL . inputsTxBodyL)
                      )
                )
                ts
        inps_j = tx_j ^. bodyTxL . inputsTxBodyL

withdrawals ::
  forall era.
  EraGen era =>
  Block (BHeader (EraCrypto era)) era ->
  Coin
withdrawals (UnserialisedBlock _ txseq) =
  foldl'
    ( \c tx ->
        let wdrls = unWithdrawals $ tx ^. bodyTxL . withdrawalsTxBodyL
         in if hasFailedScripts tx then c else c <> fold wdrls
    )
    (Coin 0)
    $ fromTxSeq @era txseq

txFees ::
  forall era ledger.
  (EraGen era, TestingLedger era ledger) =>
  Trace ledger ->
  Coin
txFees ledgerTr =
  foldl' f (Coin 0) (sourceSignalTargets ledgerTr)
  where
    f
      c
      SourceSignalTarget
        { source = LedgerState UTxOState {utxosUtxo = utxo} _
        , signal = tx
        } = c <> feeOrCollateral tx utxo

-- | Check that deposits are always non-negative
nonNegativeDeposits ::
  SourceSignalTarget (CHAIN era) ->
  Property
nonNegativeDeposits SourceSignalTarget {source = chainSt} =
  let es = (nesEs . chainNes) chainSt
      UTxOState {utxosDeposited = d} = (lsUTxOState . esLState) es
   in counterexample ("nonNegativeDeposits: " ++ show d) (d >= mempty)

-- | Check that the sum of key Deposits (in the UMap) and the pool Depoits (in psDeposits) are equal to the utsosDeposits
depositInvariant ::
  SourceSignalTarget (CHAIN era) ->
  Property
depositInvariant SourceSignalTarget {source = chainSt} =
  let LedgerState {lsUTxOState = utxost, lsDPState = DPState dstate pstate} = (esLState . nesEs . chainNes) chainSt
      allDeposits = utxosDeposited utxost
      sumCoin m = Map.foldl' (<+>) (Coin 0) m
      keyDeposits = (UM.fromCompact . UM.sumDepositView . UM.RewardDeposits . dsUnified) dstate
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
  SourceSignalTarget (CHAIN era) ->
  Property
rewardDepositDomainInvariant SourceSignalTarget {source = chainSt} =
  let LedgerState {lsDPState = DPState dstate _} = (esLState . nesEs . chainNes) chainSt
      rewardDomain = UM.domain (UM.RewardDeposits (dsUnified dstate))
      depositDomain = Map.keysSet (depositView (dsUnified dstate))
   in counterexample
        ( unlines
            [ "Reward-Deposit domain invariant fails"
            , diffExpr rewardDomain depositDomain
            ]
        )
        (rewardDomain === depositDomain)

-- | Checks that the fees are non-decreasing when not at an epoch boundary
feesNonDecreasing ::
  SourceSignalTarget (CHAIN era) ->
  Property
feesNonDecreasing SourceSignalTarget {source, target} =
  counterexample ("feesNonDecreasing: " <> show (fees_ source) <> " <= " <> show (fees_ target)) $
    fees_ source <= fees_ target
  where
    fees_ chainSt =
      let UTxOState {utxosFees = fees} =
            lsUTxOState . esLState . nesEs . chainNes $ chainSt
       in fees

-- ===================================================

-- | Properties on really short chains, with only 100 successes
shortChainTrace ::
  forall era.
  ( EraGen era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , EraGovernance era
  ) =>
  (SourceSignalTarget (CHAIN era) -> Property) ->
  Property
shortChainTrace f = withMaxSuccess 100 $ forAllChainTrace @era 10 $ \tr -> conjoin (map f (sourceSignalTargets tr))

-- | Tests that redundant Deposit information is consistent
depositTests ::
  forall era.
  ( EraGen era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , EraGovernance era
  ) =>
  TestTree
depositTests =
  testGroup
    "Deposit Invariants"
    [ TQC.testProperty "Non negative deposits" (shortChainTrace (nonNegativeDeposits @era))
    , TQC.testProperty "Deposits = KeyDeposits + PoolDeposits" (shortChainTrace (depositInvariant @era))
    , TQC.testProperty "Reward domain = Deposit domain" (shortChainTrace (rewardDepositDomainInvariant @era))
    ]

----------------------------------------------------------------------
-- POOL Properties
----------------------------------------------------------------------

-- | Various properties of the POOL STS Rule, tested on longer traces
-- (double the default length)
poolProperties ::
  forall era.
  ( EraGen era
  , EraGovernance era
  , ChainProperty era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , ProtVerAtMost era 8
  ) =>
  Property
poolProperties =
  forAllChainTrace @era traceLen $ \tr -> do
    let ssts = sourceSignalTargets tr
    conjoin . concat $
      [ map poolRetirement ssts
      , map poolRegistration ssts
      , map poolStateIsInternallyConsistent ssts
      ]

-- | Check that a `RetirePool` certificate properly marks a stake pool for
-- retirement.
poolRetirement ::
  ( ChainProperty era
  , EraSegWits era
  , ShelleyEraTxBody era
  , ProtVerAtMost era 8
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
poolRetirement SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map (TestPool.poolRetirement currentEpoch maxEpoch) (sourceSignalTargets poolTr)
  where
    (chainSt', poolTr) = poolTraceFromBlock chainSt block
    bhb = bhbody $ bheader block
    currentEpoch = (epochFromSlotNo . bheaderSlotNo) bhb
    maxEpoch = (view ppEMaxL . esPp . nesEs . chainNes) chainSt'

-- | Check that a newly registered pool key is registered and not
-- in the retiring map.
poolRegistration ::
  ( ChainProperty era
  , EraSegWits era
  , ShelleyEraTxBody era
  , ProtVerAtMost era 8
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
poolRegistration (SourceSignalTarget {source = chainSt, signal = block}) =
  conjoin $
    map TestPool.poolRegistration (sourceSignalTargets poolTr)
  where
    (_, poolTr) = poolTraceFromBlock chainSt block

-- | Assert that PState maps are in sync with each other after each `Signal
-- POOL` transition.
poolStateIsInternallyConsistent ::
  ( ChainProperty era
  , EraSegWits era
  , ShelleyEraTxBody era
  , ProtVerAtMost era 8
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
poolStateIsInternallyConsistent (SourceSignalTarget {source = chainSt, signal = block}) =
  conjoin $
    map TestPool.poolStateIsInternallyConsistent (traceStates OldestFirst poolTr)
  where
    (_, poolTr) = poolTraceFromBlock chainSt block

----------------------------------------------------------------------
-- DELEG Properties
----------------------------------------------------------------------

-- | Various properties of the POOL STS Rule, tested on longer traces
-- (double the default length)
delegProperties ::
  forall era.
  ( EraGen era
  , EraGovernance era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , ChainProperty era
  , ProtVerAtMost era 8
  ) =>
  Property
delegProperties =
  forAllChainTrace @era traceLen $ \tr -> do
    conjoin $
      map chainProp (sourceSignalTargets tr)
  where
    delegProp :: DelegEnv era -> SourceSignalTarget (ShelleyDELEG era) -> Property
    delegProp denv delegSst =
      conjoin $
        [ TestDeleg.keyRegistration delegSst
        , TestDeleg.keyDeRegistration delegSst
        , TestDeleg.keyDelegation delegSst
        , TestDeleg.rewardsSumInvariant delegSst
        , TestDeleg.checkInstantaneousRewards denv delegSst
        ]
    chainProp :: SourceSignalTarget (CHAIN era) -> Property
    chainProp (SourceSignalTarget {source = chainSt, signal = block}) =
      let delegInfo = delegTraceFromBlock chainSt block
          delegEnv = fst delegInfo
          delegTr = snd delegInfo
          delegSsts = sourceSignalTargets delegTr
       in conjoin (map (delegProp delegEnv) delegSsts)

----------------------------------------------------------------------
-- Projections of CHAIN Trace
----------------------------------------------------------------------

-- | Reconstruct a LEDGER trace from the transactions in a Block and ChainState
ledgerTraceFromBlock ::
  forall era ledger.
  ( ChainProperty era
  , EraSegWits era
  , TestingLedger era ledger
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (ChainState era, Trace ledger)
ledgerTraceFromBlock chainSt block =
  ( tickedChainSt
  , runShelleyBase $
      Trace.closure @ledger ledgerEnv ledgerSt0 txs
  )
  where
    (tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block

-- | This function is nearly the same as ledgerTraceFromBlock, but
-- it restricts the UTxO state to only those needed by the block.
-- It also returns the unused UTxO for comparison later.
ledgerTraceFromBlockWithRestrictedUTxO ::
  forall era ledger.
  ( ChainProperty era
  , EraSegWits era
  , TestingLedger era ledger
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (UTxO era, Trace ledger)
ledgerTraceFromBlockWithRestrictedUTxO chainSt block =
  ( UTxO irrelevantUTxO
  , runShelleyBase $
      Trace.closure @ledger ledgerEnv ledgerSt0' txs
  )
  where
    (_tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block
    txIns = neededTxInsForBlock block
    LedgerState utxoSt delegationSt = ledgerSt0
    utxo = unUTxO . utxosUtxo $ utxoSt
    (relevantUTxO, irrelevantUTxO) = Map.partitionWithKey (const . (`Set.member` txIns)) utxo
    ledgerSt0' = LedgerState (utxoSt {utxosUtxo = UTxO relevantUTxO}) delegationSt

-- | Reconstruct a POOL trace from the transactions in a Block and ChainState
poolTraceFromBlock ::
  forall era.
  ( ChainProperty era
  , ShelleyEraTxBody era
  , EraSegWits era
  , ProtVerAtMost era 8
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (ChainState era, Trace (ShelleyPOOL era))
poolTraceFromBlock chainSt block =
  ( tickedChainSt
  , runShelleyBase $
      Trace.closure @(ShelleyPOOL era) poolEnv poolSt0 poolCerts
  )
  where
    (tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block
    certs = concatMap (toList . view certsTxBodyL . view bodyTxL)
    poolCerts = filter poolCert (certs txs)
    poolEnv =
      let (LedgerEnv s _ pp _) = ledgerEnv
       in PoolEnv s pp
    poolSt0 =
      let LedgerState _ (DPState _ poolSt0_) = ledgerSt0
       in poolSt0_
    poolCert (DCertPool _) = True
    poolCert _ = False

-- | Reconstruct a DELEG trace from all the transaction certificates in a Block
delegTraceFromBlock ::
  forall era.
  ( ChainProperty era
  , ShelleyEraTxBody era
  , EraSegWits era
  , ProtVerAtMost era 8
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (DelegEnv era, Trace (ShelleyDELEG era))
delegTraceFromBlock chainSt block =
  ( delegEnv
  , runShelleyBase $
      Trace.closure @(ShelleyDELEG era) delegEnv delegSt0 blockCerts
  )
  where
    (_tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block
    certs = concatMap (reverse . toList . view certsTxBodyL . view bodyTxL)
    blockCerts = filter delegCert (certs txs)
    delegEnv =
      let (LedgerEnv s txIx pp reserves) = ledgerEnv
          dummyCertIx = minBound
          ptr = Ptr s txIx dummyCertIx
       in DelegEnv s ptr reserves pp
    delegSt0 =
      let LedgerState _ (DPState delegSt0_ _) = ledgerSt0
       in delegSt0_
    delegCert (DCertDeleg _) = True
    delegCert (DCertMir _) = True
    delegCert _ = False

-- | Reconstruct a POOL trace from the transactions in a Block and ChainState
--
-- NOTE: we need to tick the slot before processing transactions
-- (in the same way that the CHAIN rule TICKs the slot before processing
-- transactions with the LEDGERS rule)
ledgerTraceBase ::
  forall era.
  ( EraSegWits era
  , GetLedgerView era
  , ApplyBlock era
  ) =>
  ChainState era ->
  Block (BHeader (EraCrypto era)) era ->
  (ChainState era, LedgerEnv era, LedgerState era, [Tx era])
ledgerTraceBase chainSt block =
  ( tickedChainSt
  , LedgerEnv slot minBound pp_ (esAccountState nes)
  , esLState nes
  , txs
  )
  where
    (UnserialisedBlock (BHeader bhb _) txSeq) = block
    slot = bheaderSlotNo bhb
    tickedChainSt = tickChainState slot chainSt
    nes = (nesEs . chainNes) tickedChainSt
    pp_ = esPp nes
    -- Oldest to Newest first
    txs = (reverse . toList . fromTxSeq) txSeq -- HERE WE USE SOME SegWit function

-- | Transform the [(source, signal, target)] of a CHAIN Trace
-- by manually applying the Chain TICK Rule to each source and producing
-- [(source, signal, target')].
--
-- This allows for testing properties on Chain traces while excluding the effects
-- of Transactions and Certificates. For example we can check that pools that are
-- due for retirement at an epoch transition, are indeed retired.
--
-- Had we not excluded the effects of Transactions/Certificates, we might have
-- a pool that was correctly retired, but is again registered by a certificate
-- in the block following the transition.
chainSstWithTick ::
  forall era.
  ChainProperty era =>
  Trace (CHAIN era) ->
  [SourceSignalTarget (CHAIN era)]
chainSstWithTick ledgerTr =
  map applyTick (sourceSignalTargets ledgerTr)
  where
    applyTick sst@SourceSignalTarget {source = chainSt, signal = block} =
      let bh = bheader block
          slot = (bheaderSlotNo . bhbody) bh
       in sst {target = tickChainState @era slot chainSt}

----------------------------------------------------------------------
-- Properties for PoolReap (using the CHAIN Trace) --
----------------------------------------------------------------------

removedAfterPoolreap ::
  forall era.
  ( ChainProperty era
  , EraGen era
  , EraGovernance era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  ) =>
  Property
removedAfterPoolreap =
  forAllChainTrace traceLen $ \tr ->
    conjoin $
      map removedAfterPoolreap_ $
        filter (not . sameEpoch) (chainSstWithTick tr)
  where
    poolState = dpsPState . lsDPState . esLState . nesEs . chainNes

    removedAfterPoolreap_ :: SourceSignalTarget (CHAIN era) -> Property
    removedAfterPoolreap_ (SourceSignalTarget {source, target, signal = (UnserialisedBlock bh _)}) =
      let e = (epochFromSlotNo . bheaderSlotNo . bhbody) bh
       in TestPoolreap.removedAfterPoolreap (poolState source) (poolState target) e

---------------------------
-- Utils --
---------------------------

forAllChainTrace ::
  forall era prop.
  ( Testable prop
  , EraGen era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , EraGovernance era
  ) =>
  Word64 -> -- trace length
  (Trace (CHAIN era) -> prop) ->
  Property
forAllChainTrace n prop =
  withMaxSuccess (fromIntegral numberOfTests) . property $
    forAllTraceFromInitState
      testGlobals
      n
      (Preset.genEnv p)
      (Just $ mkGenesisChainState (Preset.genEnv p))
      prop
  where
    p :: Proxy era
    p = Proxy

sameEpoch ::
  forall era.
  SourceSignalTarget (CHAIN era) ->
  Bool
sameEpoch SourceSignalTarget {source, target} =
  epoch source == epoch target
  where
    epoch = nesEL . chainNes

-- | Test a property on the first 'subtracecount' sub-Traces that end on an EpochBoundary
forEachEpochTrace ::
  forall era prop.
  ( EraGen era
  , Testable prop
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , EraGovernance era
  ) =>
  Int ->
  Word64 ->
  (Trace (CHAIN era) -> prop) ->
  Property
forEachEpochTrace subtracecount tracelen f = forAllChainTrace tracelen action
  where
    -- split at contiguous elements with different Epoch numbers
    p new old = (nesEL . chainNes) new /= (nesEL . chainNes) old
    -- At a minimum throw away the last trace which is probably an incomplete epoch
    action tr = conjoin $ map f (take (min subtracecount (m - 1)) (reverse traces))
      where
        traces = splitTrace p tr
        m = length traces

-- ================================
-- an example how one might debug one test, which can be replayed
-- import Test.Tasty (defaultMain)
-- main :: IO ()
-- main = defaultMain (minimal @(ShelleyEra TestCrypto))
-- Then in ghci, one can just type
-- :main --quickcheck-replay=443873
-- =================================
