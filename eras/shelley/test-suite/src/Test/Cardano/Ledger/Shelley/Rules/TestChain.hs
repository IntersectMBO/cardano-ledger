{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Rules.TestChain
  ( -- TestPoolReap
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
    -- Stake Comp
    stakeDistr,
    stakeIncrTest,
    incrementalStakeProp,
    aggregateUtxoCoinByCredential,
    splitTrace,
    forEachEpochTrace,
  )
where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (Globals, ProtVer, StrictMaybe (..))
import Cardano.Ledger.Block
  ( Block (..),
    bbody,
    bheader,
    neededTxInsForBlock,
  )
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (fromCompact, toCompact)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (StakeRefBase, StakeRefPtr))
import Cardano.Ledger.Era (Era (..), SupportsSegWit (fromTxSeq))
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking, Witness))
import Cardano.Ledger.Shelley.API (ApplyBlock, DELEG)
import Cardano.Ledger.Shelley.Constraints (UsesPParams, UsesValue)
import Cardano.Ledger.Shelley.EpochBoundary (SnapShot (..), Stake (..), obligation)
import Cardano.Ledger.Shelley.LedgerState
  ( DPState (..),
    DState (..),
    EpochState (..),
    IncrementalStake (..),
    LedgerState (..),
    NewEpochState (..),
    PPUPState (..),
    PState (..),
    UTxOState (..),
    completeRupd,
    credMap,
    delegations,
    deltaF,
    deltaR,
    deltaT,
    iRReserves,
    iRTreasury,
    incrementalStakeDistr,
    keyRefunds,
    ptrsMap,
    rewards,
    rs,
  )
import Cardano.Ledger.Shelley.Rewards (sumRewards)
import Cardano.Ledger.Shelley.Rules.Deleg (DelegEnv (..))
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerEnv (..))
import Cardano.Ledger.Shelley.Rules.Pool (POOL, PoolEnv (..))
import Cardano.Ledger.Shelley.Rules.Upec (votedValue)
import Cardano.Ledger.Shelley.Scripts (ScriptHash)
import Cardano.Ledger.Shelley.Tx hiding (TxIn)
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance, totalDeposits, txins, txouts, pattern UTxO)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UnifiedMap (ViewMap)
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val (coin)
import Cardano.Prelude (HasField (..))
import Cardano.Protocol.TPraos.API (GetLedgerView)
import Cardano.Protocol.TPraos.BHeader
  ( BHeader (..),
    bhbody,
    bheaderSlotNo,
  )
import Cardano.Slotting.Slot (EpochNo)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Provenance (runProvM)
import Control.SetAlgebra (dom, eval, (∩), (▷), (◁))
import Control.State.Transition
import Control.State.Transition.Trace
  ( SourceSignalTarget (..),
    Trace (..),
    TraceOrder (OldestFirst),
    sourceSignalTargets,
    splitTrace,
    traceStates,
  )
import qualified Control.State.Transition.Trace as Trace
import Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import qualified Data.Compact.SplitMap as SplitMap
import qualified Data.Compact.VMap as VMap
import Data.Default.Class (Default)
import Data.Foldable (fold, foldl', toList)
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.TreeDiff.QuickCheck (ediffEq)
import qualified Data.UMap as UM
import Data.Word (Word64)
import Test.Cardano.Ledger.Shelley.Generator.Block (tickChainState)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import qualified Test.Cardano.Ledger.Shelley.Generator.Presets as Preset (genEnv)
import Test.Cardano.Ledger.Shelley.Generator.ScriptClass (scriptKeyCombinations)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Generator.Trace.Chain (mkGenesisChainState)
import Test.Cardano.Ledger.Shelley.Orphans ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState (..), totalAda, totalAdaPots)
import qualified Test.Cardano.Ledger.Shelley.Rules.TestDeleg as TestDeleg
  ( checkInstantaneousRewards,
    keyDeRegistration,
    keyDelegation,
    keyRegistration,
    rewardsSumInvariant,
  )
import qualified Test.Cardano.Ledger.Shelley.Rules.TestPool as TestPool
  ( poolRegistration,
    poolRetirement,
    poolStateIsInternallyConsistent,
  )
import qualified Test.Cardano.Ledger.Shelley.Rules.TestPoolreap as TestPoolreap
import Test.Cardano.Ledger.Shelley.Utils
  ( ChainProperty,
    epochFromSlotNo,
    runShelleyBase,
    testGlobals,
  )
import Test.Cardano.Ledger.TerseTools (tersemapdiffs)
import Test.QuickCheck
  ( Property,
    Testable (..),
    conjoin,
    counterexample,
    withMaxSuccess,
    (.&&.),
    (.||.),
    (===),
  )

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
  ( BaseM ledger ~ ReaderT Globals Identity,
    Environment ledger ~ LedgerEnv era,
    State ledger ~ (UTxOState era, DPState (Crypto era)),
    Signal ledger ~ Core.Tx era,
    Embed (Core.EraRule "DELEGS" era) ledger,
    Embed (Core.EraRule "UTXOW" era) ledger,
    STS ledger
  )

----------------------------------------------------------------------
-- Properties for Chain
---------------------------------------------------------------------

-- | Tx inputs are eliminated, outputs added to utxo and TxIds are unique
collisionFreeComplete ::
  forall era ledger.
  ( EraGen era,
    ChainProperty era,
    TestingLedger era ledger,
    Default (State (Core.EraRule "PPUP" era)),
    QC.HasTrace (CHAIN era) (GenEnv era),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "addrWits" (Core.Witnesses era) (Set (WitVKey 'Witness (Crypto era))),
    HasField "scriptWits" (Core.Witnesses era) (Map (ScriptHash (Crypto era)) (Core.Script era))
  ) =>
  Property
collisionFreeComplete =
  forAllChainTrace @era traceLen $ \tr -> do
    let ssts = sourceSignalTargets tr
    conjoin . concat $
      [ -- collision freeness
        map (eliminateTxInputs @era @ledger) ssts,
        map (newEntriesAndUniqueTxIns @era @ledger) ssts,
        -- no double spend
        map noDoubleSpend ssts,
        -- tx signatures
        map (requiredMSigSignaturesSubset @era @ledger) ssts
      ]

-- | STAKE INCR
stakeIncrTest ::
  forall era ledger.
  ( EraGen era,
    TestingLedger era ledger,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era)
  ) =>
  Property
stakeIncrTest =
  forAllChainTrace @era longTraceLen $ \tr -> do
    let ssts = sourceSignalTargets tr

    conjoin . concat $
      [ -- preservation properties
        map (incrStakeComp @era @ledger) ssts
      ]

incrStakeComp ::
  forall era ledger.
  ( ChainProperty era,
    TestingLedger era ledger
  ) =>
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
        { source = (UTxOState {_utxo = u, _stakeDistro = sd}, dp),
          signal = tx,
          target = (UTxOState {_utxo = u', _stakeDistro = sd'}, dp')
        } =
        counterexample
          ( mconcat
              ( [ "\nDetails:\n",
                  "\ntx\n",
                  show tx,
                  "\nsize original utxo\n",
                  show (SplitMap.size $ unUTxO u),
                  "\noriginal utxo\n",
                  show u,
                  "\noriginal sd\n",
                  show sd,
                  "\nfinal utxo\n",
                  show u',
                  "\nfinal sd\n",
                  show sd',
                  "\noriginal ptrs\n",
                  show ptrs,
                  "\nfinal ptrs\n",
                  show ptrs'
                ]
              )
          )
          $ utxoBal === incrStakeBal
        where
          utxoBal = Val.coin $ balance u'
          incrStakeBal = fold (credMap sd') <> fold (ptrMap sd')
          ptrs = ptrsMap . dpsDState $ dp
          ptrs' = ptrsMap . dpsDState $ dp'

-- | Various preservation propertiesC
adaPreservationChain ::
  forall era ledger.
  ( EraGen era,
    TestingLedger era ledger,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  Property
adaPreservationChain =
  forAllChainTrace @era longTraceLen $ \tr -> do
    let ssts = sourceSignalTargets tr
        noEpochBoundarySsts = filter sameEpoch ssts

    conjoin . concat $
      [ -- preservation properties
        map (checkPreservation @era) ssts,
        map (potsSumIncreaseWdrlsPerTx @era @ledger) ssts,
        map (potsSumIncreaseByRewardsPerTx @era @ledger) ssts,
        map (preserveBalance @era @ledger) ssts,
        map (preserveBalanceRestricted @era @ledger) ssts,
        map (preserveOutputsTx @era @ledger) ssts,
        map (potsRewardsDecreaseByWdrlsPerTx @era @ledger) ssts,
        map (canRestrictUTxO @era @ledger) ssts,
        -- well formed deposits
        map nonNegativeDeposits ssts,
        -- non-epoch-boundary preservation properties
        map checkWithdrawlBound noEpochBoundarySsts,
        map (utxoDepositsIncreaseByFeesWithdrawals @era @ledger) noEpochBoundarySsts,
        map potsSumIncreaseWdrlsPerBlock noEpochBoundarySsts,
        map feesNonDecreasing noEpochBoundarySsts
      ]

-- ADA should be preserved for all state transitions in the generated trace
checkPreservation ::
  forall era.
  ( UsesValue era,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    UsesPParams era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
checkPreservation SourceSignalTarget {source, target, signal} =
  counterexample
    ( mconcat
        ( [ "\nPots before block\n",
            show (totalAdaPots source),
            "\n\nPots after block\n",
            show (totalAdaPots target),
            "\n\nTotal lovelace before block\n",
            show sourceTotal,
            "\n\nTotal lovelace after block\n",
            show targetTotal,
            "\n\nEpoch before block\n",
            show (nesEL . chainNes $ source),
            "\n\nEpoch after block\n",
            show (nesEL . chainNes $ target),
            "\n\nCurrent protocol parameters\n",
            show currPP,
            "\n\nReward Accounts before update\n",
            show (UM.unUnify oldRAs),
            "\n\nReward Accounts after update\n",
            show (UM.unUnify newRAs),
            "\n\nMIR\n",
            show mir,
            "\n\nRegistered Reserves MIR total\n",
            show (fold regMirRes),
            "\n\nUnregistered Reserves MIR total\n",
            show (fold unRegMirRes),
            "\n\nRegistered Treasury MIR total\n",
            show (fold regMirTre),
            "\n\nUnregistered Treasury MIR total\n",
            show (fold unRegMirTre),
            "\n\nPools Retiring This epoch\n",
            show (Map.filter (\e -> e == (nesEL . chainNes $ source)) (_retiring . dpsPState . lsDPState $ lsOld)),
            "\n\ntxs\n"
          ]
            ++ obligationMsgs
            ++ rewardUpdateMsgs
            ++ txs
        )
    )
    $ sourceTotal === targetTotal
  where
    sourceTotal = totalAda source
    targetTotal = totalAda target

    currPP = esPp . nesEs . chainNes $ source
    prevPP = esPrevPp . nesEs . chainNes $ source

    ru' = nesRu . chainNes $ source
    lsOld = esLState . nesEs . chainNes $ source
    lsNew = esLState . nesEs . chainNes $ target
    pools = _pParams . dpsPState . lsDPState $ lsOld
    oldRAs = rewards . dpsDState . lsDPState $ lsOld
    newRAs = rewards . dpsDState . lsDPState $ lsNew

    proposal = votedValue (proposals . _ppups . lsUTxOState $ lsOld) currPP 5
    obligationMsgs = case proposal of
      Nothing -> []
      Just proposal' ->
        let Coin oblgCurr = obligation currPP oldRAs pools
            Coin oblgNew = obligation proposal' oldRAs pools
            obligationDiff = oblgCurr - oblgNew
         in [ "\n\nProposed protocol parameter update\n",
              show proposal',
              "\n\nObligation Diff\n",
              show obligationDiff
            ]

    mir = _irwd . dpsDState . lsDPState $ lsOld
    isRegistered kh _ = UM.member kh oldRAs
    (regMirRes, unRegMirRes) = Map.partitionWithKey isRegistered (iRReserves mir)
    (regMirTre, unRegMirTre) = Map.partitionWithKey isRegistered (iRTreasury mir)

    rewardUpdateMsgs = case ru' of
      SNothing -> []
      SJust ru'' ->
        let (ru, _) = runShelleyBase . runProvM . completeRupd $ ru''
            regRewards = Map.filterWithKey (\kh _ -> UM.member kh oldRAs) (rs ru)
         in [ "\n\nSum of new rewards\n",
              show (sumRewards prevPP (rs ru)),
              "\n\nNew rewards\n",
              show (rs ru),
              "\n\nSum of new registered rewards\n",
              show (sumRewards prevPP regRewards),
              "\n\nChange in Fees\n",
              show (deltaF ru),
              "\n\nChange in Treasury\n",
              show (deltaT ru),
              "\n\nChange in Reserves\n",
              show (deltaR ru),
              "\n\nNet effect of reward update\n",
              show $
                deltaT ru
                  <> deltaF ru
                  <> deltaR ru
                  <> (toDeltaCoin $ sumRewards prevPP (rs ru))
            ]

    txs' = toList $ (fromTxSeq @era . bbody) signal
    txs = map dispTx (zip txs' [0 :: Int ..])

    dispTx (tx, ix) =
      "\nTransaction " ++ show ix
        ++ "\nfee :"
        ++ (show $ getField @"txfee" $ getField @"body" tx)
        ++ "\nwithdrawals: "
        ++ (show $ getField @"wdrls" $ getField @"body" tx)
        ++ "\ncerts: "
        ++ (show . (map dispCert) . toList $ getField @"certs" $ getField @"body" tx)
        ++ "\n"

    dispCert (DCertDeleg (RegKey kh)) = "regkey " <> show kh
    dispCert (DCertDeleg (DeRegKey kh)) = "deregkey " <> show kh
    dispCert (DCertDeleg (Delegate (Delegation _ _))) = "deleg"
    dispCert (DCertPool (RegPool p)) =
      if (_poolId p) `Map.member` pools
        then ("PoolReg" <> (show . _poolId $ p))
        else "Pool Re-Reg"
    dispCert (DCertPool (RetirePool _ _)) = "retire"
    dispCert (DCertGenesis (GenesisDelegCert _ _ _)) = "gen"
    dispCert (DCertMir _) = "mir"

-- If we are not at an Epoch Boundary (i.e. epoch source == epoch target)
-- then the total rewards should change only by withdrawals
checkWithdrawlBound ::
  ( EraGen era,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
checkWithdrawlBound SourceSignalTarget {source, signal, target} =
  counterexample "checkWithdrawlBound" $
    rewardDelta === withdrawals signal
  where
    rewardDelta :: Coin
    rewardDelta =
      fold
        ( rewards . dpsDState
            . lsDPState
            . esLState
            . nesEs
            . chainNes
            $ source
        )
        <-> fold
          ( rewards . dpsDState
              . lsDPState
              . esLState
              . nesEs
              . chainNes
              $ target
          )

-- | If we are not at an Epoch Boundary, then (Utxo + Deposits)
-- increases by Withdrawals minus Fees (for all transactions in a block)
utxoDepositsIncreaseByFeesWithdrawals ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
utxoDepositsIncreaseByFeesWithdrawals SourceSignalTarget {source, signal, target} =
  counterexample "utxoDepositsIncreaseByFeesWithdrawals" $
    circulation target <-> circulation source
      === withdrawals signal <-> txFees ledgerTr
  where
    us = lsUTxOState . esLState . nesEs . chainNes
    circulation chainSt =
      let UTxOState {_utxo = u, _deposited = d} = us chainSt
       in Val.coin (balance u) <+> d
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger source signal

-- | If we are not at an Epoch Boundary, then (Utxo + Deposits + Fees)
-- increases by sum of withdrawals for all transactions in a block
potsSumIncreaseWdrlsPerBlock ::
  ( ChainProperty era,
    EraGen era,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsSumIncreaseWdrlsPerBlock SourceSignalTarget {source, signal, target} =
  counterexample
    "potsSumIncreaseWdrlsPerBlock"
    $ potsSum target <-> potsSum source === withdrawals signal
  where
    potsSum chainSt =
      let UTxOState {_utxo = u, _deposited = d, _fees = f} =
            lsUTxOState . esLState . nesEs . chainNes $ chainSt
       in Val.coin (balance u) <+> d <+> f

-- | If we are not at an Epoch Boundary, then (Utxo + Deposits + Fees)
-- increases by sum of withdrawals in a transaction
potsSumIncreaseWdrlsPerTx ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsSumIncreaseWdrlsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "potsSumIncreaseWdrlsPerTx" $
    conjoin $
      map sumIncreaseWdrls $
        sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    sumIncreaseWdrls :: SourceSignalTarget ledger -> Property
    sumIncreaseWdrls
      SourceSignalTarget
        { source = (UTxOState {_utxo = u, _deposited = d, _fees = f}, _),
          signal = tx,
          target = (UTxOState {_utxo = u', _deposited = d', _fees = f'}, _)
        } =
        property (hasFailedScripts tx)
          .||. (Val.coin (balance u') <+> d' <+> f') <-> (Val.coin (balance u) <+> d <+> f)
          === fold (unWdrl (getField @"wdrls" (getField @"body" tx)))

-- | (Utxo + Deposits + Fees) increases by the reward delta
potsSumIncreaseByRewardsPerTx ::
  forall era ledger.
  ( ChainProperty era,
    TestingLedger era ledger
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
            ( UTxOState {_utxo = u, _deposited = d, _fees = f},
              DPState {dpsDState = DState {_unified = umap1}}
              ),
          target =
            ( UTxOState {_utxo = u', _deposited = d', _fees = f'},
              DPState {dpsDState = DState {_unified = umap2}}
              )
        } =
        (Val.coin (balance u') <+> d' <+> f') <-> (Val.coin (balance u) <+> d <+> f)
          === fold (UM.unUnify (UM.Rewards umap1)) <-> fold (UM.unUnify (UM.Rewards umap2))

-- | The Rewards pot decreases by the sum of withdrawals in a transaction
potsRewardsDecreaseByWdrlsPerTx ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsRewardsDecreaseByWdrlsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  counterexample "potsRewardsDecreaseByWdrlsPerTx" $
    conjoin $
      map rewardsDecreaseByWdrls $
        sourceSignalTargets ledgerTr
  where
    rewardsSum = fold . rewards . dpsDState
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    rewardsDecreaseByWdrls
      SourceSignalTarget
        { source = (_, dpstate),
          signal = tx,
          target = (_, dpstate')
        } =
        let totalRewards = rewardsSum dpstate
            totalRewards' = rewardsSum dpstate'
            txWithdrawals = fold (unWdrl (getField @"wdrls" (getField @"body" tx)))
         in conjoin
              [ counterexample
                  "A transaction should not increase the Rewards pot"
                  (totalRewards >= totalRewards'),
                counterexample
                  "Withdrawals should be non-negative"
                  (txWithdrawals >= Coin 0),
                counterexample
                  "Rewards should increase by withdrawals"
                  (hasFailedScripts tx || totalRewards <-> totalRewards' == txWithdrawals)
              ]

-- | Preserve the balance in a transaction, i.e., the sum of the consumed value
-- equals the sum of the created value.
preserveBalance ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
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
        (UTxOState {_utxo = u}, dstate) = ledgerSt
        (UTxOState {_utxo = u'}, _) = ledgerSt'
        txb = getField @"body" tx
        certs = toList (getField @"certs" txb)
        pools = _pParams . dpsPState $ dstate
        created =
          Val.coin (balance u')
            <+> getField @"txfee" txb
            <+> totalDeposits pp_ (`Map.notMember` pools) certs
        consumed_ =
          Val.coin (balance u)
            <+> keyRefunds pp_ txb
            <+> fold (unWdrl (getField @"wdrls" txb))

-- | Preserve balance restricted to TxIns and TxOuts of the Tx
preserveBalanceRestricted ::
  forall era ledger.
  ( ChainProperty era,
    TestingLedger era ledger,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin
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

    createdIsConsumed SourceSignalTarget {source = (UTxOState {_utxo = UTxO u}, dstate), signal = tx} =
      inps === outs
      where
        txb = getField @"body" tx
        pools = _pParams . dpsPState $ dstate
        inps =
          Val.coin (balance @era (UTxO (SplitMap.restrictKeysSet u (getField @"inputs" txb))))
            <> keyRefunds pp_ txb
            <> fold (unWdrl (getField @"wdrls" txb))
        outs =
          let certs = toList (getField @"certs" txb)
           in Val.coin (balance (txouts @era txb))
                <> getField @"txfee" txb
                <> totalDeposits pp_ (`Map.notMember` pools) certs

preserveOutputsTx ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger
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
    outputPreserved SourceSignalTarget {target = (UTxOState {_utxo = UTxO utxo}, _), signal = tx} =
      let UTxO outs = txouts @era (getField @"body" tx)
       in property $
            hasFailedScripts tx
              .||. counterexample "TxOuts are not a subset of UTxO" (outs `SplitMap.isSubmapOf` utxo)

canRestrictUTxO ::
  forall era ledger.
  ( ChainProperty era,
    TestingLedger era ledger,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
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
      SourceSignalTarget {target = (UTxOState {_utxo = UTxO uFull}, _)}
      SourceSignalTarget {target = (UTxOState {_utxo = UTxO uRestr}, _)} =
        counterexample
          (unlines ["non-disjoint:", show uRestr, show irrelevantUTxO])
          (uRestr `SplitMap.disjoint` irrelevantUTxO)
          .&&. uFull === (uRestr `SplitMap.union` irrelevantUTxO)

-- | Check that consumed inputs are eliminated from the resulting UTxO
eliminateTxInputs ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
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
    inputsEliminated SourceSignalTarget {target = (UTxOState {_utxo = (UTxO u')}, _), signal = tx} =
      property $
        hasFailedScripts tx
          || Set.null (eval (txins @era (getField @"body" tx) ∩ SplitMap.toSet u'))

-- | Collision-Freeness of new TxIds - checks that all new outputs of a Tx are
-- included in the new UTxO and that all TxIds are new.
newEntriesAndUniqueTxIns ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger
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
        { source = (UTxOState {_utxo = UTxO u}, _),
          signal = tx,
          target = (UTxOState {_utxo = UTxO u'}, _)
        } =
        let UTxO outs = txouts @era (getField @"body" tx)
            outIds = Set.map (\(TxIn _id _) -> _id) (SplitMap.toSet outs)
            oldIds = Set.map (\(TxIn _id _) -> _id) (SplitMap.toSet u)
         in property $
              hasFailedScripts tx
                || ((outIds `Set.disjoint` oldIds) && (outs `SplitMap.isSubmapOf` u'))

-- | Check for required signatures in case of Multi-Sig. There has to be one set
-- of possible signatures for a multi-sig script which is a sub-set of the
-- signatures of the tansaction.
requiredMSigSignaturesSubset ::
  forall era ledger.
  ( ChainProperty era,
    EraGen era,
    TestingLedger era ledger,
    HasField "scriptWits" (Core.Witnesses era) (Map (ScriptHash (Crypto era)) (Core.Script era)),
    HasField "addrWits" (Core.Witnesses era) (Set (WitVKey 'Witness (Crypto era)))
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
            all (existsReqKeyComb khs) (getField @"scriptWits" . getField @"wits" $ tx)

    existsReqKeyComb keyHashes msig =
      any (\kl -> Set.fromList kl `Set.isSubsetOf` keyHashes) (scriptKeyCombinations (Proxy @era) msig)
    keyHashSet :: Core.Tx era -> Set (KeyHash 'Witness (Crypto era))
    keyHashSet tx_ =
      Set.map witKeyHash (getField @"addrWits" . getField @"wits" $ tx_)

--- | Check for absence of double spend in a block
noDoubleSpend ::
  forall era.
  ( ChainProperty era,
    EraGen era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
noDoubleSpend SourceSignalTarget {signal} =
  counterexample "noDoubleSpend" $
    [] === getDoubleInputs txs
  where
    txs = toList $ (fromTxSeq @era . bbody) signal

    getDoubleInputs :: [Core.Tx era] -> [(Core.Tx era, [Core.Tx era])]
    getDoubleInputs [] = []
    getDoubleInputs (t : ts) = lookForDoubleSpends t ts ++ getDoubleInputs ts
    lookForDoubleSpends :: Core.Tx era -> [Core.Tx era] -> [(Core.Tx era, [Core.Tx era])]
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
                          || Set.disjoint inps_j (getField @"inputs" (getField @"body" tx_i))
                      )
                )
                ts
        inps_j = getField @"inputs" $ getField @"body" tx_j

withdrawals ::
  forall era.
  ( EraGen era,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  Block (BHeader (Crypto era)) era ->
  Coin
withdrawals (UnserialisedBlock _ txseq) =
  foldl'
    ( \c tx ->
        let wdrls = unWdrl $ getField @"wdrls" (getField @"body" tx)
         in if hasFailedScripts tx then c else c <> fold wdrls
    )
    (Coin 0)
    $ fromTxSeq @era txseq

txFees ::
  forall era ledger.
  ( EraGen era,
    TestingLedger era ledger
  ) =>
  Trace ledger ->
  Coin
txFees ledgerTr =
  foldl' f (Coin 0) (sourceSignalTargets ledgerTr)
  where
    f
      c
      SourceSignalTarget
        { source = (UTxOState {_utxo = utxo}, _),
          signal = tx
        } = c <> feeOrCollateral tx utxo

-- | Check that deposits are always non-negative
nonNegativeDeposits ::
  SourceSignalTarget (CHAIN era) ->
  Property
nonNegativeDeposits SourceSignalTarget {source = chainSt} =
  let es = (nesEs . chainNes) chainSt
      UTxOState {_deposited = d} = (lsUTxOState . esLState) es
   in counterexample ("nonNegativeDeposits: " ++ show d) (d >= mempty)

-- | Checks that the fees are non-decreasing when not at an epoch boundary
feesNonDecreasing ::
  SourceSignalTarget (CHAIN era) ->
  Property
feesNonDecreasing SourceSignalTarget {source, target} =
  counterexample ("feesNonDecreasing: " <> show (fees_ source) <> " <= " <> show (fees_ target)) $
    fees_ source <= fees_ target
  where
    fees_ chainSt =
      let UTxOState {_fees = fees} =
            lsUTxOState . esLState . nesEs . chainNes $ chainSt
       in fees

----------------------------------------------------------------------
-- POOL Properties
----------------------------------------------------------------------

-- | Various properties of the POOL STS Rule, tested on longer traces
-- (double the default length)
poolProperties ::
  forall era.
  ( EraGen era,
    Default (State (Core.EraRule "PPUP" era)),
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
  ) =>
  Property
poolProperties =
  forAllChainTrace @era traceLen $ \tr -> do
    let ssts = sourceSignalTargets tr
    conjoin . concat $
      [ map poolRetirement ssts,
        map poolRegistration ssts,
        map poolStateIsInternallyConsistent ssts
      ]

-- | Check that a `RetirePool` certificate properly marks a stake pool for
-- retirement.
poolRetirement ::
  ( ChainProperty era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "_eMax" (Core.PParams era) EpochNo,
    HasField "_minPoolCost" (Core.PParams era) Coin
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
    maxEpoch = (getField @"_eMax" . esPp . nesEs . chainNes) chainSt'

-- | Check that a newly registered pool key is registered and not
-- in the retiring map.
poolRegistration ::
  ( ChainProperty era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "_eMax" (Core.PParams era) EpochNo,
    HasField "_minPoolCost" (Core.PParams era) Coin
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
  ( ChainProperty era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "_eMax" (Core.PParams era) EpochNo,
    HasField "_minPoolCost" (Core.PParams era) Coin
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
  ( EraGen era,
    Default (State (Core.EraRule "PPUP" era)),
    QC.HasTrace (CHAIN era) (GenEnv era),
    ChainProperty era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
  ) =>
  Property
delegProperties =
  forAllChainTrace @era traceLen $ \tr -> do
    conjoin $
      map chainProp (sourceSignalTargets tr)
  where
    delegProp :: DelegEnv era -> SourceSignalTarget (DELEG era) -> Property
    delegProp denv delegSst =
      conjoin $
        [ TestDeleg.keyRegistration delegSst,
          TestDeleg.keyDeRegistration delegSst,
          TestDeleg.keyDelegation delegSst,
          TestDeleg.rewardsSumInvariant delegSst,
          TestDeleg.checkInstantaneousRewards denv delegSst
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
  ( ChainProperty era,
    TestingLedger era ledger
  ) =>
  ChainState era ->
  Block (BHeader (Crypto era)) era ->
  (ChainState era, Trace ledger)
ledgerTraceFromBlock chainSt block =
  ( tickedChainSt,
    runShelleyBase $
      Trace.closure @ledger ledgerEnv ledgerSt0 txs
  )
  where
    (tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block

-- | This function is nearly the same as ledgerTraceFromBlock, but
-- it restricts the UTxO state to only those needed by the block.
-- It also returns the unused UTxO for comparison later.
ledgerTraceFromBlockWithRestrictedUTxO ::
  forall era ledger.
  ( ChainProperty era,
    TestingLedger era ledger,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  ChainState era ->
  Block (BHeader (Crypto era)) era ->
  (UTxO era, Trace ledger)
ledgerTraceFromBlockWithRestrictedUTxO chainSt block =
  ( UTxO irrelevantUTxO,
    runShelleyBase $
      Trace.closure @ledger ledgerEnv ledgerSt0' txs
  )
  where
    (_tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block
    txIns = neededTxInsForBlock block
    (utxoSt, delegationSt) = ledgerSt0
    utxo = unUTxO . _utxo $ utxoSt
    (relevantUTxO, irrelevantUTxO) = SplitMap.partitionWithKey (const . (`Set.member` txIns)) utxo
    ledgerSt0' = (utxoSt {_utxo = UTxO relevantUTxO}, delegationSt)

-- | Reconstruct a POOL trace from the transactions in a Block and ChainState
poolTraceFromBlock ::
  forall era.
  ( ChainProperty era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "_eMax" (Core.PParams era) EpochNo,
    HasField "_minPoolCost" (Core.PParams era) Coin
  ) =>
  ChainState era ->
  Block (BHeader (Crypto era)) era ->
  (ChainState era, Trace (POOL era))
poolTraceFromBlock chainSt block =
  ( tickedChainSt,
    runShelleyBase $
      Trace.closure @(POOL era) poolEnv poolSt0 poolCerts
  )
  where
    (tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block
    certs = concatMap (toList . getField @"certs" . getField @"body")
    poolCerts = filter poolCert (certs txs)
    poolEnv =
      let (LedgerEnv s _ pp _) = ledgerEnv
       in PoolEnv s pp
    poolSt0 =
      let (_, DPState _ poolSt0_) = ledgerSt0
       in poolSt0_
    poolCert (DCertPool _) = True
    poolCert _ = False

-- | Reconstruct a DELEG trace from all the transaction certificates in a Block
delegTraceFromBlock ::
  forall era.
  ( ChainProperty era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
  ) =>
  ChainState era ->
  Block (BHeader (Crypto era)) era ->
  (DelegEnv era, Trace (DELEG era))
delegTraceFromBlock chainSt block =
  ( delegEnv,
    runShelleyBase $
      Trace.closure @(DELEG era) delegEnv delegSt0 blockCerts
  )
  where
    (_tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block
    certs = concatMap (reverse . toList . getField @"certs" . getField @"body")
    blockCerts = filter delegCert (certs txs)
    delegEnv =
      let (LedgerEnv s txIx pp reserves) = ledgerEnv
          dummyCertIx = minBound
          ptr = Ptr s txIx dummyCertIx
       in DelegEnv s ptr reserves pp
    delegSt0 =
      let (_, DPState delegSt0_ _) = ledgerSt0
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
  ( Era era,
    GetLedgerView era,
    ApplyBlock era
  ) =>
  ChainState era ->
  Block (BHeader (Crypto era)) era ->
  ( ChainState era,
    LedgerEnv era,
    (UTxOState era, DPState (Crypto era)),
    [Core.Tx era]
  )
ledgerTraceBase chainSt block =
  ( tickedChainSt,
    LedgerEnv slot minBound pp_ (esAccountState nes),
    (utxoSt0, delegSt0),
    txs
  )
  where
    (UnserialisedBlock (BHeader bhb _) txSeq) = block
    slot = bheaderSlotNo bhb
    tickedChainSt = tickChainState slot chainSt
    nes = (nesEs . chainNes) tickedChainSt
    pp_ = esPp nes
    LedgerState utxoSt0 delegSt0 = esLState nes
    -- Oldest to Newest first
    txs = (reverse . toList . (fromTxSeq @era)) txSeq -- HERE WE USE SOME SegWit function

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
    applyTick sst@(SourceSignalTarget {source = chainSt, signal = block}) =
      let bh = bheader block
          slot = (bheaderSlotNo . bhbody) bh
       in sst {target = tickChainState @era slot chainSt}

----------------------------------------------------------------------
-- Properties for PoolReap (using the CHAIN Trace) --
----------------------------------------------------------------------

removedAfterPoolreap ::
  forall era.
  ( ChainProperty era,
    Default (State (Core.EraRule "PPUP" era)),
    EraGen era,
    QC.HasTrace (CHAIN era) (GenEnv era)
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
  ( Testable prop,
    Default (State (Core.EraRule "PPUP" era)),
    EraGen era,
    QC.HasTrace (CHAIN era) (GenEnv era)
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
  SourceSignalTarget (CHAIN era) ->
  Bool
sameEpoch SourceSignalTarget {source, target} =
  epoch source == epoch target
  where
    epoch = nesEL . chainNes

-- | Test a property on the first 'subtracecount' sub-Traces that end on an EpochBoundary
forEachEpochTrace ::
  forall era prop.
  ( EraGen era,
    Testable prop,
    QC.HasTrace (CHAIN era) (GenEnv era),
    Default (State (Core.EraRule "PPUP" era))
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

-- ============================================================
-- Properties for Incremental Stake Distribution  Calculation

atEpoch ::
  forall era prop.
  ( EraGen era,
    Testable prop,
    QC.HasTrace (CHAIN era) (GenEnv era),
    Default (State (Core.EraRule "PPUP" era))
  ) =>
  (LedgerState era -> LedgerState era -> prop) ->
  Property
atEpoch f =
  forAllChainTrace traceLen $ \tr ->
    conjoin $
      map g $
        filter (not . sameEpoch) (sourceSignalTargets tr)
  where
    g (SourceSignalTarget s1 s2 _) = f (ledgerStateFromChainState s1) (ledgerStateFromChainState s2)

ledgerStateFromChainState :: ChainState era -> LedgerState era
ledgerStateFromChainState = esLState . nesEs . chainNes

testIncrementalStake ::
  forall era.
  (Era era) =>
  LedgerState era ->
  LedgerState era ->
  Property
testIncrementalStake _ (LedgerState (UTxOState utxo _ _ _ incStake) (DPState dstate pstate)) =
  let stake = stakeDistr @era utxo dstate pstate

      istake = incrementalStakeDistr @(Crypto era) incStake dstate pstate
   in counterexample
        ( "\nIncremental stake distribution does not match old style stake distribution"
            ++ tersediffincremental "differences: Old vs Incremental" (_stake stake) (_stake istake)
        )
        (stake === istake)

incrementalStakeProp ::
  forall era.
  ( EraGen era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    Default (State (Core.EraRule "PPUP" era))
  ) =>
  Proxy era ->
  Property
incrementalStakeProp Proxy = atEpoch @era (testIncrementalStake @era)

tersediffincremental :: String -> Stake crypto -> Stake crypto -> String
tersediffincremental message (Stake a) (Stake c) =
  tersemapdiffs (message ++ " " ++ "hashes") (mp a) (mp c)
  where
    mp = Map.map fromCompact . VMap.toMap

-- | Compute the current Stake Distribution. This was called at the Epoch boundary in the Snap Rule.
--   Now it is called in the tests to see that its incremental analog 'incrementalStakeDistr' agrees.
stakeDistr ::
  forall era.
  Era era =>
  UTxO era ->
  DState (Crypto era) ->
  PState (Crypto era) ->
  SnapShot (Crypto era)
stakeDistr u ds ps =
  SnapShot
    (Stake $ VMap.fromMap (compactCoinOrError <$> eval (dom activeDelegs ◁ stakeRelation)))
    (VMap.fromMap (UM.unUnify delegs))
    (VMap.fromMap poolParams)
  where
    rewards' = rewards ds
    delegs = delegations ds
    ptrs' = ptrsMap ds
    PState poolParams _ _ = ps
    stakeRelation :: Map (Credential 'Staking (Crypto era)) Coin
    stakeRelation = aggregateUtxoCoinByCredential ptrs' u (UM.unUnify rewards')
    activeDelegs :: ViewMap (Crypto era) (Credential 'Staking (Crypto era)) (KeyHash 'StakePool (Crypto era))
    activeDelegs = eval ((dom rewards' ◁ delegs) ▷ dom poolParams)
    compactCoinOrError c =
      case toCompact c of
        Nothing -> error $ "Invalid ADA value in staking: " <> show c
        Just compactCoin -> compactCoin

-- | Sum up all the Coin for each staking Credential. This function has an
--   incremental analog. See 'incrementalAggregateUtxoCoinByCredential'
aggregateUtxoCoinByCredential ::
  forall era.
  ( Era era
  ) =>
  Map Ptr (Credential 'Staking (Crypto era)) ->
  UTxO era ->
  Map (Credential 'Staking (Crypto era)) Coin ->
  Map (Credential 'Staking (Crypto era)) Coin
aggregateUtxoCoinByCredential ptrs (UTxO u) initial =
  SplitMap.foldl' accum initial u
  where
    accum ans out =
      let c = Val.coin (getField @"value" out)
       in case getTxOutAddr out of
            Addr _ _ (StakeRefPtr p)
              | Just cred <- Map.lookup p ptrs -> Map.insertWith (<>) cred c ans
            Addr _ _ (StakeRefBase hk) -> Map.insertWith (<>) hk c ans
            _other -> ans
