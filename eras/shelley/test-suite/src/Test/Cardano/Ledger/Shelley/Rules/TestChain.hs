{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  )
where

import Cardano.Ledger.BaseTypes (Globals, StrictMaybe (..))
import Cardano.Ledger.Block
  ( Block (..),
    bbody,
    bheader,
    neededTxInsForBlock,
  )
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, SupportsSegWit (fromTxSeq))
import Cardano.Ledger.Keys (KeyHash, KeyRole (Witness))
import Cardano.Ledger.Shelley.API
  ( ApplyBlock,
    DELEG,
    GetLedgerView,
  )
import Cardano.Ledger.Shelley.Constraints (UsesPParams, UsesValue)
import Cardano.Ledger.Shelley.EpochBoundary (obligation)
import Cardano.Ledger.Shelley.LedgerState hiding (circulation)
import Cardano.Ledger.Shelley.PParams (ProtVer)
import Cardano.Ledger.Shelley.Rewards (sumRewards)
import Cardano.Ledger.Shelley.Rules.Deleg (DelegEnv (..))
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerEnv (..))
import Cardano.Ledger.Shelley.Rules.Pool (POOL, PoolEnv (..))
import Cardano.Ledger.Shelley.Rules.Upec (votedValue)
import Cardano.Ledger.Shelley.Scripts (ScriptHash)
import Cardano.Ledger.Shelley.Tx hiding (TxIn)
import Cardano.Ledger.Shelley.TxBody hiding (TxIn)
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance, totalDeposits, txins, txouts, pattern UTxO)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val (coin)
import Cardano.Prelude (HasField (..))
import Cardano.Protocol.TPraos.BHeader
  ( BHeader (..),
    bhbody,
    bheaderSlotNo,
  )
import Cardano.Slotting.Slot (EpochNo)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Provenance (runProvM)
import Control.SetAlgebra (dom, domain, eval, (<|), (∩), (⊆))
import Control.State.Transition
import Control.State.Transition.Trace
  ( SourceSignalTarget (..),
    Trace (..),
    TraceOrder (OldestFirst),
    sourceSignalTargets,
    traceStates,
  )
import qualified Control.State.Transition.Trace as Trace
import Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
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
            show oldRAs,
            "\n\nReward Accounts after update\n",
            show newRAs,
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
            show (Map.filter (\e -> e == (nesEL . chainNes $ source)) (_retiring . _pstate . _delegationState $ lsOld)),
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
    pools = _pParams . _pstate . _delegationState $ lsOld
    oldRAs = _rewards . _dstate . _delegationState $ lsOld
    newRAs = _rewards . _dstate . _delegationState $ lsNew

    proposal = votedValue (proposals . _ppups . _utxoState $ lsOld) currPP 5
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

    mir = _irwd . _dstate . _delegationState $ lsOld
    isRegistered kh _ = Map.member kh oldRAs
    (regMirRes, unRegMirRes) = Map.partitionWithKey isRegistered (iRReserves mir)
    (regMirTre, unRegMirTre) = Map.partitionWithKey isRegistered (iRTreasury mir)

    rewardUpdateMsgs = case ru' of
      SNothing -> []
      SJust ru'' ->
        let ru = runShelleyBase . runProvM . completeRupd $ ru''
            regRewards = Map.filterWithKey (\kh _ -> Map.member kh oldRAs) (rs ru)
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
  rewardDelta === withdrawals signal
  where
    rewardDelta :: Coin
    rewardDelta =
      fold
        ( _rewards . _dstate
            . _delegationState
            . esLState
            . nesEs
            . chainNes
            $ source
        )
        <-> fold
          ( _rewards . _dstate
              . _delegationState
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
  circulation target <-> circulation source
    === withdrawals signal <-> txFees ledgerTr
  where
    us = _utxoState . esLState . nesEs . chainNes
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
  potsSum target <-> potsSum source === withdrawals signal
  where
    potsSum chainSt =
      let (UTxOState {_utxo = u, _deposited = d, _fees = f}) =
            _utxoState . esLState . nesEs . chainNes $ chainSt
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
  conjoin $
    map sumIncreaseRewards $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    sumIncreaseRewards
      SourceSignalTarget
        { source =
            ( UTxOState {_utxo = u, _deposited = d, _fees = f},
              DPState {_dstate = DState {_rewards = rewards}}
              ),
          target =
            ( UTxOState {_utxo = u', _deposited = d', _fees = f'},
              DPState {_dstate = DState {_rewards = rewards'}}
              )
        } =
        (Val.coin (balance u') <+> d' <+> f') <-> (Val.coin (balance u) <+> d <+> f)
          === fold rewards <-> fold rewards'

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
  conjoin $
    map rewardsDecreaseByWdrls $
      sourceSignalTargets ledgerTr
  where
    rewardsSum = (foldl' (<+>) (Coin 0)) . _rewards . _dstate
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
        pools = _pParams . _pstate $ dstate
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
  conjoin $
    map createdIsConsumed $
      sourceSignalTargets ledgerTr
  where
    (tickedChainSt, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    pp_ = (esPp . nesEs . chainNes) tickedChainSt

    createdIsConsumed SourceSignalTarget {source = (UTxOState {_utxo = u}, dstate), signal = tx} =
      inps === outs
      where
        txb = getField @"body" tx
        pools = _pParams . _pstate $ dstate
        inps =
          Val.coin (balance @era (eval ((getField @"inputs" txb) <| u)))
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
  conjoin $
    map outputPreserved $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    outputPreserved SourceSignalTarget {target = (UTxOState {_utxo = (UTxO u')}, _), signal = tx} =
      let UTxO outs = txouts @era (getField @"body" tx)
       in property $
            hasFailedScripts tx || outs `Map.isSubmapOf` u'

canRestrictUTxO ::
  forall era ledger.
  ( ChainProperty era,
    TestingLedger era ledger,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
canRestrictUTxO SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map outputPreserved $
      zip (sourceSignalTargets ledgerTrFull) (sourceSignalTargets ledgerTrRestr)
  where
    (_, ledgerTrFull) = ledgerTraceFromBlock @era @ledger chainSt block
    (UTxO irrelevantUTxO, ledgerTrRestr) =
      ledgerTraceFromBlockWithRestrictedUTxO @era @ledger chainSt block
    outputPreserved
      ( SourceSignalTarget {target = (UTxOState {_utxo = UTxO uFull}, _)},
        SourceSignalTarget {target = (UTxOState {_utxo = UTxO uRestr}, _)}
        ) =
        (uRestr `Map.disjoint` irrelevantUTxO) .&&. uFull === (uRestr `Map.union` irrelevantUTxO)

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
  conjoin $
    map inputsEliminated $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    inputsEliminated SourceSignalTarget {target = (UTxOState {_utxo = (UTxO u')}, _), signal = tx} =
      property $
        (hasFailedScripts tx)
          || (Set.null $ eval (txins @era (getField @"body" tx) ∩ dom u'))

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
  conjoin $
    map newEntryPresent $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock @era @ledger chainSt block
    newEntryPresent
      SourceSignalTarget
        { source = (UTxOState {_utxo = (UTxO u)}, _),
          signal = tx,
          target = (UTxOState {_utxo = (UTxO u')}, _)
        } =
        let UTxO outs = txouts @era (getField @"body" tx)
            outIds = Set.map (\(TxIn _id _) -> _id) (domain outs)
            oldIds = Set.map (\(TxIn _id _) -> _id) (domain u)
         in property $
              hasFailedScripts tx
                || ( null (outIds `Set.intersection` oldIds)
                       && eval ((dom outs) ⊆ (dom u'))
                   )

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
      any (\kl -> (Set.fromList kl) `Set.isSubsetOf` keyHashes) (scriptKeyCombinations (Proxy @era) msig)
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
  [] === (getDoubleInputs txs)
  where
    txs = toList $ (fromTxSeq @era . bbody) signal

    getDoubleInputs :: [Core.Tx era] -> [(Core.Tx era, [Core.Tx era])]
    getDoubleInputs [] = []
    getDoubleInputs (t : ts) = lookForDoubleSpends t ts ++ getDoubleInputs ts
    lookForDoubleSpends :: Core.Tx era -> [Core.Tx era] -> [(Core.Tx era, [Core.Tx era])]
    lookForDoubleSpends _ [] = []
    lookForDoubleSpends tx_j ts =
      if null doubles then [] else [(tx_j, doubles)]
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
  Block BHeader era ->
  Coin
withdrawals (UnserialisedBlock _ txseq) =
  foldl'
    ( \c tx ->
        let wdrls = unWdrl $ getField @"wdrls" (getField @"body" tx)
         in if hasFailedScripts tx then c else c <> fold wdrls
    )
    (Coin 0)
    $ (fromTxSeq @era txseq)

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
      UTxOState {_deposited = d} = (_utxoState . esLState) es
   in (d >= mempty) === True

-- | Checks that the fees are non-decreasing when not at an epoch boundary
feesNonDecreasing ::
  SourceSignalTarget (CHAIN era) ->
  Property
feesNonDecreasing SourceSignalTarget {source, target} =
  property $
    fees_ source <= fees_ target
  where
    fees_ chainSt =
      let UTxOState {_fees = fees} =
            _utxoState . esLState . nesEs . chainNes $ chainSt
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
  Block BHeader era ->
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
  Block BHeader era ->
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
    (relevantUTxO, irrelevantUTxO) = Map.partitionWithKey (const . (`Set.member` txIns)) utxo
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
  Block BHeader era ->
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
  Block BHeader era ->
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
          dummyCertIx = 0
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
  Block BHeader era ->
  ( ChainState era,
    LedgerEnv era,
    (UTxOState era, DPState (Crypto era)),
    [Core.Tx era]
  )
ledgerTraceBase chainSt block =
  ( tickedChainSt,
    LedgerEnv slot 0 pp_ (esAccountState nes),
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
    poolState = _pstate . _delegationState . esLState . nesEs . chainNes

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
