{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Rules.TestChain
  ( -- TestPoolReap
    removedAfterPoolreap,
    -- TestNewEpoch
    adaPreservationChain,
    collisionFreeComplete,
    -- Test Pool
    poolProperties,
    -- Test Delegation
    delegProperties,
  )
where

import Cardano.Binary (ToCBOR)
import qualified Cardano.Ledger.Core as Core (EraRule, TxBody, TxOut)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley.Constraints (TransValue, UsesTxOut, UsesValue)
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val (coin)
import Cardano.Prelude (HasField (..))
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
import Data.Foldable (fold, foldl', toList)
import qualified Data.Map.Strict as Map (isSubmapOf)
import Data.Proxy
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, intersection, isSubsetOf, map, null)
import Data.Word (Word64)
import Shelley.Spec.Ledger.API
  ( ApplyBlock,
    CHAIN,
    DELEG,
    DelegsEnv,
    GetLedgerView,
    LEDGER,
    UtxoEnv,
  )
import Shelley.Spec.Ledger.BlockChain
  ( BHeader (..),
    Block (..),
    TxSeq (..),
    bbody,
    bhbody,
    bheaderSlotNo,
  )
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.LedgerState hiding (circulation)
import Shelley.Spec.Ledger.PParams (_eMax)
import Shelley.Spec.Ledger.STS.Chain (ChainState (..), totalAda, totalAdaPots)
import Shelley.Spec.Ledger.STS.Deleg (DelegEnv (..))
import Shelley.Spec.Ledger.STS.Ledger (LedgerEnv (..))
import Shelley.Spec.Ledger.STS.Pool (POOL, PoolEnv (..))
import Shelley.Spec.Ledger.Tx
import Shelley.Spec.Ledger.TxBody
import Shelley.Spec.Ledger.UTxO (balance, totalDeposits, txins, txouts, pattern UTxO)
import Test.QuickCheck
import Test.Shelley.Spec.Ledger.Generator.Block (tickChainState)
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv)
import Test.Shelley.Spec.Ledger.Generator.EraGen (EraGen (..))
import qualified Test.Shelley.Spec.Ledger.Generator.Presets as Preset (genEnv)
import Test.Shelley.Spec.Ledger.Generator.ScriptClass (scriptKeyCombinations)
import Test.Shelley.Spec.Ledger.Generator.ShelleyEraGen ()
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain (mkGenesisChainState)
import Test.Shelley.Spec.Ledger.Orphans ()
import qualified Test.Shelley.Spec.Ledger.Rules.TestDeleg as TestDeleg
  ( checkInstantaneousRewards,
    keyDeRegistration,
    keyDelegation,
    keyRegistration,
    rewardsSumInvariant,
  )
import qualified Test.Shelley.Spec.Ledger.Rules.TestPool as TestPool
  ( poolRegistration,
    poolRetirement,
    poolStateIsInternallyConsistent,
  )
import qualified Test.Shelley.Spec.Ledger.Rules.TestPoolreap as TestPoolreap
import Test.Shelley.Spec.Ledger.Utils (ChainProperty, epochFromSlotNo,
                                       runShelleyBase, testGlobals)
import Data.Default.Class (Default)


-- ======================================================

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Word64
numberOfTests = 300

traceLen :: Word64
traceLen = 100

longTraceLen :: Word64
longTraceLen = 150

----------------------------------------------------------------------
-- Properties for Chain
---------------------------------------------------------------------

-- | Tx inputs are eliminated, outputs added to utxo and TxIds are unique
collisionFreeComplete ::
  forall era.
  ( EraGen era,
    UsesTxOut era,
    TransValue ToCBOR era,
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "outputs" (Core.TxBody era) (StrictSeq (Core.TxOut era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    Default (State (Core.EraRule "PPUP" era)),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  Property
collisionFreeComplete =
  forAllChainTrace @era traceLen $ \tr -> do
    let ssts = sourceSignalTargets tr
    conjoin . concat $
      [ -- collision freeness
        map eliminateTxInputs ssts,
        map newEntriesAndUniqueTxIns ssts,
        -- no double spend
        map noDoubleSpend ssts,
        -- tx signatures
        map requiredMSigSignaturesSubset ssts
      ]

-- | Various preservation properties
adaPreservationChain ::
  forall era.
  ( EraGen era,
    UsesTxOut era,
    TransValue ToCBOR era,
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "outputs" (Core.TxBody era) (StrictSeq (Core.TxOut era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "txfee" (Core.TxBody era) Coin,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    Default (State (Core.EraRule "PPUP" era)),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  Property
adaPreservationChain =
  forAllChainTrace @era longTraceLen $ \tr -> do
    let ssts = sourceSignalTargets tr
        noEpochBoundarySsts = filter sameEpoch ssts

    conjoin . concat $
      [ -- preservation properties
        map checkPreservation ssts,
        map potsSumIncreaseWdrlsPerTx ssts,
        map potsSumIncreaseByRewardsPerTx ssts,
        map preserveBalance ssts,
        map preserveBalanceRestricted ssts,
        map preserveOutputsTx ssts,
        map potsRewardsDecreaseByWdrlsPerTx ssts,
        -- well formed deposits
        map nonNegativeDeposits ssts,
        -- non-epoch-boundary preservation properties
        map checkWithdrawlBound noEpochBoundarySsts,
        map utxoDepositsIncreaseByFeesWithdrawals noEpochBoundarySsts,
        map potsSumIncreaseWdrlsPerBlock noEpochBoundarySsts,
        map feesNonDecreasing noEpochBoundarySsts
      ]

-- ADA should be preserved for all state transitions in the generated trace
checkPreservation ::
  (UsesTxOut era, UsesValue era) =>
  SourceSignalTarget (CHAIN era) ->
  Property
checkPreservation SourceSignalTarget {source, target} =
  counterexample
    ( mconcat
        [ "\nsource pots\n",
          show (totalAdaPots source),
          "\ntarget pots\n",
          show (totalAdaPots target),
          "\nsource total\n",
          show sourceTotal,
          "\ntarget total\n",
          show targetTotal
        ]
    )
    $ sourceTotal === targetTotal
  where
    sourceTotal = totalAda source
    targetTotal = totalAda target

-- If we are not at an Epoch Boundary (i.e. epoch source == epoch target)
-- then the total rewards should change only by withdrawals
checkWithdrawlBound ::
  ( ChainProperty era,
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
-- increases by Withdrawals min Fees (for all transactions in a block)
utxoDepositsIncreaseByFeesWithdrawals ::
  ( ChainProperty era,
    UsesTxOut era,
    HasField "txfee" (Core.TxBody era) Coin,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
utxoDepositsIncreaseByFeesWithdrawals SourceSignalTarget {source, signal, target} =
  circulation target <-> circulation source
    === withdrawals signal <-> txFees signal
  where
    circulation chainSt =
      let es = (nesEs . chainNes) chainSt
          (UTxOState {_utxo = u, _deposited = d}) = (_utxoState . esLState) es
       in Val.coin (balance u) <+> d

-- | If we are not at an Epoch Boundary, then (Utxo + Deposits + Fees)
-- increases by sum of withdrawals for all transactions in a block
potsSumIncreaseWdrlsPerBlock ::
  ( UsesTxOut era,
    ChainProperty era,
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
  forall era.
  ( ChainProperty era,
    UsesTxOut era,
    TransValue ToCBOR era,
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsSumIncreaseWdrlsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map sumIncreaseWdrls $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock chainSt block
    sumIncreaseWdrls :: SourceSignalTarget (LEDGER era) -> Property
    sumIncreaseWdrls
      SourceSignalTarget
        { source = (UTxOState {_utxo = u, _deposited = d, _fees = f}, _),
          signal = tx,
          target = (UTxOState {_utxo = u', _deposited = d', _fees = f'}, _)
        } =
        (Val.coin (balance u') <+> d' <+> f') <-> (Val.coin (balance u) <+> d <+> f)
          === fold (unWdrl (getField @"wdrls" (_body tx)))

-- | (Utxo + Deposits + Fees) increases by the reward delta
potsSumIncreaseByRewardsPerTx ::
  ( ChainProperty era,
    UsesTxOut era,
    TransValue ToCBOR era,
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsSumIncreaseByRewardsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map sumIncreaseRewards $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock chainSt block
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
        (Val.coin (balance u') <+> d' <+> f')
          <-> (Val.coin (balance u) <+> d <+> f)
            === fold rewards
          <-> fold rewards'

-- | The Rewards pot decreases by the sum of withdrawals in a transaction
potsRewardsDecreaseByWdrlsPerTx ::
  ( ChainProperty era,
    UsesTxOut era,
    TransValue ToCBOR era,
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
potsRewardsDecreaseByWdrlsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map rewardsDecreaseByWdrls $
      sourceSignalTargets ledgerTr
  where
    rewardsSum = (foldl' (<+>) (Coin 0)) . _rewards . _dstate
    (_, ledgerTr) = ledgerTraceFromBlock chainSt block
    rewardsDecreaseByWdrls
      SourceSignalTarget
        { source = (_, dpstate),
          signal = tx,
          target = (_, dpstate')
        } =
        let totalRewards = rewardsSum dpstate
            totalRewards' = rewardsSum dpstate'
            txWithdrawals = fold (unWdrl (getField @"wdrls" (_body tx)))
         in conjoin
              [ counterexample
                  "A transaction should not increase the Rewards pot"
                  (totalRewards >= totalRewards'),
                counterexample
                  "Withdrawals should be non-negative"
                  (txWithdrawals >= Coin 0),
                counterexample
                  "Rewards should increase by withdrawals"
                  (totalRewards <-> totalRewards' == txWithdrawals)
              ]

-- | Preserve the balance in a transaction, i.e., the sum of the consumed value
-- equals the sum of the created value.
preserveBalance ::
  ( ChainProperty era,
    UsesTxOut era,
    TransValue ToCBOR era,
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era,
    HasField "txfee" (Core.TxBody era) Coin,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
preserveBalance SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map createdIsConsumed $
      sourceSignalTargets ledgerTr
  where
    (tickedChainSt, ledgerTr) = ledgerTraceFromBlock chainSt block
    pp_ = (esPp . nesEs . chainNes) tickedChainSt

    createdIsConsumed SourceSignalTarget {source = ledgerSt, signal = tx, target = ledgerSt'} =
      counterexample
        ("preserveBalance created /= consumed ... " <> show created <> " /= " <> show consumed_)
        (created === consumed_)
      where
        (UTxOState {_utxo = u}, dstate) = ledgerSt
        (UTxOState {_utxo = u'}, _) = ledgerSt'
        txb = _body tx
        certs = toList (getField @"certs" txb)
        pools = _pParams . _pstate $ dstate
        created =
          Val.coin (balance u')
            <+> getField @"txfee" txb
            <+> totalDeposits pp_ pools certs
        consumed_ =
          Val.coin (balance u)
            <+> keyRefunds pp_ txb
            <+> fold (unWdrl (getField @"wdrls" txb))

-- | Preserve balance restricted to TxIns and TxOuts of the Tx
preserveBalanceRestricted ::
  forall era.
  ( ChainProperty era,
    UsesTxOut era,
    TransValue ToCBOR era,
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "outputs" (Core.TxBody era) (StrictSeq (Core.TxOut era)),
    HasField "txfee" (Core.TxBody era) Coin,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
preserveBalanceRestricted SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map createdIsConsumed $
      sourceSignalTargets ledgerTr
  where
    (tickedChainSt, ledgerTr) = ledgerTraceFromBlock chainSt block
    pp_ = (esPp . nesEs . chainNes) tickedChainSt

    createdIsConsumed SourceSignalTarget {source = (UTxOState {_utxo = u}, dstate), signal = tx} =
      inps === outs
      where
        txb = _body tx
        pools = _pParams . _pstate $ dstate
        inps =
          Val.coin (balance @era (eval ((getField @"inputs" txb) <| u)))
            <> keyRefunds pp_ txb
            <> fold (unWdrl (getField @"wdrls" txb))
        outs =
          let certs = toList (getField @"certs" txb)
           in Val.coin (balance (txouts @era txb))
                <> getField @"txfee" txb
                <> totalDeposits pp_ pools certs

preserveOutputsTx :: forall era.
  ( ChainProperty era,
    UsesTxOut era,
    TransValue ToCBOR era,
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era,
    HasField "outputs" (Core.TxBody era) (StrictSeq (Core.TxOut era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
preserveOutputsTx SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map outputPreserved $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock chainSt block
    outputPreserved SourceSignalTarget {target = (UTxOState {_utxo = (UTxO u')}, _), signal = tx} =
      let UTxO outs = txouts @era (_body tx)
       in property $
            outs `Map.isSubmapOf` u'

-- | Check that consumed inputs are eliminated from the resulting UTxO
eliminateTxInputs ::
  forall era.
  ( ChainProperty era,
    UsesTxOut era,
    TransValue ToCBOR era,
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
eliminateTxInputs SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map inputsEliminated $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock chainSt block
    inputsEliminated SourceSignalTarget {target = (UTxOState {_utxo = (UTxO u')}, _), signal = tx} =
      property $
        Set.null $ eval (txins @era (_body tx) ∩ dom u')

-- | Collision-Freeness of new TxIds - checks that all new outputs of a Tx are
-- included in the new UTxO and that all TxIds are new.
newEntriesAndUniqueTxIns :: forall era.
  ( ChainProperty era,
    UsesTxOut era,
    TransValue ToCBOR era,
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era,
    HasField "outputs" (Core.TxBody era) (StrictSeq (Core.TxOut era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
newEntriesAndUniqueTxIns SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map newEntryPresent $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock chainSt block
    newEntryPresent
      SourceSignalTarget
        { source = (UTxOState {_utxo = (UTxO u)}, _),
          signal = tx,
          target = (UTxOState {_utxo = (UTxO u')}, _)
        } =
        let UTxO outs = txouts @era (_body tx)
            outIds = Set.map (\(TxIn _id _) -> _id) (domain outs)
            oldIds = Set.map (\(TxIn _id _) -> _id) (domain u)
         in property $
              null (outIds `Set.intersection` oldIds)
                && eval ((dom outs) ⊆ (dom u'))

-- | Check for required signatures in case of Multi-Sig. There has to be one set
-- of possible signatures for a multi-sig script which is a sub-set of the
-- signatures of the tansaction.
requiredMSigSignaturesSubset ::
  forall era.
  ( EraGen era,
    UsesTxOut era,
    TransValue ToCBOR era,
    ChainProperty era,
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
requiredMSigSignaturesSubset SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map signaturesSubset $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock chainSt block
    signaturesSubset :: SourceSignalTarget (LEDGER era) -> Property
    signaturesSubset SourceSignalTarget {signal = tx} =
      let khs = keyHashSet tx
       in property $
            all (existsReqKeyComb khs) (scriptWits . _witnessSet $ tx)

    existsReqKeyComb keyHashes msig =
      any (\kl -> (Set.fromList kl) `Set.isSubsetOf` keyHashes) (scriptKeyCombinations (Proxy @era) msig)

    keyHashSet tx_ =
      Set.map witKeyHash (addrWits . _witnessSet $ tx_)

--- | Check for absence of double spend in a block
noDoubleSpend ::
  forall era.
  ( ChainProperty era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
noDoubleSpend SourceSignalTarget {signal} =
  [] === getDoubleInputs txs
  where
    txs = toList $ (txSeqTxns' . bbody) signal

    getDoubleInputs :: [Tx era] -> [(Tx era, [Tx era])]
    getDoubleInputs [] = []
    getDoubleInputs (t : ts) = lookForDoubleSpends t ts ++ getDoubleInputs ts
    lookForDoubleSpends :: Tx era -> [Tx era] -> [(Tx era, [Tx era])]
    lookForDoubleSpends _ [] = []
    lookForDoubleSpends tx_j ts =
      if null doubles then [] else [(tx_j, doubles)]
      where
        doubles =
          filter
            ( \tx_i ->
                (not . Set.null)
                  (inps_j `Set.intersection` getField @"inputs" (_body tx_i))
            )
            ts
        inps_j = getField @"inputs" $ _body tx_j

withdrawals ::
  ( ChainProperty era,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  Block era ->
  Coin
withdrawals block =
  foldl'
    ( \c tx ->
        let wdrls = unWdrl $ getField @"wdrls" (_body tx)
         in c <> fold wdrls
    )
    (Coin 0)
    $ (txSeqTxns' . bbody) block

txFees ::
  ( ChainProperty era,
    HasField "txfee" (Core.TxBody era) Coin
  ) =>
  Block era ->
  Coin
txFees block =
  foldl'
    (\c tx -> c <> (getField @"txfee" (_body tx)))
    (Coin 0)
    $ (txSeqTxns' . bbody) block

-- | Check that deposits are always non-negative
nonNegativeDeposits ::
  SourceSignalTarget (CHAIN era) ->
  Property
nonNegativeDeposits SourceSignalTarget {source = chainSt} =
  let es = (nesEs . chainNes) chainSt
      (UTxOState {_deposited = d}) = (_utxoState . esLState) es
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
      let (UTxOState {_fees = fees}) =
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
    UsesTxOut era,
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    Default (State (Core.EraRule "PPUP" era))
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
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
poolRetirement SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map (TestPool.poolRetirement currentEpoch maxEpoch) (sourceSignalTargets poolTr)
  where
    (chainSt', poolTr) = poolTraceFromBlock chainSt block
    (Block (BHeader bhb _) _) = block
    currentEpoch = (epochFromSlotNo . bheaderSlotNo) bhb
    maxEpoch = (_eMax . esPp . nesEs . chainNes) chainSt'

-- | Check that a newly registered pool key is registered and not
-- in the retiring map.
poolRegistration ::
  ( ChainProperty era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
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
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
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
    UsesTxOut era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    ChainProperty era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    Default (State (Core.EraRule "PPUP" era))
  ) =>
  Property
delegProperties =
  forAllChainTrace @era traceLen $ \tr -> do
    conjoin $
      map chainProp (sourceSignalTargets tr)
  where
    delegProp :: SourceSignalTarget (DELEG era) -> Property
    delegProp delegSst =
      conjoin $
        [ TestDeleg.keyRegistration delegSst,
          TestDeleg.keyDeRegistration delegSst,
          TestDeleg.keyDelegation delegSst,
          TestDeleg.rewardsSumInvariant delegSst,
          TestDeleg.checkInstantaneousRewards delegSst
        ]
    chainProp :: SourceSignalTarget (CHAIN era) -> Property
    chainProp (SourceSignalTarget {source = chainSt, signal = block}) =
      let delegTr = snd $ delegTraceFromBlock chainSt block
          delegSsts = sourceSignalTargets delegTr
       in conjoin (map delegProp delegSsts)

----------------------------------------------------------------------
-- Projections of CHAIN Trace
----------------------------------------------------------------------

-- | Reconstruct a LEDGER trace from the transactions in a Block and ChainState
ledgerTraceFromBlock ::
  forall era.
  ( ChainProperty era,
    UsesTxOut era,
    TransValue ToCBOR era,
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Tx era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    Show (State (Core.EraRule "PPUP" era))
  ) =>
  ChainState era ->
  Block era ->
  (ChainState era, Trace (LEDGER era))
ledgerTraceFromBlock chainSt block =
  ( tickedChainSt,
    runShelleyBase $
      Trace.closure @(LEDGER era) ledgerEnv ledgerSt0 txs
  )
  where
    (tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block

-- | Reconstruct a POOL trace from the transactions in a Block and ChainState
poolTraceFromBlock ::
  forall era.
  ( ChainProperty era,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
  ) =>
  ChainState era ->
  Block era ->
  (ChainState era, Trace (POOL era))
poolTraceFromBlock chainSt block =
  ( tickedChainSt,
    runShelleyBase $
      Trace.closure @(POOL era) poolEnv poolSt0 poolCerts
  )
  where
    (tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block
    certs = concatMap (toList . (getField @"certs") . _body)
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
  Block era ->
  (ChainState era, Trace (DELEG era))
delegTraceFromBlock chainSt block =
  ( tickedChainSt,
    runShelleyBase $
      Trace.closure @(DELEG era) delegEnv delegSt0 blockCerts
  )
  where
    (tickedChainSt, ledgerEnv, ledgerSt0, txs) = ledgerTraceBase chainSt block
    certs = concatMap (reverse . toList . (getField @"certs") . _body)
    blockCerts = filter delegCert (certs txs)
    delegEnv =
      let (LedgerEnv s txIx _ reserves) = ledgerEnv
          dummyCertIx = 0
          ptr = Ptr s txIx dummyCertIx
       in DelegEnv s ptr reserves
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
  (Era era, GetLedgerView era, ApplyBlock era) =>
  ChainState era ->
  Block era ->
  ( ChainState era,
    LedgerEnv era,
    (UTxOState era, DPState (Crypto era)),
    [Tx era]
  )
ledgerTraceBase chainSt block =
  ( tickedChainSt,
    LedgerEnv slot 0 pp_ (esAccountState nes),
    (utxoSt0, delegSt0),
    txs
  )
  where
    (Block (BHeader bhb _) txSeq) = block
    slot = bheaderSlotNo bhb
    tickedChainSt = tickChainState slot chainSt
    nes = (nesEs . chainNes) tickedChainSt
    pp_ = esPp nes
    LedgerState utxoSt0 delegSt0 = esLState nes
    -- Oldest to Newest first
    txs = (reverse . toList . txSeqTxns') txSeq

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
      let (Block bh _) = block
          slot = (bheaderSlotNo . bhbody) bh
       in sst {target = tickChainState @era slot chainSt}

----------------------------------------------------------------------
-- Properties for PoolReap (using the CHAIN Trace) --
----------------------------------------------------------------------

removedAfterPoolreap ::
  forall era.
  ( ChainProperty era,
    EraGen era,
    UsesTxOut era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    Default (State (Core.EraRule "PPUP" era))
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
    removedAfterPoolreap_ (SourceSignalTarget {source, target, signal = (Block bh _)}) =
      let e = (epochFromSlotNo . bheaderSlotNo . bhbody) bh
       in TestPoolreap.removedAfterPoolreap (poolState source) (poolState target) e

---------------------------
-- Utils --
---------------------------

forAllChainTrace ::
  forall era prop.
  ( Testable prop,
    EraGen era,
    UsesTxOut era,
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    Default (State (Core.EraRule "PPUP" era))
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
