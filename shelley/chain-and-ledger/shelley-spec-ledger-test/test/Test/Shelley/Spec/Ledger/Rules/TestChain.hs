{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Rules.TestChain
  ( -- TestPoolReap
    constantSumPots,
    nonNegativeDeposits,
    removedAfterPoolreap,
    -- TestNewEpoch
    adaPreservationChain,
    collisionFreeComplete,
  )
where

import qualified Cardano.Ledger.Val as Val
import Control.Iterate.SetAlgebra (dom, domain, eval, (<|), (∩), (⊆))
import Control.State.Transition.Extended (TRC (TRC))
import Control.State.Transition.Trace
  ( SourceSignalTarget (..),
    Trace (..),
    sourceSignalTargets,
  )
import qualified Control.State.Transition.Trace as Trace
import Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState)
import Data.Foldable (fold, foldl', toList)
import qualified Data.Map.Strict as Map (isSubmapOf)
import Data.Proxy
import qualified Data.Set as Set (intersection, map, null)
import Data.Word (Word64)
import Shelley.Spec.Ledger.API
  ( CHAIN,
    LEDGER,
    POOLREAP,
    TICK,
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
import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.STS.Chain (ChainState (..), totalAda, totalAdaPots)
import Shelley.Spec.Ledger.STS.Ledger (LedgerEnv (..))
import Shelley.Spec.Ledger.STS.PoolReap (PoolreapState (..))
import Shelley.Spec.Ledger.Tx
import Shelley.Spec.Ledger.TxBody
import Shelley.Spec.Ledger.UTxO (balance, totalDeposits, txins, txouts, pattern UTxO)
import Test.QuickCheck
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( C,
  )
import Test.Shelley.Spec.Ledger.Generator.Block (tickChainState)
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (geConstants))
import qualified Test.Shelley.Spec.Ledger.Generator.Presets as Preset (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain (mkGenesisChainState)
import qualified Test.Shelley.Spec.Ledger.Rules.TestPoolreap as TestPoolreap
import Test.Shelley.Spec.Ledger.Utils
  ( applySTSTest,
    epochFromSlotNo,
    runShelleyBase,
    testGlobals,
  )

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Word64
numberOfTests = 300

traceLen :: Word64
traceLen = 100

----------------------------------------------------------------------
-- Properties for Chain
---------------------------------------------------------------------

-- | Tx inputs are eliminated, outputs added to utxo and TxIds are unique
collisionFreeComplete :: Property
collisionFreeComplete =
  forAllChainTrace traceLen $ \tr -> do
    let ssts = sourceSignalTargets tr
    conjoin . concat $
      [ -- collision freeness
        map eliminateTxInputs ssts,
        map newEntriesAndUniqueTxIns ssts,
        -- no double spend
        map noDoubleSpend ssts
      ]

-- | Various preservation properties, tested on longer traces (double the
-- default length)
adaPreservationChain :: Property
adaPreservationChain =
  forAllChainTrace (traceLen * 2) $ \tr -> do
    let ssts = sourceSignalTargets tr
        noEpochBoundarySsts = filter sameEpoch ssts

    conjoin . concat $
      [ -- preservation properties
        map checkPreservation ssts,
        map potsSumIncreaseWdrlsPerTx ssts,
        map preserveBalance ssts,
        map preserveBalanceRestricted ssts,
        map preserveOutputsTx ssts,
        -- non-epoch-boundary preservation properties
        map checkWithdrawlBound noEpochBoundarySsts,
        map utxoDepositsIncreaseByFeesWithdrawals noEpochBoundarySsts,
        map potsSumIncreaseWdrlsPerBlock noEpochBoundarySsts
      ]
  where
    epoch s = nesEL . chainNes $ s
    sameEpoch :: SourceSignalTarget (CHAIN C) -> Bool
    sameEpoch (SourceSignalTarget {source, target}) =
      epoch source == epoch target

-- ADA should be preserved for all state transitions in the generated trace
checkPreservation :: SourceSignalTarget (CHAIN C) -> Property
checkPreservation SourceSignalTarget {source, signal, target} =
  counterexample
    ( mconcat
        [ "source\n",
          show source,
          "\nsignal\n",
          show signal,
          "\ntarget\n",
          show target,
          "\nsource pots\n",
          show sourcePots,
          "\ntarget pots\n",
          show targetPots
        ]
    )
    $ totalAda source === totalAda target
  where
    sourcePots = totalAdaPots source
    targetPots = totalAdaPots target

-- If we are not at an Epoch Boundary (i.e. epoch source == epoch target)
-- then the total rewards should change only by withdrawals
checkWithdrawlBound :: SourceSignalTarget (CHAIN C) -> Property
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
        Val.~~ fold
          ( _rewards . _dstate
              . _delegationState
              . esLState
              . nesEs
              . chainNes
              $ target
          )

-- | If we are not at an Epoch Boundary , then (Utxo + Deposits)
-- increases by Withdrawals min Fees (for all transactions in a block)
utxoDepositsIncreaseByFeesWithdrawals :: SourceSignalTarget (CHAIN C) -> Property
utxoDepositsIncreaseByFeesWithdrawals SourceSignalTarget {source, signal, target} =
  circulation target Val.~~ circulation source
    === withdrawals signal Val.~~ txFees signal
  where
    circulation chainSt =
      let es = (nesEs . chainNes) chainSt
          (UTxOState {_utxo = u, _deposited = d}) = (_utxoState . esLState) es
       in balance u <> d

-- | Reconstruct a LEDGER trace from the transactions in a Block and ChainState
--
-- NOTE: we need to tick the slot before processing transactions
-- (in the same way that the CHAIN rule TICKs the slot before processing
-- transactions with the LEDGERS rule)
ledgerTraceFromBlock :: ChainState C -> Block C -> (ChainState C, Trace (LEDGER C))
ledgerTraceFromBlock chainSt block =
  ( tickedChainSt,
    runShelleyBase $
      Trace.closure @(LEDGER C) ledgerEnv ledgerSt0 (reverse txs) -- Oldest to Newest first
  )
  where
    (Block (BHeader bhb _) txSeq) = block
    slot = bheaderSlotNo bhb
    tickedChainSt = tickChainState slot chainSt
    nes = (nesEs . chainNes) tickedChainSt
    pp_ = esPp nes
    LedgerState utxoSt0 delegSt0 = esLState nes
    ledgerEnv = LedgerEnv slot 0 pp_ (esAccountState nes)
    ledgerSt0 = (utxoSt0, delegSt0)
    txs = (toList . txSeqTxns') txSeq

-- | If we are not at an Epoch Boundary , then (Utxo + Deposits + Fees)
-- increases by sum of withdrawals for all transactions in a block
potsSumIncreaseWdrlsPerBlock :: SourceSignalTarget (CHAIN C) -> Property
potsSumIncreaseWdrlsPerBlock SourceSignalTarget {source, signal, target} =
  potsSum target Val.~~ potsSum source === withdrawals signal
  where
    potsSum chainSt =
      let (UTxOState {_utxo = u, _deposited = d, _fees = f}) =
            _utxoState . esLState . nesEs . chainNes $ chainSt
       in balance u <> d <> f

-- | If we are not at an Epoch Boundary , then (Utxo + Deposits + Fees)
-- increases by sum of withdrawals in a transaction
potsSumIncreaseWdrlsPerTx :: SourceSignalTarget (CHAIN C) -> Property
potsSumIncreaseWdrlsPerTx SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map sumIncreaseWdrls $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock chainSt block
    sumIncreaseWdrls
      SourceSignalTarget
        { source = (UTxOState {_utxo = u, _deposited = d, _fees = f}, _),
          signal = tx,
          target = (UTxOState {_utxo = u', _deposited = d', _fees = f'}, _)
        } =
        (balance u' <> d' <> f') Val.~~ (balance u <> d <> f) === fold (unWdrl . _wdrls $ _body tx)

-- | Preserve the balance in a transaction, i.e., the sum of the consumed value
-- equals the sum of the created value.
preserveBalance :: SourceSignalTarget (CHAIN C) -> Property
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
        certs = toList . _certs $ txb
        pools = _pParams . _pstate $ dstate
        created =
          balance u'
            <> _txfee txb
            <> totalDeposits pp_ pools certs
        consumed_ =
          balance u
            <> keyRefunds pp_ txb
            <> fold (unWdrl . _wdrls $ txb)

-- | Preserve balance restricted to TxIns and TxOuts of the Tx
preserveBalanceRestricted :: SourceSignalTarget (CHAIN C) -> Property
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
          balance (eval ((_inputs txb) <| u))
            <> keyRefunds pp_ txb
            <> fold (unWdrl . _wdrls $ txb)
        outs =
          let certs = toList (_certs txb)
           in balance (txouts txb)
                <> _txfee txb
                <> totalDeposits pp_ pools certs

preserveOutputsTx :: SourceSignalTarget (CHAIN C) -> Property
preserveOutputsTx SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map outputPreserved $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock chainSt block
    outputPreserved SourceSignalTarget {target = (UTxOState {_utxo = (UTxO u')}, _), signal = tx} =
      let UTxO outs = txouts (_body tx)
       in property $
            outs `Map.isSubmapOf` u'

-- | Check that consumed inputs are eliminated from the resulting UTxO
eliminateTxInputs :: SourceSignalTarget (CHAIN C) -> Property
eliminateTxInputs SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map inputsEliminated $
      sourceSignalTargets ledgerTr
  where
    (_, ledgerTr) = ledgerTraceFromBlock chainSt block
    inputsEliminated SourceSignalTarget {target = (UTxOState {_utxo = (UTxO u')}, _), signal = tx} =
      property $
        Set.null $ eval (txins (_body tx) ∩ dom u')

-- | Collision-Freeness of new TxIds - checks that all new outputs of a Tx are
-- included in the new UTxO and that all TxIds are new.
newEntriesAndUniqueTxIns :: SourceSignalTarget (CHAIN C) -> Property
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
        let UTxO outs = txouts (_body tx)
            outIds = Set.map (\(TxIn _id _) -> _id) (domain outs)
            oldIds = Set.map (\(TxIn _id _) -> _id) (domain u)
         in property $
              null (outIds `Set.intersection` oldIds)
                && eval ((dom outs) ⊆ (dom u'))

--- | Check for absence of double spend in a block
noDoubleSpend :: SourceSignalTarget (CHAIN C) -> Property
noDoubleSpend SourceSignalTarget {signal} =
  [] === getDoubleInputs txs
  where
    txs = toList $ (txSeqTxns' . bbody) signal

    getDoubleInputs :: [Tx C] -> [(Tx C, [Tx C])]
    getDoubleInputs [] = []
    getDoubleInputs (t : ts) = lookForDoubleSpends t ts ++ getDoubleInputs ts
    lookForDoubleSpends :: Tx C -> [Tx C] -> [(Tx C, [Tx C])]
    lookForDoubleSpends _ [] = []
    lookForDoubleSpends tx_j ts =
      if null doubles then [] else [(tx_j, doubles)]
      where
        doubles =
          filter
            ( \tx_i ->
                (not . Set.null)
                  (inps_j `Set.intersection` _inputs (_body tx_i))
            )
            ts
        inps_j = _inputs $ _body tx_j

withdrawals :: Block C -> Coin
withdrawals block =
  foldl'
    ( \c tx ->
        let wdrls =
              unWdrl . _wdrls . _body $
                tx
         in c <> fold wdrls
    )
    (Coin 0)
    $ (txSeqTxns' . bbody) block

txFees :: Block C -> Coin
txFees block =
  foldl'
    (\c tx -> c <> (_txfee . _body $ tx))
    (Coin 0)
    $ (txSeqTxns' . bbody) block

----------------------------------------------------------------------
-- Properties for PoolReap (using the CHAIN Trace) --
----------------------------------------------------------------------

constantSumPots :: Property
constantSumPots =
  forAllChainTrace traceLen $ \tr ->
    let sst = map chainToPoolreapSst (sourceSignalTargets tr)
     in TestPoolreap.constantSumPots sst

nonNegativeDeposits :: Property
nonNegativeDeposits =
  forAllChainTrace traceLen $ \tr ->
    let sst = map chainToPoolreapSst (sourceSignalTargets tr)
     in TestPoolreap.nonNegativeDeposits sst

removedAfterPoolreap :: Property
removedAfterPoolreap =
  forAllChainTrace traceLen $ \tr ->
    let ssts = map chainToPoolreapSst (chainSstWithTick tr)
     in TestPoolreap.removedAfterPoolreap ssts

---------------------------
-- Utils --
---------------------------

forAllChainTrace ::
  (Testable prop) =>
  Word64 -> -- trace length
  (Trace (CHAIN C) -> prop) ->
  Property
forAllChainTrace n prop =
  withMaxSuccess (fromIntegral numberOfTests) . property $
    forAllTraceFromInitState testGlobals n (Preset.genEnv p) (Just $ mkGenesisChainState (geConstants (Preset.genEnv p))) prop
  where
    p :: Proxy C
    p = Proxy

-- | Transform CHAIN `sourceSignalTargets`s to POOLREAP ones.
chainToPoolreapSst ::
  SourceSignalTarget (CHAIN C) ->
  SourceSignalTarget (POOLREAP C)
chainToPoolreapSst
  ( SourceSignalTarget
      ChainState {chainNes = nes}
      ChainState {chainNes = nes'}
      (Block bh _)
    ) =
    SourceSignalTarget
      ( PoolreapState
          (utxoSt nes)
          (accountSt nes)
          dstate
          pstate
      )
      ( PoolreapState
          (utxoSt nes')
          (accountSt nes')
          dstate'
          pstate'
      )
      (epochFromSlotNo s)
    where
      s = (bheaderSlotNo . bhbody) bh
      DPState dstate pstate = (_delegationState . esLState . nesEs) nes
      DPState dstate' pstate' = (_delegationState . esLState . nesEs) nes'
      utxoSt = _utxoState . esLState . nesEs
      accountSt = esAccountState . nesEs

-- | Transform the [(source, signal, target)] of a CHAIN Trace
-- by manually applying the Chain TICK Rule to each source and producing
-- [(source, signal, target')].
--
-- This allows for testing properties on traces that exclude effects of the
-- "UTXO branches" of the STS Rule tree.
chainSstWithTick :: Trace (CHAIN C) -> [SourceSignalTarget (CHAIN C)]
chainSstWithTick ledgerTr =
  map applyTick ssts
  where
    ssts = sourceSignalTargets ledgerTr
    applyTick ::
      SourceSignalTarget (CHAIN C) ->
      SourceSignalTarget (CHAIN C)
    applyTick
      ( SourceSignalTarget
          chainSt@ChainState {chainNes = nes}
          _
          b@(Block bh _)
        ) =
        case runShelleyBase (applySTSTest @(TICK C) (TRC ((), nes, (bheaderSlotNo . bhbody) bh))) of
          Left pf ->
            error ("chainSstWithTick.applyTick Predicate failure " <> show pf)
          Right nes' ->
            SourceSignalTarget
              chainSt
              (chainSt {chainNes = nes'})
              b
