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
    rewardStkCredSync,
  )
where

import Cardano.Crypto.Hash (ShortHash)
import Control.Iterate.SetAlgebra (dom, domain, eval)
import Control.Monad (join)
import Control.State.Transition.Extended (TRC (TRC))
import Control.State.Transition.Trace
  ( SourceSignalTarget (..),
    Trace (..),
    sourceSignalTargets,
  )
import Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState)
import Data.Foldable (foldl')
import Data.Proxy
import Data.Word (Word64)
import Shelley.Spec.Ledger.BlockChain
  ( Block (..),
    TxSeq (..),
    bbody,
    bhbody,
    bheaderSlotNo,
  )
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.STS.Chain (ChainState (..), totalAda, totalAdaPots)
import Shelley.Spec.Ledger.STS.PoolReap (PoolreapState (..))
import Shelley.Spec.Ledger.STS.Tick (TickEnv (TickEnv))
import Shelley.Spec.Ledger.Tx
import Shelley.Spec.Ledger.TxData
import Test.QuickCheck
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (CHAIN, POOLREAP, TICK)
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

-- | Verify that the domains for '_rewards' and '_srkCreds' remain in sync.
rewardStkCredSync :: Property
rewardStkCredSync =
  forAllChainTrace $ \tr ->
    conjoin $
      map checkSync $
        sourceSignalTargets tr
  where
    checkSync SourceSignalTarget {source, signal, target} =
      let ds =
            _dstate
              . _delegationState
              . esLState
              . nesEs
              . chainNes
              $ target
       in counterexample
            ( mconcat
                [ "source\n",
                  show source,
                  "signal\n",
                  show signal,
                  "target\n",
                  show target
                ]
            )
            $ eval (dom (_stkCreds ds))
              === domain (_rewards ds)

adaPreservationChain :: Property
adaPreservationChain =
  forAllChainTrace $ \tr ->
    conjoin . join $
      map (\x -> [checkPreservation x, checkWithdrawlBound x]) $
        sourceSignalTargets tr
  where
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
    checkWithdrawlBound SourceSignalTarget {source, signal, target} =
      epoch source == epoch target ==> rewardDelta === withdrawls
      where
        epoch s = nesEL . chainNes $ s
        sum_ :: Foldable f => f Coin -> Coin
        sum_ = foldl' (+) (Coin 0)
        withdrawls :: Coin
        withdrawls =
          foldl'
            ( \c tx ->
                let wdrls =
                      unWdrl . _wdrls . _body $
                        tx
                 in c + sum_ wdrls
            )
            (Coin 0)
            $ txSeqTxns' . bbody $
              signal
        rewardDelta :: Coin
        rewardDelta =
          sum_
            ( _rewards . _dstate
                . _delegationState
                . esLState
                . nesEs
                . chainNes
                $ source
            )
            - sum_
              ( _rewards . _dstate
                  . _delegationState
                  . esLState
                  . nesEs
                  . chainNes
                  $ target
              )

----------------------------------------------------------------------
-- Properties for PoolReap (using the CHAIN Trace) --
----------------------------------------------------------------------

constantSumPots :: Property
constantSumPots =
  forAllChainTrace $ \tr ->
    let sst = map chainToPoolreapSst (sourceSignalTargets tr)
     in TestPoolreap.constantSumPots sst

nonNegativeDeposits :: Property
nonNegativeDeposits =
  forAllChainTrace $ \tr ->
    let sst = map chainToPoolreapSst (sourceSignalTargets tr)
     in TestPoolreap.nonNegativeDeposits sst

removedAfterPoolreap :: Property
removedAfterPoolreap =
  forAllChainTrace $ \tr ->
    let ssts = map chainToPoolreapSst (chainSstWithTick tr)
     in TestPoolreap.removedAfterPoolreap ssts

---------------------------
-- Utils --
---------------------------

forAllChainTrace ::
  (Testable prop) =>
  (Trace (CHAIN ShortHash) -> prop) ->
  Property
forAllChainTrace prop =
  withMaxSuccess (fromIntegral numberOfTests) . property $
    forAllTraceFromInitState testGlobals traceLen (Preset.genEnv p) (Just $ mkGenesisChainState (geConstants (Preset.genEnv p))) prop
  where
    p :: Proxy ShortHash
    p = Proxy

-- | Transform CHAIN `sourceSignalTargets`s to POOLREAP ones.
chainToPoolreapSst ::
  SourceSignalTarget (CHAIN ShortHash) ->
  SourceSignalTarget (POOLREAP ShortHash)
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
chainSstWithTick :: Trace (CHAIN ShortHash) -> [SourceSignalTarget (CHAIN ShortHash)]
chainSstWithTick ledgerTr =
  map applyTick ssts
  where
    ssts = sourceSignalTargets ledgerTr
    applyTick ::
      SourceSignalTarget (CHAIN ShortHash) ->
      SourceSignalTarget (CHAIN ShortHash)
    applyTick
      ( SourceSignalTarget
          chainSt@ChainState {chainNes = nes}
          _
          b@(Block bh _)
        ) =
        case runShelleyBase (applySTSTest @(TICK ShortHash) (TRC (TickEnv (getGKeys nes), nes, (bheaderSlotNo . bhbody) bh))) of
          Left pf ->
            error ("chainSstWithTick.applyTick Predicate failure " <> show pf)
          Right nes' ->
            SourceSignalTarget
              chainSt
              (chainSt {chainNes = nes'})
              b
