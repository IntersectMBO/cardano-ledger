{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Rules.TestChain
  ( -- TestPoolReap
    constantSumPots,
    nonNegativeDeposits,
    removedAfterPoolreap,
    -- TestNewEpoch
    preservationOfAda,
  )
where

import Cardano.Crypto.Hash (ShortHash)
import Control.State.Transition.Extended (TRC (TRC), applySTS)
import Control.State.Transition.Trace
  ( SourceSignalTarget (..),
    Trace (..),
    sourceSignalTargets,
  )
import Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState)
import Data.Proxy
import Data.Word (Word64)
import Shelley.Spec.Ledger.BlockChain (Block (..), bhbody, bheaderSlotNo)
import Shelley.Spec.Ledger.LedgerState
  ( _delegationState,
    _utxoState,
    esAccountState,
    esLState,
    getGKeys,
    nesEs,
    pattern DPState,
  )
import Shelley.Spec.Ledger.STS.Chain (ChainState (..))
import Shelley.Spec.Ledger.STS.PoolReap (PoolreapState (..))
import Shelley.Spec.Ledger.STS.Tick (TickEnv (TickEnv))
import Test.QuickCheck (Property, Testable, property, withMaxSuccess)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (CHAIN, NEWEPOCH, POOLREAP, TICK)
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (geConstants))
import qualified Test.Shelley.Spec.Ledger.Generator.Presets as Preset (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain (mkGenesisChainState)
import qualified Test.Shelley.Spec.Ledger.Rules.TestNewEpoch as TestNewEpoch
import qualified Test.Shelley.Spec.Ledger.Rules.TestPoolreap as TestPoolreap
import Test.Shelley.Spec.Ledger.Utils (epochFromSlotNo, runShelleyBase, testGlobals)

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Word64
numberOfTests = 300

traceLen :: Word64
traceLen = 100

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

----------------------------------------------------------------------
-- Properties for NewEpoch (using the CHAIN Trace) --
----------------------------------------------------------------------

preservationOfAda :: Property
preservationOfAda =
  forAllChainTrace $ \tr ->
    let sst = map chainToNewEpochSst (sourceSignalTargets tr)
     in TestNewEpoch.preservationOfAda sst

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

-- | Transform CHAIN `sourceSignalTargets`s to NEWEPOCH ones.
chainToNewEpochSst ::
  SourceSignalTarget (CHAIN ShortHash) ->
  SourceSignalTarget (NEWEPOCH ShortHash)
chainToNewEpochSst
  ( SourceSignalTarget
      ChainState {chainNes = nes}
      ChainState {chainNes = nes'}
      (Block bh _)
    ) =
    SourceSignalTarget nes nes' (epochFromSlotNo s)
    where
      s = (bheaderSlotNo . bhbody) bh

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
        case runShelleyBase (applySTS @(TICK ShortHash) (TRC (TickEnv (getGKeys nes), nes, (bheaderSlotNo . bhbody) bh))) of
          Left pf ->
            error ("chainSstWithTick.applyTick Predicate failure " <> show pf)
          Right nes' ->
            SourceSignalTarget
              chainSt
              (chainSt {chainNes = nes'})
              b
