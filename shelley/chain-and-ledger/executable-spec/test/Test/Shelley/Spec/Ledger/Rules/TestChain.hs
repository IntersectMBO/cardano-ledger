{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.Rules.TestChain
  ( -- TestPoolReap
    constantSumPots
  , nonNegativeDeposits
  , removedAfterPoolreap
    -- TestNewEpoch
  , preservationOfAda)
where

import           Data.Word (Word64)
import           Test.QuickCheck (Property, Testable, property, withMaxSuccess)

import           Control.State.Transition.Extended (TRC (TRC), applySTS)
import           Control.State.Transition.Trace (SourceSignalTarget (..), Trace (..),
                     sourceSignalTargets)
import           Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState)

import           Shelley.Spec.Ledger.BlockChain (Block (..), bhbody, bheaderSlotNo)
import           Shelley.Spec.Ledger.LedgerState (pattern DPState, esAccountState, esLState,
                     getGKeys, nesEs, _delegationState, _utxoState)
import           Shelley.Spec.Ledger.STS.Chain (ChainState (..))
import           Shelley.Spec.Ledger.STS.PoolReap (PoolreapState (..))
import           Shelley.Spec.Ledger.STS.Tick (TickEnv (TickEnv))
import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (CHAIN, NEWEPOCH, POOLREAP, TICK)
import           Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (geConstants))
import qualified Test.Shelley.Spec.Ledger.Generator.Presets as Preset (genEnv)
import           Test.Shelley.Spec.Ledger.Generator.Trace.Chain (mkGenesisChainState)
import qualified Test.Shelley.Spec.Ledger.Rules.TestNewEpoch as TestNewEpoch
import qualified Test.Shelley.Spec.Ledger.Rules.TestPoolreap as TestPoolreap
import           Test.Shelley.Spec.Ledger.Utils (epochFromSlotNo, runShelleyBase, testGlobals)
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

forAllChainTrace
  :: (Testable prop)
  => (Trace CHAIN -> prop)
  -> Property
forAllChainTrace prop =
  withMaxSuccess (fromIntegral numberOfTests) . property $
    forAllTraceFromInitState testGlobals traceLen Preset.genEnv (Just $ mkGenesisChainState (geConstants Preset.genEnv)) prop

-- | Transform CHAIN `sourceSignalTargets`s to POOLREAP ones.
chainToPoolreapSst
  :: SourceSignalTarget CHAIN
  -> SourceSignalTarget POOLREAP
chainToPoolreapSst (SourceSignalTarget ChainState {chainNes = nes}
                                       ChainState  {chainNes = nes'}
                                       (Block bh _)) =
  SourceSignalTarget (PoolreapState (utxoSt nes)
                                    (accountSt nes)
                                    dstate
                                    pstate)
                     (PoolreapState (utxoSt nes')
                                    (accountSt nes')
                                    dstate'
                                    pstate')
                     (epochFromSlotNo s)
  where
    s = (bheaderSlotNo . bhbody) bh

    DPState dstate pstate = (_delegationState . esLState . nesEs) nes
    DPState dstate' pstate' = (_delegationState . esLState . nesEs) nes'

    utxoSt = _utxoState . esLState . nesEs
    accountSt = esAccountState . nesEs

-- | Transform CHAIN `sourceSignalTargets`s to NEWEPOCH ones.
chainToNewEpochSst
  :: SourceSignalTarget CHAIN
  -> SourceSignalTarget NEWEPOCH
chainToNewEpochSst (SourceSignalTarget ChainState {chainNes = nes}
                                       ChainState {chainNes = nes'}
                                       (Block bh _)) =
  SourceSignalTarget nes nes' (epochFromSlotNo s)
  where
    s = (bheaderSlotNo . bhbody) bh

-- | Transform the [(source, signal, target)] of a CHAIN Trace
-- by manually applying the Chain TICK Rule to each source and producing
-- [(source, signal, target')].
--
-- This allows for testing properties on traces that exclude effects of the
-- "UTXO branches" of the STS Rule tree.
chainSstWithTick :: Trace CHAIN -> [SourceSignalTarget CHAIN]
chainSstWithTick ledgerTr =
  map applyTick ssts
  where
    ssts = sourceSignalTargets ledgerTr

    applyTick
      :: SourceSignalTarget CHAIN
      -> SourceSignalTarget CHAIN
    applyTick (SourceSignalTarget
                chainSt@ChainState {chainNes = nes}
                _
                b@(Block bh _)) =
      case runShelleyBase (applySTS @TICK (TRC(TickEnv (getGKeys nes), nes, (bheaderSlotNo . bhbody) bh))) of
        Left pf ->
          error ("chainSstWithTick.applyTick Predicate failure " <> show pf)
        Right nes' ->
          SourceSignalTarget
            chainSt
            (chainSt {chainNes = nes'})
            b
