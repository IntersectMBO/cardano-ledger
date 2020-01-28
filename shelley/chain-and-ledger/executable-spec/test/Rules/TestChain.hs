{-# LANGUAGE PatternSynonyms #-}

module Rules.TestChain
  ( -- TestPoolReap
    constantSumPots
  , nonNegativeDeposits
  , removedAfterPoolreap
    -- TestNewEpoch
  , circulationDepositsInvariant
  , rewardDecreaseEqualsTreasuryRewardPot)
where

import           Control.Monad.Trans.Reader (asks)
import           Data.Word (Word64)
import           Test.QuickCheck (Property, Testable, property, withMaxSuccess)

import           BaseTypes (epochInfo)
import           BlockChain (Block (..), bhbody, bheaderSlotNo)
import           ConcreteCryptoTypes (CHAIN, NEWEPOCH, POOLREAP)
import           Control.State.Transition.Trace (SourceSignalTarget (..), Trace,
                     sourceSignalTargets)
import           Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState)
import           Generator.ChainTrace (mkGenesisChainState)
import           LedgerState (pattern DPState, esAccountState, esLState, nesEs, _delegationState,
                     _utxoState)
import qualified Rules.TestNewEpoch as TestNewEpoch
import qualified Rules.TestPoolreap as TestPoolreap
import           Slot (EpochNo, SlotNo, epochInfoEpoch)
import           STS.Chain (ChainState (..))
import           STS.PoolReap (PoolreapState (..))

import           Test.Utils (runShelleyBase, testGlobals)


------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Word64
numberOfTests = 300

traceLen :: Word64
traceLen = 10 -- TODO @uroboros use short traces while we implement proper KES block signatures

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
    let sst = map chainToPoolreapSst (sourceSignalTargets tr)
    in TestPoolreap.removedAfterPoolreap sst

----------------------------------------------------------------------
-- Properties for NewEpoch (using the CHAIN Trace) --
----------------------------------------------------------------------

rewardDecreaseEqualsTreasuryRewardPot :: Property
rewardDecreaseEqualsTreasuryRewardPot =
  forAllChainTrace $ \tr ->
    let sst = map chainToNewEpochSst (sourceSignalTargets tr)
    in TestNewEpoch.rewardDecreaseEqualsTreasuryRewardPot sst

circulationDepositsInvariant :: Property
circulationDepositsInvariant =
  forAllChainTrace $ \tr ->
    let sst = map chainToNewEpochSst (sourceSignalTargets tr)
    in TestNewEpoch.rewardDecreaseEqualsTreasuryRewardPot sst

---------------------------
-- Utils --
---------------------------

forAllChainTrace
  :: (Testable prop)
  => (Trace CHAIN -> prop)
  -> Property
forAllChainTrace prop =
  withMaxSuccess (fromIntegral numberOfTests) . property $
    forAllTraceFromInitState testGlobals traceLen traceLen (Just mkGenesisChainState) prop

epochFromSlot :: SlotNo -> EpochNo
epochFromSlot s =
  runShelleyBase $ do
    ei <- asks epochInfo
    epochInfoEpoch ei s

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
                     (epochFromSlot s)
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
  SourceSignalTarget nes nes' (epochFromSlot s)
  where
    s = (bheaderSlotNo . bhbody) bh

