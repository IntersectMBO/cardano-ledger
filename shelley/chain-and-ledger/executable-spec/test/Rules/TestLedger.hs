{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestLedger
  ( rewardZeroAfterRegKey
  , credentialRemovedAfterDereg
  , credentialMappingAfterDelegation
  , rewardsSumInvariant
  , rewardsDecreasesByWithdrawals
  , feesNonDecreasing
  , potsSumIncreaseWdrls
  , preserveBalance
  , preserveBalanceRestricted
  , preserveOutputsTx
  , eliminateTxInputs
  , newEntriesAndUniqueTxIns
  , noDoubleSpend
  , consumedEqualsProduced
  , registeredPoolIsAdded
  , rewardZeroAfterRegPool
  , poolIsMarkedForRetirement
  , poolRetireInEpoch
  , pStateIsInternallyConsistent
  , prop_MIRentriesEndUpInMap
  , prop_MIRValuesEndUpInMap
  , requiredMSigSignaturesSubset
  )
where

import           Data.Foldable (toList)
import           Data.Word (Word64)
import           Lens.Micro ((^.))

import           Test.QuickCheck (Property, Testable, conjoin, property, withMaxSuccess, (===))

import           Control.State.Transition.Trace (SourceSignalTarget (..), Trace, source,
                     sourceSignalTargets, target, traceEnv)
import           Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState)
import           Generator.LedgerTrace.QuickCheck (mkGenesisLedgerState)

import           Coin (pattern Coin)
import           ConcreteCryptoTypes (DELEG, DELEGS, LEDGER, POOL, StakeCreds, StakePools, UTXO,
                     UTXOW, Wdrl)
import           LedgerState (pattern DPState, pattern DState, pattern UTxOState, dstate, pstate,
                     stPools, stkCreds, _deposited, _dstate, _fees, _rewards, _utxo)
import qualified Rules.TestDeleg as TestDeleg
import qualified Rules.TestDelegs as TestDelegs
import qualified Rules.TestPool as TestPool
import qualified Rules.TestUtxo as TestUtxo
import qualified Rules.TestUtxow as TestUtxow
import           STS.Ledger (LedgerEnv (ledgerPp))
import           Tx (body)
import           TxData (certs, wdrls)
import           UTxO (balance)

import           Test.Utils (testGlobals)

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Word64
numberOfTests = 300

traceLen :: Word64
traceLen = 100

----------------------------------------------------------------------
-- Properties for Delegations (using the LEDGER Trace) --
----------------------------------------------------------------------

-- | Check that a newly registered key has a reward of 0.
rewardZeroAfterRegKey :: Property
rewardZeroAfterRegKey =
  forAllLedgerTrace $ \tr ->
    let sst = concatMap ledgerToDelegSsts (sourceSignalTargets tr)
    in TestDeleg.rewardZeroAfterReg sst

credentialRemovedAfterDereg :: Property
credentialRemovedAfterDereg =
  forAllLedgerTrace $ \tr ->
    let sst = concatMap ledgerToDelegSsts (sourceSignalTargets tr)
    in TestDeleg.credentialRemovedAfterDereg sst

credentialMappingAfterDelegation :: Property
credentialMappingAfterDelegation =
  forAllLedgerTrace $ \tr ->
    let sst = concatMap ledgerToDelegSsts (sourceSignalTargets tr)
    in TestDeleg.credentialMappingAfterDelegation sst

rewardsSumInvariant :: Property
rewardsSumInvariant =
  forAllLedgerTrace $ \tr ->
    let sst = concatMap ledgerToDelegSsts (sourceSignalTargets tr)
    in TestDeleg.rewardsSumInvariant sst

rewardsDecreasesByWithdrawals :: Property
rewardsDecreasesByWithdrawals =
  forAllLedgerTrace $ \tr ->
    let sst = map ledgerToDelegsSsts (sourceSignalTargets tr)
    in TestDelegs.rewardsDecreasesByWithdrawals sst

----------------------------------------------------------------------
-- Properties for Utxo (using the LEDGER Trace) --
----------------------------------------------------------------------

-- | Check that the value consumed by UTXO is equal to the value produced in
-- DELEGS
consumedEqualsProduced :: Property
consumedEqualsProduced =
  forAllLedgerTrace $ \tr ->
    conjoin $
      map consumedSameAsGained (sourceSignalTargets tr)

  where consumedSameAsGained SourceSignalTarget
                               { source = (UTxOState
                                           { _utxo = u
                                           , _deposited = d
                                           , _fees = fees
                                           }
                                          , DPState
                                            { _dstate = DState { _rewards = rewards }
                                            }
                                          )
                               , target = (UTxOState
                                            { _utxo = u'
                                            , _deposited = d'
                                            , _fees = fees'
                                            }
                                         , DPState
                                           { _dstate = DState { _rewards = rewards' }})} =

          (balance u  + d  + fees  + foldl (+) (Coin 0) rewards ) ===
          (balance u' + d' + fees' + foldl (+) (Coin 0) rewards')

feesNonDecreasing :: Property
feesNonDecreasing =
  forAllLedgerTrace $ \tr ->
    let ssts = map ledgerToUtxoSsts (sourceSignalTargets tr)
    in TestUtxo.feesNonDecreasing ssts

potsSumIncreaseWdrls :: Property
potsSumIncreaseWdrls =
  forAllLedgerTrace $ \tr ->
    let ssts = map ledgerToUtxoSsts (sourceSignalTargets tr)
    in TestUtxo.potsSumIncreaseWdrls ssts

preserveBalance :: Property
preserveBalance =
  forAllLedgerTrace $ \tr ->
    let ssts = map ledgerToUtxowSsts (sourceSignalTargets tr)
        pp = ledgerPp (tr ^. traceEnv)

    in TestUtxow.preserveBalance pp ssts

preserveBalanceRestricted :: Property
preserveBalanceRestricted =
  forAllLedgerTrace $ \tr ->
    let ssts = map ledgerToUtxowSsts (sourceSignalTargets tr)
        pp = ledgerPp (tr ^. traceEnv)

    in TestUtxow.preserveBalanceRestricted pp ssts

preserveOutputsTx :: Property
preserveOutputsTx =
  forAllLedgerTrace $ \tr ->
    let ssts = map ledgerToUtxoSsts (sourceSignalTargets tr)
    in TestUtxow.preserveOutputsTx ssts

eliminateTxInputs :: Property
eliminateTxInputs =
  forAllLedgerTrace $ \tr ->
    let ssts = map ledgerToUtxoSsts (sourceSignalTargets tr)
    in TestUtxow.eliminateTxInputs ssts

newEntriesAndUniqueTxIns :: Property
newEntriesAndUniqueTxIns =
  forAllLedgerTrace $ \tr ->
    let ssts = map ledgerToUtxoSsts (sourceSignalTargets tr)
    in TestUtxow.newEntriesAndUniqueTxIns ssts

noDoubleSpend :: Property
noDoubleSpend =
  forAllLedgerTrace $ \tr ->
    let ssts = map ledgerToUtxoSsts (sourceSignalTargets tr)
    in TestUtxow.noDoubleSpend ssts

requiredMSigSignaturesSubset :: Property
requiredMSigSignaturesSubset =
  forAllLedgerTrace $ \tr ->
  let ssts = map (\(_, _, s) -> s) $ map ledgerToUtxowSsts (sourceSignalTargets tr)
  in  TestUtxow.requiredMSigSignaturesSubset ssts

----------------------------------------------------------------------
-- Properties for Pool (using the LEDGER Trace) --
----------------------------------------------------------------------

-- | Check that a `RegPool` certificate properly adds a stake pool.
registeredPoolIsAdded :: Property
registeredPoolIsAdded =
  forAllLedgerTrace $ \tr ->
    let sst = concatMap ledgerToPoolSsts (sourceSignalTargets tr)
    in TestPool.registeredPoolIsAdded (tr ^. traceEnv) sst

-- | Check that a newly registered pool has a reward of 0.
rewardZeroAfterRegPool :: Property
rewardZeroAfterRegPool =
  forAllLedgerTrace $ \tr ->
    let sst = concatMap ledgerToPoolSsts (sourceSignalTargets tr)
    in TestPool.rewardZeroAfterReg sst

poolRetireInEpoch :: Property
poolRetireInEpoch =
  forAllLedgerTrace $ \tr ->
    let sst = concatMap ledgerToPoolSsts (sourceSignalTargets tr)
    in TestPool.poolRetireInEpoch (tr ^. traceEnv) sst

-- | Check that a `RetirePool` certificate properly removes a stake pool.
poolIsMarkedForRetirement :: Property
poolIsMarkedForRetirement =
  forAllLedgerTrace $ \tr ->
    let sst = concatMap ledgerToPoolSsts (sourceSignalTargets tr)
    in TestPool.poolIsMarkedForRetirement sst

-- | Check that `InstantaneousRewards` certificate entries are added to the
-- Instantaneous Rewards map.
prop_MIRentriesEndUpInMap :: Property
prop_MIRentriesEndUpInMap =
  forAllLedgerTrace $ \tr ->
    let sst = concatMap ledgerToDelegSsts (sourceSignalTargets tr)
    in  TestDeleg.instantaneousRewardsAdded sst

-- | Check that the coin values in `InstantaneousRewards` certificate entries
-- are added to the Instantaneous Rewards map.
prop_MIRValuesEndUpInMap :: Property
prop_MIRValuesEndUpInMap =
  forAllLedgerTrace $ \tr ->
    let sst = concatMap ledgerToDelegSsts (sourceSignalTargets tr)
    in  TestDeleg.instantaneousRewardsValue sst

pStateIsInternallyConsistent :: Property
pStateIsInternallyConsistent =
  forAllLedgerTrace $ \tr ->
    let sst = concatMap ledgerToPoolSsts (sourceSignalTargets tr)
    in TestPool.pStateIsInternallyConsistent sst

---------------------------
-- Utils --
---------------------------

forAllLedgerTrace
  :: (Testable prop)
  => (Trace LEDGER -> prop)
  -> Property
forAllLedgerTrace prop =
  withMaxSuccess (fromIntegral numberOfTests) . property $
    forAllTraceFromInitState testGlobals traceLen traceLen (Just mkGenesisLedgerState) prop

-- | Transform LEDGER `sourceSignalTargets`s to DELEG ones.
ledgerToDelegSsts
  :: SourceSignalTarget LEDGER
  -> [SourceSignalTarget DELEG]
ledgerToDelegSsts (SourceSignalTarget (_, DPState d _) (_, DPState d' _) tx) =
  [SourceSignalTarget d d' cert | cert <- toList (tx ^. body . certs)]

-- | Transform LEDGER `sourceSignalTargets`s to DELEGS ones.
ledgerToDelegsSsts
  :: SourceSignalTarget LEDGER
  -> (Wdrl, SourceSignalTarget DELEGS)
ledgerToDelegsSsts (SourceSignalTarget (_, dpSt) (_, dpSt') tx) =
  ( tx ^. body . wdrls
  , SourceSignalTarget dpSt dpSt' (tx ^. body . certs))

-- | Transform LEDGER `SourceSignalTargets`s to POOL ones.
ledgerToPoolSsts
  :: SourceSignalTarget LEDGER
  -> [SourceSignalTarget POOL]
ledgerToPoolSsts (SourceSignalTarget (_, DPState _ p) (_, DPState _ p') tx) =
  [SourceSignalTarget p p' cert | cert <- toList (tx ^. body . certs)]

-- | Transform LEDGER to UTXO `SourceSignalTargets`s
ledgerToUtxoSsts
  :: SourceSignalTarget LEDGER
  -> SourceSignalTarget UTXO
ledgerToUtxoSsts (SourceSignalTarget (utxoSt, _) (utxoSt', _) tx) =
  (SourceSignalTarget utxoSt utxoSt' tx)

-- | Transform LEDGER to UTXOW `SourceSignalTargets`s
ledgerToUtxowSsts
  :: SourceSignalTarget LEDGER
  -> (StakeCreds, StakePools, SourceSignalTarget UTXOW)
ledgerToUtxowSsts (SourceSignalTarget (utxoSt, delegSt) (utxoSt', _) tx) =
  ( delegSt ^. dstate . stkCreds
  , delegSt ^. pstate . stPools
  , SourceSignalTarget utxoSt utxoSt' tx)
