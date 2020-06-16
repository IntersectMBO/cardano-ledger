{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Shelley.Spec.Ledger.Rules.TestLedger
  ( rewardZeroAfterRegKey,
    credentialRemovedAfterDereg,
    credentialMappingAfterDelegation,
    rewardsSumInvariant,
    rewardsDecreasesByWithdrawals,
    feesNonDecreasing,
    potsSumIncreaseWdrls,
    preserveBalance,
    preserveBalanceRestricted,
    preserveOutputsTx,
    eliminateTxInputs,
    newEntriesAndUniqueTxIns,
    noDoubleSpend,
    consumedEqualsProduced,
    registeredPoolIsAdded,
    rewardZeroAfterRegPool,
    poolIsMarkedForRetirement,
    poolRetireInEpoch,
    pStateIsInternallyConsistent,
    prop_MIRentriesEndUpInMap,
    prop_MIRValuesEndUpInMap,
    requiredMSigSignaturesSubset,
  )
where

import Cardano.Crypto.Hash (ShortHash)
import Control.State.Transition.Trace
  ( SourceSignalTarget (..),
    Trace (..),
    TraceOrder (NewestFirst),
    source,
    sourceSignalTargets,
    target,
    traceSignals,
  )
import qualified Control.State.Transition.Trace as Trace
import Control.State.Transition.Trace.Generator.QuickCheck (forAllTraceFromInitState)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word64)
import Shelley.Spec.Ledger.Coin (pattern Coin)
import Shelley.Spec.Ledger.LedgerState
  ( _deposited,
    _dstate,
    _fees,
    _pstate,
    _rewards,
    _stPools,
    _utxo,
    pattern DPState,
    pattern DState,
    pattern UTxOState,
  )
import Shelley.Spec.Ledger.STS.Deleg (DelegEnv (..))
import Shelley.Spec.Ledger.STS.Ledger (LedgerEnv (..))
import Shelley.Spec.Ledger.STS.Pool (PoolEnv (..))
import Shelley.Spec.Ledger.STS.Pool ()
import Shelley.Spec.Ledger.Tx (_body)
import Shelley.Spec.Ledger.TxData (Ptr (..), _certs, _wdrls)
import Shelley.Spec.Ledger.UTxO (balance)
import Test.QuickCheck ((===), Property, Testable, conjoin, property, withMaxSuccess)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( DELEG,
    DELEGS,
    LEDGER,
    POOL,
    StakePools,
    UTXO,
    UTXOW,
    Wdrl,
  )
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (geConstants))
import qualified Test.Shelley.Spec.Ledger.Generator.Presets as Preset (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Trace.Ledger (mkGenesisLedgerState)
import qualified Test.Shelley.Spec.Ledger.Rules.TestDeleg as TestDeleg
import qualified Test.Shelley.Spec.Ledger.Rules.TestDelegs as TestDelegs
import qualified Test.Shelley.Spec.Ledger.Rules.TestPool as TestPool
import qualified Test.Shelley.Spec.Ledger.Rules.TestUtxo as TestUtxo
import qualified Test.Shelley.Spec.Ledger.Rules.TestUtxow as TestUtxow
import Test.Shelley.Spec.Ledger.Utils (runShelleyBase, testGlobals)

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
    let sst = sourceSignalTargets (ledgerToDelegTrace tr)
     in TestDeleg.rewardZeroAfterReg sst

credentialRemovedAfterDereg :: Property
credentialRemovedAfterDereg =
  forAllLedgerTrace $ \tr ->
    let sst = sourceSignalTargets (ledgerToDelegTrace tr)
     in TestDeleg.credentialRemovedAfterDereg sst

credentialMappingAfterDelegation :: Property
credentialMappingAfterDelegation =
  forAllLedgerTrace $ \tr ->
    let sst = sourceSignalTargets (ledgerToDelegTrace tr)
     in TestDeleg.credentialMappingAfterDelegation sst

rewardsSumInvariant :: Property
rewardsSumInvariant =
  forAllLedgerTrace $ \tr ->
    let sst = sourceSignalTargets (ledgerToDelegTrace tr)
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
  where
    consumedSameAsGained
      SourceSignalTarget
        { source =
            ( UTxOState
                { _utxo = u,
                  _deposited = d,
                  _fees = fees
                },
              DPState
                { _dstate = DState {_rewards = rewards}
                }
              ),
          target =
            ( UTxOState
                { _utxo = u',
                  _deposited = d',
                  _fees = fees'
                },
              DPState
                { _dstate = DState {_rewards = rewards'}
                }
              )
        } =
        (balance u + d + fees + foldl' (+) (Coin 0) rewards)
          === (balance u' + d' + fees' + foldl' (+) (Coin 0) rewards')

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
        pp = ledgerPp (_traceEnv tr)
     in TestUtxow.preserveBalance pp ssts

preserveBalanceRestricted :: Property
preserveBalanceRestricted =
  forAllLedgerTrace $ \tr ->
    let ssts = map ledgerToUtxowSsts (sourceSignalTargets tr)
        pp = ledgerPp (_traceEnv tr)
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
    let ssts = map (\(_, s) -> s) $ map ledgerToUtxowSsts (sourceSignalTargets tr)
     in TestUtxow.requiredMSigSignaturesSubset ssts

----------------------------------------------------------------------
-- Properties for Pool (using the LEDGER Trace) --
----------------------------------------------------------------------

-- | Check that a `RegPool` certificate properly adds a stake pool.
registeredPoolIsAdded :: Property
registeredPoolIsAdded =
  forAllLedgerTrace $ \tr ->
    let sst = sourceSignalTargets (ledgerToPoolTrace tr)
     in TestPool.registeredPoolIsAdded (_traceEnv tr) sst

-- | Check that a newly registered pool has a reward of 0.
rewardZeroAfterRegPool :: Property
rewardZeroAfterRegPool =
  forAllLedgerTrace $ \tr ->
    let sst = sourceSignalTargets (ledgerToPoolTrace tr)
     in TestPool.rewardZeroAfterReg sst

poolRetireInEpoch :: Property
poolRetireInEpoch =
  forAllLedgerTrace $ \tr ->
    let sst = sourceSignalTargets (ledgerToPoolTrace tr)
     in TestPool.poolRetireInEpoch (_traceEnv tr) sst

-- | Check that a `RetirePool` certificate properly removes a stake pool.
poolIsMarkedForRetirement :: Property
poolIsMarkedForRetirement =
  forAllLedgerTrace $ \tr ->
    let sst = sourceSignalTargets (ledgerToPoolTrace tr)
     in TestPool.poolIsMarkedForRetirement sst

pStateIsInternallyConsistent :: Property
pStateIsInternallyConsistent =
  forAllLedgerTrace $ \tr ->
    let sst = sourceSignalTargets (ledgerToPoolTrace tr)
     in TestPool.pStateIsInternallyConsistent sst

-- | Check that `InstantaneousRewards` certificate entries are added to the
-- Instantaneous Rewards map.
prop_MIRentriesEndUpInMap :: Property
prop_MIRentriesEndUpInMap =
  forAllLedgerTrace $ \tr ->
    let sst = sourceSignalTargets (ledgerToDelegTrace tr)
     in TestDeleg.instantaneousRewardsAdded sst

-- | Check that the coin values in `InstantaneousRewards` certificate entries
-- are added to the Instantaneous Rewards map.
prop_MIRValuesEndUpInMap :: Property
prop_MIRValuesEndUpInMap =
  forAllLedgerTrace $ \tr ->
    let sst = sourceSignalTargets (ledgerToDelegTrace tr)
     in TestDeleg.instantaneousRewardsValue sst

---------------------------
-- Utils --
---------------------------

forAllLedgerTrace ::
  (Testable prop) =>
  (Trace (LEDGER ShortHash) -> prop) ->
  Property
forAllLedgerTrace prop =
  withMaxSuccess (fromIntegral numberOfTests) . property $
    forAllTraceFromInitState testGlobals traceLen (Preset.genEnv p) (Just $ mkGenesisLedgerState (geConstants (Preset.genEnv p))) prop
  where
    p :: Proxy ShortHash
    p = Proxy

-- | Transform LEDGER `sourceSignalTargets`s to DELEGS ones.
ledgerToDelegsSsts ::
  SourceSignalTarget (LEDGER ShortHash) ->
  (Wdrl ShortHash, SourceSignalTarget (DELEGS ShortHash))
ledgerToDelegsSsts (SourceSignalTarget (_, dpSt) (_, dpSt') tx) =
  ( (_wdrls . _body) tx,
    SourceSignalTarget dpSt dpSt' ((StrictSeq.getSeq . _certs . _body) tx)
  )

-- | Transform LEDGER to UTXO `SourceSignalTargets`s
ledgerToUtxoSsts ::
  SourceSignalTarget (LEDGER ShortHash) ->
  SourceSignalTarget (UTXO ShortHash)
ledgerToUtxoSsts (SourceSignalTarget (utxoSt, _) (utxoSt', _) tx) =
  (SourceSignalTarget utxoSt utxoSt' tx)

-- | Transform LEDGER to UTXOW `SourceSignalTargets`s
ledgerToUtxowSsts ::
  SourceSignalTarget (LEDGER ShortHash) ->
  (StakePools ShortHash, SourceSignalTarget (UTXOW ShortHash))
ledgerToUtxowSsts (SourceSignalTarget (utxoSt, delegSt) (utxoSt', _) tx) =
  ( (_stPools . _pstate) delegSt,
    SourceSignalTarget utxoSt utxoSt' tx
  )

-- | Transform a LEDGER Trace to a POOL Trace by extracting the certificates
-- from the LEDGER transactions and then reconstructing a new POOL trace from
-- those certificates.
ledgerToPoolTrace :: Trace (LEDGER ShortHash) -> Trace (POOL ShortHash)
ledgerToPoolTrace ledgerTr =
  runShelleyBase $
    Trace.closure @(POOL ShortHash) poolEnv poolSt0 (certs txs)
  where
    txs = traceSignals NewestFirst ledgerTr
    certs = concatMap (toList . _certs . _body)
    poolEnv =
      let (LedgerEnv s _ pp _) = _traceEnv ledgerTr
       in PoolEnv s pp
    poolSt0 =
      let (_, DPState _ poolSt0_) = _traceInitState ledgerTr
       in poolSt0_

-- | Transform a LEDGER Trace to a DELEG Trace by extracting the certificates
-- from the LEDGER transactions and then reconstructing a new DELEG trace from
-- those certificates.
ledgerToDelegTrace :: Trace (LEDGER ShortHash) -> Trace (DELEG ShortHash)
ledgerToDelegTrace ledgerTr =
  runShelleyBase $
    Trace.closure @(DELEG ShortHash) delegEnv delegSt0 (certs txs)
  where
    txs = traceSignals NewestFirst ledgerTr
    certs = concatMap (toList . _certs . _body)
    delegEnv =
      let (LedgerEnv s txIx _ reserves) = _traceEnv ledgerTr
          dummyCertIx = 0
          ptr = Ptr s txIx dummyCertIx
       in DelegEnv s ptr reserves
    delegSt0 =
      let (_, DPState delegSt0_ _) = _traceInitState ledgerTr
       in delegSt0_
