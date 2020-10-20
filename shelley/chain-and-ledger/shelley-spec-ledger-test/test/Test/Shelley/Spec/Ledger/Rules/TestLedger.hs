{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}

module Test.Shelley.Spec.Ledger.Rules.TestLedger
  ( rewardZeroAfterRegKey,
    credentialRemovedAfterDereg,
    credentialMappingAfterDelegation,
    rewardsSumInvariant,
    rewardsDecreasesByWithdrawals,
    consumedEqualsProduced,
    prop_MIRentriesEndUpInMap,
    prop_MIRValuesEndUpInMap
  )
where

import qualified Cardano.Ledger.Val as Val
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
import Data.Foldable (fold, toList)
import Data.Proxy
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word64)
import Shelley.Spec.Ledger.API
  ( DELEG,
    DELEGS,
    LEDGER,
  )
import Shelley.Spec.Ledger.LedgerState
  ( _deposited,
    _dstate,
    _fees,
    _rewards,
    _utxo,
    pattern DPState,
    pattern DState,
    pattern UTxOState,
  )
import Shelley.Spec.Ledger.STS.Deleg (DelegEnv (..))
import Shelley.Spec.Ledger.STS.Ledger (LedgerEnv (..))
import Shelley.Spec.Ledger.Tx (_body)
import Shelley.Spec.Ledger.TxBody
  ( Ptr (..),
    Wdrl,
    _certs,
    _wdrls,
  )
import Shelley.Spec.Ledger.UTxO (balance)
import Test.QuickCheck (Property, Testable, conjoin, property, withMaxSuccess, (===))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( C,
  )
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (geConstants))
import qualified Test.Shelley.Spec.Ledger.Generator.Presets as Preset (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Trace.Ledger (mkGenesisLedgerState)
import qualified Test.Shelley.Spec.Ledger.Rules.TestDeleg as TestDeleg
import qualified Test.Shelley.Spec.Ledger.Rules.TestDelegs as TestDelegs
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
          -- this equality still holds as is in Shelley tests as no forging is possible
        (balance u <> (Val.inject $ d <> fees <> fold rewards))
          === (balance u' <> (Val.inject $ d' <> fees' <> fold rewards'))

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
  (Trace (LEDGER C) -> prop) ->
  Property
forAllLedgerTrace prop =
  withMaxSuccess (fromIntegral numberOfTests) . property $
    forAllTraceFromInitState testGlobals traceLen (Preset.genEnv p) (Just $ mkGenesisLedgerState (geConstants (Preset.genEnv p))) prop
  where
    p :: Proxy C
    p = Proxy

-- | Transform LEDGER `sourceSignalTargets`s to DELEGS ones.
ledgerToDelegsSsts ::
  SourceSignalTarget (LEDGER C) ->
  (Wdrl C, SourceSignalTarget (DELEGS C))
ledgerToDelegsSsts (SourceSignalTarget (_, dpSt) (_, dpSt') tx) =
  ( (_wdrls . _body) tx,
    SourceSignalTarget dpSt dpSt' ((StrictSeq.getSeq . _certs . _body) tx)
  )

-- | Transform a LEDGER Trace to a DELEG Trace by extracting the certificates
-- from the LEDGER transactions and then reconstructing a new DELEG trace from
-- those certificates.
ledgerToDelegTrace :: Trace (LEDGER C) -> Trace (DELEG C)
ledgerToDelegTrace ledgerTr =
  runShelleyBase $
    Trace.closure @(DELEG C) delegEnv delegSt0 (certs txs)
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
