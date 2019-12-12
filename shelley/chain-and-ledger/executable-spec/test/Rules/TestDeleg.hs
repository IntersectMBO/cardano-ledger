{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestDeleg
  ( credentialMappingAfterDelegation
  , credentialRemovedAfterDereg
  , rewardZeroAfterReg
  , rewardsSumInvariant
  , instantaneousRewardsAdded
  , instantaneousRewardsValue
  )
where

import           Data.Map (Map)
import qualified Data.Map.Strict as Map (difference, filter, foldl, keysSet, lookup, (\\))
import qualified Data.Maybe as Maybe (maybe)
import           Data.Set (Set)
import qualified Data.Set as Set (isSubsetOf, singleton, size)
import           Data.Word (Word64)
import           Hedgehog (MonadTest, Property, TestLimit, forAll, property, withTests)

import qualified Test.QuickCheck as QC

import           Address (mkRwdAcnt)
import           BaseTypes ((==>))

import           Control.State.Transition.Generator (ofLengthAtLeast, trace)
import           Control.State.Transition.Trace (SourceSignalTarget, pattern SourceSignalTarget,
                     signal, source, sourceSignalTargets, target)
import           Generator.LedgerTrace ()
import           Ledger.Core (dom, range, (∈), (∉), (◁))

import           Coin (Coin, pattern Coin)
import           LedgerState (_delegations, _irwd, _rewards, _stkCreds)
import           MockTypes (DELEG, DState, KeyHash, RewardAcnt, StakeCredential)
import           Test.Utils (assertAll)
import           TxData (pattern DeRegKey, pattern Delegate, pattern Delegation,
                     pattern InstantaneousRewards, pattern RegKey)

-------------------------------
-- helper accessor functions --
-------------------------------

getStDelegs :: DState -> Set StakeCredential
getStDelegs = dom . _stkCreds

getRewards :: DState -> Map RewardAcnt Coin
getRewards = _rewards

getDelegations :: DState -> Map StakeCredential KeyHash
getDelegations = _delegations

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: TestLimit
numberOfTests = 300

traceLen :: Word64
traceLen = 100

--------------------------
-- Properties for DELEG --
--------------------------

-- | Check that a newly registered key has a reward of 0.
rewardZeroAfterReg
  :: MonadTest m
  => [SourceSignalTarget DELEG]
  -> m ()
rewardZeroAfterReg tr =
  assertAll credNewlyRegisteredAndRewardZero tr

  where credNewlyRegisteredAndRewardZero (SourceSignalTarget d d' (RegKey hk)) =
          (hk ∉ getStDelegs d) ==>
          (hk ∈ getStDelegs d'
           && Maybe.maybe True (== 0) (Map.lookup (mkRwdAcnt hk) (getRewards d')))
        credNewlyRegisteredAndRewardZero _ = True

-- | Check that when a stake credential is deregistered, it will not be in the
-- rewards mapping or delegation mapping of the target state.
credentialRemovedAfterDereg
  :: MonadTest m
  => [SourceSignalTarget DELEG]
  -> m ()
credentialRemovedAfterDereg tr = do
  assertAll removedDeregCredential tr

  where removedDeregCredential (SourceSignalTarget
                                 { signal = DeRegKey cred
                                 , target = d'}) =
             cred ∉ getStDelegs d'
          && mkRwdAcnt cred ∉ dom (getRewards d')
          && cred ∉ dom (getDelegations d')
        removedDeregCredential _ = True

-- |Check that a registered stake credential get correctly delegated when
-- applying a delegation certificate.
credentialMappingAfterDelegation :: Property
credentialMappingAfterDelegation = withTests (fromIntegral numberOfTests) . property $ do
  tr <- fmap sourceSignalTargets
     $ forAll
     $ trace @DELEG  traceLen `ofLengthAtLeast` 1
  assertAll delegatedCredential tr

  where delegatedCredential (SourceSignalTarget
                              { signal = Delegate (Delegation cred to)
                              , target = d'}) =
          let credImage = range (Set.singleton cred ◁ getDelegations d') in
             cred ∈ getStDelegs d'
          && to ∈ credImage
          && Set.size credImage == 1
        delegatedCredential _ = True

-- | Check that the sum of rewards does not change and that each element that is
-- either removed or added has a zero balance.
rewardsSumInvariant :: Property
rewardsSumInvariant = withTests (fromIntegral numberOfTests) . property $ do
  tr <- fmap sourceSignalTargets
     $ forAll
     $ trace @DELEG  traceLen `ofLengthAtLeast` 1
  assertAll rewardsSumZeroDiff tr

  where rewardsSumZeroDiff (SourceSignalTarget
                              { source = d
                              , target = d'}) =
          let rew  = _rewards d
              rew' = _rewards d'
              sumRew = foldl (+) (Coin 0)
          in
             -- sum of rewards is not changed
             sumRew rew == sumRew rew'
          && -- dropped elements had a zero reward balance
             null (Map.filter (/= Coin 0) $ rew `Map.difference` rew')
          && -- added elements have a zero reward balance
             null (Map.filter (/= Coin 0) $ rew' `Map.difference` rew)

-- | Check that an accepted MIR certificate adds all entries to the `irwd` mapping
instantaneousRewardsAdded ::  [SourceSignalTarget DELEG] -> QC.Property
instantaneousRewardsAdded ssts =
  QC.conjoin (map checkMIR ssts)
  where
    checkMIR :: SourceSignalTarget DELEG -> QC.Property
    checkMIR (SourceSignalTarget _ t sig) =
      case sig of
        InstantaneousRewards irwd -> QC.property $ Map.keysSet irwd `Set.isSubsetOf` Map.keysSet (_irwd t)
        _                         -> QC.property ()

-- | Check that an accepted MIR certificate adds the overall value in the
-- certificate to the existing value in the `irwd` map, overwriting any entries
-- that already existed.
instantaneousRewardsValue ::  [SourceSignalTarget DELEG] -> QC.Property
instantaneousRewardsValue ssts =
  QC.conjoin (map checkMIR ssts)
  where
    checkMIR :: SourceSignalTarget DELEG -> QC.Property
    checkMIR (SourceSignalTarget s t sig) =
      case sig of
        InstantaneousRewards irwd ->
          QC.property $
          ((Map.foldl (+) (Coin 0) $ _irwd s Map.\\ irwd) +
           (Map.foldl (+) (Coin 0) $ irwd) ==
           (Map.foldl (+) (Coin 0) $ _irwd t))
        _                         -> QC.property ()
