{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestDeleg
  ( credentialMappingAfterDelegation
  , credentialRemovedAfterDereg
  , rewardZeroAfterReg
  )
where

import           Data.Map (Map)
import qualified Data.Map.Strict as Map (lookup)
import qualified Data.Maybe as Maybe (maybe)
import           Data.Set (Set)
import qualified Data.Set as Set (singleton, size)
import           Data.Word (Word64)
import           Hedgehog (MonadTest, Property, TestLimit, forAll, property, withTests)

import           Address (mkRwdAcnt)
import           BaseTypes ((==>))
import           Coin (Coin)
import           Control.State.Transition.Generator (ofLengthAtLeast, trace)
import           Control.State.Transition.Trace (SourceSignalTarget, pattern SourceSignalTarget,
                     signal, sourceSignalTargets, target)
import           Generator.LedgerTrace ()
import           Ledger.Core (dom, range, (∈), (∉), (◁))

import           LedgerState (_delegations, _rewards, _stKeys)
import           MockTypes (DELEG, DState, KeyHash, RewardAcnt, StakeCredential)
import           Test.Utils (assertAll)
import           TxData (pattern DeRegKey, pattern Delegate, pattern Delegation, pattern RegKey)

-------------------------------
-- helper accessor functions --
-------------------------------

getStDelegs :: DState -> Set StakeCredential
getStDelegs = dom . _stKeys

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
credentialRemovedAfterDereg :: Property
credentialRemovedAfterDereg = withTests numberOfTests . property $ do
  tr <- fmap sourceSignalTargets
      $ forAll
      $ trace @DELEG traceLen `ofLengthAtLeast` 1
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
