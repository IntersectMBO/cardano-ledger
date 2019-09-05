{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestDeleg where

import           Control.Monad (when)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map (lookup)
import qualified Data.Maybe as Maybe (maybe)
import           Data.Set (Set)
import qualified Data.Set as Set (singleton, size)

import           Hedgehog (Property, forAll, property, withTests, (===))

import           Control.State.Transition.Generator (trace)
import           Control.State.Transition.Trace (sourceSignalTargets, traceLength)

import           Address (mkRwdAcnt)
import           BaseTypes ((==>))
import           Coin (Coin)
import           LedgerState (_delegations, _rewards, _stKeys)
import           MockTypes (DELEG, DState, KeyHash, RewardAcnt, StakeCredential)
import           TxData (pattern DeRegKey, pattern Delegate, pattern Delegation, pattern RegKey)

import           Ledger.Core (dom, range, (∈), (∉), (◁))

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

numberOfTests :: Int
numberOfTests = 300

traceLen :: Int
traceLen = 100

--------------------------
-- Properties for DELEG --
--------------------------

-- | Check that a newly registered key has a reward of 0.
rewardZeroAfterReg :: Property
rewardZeroAfterReg = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll (trace @DELEG $ fromIntegral traceLen)
  let
    n :: Integer
    n = fromIntegral $ traceLength t
    tr = sourceSignalTargets t

  when (n > 1) $
    [] === filter (not . credNewlyRegisteredAndRewardZero) tr

  where credNewlyRegisteredAndRewardZero (d, RegKey hk, d') =
          (hk ∉ getStDelegs d) ==>
          (   hk ∈ getStDelegs d'
           && Maybe.maybe True (== 0) (Map.lookup (mkRwdAcnt hk) (getRewards d')))
        credNewlyRegisteredAndRewardZero (_, _, _) = True

-- | Check that when a stake credential is deregistered, it will not be in the
-- rewards mapping or delegation mapping of the target state.
credentialRemovedAfterDereg :: Property
credentialRemovedAfterDereg = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll (trace @DELEG $ fromIntegral traceLen)
  let
    n :: Integer
    n = fromIntegral $ traceLength t
    tr = sourceSignalTargets t

  when (n > 1) $
    [] === filter (not . removedDeregCredential) tr

  where removedDeregCredential (_, DeRegKey cred, d') =
             cred ∉ getStDelegs d'
          && mkRwdAcnt cred ∉ dom (getRewards d')
          && cred ∉ dom (getDelegations d')
        removedDeregCredential (_, _, _) = True

-- |Check that a registered stake credential get correctly delegated when
-- applying a delegation certificate.
credentialMappingAfterDelegation :: Property
credentialMappingAfterDelegation = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll (trace @DELEG $ fromIntegral traceLen)
  let
    n :: Integer
    n = fromIntegral $ traceLength t
    tr = sourceSignalTargets t

  when (n > 1) $
    [] === filter (not . delegatedCredential) tr

  where delegatedCredential (_, Delegate (Delegation cred to), d') =
          let credImage = range (Set.singleton cred ◁ getDelegations d') in
             cred ∈ getStDelegs d'
          && to ∈ credImage
          && Set.size credImage == 1
        delegatedCredential (_, _, _) = True
