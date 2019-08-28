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

import           Hedgehog (Property, forAll, property, withTests, (===))

import           Control.State.Transition.Generator (trace)
import           Control.State.Transition.Trace (sourceSignalTargets, traceLength)

import           Address (mkRwdAcnt)
import           Coin (Coin)
import           LedgerState (_delegationState, _delegations, _dstate, _pstate, _retiring, _rewards,
                     _stKeys, _stPools)
import           MockTypes (DELEG, DState, KeyHash, LedgerState, RewardAcnt, StakeCredential,
                     StakePools)
import           Slot (Epoch)
import           TxData (pattern RegKey)

import           Ledger.Core (dom, (∈))

-------------------------------
-- helper accessor functions --
-------------------------------

getStDelegs :: DState -> Set StakeCredential
getStDelegs l = dom $ _stKeys l

getRewards :: DState -> Map RewardAcnt Coin
getRewards l = _rewards l

getDelegations :: LedgerState -> Map StakeCredential KeyHash
getDelegations l = _delegations $ _dstate $ _delegationState l

getStPools :: LedgerState -> StakePools
getStPools l = _stPools $ _pstate $ _delegationState l

getRetiring :: LedgerState -> Map KeyHash Epoch
getRetiring l = _retiring $ _pstate $ _delegationState l


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
    True === (all credNewlyRegisteredAndRewardZero tr)

  where credNewlyRegisteredAndRewardZero (d, RegKey hk, d') =
                 hk ∈ getStDelegs d
          || (   hk ∈ getStDelegs d'
              && (Maybe.maybe True (== 0) $ Map.lookup (mkRwdAcnt hk) (getRewards d')))
        credNewlyRegisteredAndRewardZero (_, _, _) = True
