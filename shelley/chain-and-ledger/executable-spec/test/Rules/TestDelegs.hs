{-# LANGUAGE PatternSynonyms #-}

module Rules.TestDelegs where

import           Data.Map (Map)
import qualified Data.Map.Strict as Map (keysSet)
import           Data.Set (Set)

import           Coin (Coin)
import           LedgerState (_delegationState, _delegations, _dstate, _pstate, _retiring, _rewards,
                     _stKeys, _stPools)
import           MockTypes (Credential, KeyHash, LedgerState, RewardAcnt, StakeCredential,
                     StakePools)
import           Slot (Epoch)
import           TxData (pattern StakeKeys)

-------------------------------
-- helper accessor functions --
-------------------------------

getStDelegs :: LedgerState -> Set Credential
getStDelegs l = Map.keysSet keys
  where StakeKeys keys = _stKeys $ _dstate $ _delegationState l

getRewards :: LedgerState -> Map RewardAcnt Coin
getRewards l = _rewards $ _dstate $ _delegationState l

getDelegations :: LedgerState -> Map StakeCredential KeyHash
getDelegations l = _delegations $ _dstate $ _delegationState l

getStPools :: LedgerState -> StakePools
getStPools l = _stPools $ _pstate $ _delegationState l

getRetiring :: LedgerState -> Map KeyHash Epoch
getRetiring l = _retiring $ _pstate $ _delegationState l
