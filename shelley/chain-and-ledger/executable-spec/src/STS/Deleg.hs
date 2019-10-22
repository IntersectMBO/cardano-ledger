{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Deleg
  ( DELEG
  , DelegEnv (..)
  , PredicateFailure(..)
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           BlockChain (slotsPrior)
import           Coin (Coin (..))
import           Delegation.Certificates
import           Keys
import           Ledger.Core (dom, range, singleton, (∈), (∉), (∪), (⋪), (⋫), (⨃))
import           LedgerState
import           Slot
import           TxData

import           Control.State.Transition
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Hedgehog (Gen)

data DELEG hashAlgo dsignAlgo vrfAlgo

data DelegEnv
  = DelegEnv
  { slot :: Slot
  , ptr :: Ptr
  , reserves :: Coin
  }
  deriving (Show, Eq)

instance STS (DELEG hashAlgo dsignAlgo vrfAlgo)
 where
  type State (DELEG hashAlgo dsignAlgo vrfAlgo) = DState hashAlgo dsignAlgo
  type Signal (DELEG hashAlgo dsignAlgo vrfAlgo) = DCert hashAlgo dsignAlgo vrfAlgo
  type Environment (DELEG hashAlgo dsignAlgo vrfAlgo) = DelegEnv
  data PredicateFailure (DELEG hashAlgo dsignAlgo vrfAlgo)
    = StakeKeyAlreadyRegisteredDELEG
    | StakeKeyNotRegisteredDELEG
    | StakeKeyNonZeroAccountBalanceDELEG
    | StakeDelegationImpossibleDELEG
    | WrongCertificateTypeDELEG
    | GenesisKeyNotInpMappingDELEG
    | DuplicateGenesisDelegateDELEG
    | InsufficientForInstantaneousRewardsDELEG
    deriving (Show, Eq)

  initialRules = [pure emptyDState]
  transitionRules = [delegationTransition]

delegationTransition
  :: TransitionRule (DELEG hashAlgo dsignAlgo vrfAlgo)
delegationTransition = do
  TRC (DelegEnv slot_ ptr_ reserves_, ds, c) <- judgmentContext

  case c of
    RegKey key -> do
      key ∉ dom (_stkCreds ds) ?! StakeKeyAlreadyRegisteredDELEG

      pure $ ds
        { _stkCreds  = _stkCreds ds  ∪ singleton key slot_
        , _rewards = _rewards ds ∪ Map.singleton (RewardAcnt key) (Coin 0)
        , _ptrs    = _ptrs ds    ∪ Map.singleton ptr_ key
        }

    DeRegKey key -> do
      key ∈ dom (_stkCreds ds) ?! StakeKeyNotRegisteredDELEG

      let rewardCoin = Map.lookup (RewardAcnt key) (_rewards ds)
      rewardCoin == Just 0 ?! StakeKeyNonZeroAccountBalanceDELEG

      pure $ ds
        { _stkCreds      = Set.singleton key              ⋪ _stkCreds ds
        , _rewards     = Set.singleton (RewardAcnt key) ⋪ _rewards ds
        , _delegations = Set.singleton key              ⋪ _delegations ds
        , _ptrs        = _ptrs ds                       ⋫ Set.singleton key
        }

    Delegate (Delegation delegator_ delegatee_) -> do
      delegator_ ∈ dom (_stkCreds ds) ?! StakeDelegationImpossibleDELEG

      pure $ ds
        { _delegations = _delegations ds ⨃ [(delegator_, delegatee_)] }

    GenesisDelegate (gkey, vk) -> do
      let s' = slot_ +* slotsPrior
          (GenDelegs genDelegs_) = _genDelegs ds

      gkey ∈ dom genDelegs_ ?! GenesisKeyNotInpMappingDELEG
      vk ∉ range genDelegs_ ?! DuplicateGenesisDelegateDELEG
      pure $ ds
        { _fGenDelegs = _fGenDelegs ds ⨃ [((s', gkey), vk)]}

    InstantaneousRewards credCoinMap -> do
      let combinedMap = Map.union credCoinMap (_irwd ds)
          requiredForRewards = foldl (+) (Coin 0) (range combinedMap)

      requiredForRewards <= reserves_ ?! InsufficientForInstantaneousRewardsDELEG

      pure $ ds { _irwd = combinedMap }

    _ -> do
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure ds


instance (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => HasTrace (DELEG hashAlgo dsignAlgo vrfAlgo) where
  envGen _ = undefined :: Gen DelegEnv
  sigGen _ _ = undefined :: Gen (DCert hashAlgo dsignAlgo vrfAlgo)
