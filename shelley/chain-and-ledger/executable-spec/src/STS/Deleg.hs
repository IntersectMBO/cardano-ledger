{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Deleg
  ( DELEG
  , DelegEnv (..)
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

data DELEG hashAlgo dsignAlgo

data DelegEnv
  = DelegEnv Slot Ptr
  deriving (Show, Eq)

instance STS (DELEG hashAlgo dsignAlgo)
 where
  type State (DELEG hashAlgo dsignAlgo) = DState hashAlgo dsignAlgo
  type Signal (DELEG hashAlgo dsignAlgo) = DCert hashAlgo dsignAlgo
  type Environment (DELEG hashAlgo dsignAlgo) = DelegEnv
  data PredicateFailure (DELEG hashAlgo dsignAlgo)
    = StakeKeyAlreadyRegisteredDELEG
    | StakeKeyNotRegisteredDELEG
    | StakeKeyNonZeroAccountBalanceDELEG
    | StakeDelegationImpossibleDELEG
    | WrongCertificateTypeDELEG
    | GenesisKeyNotInpMappingDELEG
    | DuplicateGenesisDelegateDELEG
    deriving (Show, Eq)

  initialRules = [pure emptyDState]
  transitionRules = [delegationTransition]

delegationTransition
  :: TransitionRule (DELEG hashAlgo dsignAlgo)
delegationTransition = do
  TRC (DelegEnv slot_ ptr_, ds, c) <- judgmentContext

  case c of
    RegKey key -> do
      key ∉ dom (_stKeys ds) ?! StakeKeyAlreadyRegisteredDELEG

      pure $ ds
        { _stKeys  = _stKeys ds  ∪ singleton key slot_
        , _rewards = _rewards ds ∪ Map.singleton (RewardAcnt key) (Coin 0)
        , _ptrs    = _ptrs ds    ∪ Map.singleton ptr_ key
        }

    DeRegKey key -> do
      key ∈ dom (_stKeys ds) ?! StakeKeyNotRegisteredDELEG

      let rewardCoin = Map.lookup (RewardAcnt key) (_rewards ds)
      rewardCoin == Just 0 ?! StakeKeyNonZeroAccountBalanceDELEG

      pure $ ds
        { _stKeys      = Set.singleton key              ⋪ _stKeys ds
        , _rewards     = Set.singleton (RewardAcnt key) ⋪ _rewards ds
        , _delegations = Set.singleton key              ⋪ _delegations ds
        , _ptrs        = _ptrs ds                       ⋫ Set.singleton key
        }

    Delegate (Delegation delegator_ delegatee_) -> do
      delegator_ ∈ dom (_stKeys ds) ?! StakeDelegationImpossibleDELEG

      pure $ ds
        { _delegations = _delegations ds ⨃ [(delegator_, delegatee_)] }

    GenesisDelegate (gkey, vk) -> do
      let s' = slot_ +* slotsPrior
          (Dms dms_) = _dms ds

      gkey ∈ dom dms_ ?! GenesisKeyNotInpMappingDELEG
      vk ∉ range dms_ ?! DuplicateGenesisDelegateDELEG
      pure $ ds
        { _fdms = _fdms ds ⨃ [((s', gkey), vk)]}

    _ -> do
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure ds


instance (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => HasTrace (DELEG hashAlgo dsignAlgo) where
  envGen _ = undefined :: Gen DelegEnv
  sigGen _ _ = undefined :: Gen (DCert hashAlgo dsignAlgo)
