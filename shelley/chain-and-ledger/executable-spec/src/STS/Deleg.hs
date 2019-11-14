{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Deleg
  ( DELEG
  , DelegEnv (..)
  , PredicateFailure(..)
  )
where

import           Data.Data (Data)
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

import           Cardano.Ledger.Shelley.Crypto
import           Control.State.Transition
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Hedgehog (Gen)

data DELEG crypto
  deriving Data

data DelegEnv
  = DelegEnv
  { slot :: Slot
  , ptr :: Ptr
  , reserves :: Coin
  }
  deriving (Show, Eq)

instance STS (DELEG crypto)
 where
  type State (DELEG crypto) = DState crypto
  type Signal (DELEG crypto) = DCert crypto
  type Environment (DELEG crypto) = DelegEnv
  data PredicateFailure (DELEG crypto)
    = StakeKeyAlreadyRegisteredDELEG
    | StakeKeyNotRegisteredDELEG
    | StakeKeyNonZeroAccountBalanceDELEG
    | StakeDelegationImpossibleDELEG
    | WrongCertificateTypeDELEG
    | GenesisKeyNotInpMappingDELEG
    | DuplicateGenesisDelegateDELEG
    | InsufficientForInstantaneousRewardsDELEG
    | MIRCertificateTooLateinEpochDELEG
    deriving (Show, Eq, Data)

  initialRules = [pure emptyDState]
  transitionRules = [delegationTransition]

delegationTransition
  :: TransitionRule (DELEG crypto)
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
          Epoch currEpoch = epochFromSlot slot_
      slot_ < (firstSlot $ Epoch (currEpoch + 1)) *- slotsPrior
        ?! MIRCertificateTooLateinEpochDELEG
      requiredForRewards <= reserves_ ?! InsufficientForInstantaneousRewardsDELEG

      pure $ ds { _irwd = combinedMap }

    _ -> do
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure ds


instance Crypto crypto
  => HasTrace (DELEG crypto) where
  envGen _ = undefined :: Gen DelegEnv
  sigGen _ _ = undefined :: Gen (DCert crypto)
