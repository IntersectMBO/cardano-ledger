{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Deleg
  ( DELEG
  , DelegEnv (..)
  , PredicateFailure(..)
  )
where

import           BaseTypes
import           BlockChain (slotsPrior)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Coin (Coin (..))
import           Control.Monad.Trans.Reader (asks, runReaderT)
import           Control.State.Transition
import           Control.State.Transition.Generator
import           Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Delegation.Certificates
import           GHC.Generics (Generic)
import           Hedgehog (Gen)
import           Keys
import           Ledger.Core (dom, range, singleton, (∈), (∉), (∪), (⋪), (⋫), (⨃))
import           LedgerState
import           Slot
import           TxData

data DELEG crypto

data DelegEnv
  = DelegEnv
  { slot     :: SlotNo
  , ptr      :: Ptr
  , reserves :: Coin
  }
  deriving (Show, Eq)

instance STS (DELEG crypto)
 where
  type State (DELEG crypto) = DState crypto
  type Signal (DELEG crypto) = DCert crypto
  type Environment (DELEG crypto) = DelegEnv
  type BaseM (DELEG crypto) = ShelleyBase
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
    deriving (Show, Eq, Generic)

  initialRules = [pure emptyDState]
  transitionRules = [delegationTransition]

instance NoUnexpectedThunks (PredicateFailure (DELEG crypto))

delegationTransition
  :: TransitionRule (DELEG crypto)
delegationTransition = do
  TRC (DelegEnv slot_ ptr_ reserves_, ds, c) <- judgmentContext

  case c of
    DCertDeleg (RegKey key) -> do
      -- note that pattern match is used instead of regCred, as in the spec
      key ∉ dom (_stkCreds ds) ?! StakeKeyAlreadyRegisteredDELEG

      pure $ ds
        { _stkCreds  = _stkCreds ds  ∪ singleton key slot_
        , _rewards = _rewards ds ∪ Map.singleton (RewardAcnt key) (Coin 0) -- ∪ is override left
        , _ptrs    = _ptrs ds    ∪ Map.singleton ptr_ key
        }

    DCertDeleg (DeRegKey key) -> do
      -- note that pattern match is used instead of cwitness, as in the spec
      key ∈ dom (_stkCreds ds) ?! StakeKeyNotRegisteredDELEG

      let rewardCoin = Map.lookup (RewardAcnt key) (_rewards ds)
      rewardCoin == Just 0 ?! StakeKeyNonZeroAccountBalanceDELEG

      pure $ ds
        { _stkCreds      = Set.singleton key              ⋪ _stkCreds ds
        , _rewards     = Set.singleton (RewardAcnt key) ⋪ _rewards ds
        , _delegations = Set.singleton key              ⋪ _delegations ds
        , _ptrs        = _ptrs ds                       ⋫ Set.singleton key
        }

    DCertDeleg (Delegate (Delegation delegator_ delegatee_)) -> do
      -- note that pattern match is used instead of cwitness, as in the spec
      delegator_ ∈ dom (_stkCreds ds) ?! StakeDelegationImpossibleDELEG

      pure $ ds
        { _delegations = _delegations ds ⨃ [(delegator_, delegatee_)] }

    DCertGenesis (GenesisDelegate (gkey, vk)) -> do
      -- note that pattern match is used instead of genesisDeleg, as in the spec
      let s' = slot_ +* slotsPrior
          (GenDelegs genDelegs_) = _genDelegs ds

      gkey ∈ dom genDelegs_ ?! GenesisKeyNotInpMappingDELEG
      vk ∉ range genDelegs_ ?! DuplicateGenesisDelegateDELEG
      pure $ ds
        { _fGenDelegs = _fGenDelegs ds ⨃ [((s', gkey), vk)]}

    DCertMir (MIRCert credCoinMap) -> do
      let combinedMap = Map.union credCoinMap (_irwd ds)
          requiredForRewards = foldl (+) (Coin 0) (range combinedMap)
      firstSlot <- liftSTS $ do
        ei <- asks epochInfo
        EpochNo currEpoch <- epochInfoEpoch ei slot_
        epochInfoFirst ei $ EpochNo (currEpoch + 1)
      slot_ < firstSlot *- slotsPrior
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

  type BaseEnv (DELEG crypto) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals
