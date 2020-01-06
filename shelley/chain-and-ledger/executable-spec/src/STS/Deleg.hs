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
import           LedgerState (DState, emptyDState, _delegations, _fGenDelegs, _genDelegs, _irwd,
                     _ptrs, _rewards, _stkCreds)
import           Slot
import           TxData

data DELEG crypto

data DelegEnv
  = DelegEnv
  { slotNo    :: SlotNo
  , ptr_      :: Ptr
  , reserves_ :: Coin
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
  TRC (DelegEnv slot ptr reserves, ds, c) <- judgmentContext

  case c of
    DCertDeleg (RegKey hk) -> do
      -- note that pattern match is used instead of regCred, as in the spec
      hk ∉ dom (_stkCreds ds) ?! StakeKeyAlreadyRegisteredDELEG

      pure $ ds
        { _stkCreds  = _stkCreds ds  ∪ singleton hk slot
        , _rewards = _rewards ds ∪ Map.singleton (RewardAcnt hk) (Coin 0) -- ∪ is override left
        , _ptrs    = _ptrs ds    ∪ Map.singleton ptr hk
        }

    DCertDeleg (DeRegKey hk) -> do
      -- note that pattern match is used instead of cwitness, as in the spec
      hk ∈ dom (_stkCreds ds) ?! StakeKeyNotRegisteredDELEG

      let rewardCoin = Map.lookup (RewardAcnt hk) (_rewards ds)
      rewardCoin == Just 0 ?! StakeKeyNonZeroAccountBalanceDELEG

      pure $ ds
        { _stkCreds    = Set.singleton hk              ⋪ _stkCreds ds
        , _rewards     = Set.singleton (RewardAcnt hk) ⋪ _rewards ds
        , _delegations = Set.singleton hk              ⋪ _delegations ds
        , _ptrs        = _ptrs ds                      ⋫ Set.singleton hk
        }

    DCertDeleg (Delegate (Delegation hk dpool)) -> do
      -- note that pattern match is used instead of cwitness and dpool, as in the spec
      hk ∈ dom (_stkCreds ds) ?! StakeDelegationImpossibleDELEG

      pure $ ds
        { _delegations = _delegations ds ⨃ [(hk, dpool)] }

    DCertGenesis (GenesisDelegate (gkh, vkh)) -> do
      sp <- liftSTS $ asks slotsPrior
      -- note that pattern match is used instead of genesisDeleg, as in the spec
      let s' = slot +* Duration sp
          (GenDelegs genDelegs) = _genDelegs ds

      gkh ∈ dom genDelegs ?! GenesisKeyNotInpMappingDELEG
      vkh ∉ range genDelegs ?! DuplicateGenesisDelegateDELEG
      pure $ ds
        { _fGenDelegs = _fGenDelegs ds ⨃ [((s', gkh), vkh)]}

    DCertMir (MIRCert credCoinMap) -> do
      sp <- liftSTS $ asks slotsPrior
      firstSlot <- liftSTS $ do
        ei <- asks epochInfo
        EpochNo currEpoch <- epochInfoEpoch ei slot
        epochInfoFirst ei $ EpochNo (currEpoch + 1)
      slot < firstSlot *- Duration sp
        ?! MIRCertificateTooLateinEpochDELEG

      let combinedMap = Map.union credCoinMap (_irwd ds)
          requiredForRewards = foldl (+) (Coin 0) (range combinedMap)
      requiredForRewards <= reserves ?! InsufficientForInstantaneousRewardsDELEG

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
