{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Deleg
  ( DELEG
  , DelegEnv (..)
  , PredicateFailure(..)
  )
where

import           Byron.Spec.Ledger.Core (dom, range, singleton, (∈), (∉), (∪), (⋪), (⋫), (⨃))
import           Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeWord)
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad.Trans.Reader (asks)
import           Control.State.Transition
import           Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           Shelley.Spec.Ledger.BaseTypes
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Crypto
import           Shelley.Spec.Ledger.Delegation.Certificates
import           Shelley.Spec.Ledger.Keys
import           Shelley.Spec.Ledger.LedgerState (DState, FutureGenDeleg (..), emptyDState,
                     _delegations, _fGenDelegs, _genDelegs, _irwd, _ptrs, _rewards, _stkCreds)
import           Shelley.Spec.Ledger.Slot
import           Shelley.Spec.Ledger.TxData

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

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (PredicateFailure (DELEG crypto))
 where
   toCBOR = \case
      StakeKeyAlreadyRegisteredDELEG           -> toCBOR (0 :: Word8)
      StakeKeyNotRegisteredDELEG               -> toCBOR (1 :: Word8)
      StakeKeyNonZeroAccountBalanceDELEG       -> toCBOR (2 :: Word8)
      StakeDelegationImpossibleDELEG           -> toCBOR (3 :: Word8)
      WrongCertificateTypeDELEG                -> toCBOR (4 :: Word8)
      GenesisKeyNotInpMappingDELEG             -> toCBOR (5 :: Word8)
      DuplicateGenesisDelegateDELEG            -> toCBOR (6 :: Word8)
      InsufficientForInstantaneousRewardsDELEG -> toCBOR (7 :: Word8)
      MIRCertificateTooLateinEpochDELEG        -> toCBOR (8 :: Word8)

instance
  (Crypto crypto)
  => FromCBOR (PredicateFailure (DELEG crypto))
 where
  fromCBOR = do
    decodeWord >>= \case
      0 -> pure StakeKeyAlreadyRegisteredDELEG
      1 -> pure StakeKeyNotRegisteredDELEG
      2 -> pure StakeKeyNonZeroAccountBalanceDELEG
      3 -> pure StakeDelegationImpossibleDELEG
      4 -> pure WrongCertificateTypeDELEG
      5 -> pure GenesisKeyNotInpMappingDELEG
      6 -> pure DuplicateGenesisDelegateDELEG
      7 -> pure InsufficientForInstantaneousRewardsDELEG
      8 -> pure MIRCertificateTooLateinEpochDELEG
      k -> invalidKey k

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

    DCertGenesis (GenesisDelegate gkh vkh) -> do
      sp <- liftSTS $ asks slotsPrior
      -- note that pattern match is used instead of genesisDeleg, as in the spec
      let s' = slot +* Duration sp
          (GenDelegs genDelegs) = _genDelegs ds

      gkh ∈ dom genDelegs ?! GenesisKeyNotInpMappingDELEG
      vkh ∉ range genDelegs ?! DuplicateGenesisDelegateDELEG
      pure $ ds
        { _fGenDelegs = _fGenDelegs ds ⨃ [(FutureGenDeleg s' gkh, vkh)]}

    DCertMir (MIRCert credCoinMap) -> do
      sp <- liftSTS $ asks slotsPrior
      firstSlot <- liftSTS $ do
        ei <- asks epochInfo
        EpochNo currEpoch <- epochInfoEpoch ei slot
        epochInfoFirst ei $ EpochNo (currEpoch + 1)
      slot < firstSlot *- Duration sp
        ?! MIRCertificateTooLateinEpochDELEG

      let combinedMap = Map.union credCoinMap (_irwd ds)
          requiredForRewards = foldl' (+) (Coin 0) (range combinedMap)
      requiredForRewards <= reserves ?! InsufficientForInstantaneousRewardsDELEG

      pure $ ds { _irwd = combinedMap }

    _ -> do
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure ds
