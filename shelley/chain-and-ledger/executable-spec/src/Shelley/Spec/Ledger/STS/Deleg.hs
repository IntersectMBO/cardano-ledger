{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Deleg
  ( DELEG,
    DelegEnv (..),
    PredicateFailure,
    DelegPredicateFailure (..),
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.Era (Era)
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Iterate.SetAlgebra (dom, eval, range, setSingleton, singleton, (∈), (∉), (∪), (⋪), (⋫), (⨃))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    ShelleyBase,
    invalidKey,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential)
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    Hash,
    KeyHash,
    KeyRole (..),
    VerKeyVRF,
  )
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    DState,
    FutureGenDeleg (..),
    InstantaneousRewards (..),
    emptyDState,
    _delegations,
    _fGenDelegs,
    _genDelegs,
    _irwd,
    _ptrs,
    _rewards,
  )
import Shelley.Spec.Ledger.Serialization (decodeRecordSum)
import Shelley.Spec.Ledger.Slot
  ( Duration (..),
    EpochNo (..),
    SlotNo,
    epochInfoEpoch,
    epochInfoFirst,
    (*-),
    (+*),
  )
import Shelley.Spec.Ledger.TxData
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    MIRCert (..),
    MIRPot (..),
    Ptr,
  )

data DELEG era

data DelegEnv = DelegEnv
  { slotNo :: SlotNo,
    ptr_ :: Ptr,
    acnt_ :: AccountState
  }
  deriving (Show, Eq)

data DelegPredicateFailure era
  = StakeKeyAlreadyRegisteredDELEG
      !(Credential 'Staking era) -- Credential which is already registered
  | -- | Indicates that the stake key is somehow already in the rewards map.
    --   This error is now redundant with StakeKeyAlreadyRegisteredDELEG.
    --   We should remove it and replace its one use with StakeKeyAlreadyRegisteredDELEG.
    StakeKeyInRewardsDELEG
      !(Credential 'Staking era) -- Credential which is already registered
  | StakeKeyNotRegisteredDELEG
      !(Credential 'Staking era) -- Credential which is not registered
  | StakeKeyNonZeroAccountBalanceDELEG
      !(Maybe Coin) -- The remaining reward account balance, if it exists
  | StakeDelegationImpossibleDELEG
      !(Credential 'Staking era) -- Credential that is not registered
  | WrongCertificateTypeDELEG -- The DCertPool constructor should not be used by this transition
  | GenesisKeyNotInpMappingDELEG
      !(KeyHash 'Genesis era) -- Unknown Genesis KeyHash
  | DuplicateGenesisDelegateDELEG
      !(KeyHash 'GenesisDelegate era) -- Keyhash which is already delegated to
  | InsufficientForInstantaneousRewardsDELEG
      !MIRPot -- which pot the rewards are to be drawn from, treasury or reserves
      !Coin -- amount of rewards to be given out
      !Coin -- size of the pot from which the lovelace is drawn
  | MIRCertificateTooLateinEpochDELEG
      !SlotNo -- current slot
      !SlotNo -- MIR must be submitted before this slot
  | DuplicateGenesisVRFDELEG
      !(Hash era (VerKeyVRF era)) --VRF KeyHash which is already delegated to
  deriving (Show, Eq, Generic)

instance Typeable era => STS (DELEG era) where
  type State (DELEG era) = DState era
  type Signal (DELEG era) = DCert era
  type Environment (DELEG era) = DelegEnv
  type BaseM (DELEG era) = ShelleyBase
  type PredicateFailure (DELEG era) = DelegPredicateFailure era

  initialRules = [pure emptyDState]
  transitionRules = [delegationTransition]

instance NoUnexpectedThunks (DelegPredicateFailure era)

instance
  (Typeable era, Era era) =>
  ToCBOR (DelegPredicateFailure era)
  where
  toCBOR = \case
    StakeKeyAlreadyRegisteredDELEG cred ->
      encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR cred
    StakeKeyInRewardsDELEG cred ->
      encodeListLen 2 <> toCBOR (10 :: Word8) <> toCBOR cred
    StakeKeyNotRegisteredDELEG cred ->
      encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR cred
    StakeKeyNonZeroAccountBalanceDELEG rewardBalance ->
      encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR rewardBalance
    StakeDelegationImpossibleDELEG cred ->
      encodeListLen 2 <> toCBOR (3 :: Word8) <> toCBOR cred
    WrongCertificateTypeDELEG ->
      encodeListLen 1 <> toCBOR (4 :: Word8)
    GenesisKeyNotInpMappingDELEG gkh ->
      encodeListLen 2 <> toCBOR (5 :: Word8) <> toCBOR gkh
    DuplicateGenesisDelegateDELEG kh ->
      encodeListLen 2 <> toCBOR (6 :: Word8) <> toCBOR kh
    InsufficientForInstantaneousRewardsDELEG pot needed potAmount ->
      encodeListLen 4 <> toCBOR (7 :: Word8)
        <> toCBOR pot
        <> toCBOR needed
        <> toCBOR potAmount
    MIRCertificateTooLateinEpochDELEG sNow sTooLate ->
      encodeListLen 3 <> toCBOR (8 :: Word8) <> toCBOR sNow <> toCBOR sTooLate
    DuplicateGenesisVRFDELEG vrf ->
      encodeListLen 2 <> toCBOR (9 :: Word8) <> toCBOR vrf

instance
  (Era era) =>
  FromCBOR (DelegPredicateFailure era)
  where
  fromCBOR = decodeRecordSum "PredicateFailure (DELEG era)" $
    \case
      0 -> do
        kh <- fromCBOR
        pure (2, StakeKeyAlreadyRegisteredDELEG kh)
      10 -> do
        kh <- fromCBOR
        pure (2, StakeKeyInRewardsDELEG kh)
      1 -> do
        kh <- fromCBOR
        pure (2, StakeKeyNotRegisteredDELEG kh)
      2 -> do
        b <- fromCBOR
        pure (2, StakeKeyNonZeroAccountBalanceDELEG b)
      3 -> do
        kh <- fromCBOR
        pure (2, StakeDelegationImpossibleDELEG kh)
      4 -> do
        pure (1, WrongCertificateTypeDELEG)
      5 -> do
        gkh <- fromCBOR
        pure (2, GenesisKeyNotInpMappingDELEG gkh)
      6 -> do
        kh <- fromCBOR
        pure (2, DuplicateGenesisDelegateDELEG kh)
      7 -> do
        pot <- fromCBOR
        needed <- fromCBOR
        potAmount <- fromCBOR
        pure (4, InsufficientForInstantaneousRewardsDELEG pot needed potAmount)
      8 -> do
        sNow <- fromCBOR
        sTooLate <- fromCBOR
        pure (3, MIRCertificateTooLateinEpochDELEG sNow sTooLate)
      9 -> do
        vrf <- fromCBOR
        pure (2, DuplicateGenesisVRFDELEG vrf)
      k -> invalidKey k

delegationTransition ::
  Typeable era =>
  TransitionRule (DELEG era)
delegationTransition = do
  TRC (DelegEnv slot ptr acnt, ds, c) <- judgmentContext
  case c of
    DCertDeleg (RegKey hk) -> do
      -- note that pattern match is used instead of regCred, as in the spec
      -- TODO we can remove the failure StakeKeyInRewardsDELEG and replace
      -- the use below with StakeKeyAlreadyRegisteredDELEG
      eval (hk ∉ dom (_rewards ds)) ?! StakeKeyInRewardsDELEG hk

      pure $
        ds
          { _rewards = eval (_rewards ds ∪ (singleton hk mempty)),
            _ptrs = eval (_ptrs ds ∪ (singleton ptr hk))
          }
    DCertDeleg (DeRegKey hk) -> do
      -- note that pattern match is used instead of cwitness, as in the spec
      eval (hk ∈ dom (_rewards ds)) ?! StakeKeyNotRegisteredDELEG hk

      let rewardCoin = Map.lookup hk (_rewards ds)
      rewardCoin == Just mempty ?! StakeKeyNonZeroAccountBalanceDELEG rewardCoin

      pure $
        ds
          { _rewards = eval (setSingleton hk ⋪ _rewards ds),
            _delegations = eval (setSingleton hk ⋪ _delegations ds),
            _ptrs = eval (_ptrs ds ⋫ setSingleton hk)
          }
    DCertDeleg (Delegate (Delegation hk dpool)) -> do
      -- note that pattern match is used instead of cwitness and dpool, as in the spec
      eval (hk ∈ dom (_rewards ds)) ?! StakeDelegationImpossibleDELEG hk

      pure $
        ds
          { _delegations = eval (_delegations ds ⨃ (singleton hk dpool))
          }
    DCertGenesis (GenesisDelegCert gkh vkh vrf) -> do
      sp <- liftSTS $ asks stabilityWindow
      -- note that pattern match is used instead of genesisDeleg, as in the spec
      let s' = slot +* Duration sp
          (GenDelegs genDelegs) = _genDelegs ds

      -- gkh ∈ dom genDelegs ?! GenesisKeyNotInpMappingDELEG gkh
      (case Map.lookup gkh genDelegs of Just _ -> True; Nothing -> False) ?! GenesisKeyNotInpMappingDELEG gkh

      let cod =
            range $
              Map.filterWithKey (\g _ -> g /= gkh) genDelegs
          fod =
            range $
              Map.filterWithKey (\(FutureGenDeleg _ g) _ -> g /= gkh) (_fGenDelegs ds)
          currentOtherColdKeyHashes = Set.map genDelegKeyHash cod
          currentOtherVrfKeyHashes = Set.map genDelegVrfHash cod
          futureOtherColdKeyHashes = Set.map genDelegKeyHash fod
          futureOtherVrfKeyHashes = Set.map genDelegVrfHash fod

      eval (vkh ∉ (currentOtherColdKeyHashes `Set.union` futureOtherColdKeyHashes))
        ?! DuplicateGenesisDelegateDELEG vkh
      eval (vrf ∉ (currentOtherVrfKeyHashes `Set.union` futureOtherVrfKeyHashes))
        ?! DuplicateGenesisVRFDELEG vrf

      pure $
        ds
          { _fGenDelegs = eval ((_fGenDelegs ds) ⨃ (singleton (FutureGenDeleg s' gkh) (GenDelegPair vkh vrf)))
          }
    DCertMir (MIRCert targetPot credCoinMap) -> do
      sp <- liftSTS $ asks stabilityWindow
      firstSlot <- liftSTS $ do
        ei <- asks epochInfo
        EpochNo currEpoch <- epochInfoEpoch ei slot
        epochInfoFirst ei $ EpochNo (currEpoch + 1)
      let tooLate = firstSlot *- Duration sp
      slot < tooLate
        ?! MIRCertificateTooLateinEpochDELEG slot tooLate

      let (potAmount, instantaneousRewards) =
            case targetPot of
              ReservesMIR -> (_reserves acnt, iRReserves $ _irwd ds)
              TreasuryMIR -> (_treasury acnt, iRTreasury $ _irwd ds)
      let combinedMap = Map.union credCoinMap instantaneousRewards
          requiredForRewards = fold combinedMap
      requiredForRewards <= potAmount
        ?! InsufficientForInstantaneousRewardsDELEG targetPot requiredForRewards potAmount

      case targetPot of
        ReservesMIR -> pure $ ds {_irwd = (_irwd ds) {iRReserves = combinedMap}}
        TreasuryMIR -> pure $ ds {_irwd = (_irwd ds) {iRTreasury = combinedMap}}
    DCertPool _ -> do
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure ds
