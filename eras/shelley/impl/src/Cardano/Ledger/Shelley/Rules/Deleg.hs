{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Deleg
  ( ShelleyDELEG,
    DelegEnv (..),
    PredicateFailure,
    ShelleyDelegPredFailure (..),
  )
where

import Cardano.Ledger.BaseTypes (Globals (..), ProtVer, ShelleyBase, epochInfoPure, invalidKey)
import Cardano.Ledger.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeRecordSum,
    encodeListLen,
  )
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..), addDeltaCoin, toDeltaCoin)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    Hash,
    KeyHash,
    KeyRole (..),
    VerKeyVRF,
  )
import Cardano.Ledger.Shelley.Era (ShelleyDELEG)
import Cardano.Ledger.Shelley.HardForks as HardForks (allowMIRTransfer)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DState (..),
    FutureGenDeleg (..),
    InstantaneousRewards (..),
    availableAfterMIR,
    delegations,
    dsFutureGenDelegs,
    dsGenDelegs,
    dsIRewards,
    rewards,
  )
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
    Delegation (..),
    GenesisDelegCert (..),
    MIRCert (..),
    MIRPot (..),
    MIRTarget (..),
    Ptr,
  )
import Cardano.Ledger.Slot
  ( Duration (..),
    EpochNo (..),
    SlotNo,
    epochInfoEpoch,
    epochInfoFirst,
    (*-),
    (+*),
  )
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, range, singleton, (∈), (∉), (∪), (⨃))
import Control.State.Transition
import Data.Foldable (fold)
import Data.Group (Group (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.UMap (View (..))
import qualified Data.UMap as UM
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField)
import NoThunks.Class (NoThunks (..))

data DelegEnv era = DelegEnv
  { slotNo :: SlotNo,
    ptr_ :: Ptr,
    acnt_ :: AccountState,
    ppDE :: PParams era -- The protocol parameters are only used for the HardFork mechanism
  }

deriving instance (Show (PParams era)) => Show (DelegEnv era)

deriving instance (Eq (PParams era)) => Eq (DelegEnv era)

data ShelleyDelegPredFailure era
  = StakeKeyAlreadyRegisteredDELEG
      !(Credential 'Staking (EraCrypto era)) -- Credential which is already registered
  | -- | Indicates that the stake key is somehow already in the rewards map.
    --   This error is now redundant with StakeKeyAlreadyRegisteredDELEG.
    --   We should remove it and replace its one use with StakeKeyAlreadyRegisteredDELEG.
    StakeKeyInRewardsDELEG
      !(Credential 'Staking (EraCrypto era)) -- DEPRECATED, now redundant with StakeKeyAlreadyRegisteredDELEG
  | StakeKeyNotRegisteredDELEG
      !(Credential 'Staking (EraCrypto era)) -- Credential which is not registered
  | StakeKeyNonZeroAccountBalanceDELEG
      !(Maybe Coin) -- The remaining reward account balance, if it exists
  | StakeDelegationImpossibleDELEG
      !(Credential 'Staking (EraCrypto era)) -- Credential that is not registered
  | WrongCertificateTypeDELEG -- The DCertPool constructor should not be used by this transition
  | GenesisKeyNotInMappingDELEG
      !(KeyHash 'Genesis (EraCrypto era)) -- Unknown Genesis KeyHash
  | DuplicateGenesisDelegateDELEG
      !(KeyHash 'GenesisDelegate (EraCrypto era)) -- Keyhash which is already delegated to
  | InsufficientForInstantaneousRewardsDELEG
      !MIRPot -- which pot the rewards are to be drawn from, treasury or reserves
      !Coin -- amount of rewards to be given out
      !Coin -- size of the pot from which the lovelace is drawn
  | MIRCertificateTooLateinEpochDELEG
      !SlotNo -- current slot
      !SlotNo -- EraRule "MIR" must be submitted before this slot
  | DuplicateGenesisVRFDELEG
      !(Hash (EraCrypto era) (VerKeyVRF (EraCrypto era))) -- VRF KeyHash which is already delegated to
  | MIRTransferNotCurrentlyAllowed
  | MIRNegativesNotCurrentlyAllowed
  | InsufficientForTransferDELEG
      !MIRPot -- which pot the rewards are to be drawn from, treasury or reserves
      !Coin -- amount attempted to transfer
      !Coin -- amount available
  | MIRProducesNegativeUpdate
  | MIRNegativeTransfer
      !MIRPot -- which pot the rewards are to be drawn from, treasury or reserves
      !Coin -- amount attempted to transfer
  deriving (Show, Eq, Generic)

newtype ShelleyDelegEvent era = NewEpoch EpochNo

instance
  ( Typeable era,
    HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  STS (ShelleyDELEG era)
  where
  type State (ShelleyDELEG era) = DState (EraCrypto era)
  type Signal (ShelleyDELEG era) = DCert (EraCrypto era)
  type Environment (ShelleyDELEG era) = DelegEnv era
  type BaseM (ShelleyDELEG era) = ShelleyBase
  type PredicateFailure (ShelleyDELEG era) = ShelleyDelegPredFailure era
  type Event (ShelleyDELEG era) = ShelleyDelegEvent era

  transitionRules = [delegationTransition]

instance NoThunks (ShelleyDelegPredFailure era)

instance
  (Era era, Typeable (Script era)) =>
  ToCBOR (ShelleyDelegPredFailure era)
  where
  toCBOR = \case
    StakeKeyAlreadyRegisteredDELEG cred ->
      encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR cred
    StakeKeyNotRegisteredDELEG cred ->
      encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR cred
    StakeKeyNonZeroAccountBalanceDELEG rewardBalance ->
      encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR rewardBalance
    StakeDelegationImpossibleDELEG cred ->
      encodeListLen 2 <> toCBOR (3 :: Word8) <> toCBOR cred
    WrongCertificateTypeDELEG ->
      encodeListLen 1 <> toCBOR (4 :: Word8)
    GenesisKeyNotInMappingDELEG gkh ->
      encodeListLen 2 <> toCBOR (5 :: Word8) <> toCBOR gkh
    DuplicateGenesisDelegateDELEG kh ->
      encodeListLen 2 <> toCBOR (6 :: Word8) <> toCBOR kh
    InsufficientForInstantaneousRewardsDELEG pot needed potAmount ->
      encodeListLen 4
        <> toCBOR (7 :: Word8)
        <> toCBOR pot
        <> toCBOR needed
        <> toCBOR potAmount
    MIRCertificateTooLateinEpochDELEG sNow sTooLate ->
      encodeListLen 3 <> toCBOR (8 :: Word8) <> toCBOR sNow <> toCBOR sTooLate
    DuplicateGenesisVRFDELEG vrf ->
      encodeListLen 2 <> toCBOR (9 :: Word8) <> toCBOR vrf
    StakeKeyInRewardsDELEG cred ->
      encodeListLen 2 <> toCBOR (10 :: Word8) <> toCBOR cred
    MIRTransferNotCurrentlyAllowed ->
      encodeListLen 1 <> toCBOR (11 :: Word8)
    MIRNegativesNotCurrentlyAllowed ->
      encodeListLen 1 <> toCBOR (12 :: Word8)
    InsufficientForTransferDELEG pot needed available ->
      encodeListLen 4
        <> toCBOR (13 :: Word8)
        <> toCBOR pot
        <> toCBOR needed
        <> toCBOR available
    MIRProducesNegativeUpdate ->
      encodeListLen 1 <> toCBOR (14 :: Word8)
    MIRNegativeTransfer pot amt ->
      encodeListLen 3
        <> toCBOR (15 :: Word8)
        <> toCBOR pot
        <> toCBOR amt

instance
  (Era era, Typeable (Script era)) =>
  FromCBOR (ShelleyDelegPredFailure era)
  where
  fromCBOR = decodeRecordSum "PredicateFailure (DELEG era)" $
    \case
      0 -> do
        kh <- fromCBOR
        pure (2, StakeKeyAlreadyRegisteredDELEG kh)
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
        pure (2, GenesisKeyNotInMappingDELEG gkh)
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
      10 -> do
        kh <- fromCBOR
        pure (2, StakeKeyInRewardsDELEG kh)
      11 -> do
        pure (1, MIRTransferNotCurrentlyAllowed)
      12 -> do
        pure (1, MIRNegativesNotCurrentlyAllowed)
      13 -> do
        pot <- fromCBOR
        needed <- fromCBOR
        available <- fromCBOR
        pure (4, InsufficientForTransferDELEG pot needed available)
      14 -> do
        pure (1, MIRProducesNegativeUpdate)
      15 -> do
        pot <- fromCBOR
        amt <- fromCBOR
        pure (3, MIRNegativeTransfer pot amt)
      k -> invalidKey k

delegationTransition ::
  ( Typeable era,
    HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  TransitionRule (ShelleyDELEG era)
delegationTransition = do
  TRC (DelegEnv slot ptr acnt pp, ds, c) <- judgmentContext
  case c of
    DCertDeleg (RegKey hk) -> do
      eval (hk ∉ dom (rewards ds)) ?! StakeKeyAlreadyRegisteredDELEG hk
      let u1 = dsUnified ds
          u2 = Rewards u1 UM.∪ (hk, mempty)
          u3 = Ptrs u2 UM.∪ (ptr, hk)
      pure ds {dsUnified = u3}
    DCertDeleg (DeRegKey hk) -> do
      -- note that pattern match is used instead of cwitness, as in the spec
      eval (hk ∈ dom (rewards ds)) ?! StakeKeyNotRegisteredDELEG hk
      let rewardCoin = UM.lookup hk (rewards ds)
      rewardCoin == Just mempty ?! StakeKeyNonZeroAccountBalanceDELEG rewardCoin

      let u0 = dsUnified ds
          u1 = Set.singleton hk UM.⋪ Rewards u0
          u2 = Set.singleton hk UM.⋪ Delegations u1
          u3 = Ptrs u2 UM.⋫ Set.singleton hk
      pure ds {dsUnified = u3}
    DCertDeleg (Delegate (Delegation hk dpool)) -> do
      -- note that pattern match is used instead of cwitness and dpool, as in the spec
      eval (hk ∈ dom (rewards ds)) ?! StakeDelegationImpossibleDELEG hk

      pure (ds {dsUnified = delegations ds UM.⨃ Map.singleton hk dpool})
    DCertGenesis (GenesisDelegCert gkh vkh vrf) -> do
      sp <- liftSTS $ asks stabilityWindow
      -- note that pattern match is used instead of genesisDeleg, as in the spec
      let s' = slot +* Duration sp
          GenDelegs genDelegs = dsGenDelegs ds

      -- gkh ∈ dom genDelegs ?! GenesisKeyNotInMappingDELEG gkh
      isJust (Map.lookup gkh genDelegs) ?! GenesisKeyNotInMappingDELEG gkh

      let cod =
            range $
              Map.filterWithKey (\g _ -> g /= gkh) genDelegs
          fod =
            range $
              Map.filterWithKey (\(FutureGenDeleg _ g) _ -> g /= gkh) (dsFutureGenDelegs ds)
          currentOtherColdKeyHashes = Set.map genDelegKeyHash cod
          currentOtherVrfKeyHashes = Set.map genDelegVrfHash cod
          futureOtherColdKeyHashes = Set.map genDelegKeyHash fod
          futureOtherVrfKeyHashes = Set.map genDelegVrfHash fod

      eval (vkh ∉ (currentOtherColdKeyHashes ∪ futureOtherColdKeyHashes))
        ?! DuplicateGenesisDelegateDELEG vkh
      eval (vrf ∉ (currentOtherVrfKeyHashes ∪ futureOtherVrfKeyHashes))
        ?! DuplicateGenesisVRFDELEG vrf

      pure $
        ds
          { dsFutureGenDelegs =
              eval (dsFutureGenDelegs ds ⨃ singleton (FutureGenDeleg s' gkh) (GenDelegPair vkh vrf))
          }
    DCertMir (MIRCert targetPot (StakeAddressesMIR credCoinMap)) -> do
      if HardForks.allowMIRTransfer pp
        then do
          sp <- liftSTS $ asks stabilityWindow
          ei <- liftSTS $ asks epochInfoPure
          EpochNo currEpoch <- liftSTS $ epochInfoEpoch ei slot
          let newEpoch = EpochNo (currEpoch + 1)
          tellEvent (NewEpoch newEpoch)
          firstSlot <- liftSTS $ epochInfoFirst ei newEpoch
          let tooLate = firstSlot *- Duration sp
          slot
            < tooLate
            ?! MIRCertificateTooLateinEpochDELEG slot tooLate

          let (potAmount, delta, instantaneousRewards) =
                case targetPot of
                  ReservesMIR -> (asReserves acnt, deltaReserves $ dsIRewards ds, iRReserves $ dsIRewards ds)
                  TreasuryMIR -> (asTreasury acnt, deltaTreasury $ dsIRewards ds, iRTreasury $ dsIRewards ds)
              credCoinMap' = Map.map (\(DeltaCoin x) -> Coin x) credCoinMap
              combinedMap = Map.unionWith (<>) credCoinMap' instantaneousRewards
              requiredForRewards = fold combinedMap
              available = potAmount `addDeltaCoin` delta

          all (>= mempty) combinedMap ?! MIRProducesNegativeUpdate

          requiredForRewards
            <= available
            ?! InsufficientForInstantaneousRewardsDELEG targetPot requiredForRewards available

          pure $
            case targetPot of
              ReservesMIR -> ds {dsIRewards = (dsIRewards ds) {iRReserves = combinedMap}}
              TreasuryMIR -> ds {dsIRewards = (dsIRewards ds) {iRTreasury = combinedMap}}
        else do
          sp <- liftSTS $ asks stabilityWindow
          ei <- liftSTS $ asks epochInfoPure
          EpochNo currEpoch <- liftSTS $ epochInfoEpoch ei slot
          let newEpoch = EpochNo (currEpoch + 1)
          tellEvent (NewEpoch newEpoch)
          firstSlot <- liftSTS $ epochInfoFirst ei newEpoch
          let tooLate = firstSlot *- Duration sp
          slot
            < tooLate
            ?! MIRCertificateTooLateinEpochDELEG slot tooLate

          all (>= mempty) credCoinMap ?! MIRNegativesNotCurrentlyAllowed

          let (potAmount, instantaneousRewards) =
                case targetPot of
                  ReservesMIR -> (asReserves acnt, iRReserves $ dsIRewards ds)
                  TreasuryMIR -> (asTreasury acnt, iRTreasury $ dsIRewards ds)
          let credCoinMap' = Map.map (\(DeltaCoin x) -> Coin x) credCoinMap
              combinedMap = Map.union credCoinMap' instantaneousRewards
              requiredForRewards = fold combinedMap
          requiredForRewards
            <= potAmount
            ?! InsufficientForInstantaneousRewardsDELEG targetPot requiredForRewards potAmount

          case targetPot of
            ReservesMIR -> pure $ ds {dsIRewards = (dsIRewards ds) {iRReserves = combinedMap}}
            TreasuryMIR -> pure $ ds {dsIRewards = (dsIRewards ds) {iRTreasury = combinedMap}}
    DCertMir (MIRCert targetPot (SendToOppositePotMIR coin)) ->
      if HardForks.allowMIRTransfer pp
        then do
          sp <- liftSTS $ asks stabilityWindow
          ei <- liftSTS $ asks epochInfoPure
          EpochNo currEpoch <- liftSTS $ epochInfoEpoch ei slot
          let newEpoch = EpochNo (currEpoch + 1)
          tellEvent (NewEpoch newEpoch)
          firstSlot <- liftSTS $ epochInfoFirst ei newEpoch
          let tooLate = firstSlot *- Duration sp
          slot
            < tooLate
            ?! MIRCertificateTooLateinEpochDELEG slot tooLate

          let available = availableAfterMIR targetPot acnt (dsIRewards ds)
          coin
            >= mempty
            ?! MIRNegativeTransfer targetPot coin
          coin
            <= available
            ?! InsufficientForTransferDELEG targetPot coin available

          let ir = dsIRewards ds
              dr = deltaReserves ir
              dt = deltaTreasury ir
          case targetPot of
            ReservesMIR ->
              pure $
                ds
                  { dsIRewards =
                      ir
                        { deltaReserves = dr <> invert (toDeltaCoin coin),
                          deltaTreasury = dt <> toDeltaCoin coin
                        }
                  }
            TreasuryMIR ->
              pure $
                ds
                  { dsIRewards =
                      ir
                        { deltaReserves = dr <> toDeltaCoin coin,
                          deltaTreasury = dt <> invert (toDeltaCoin coin)
                        }
                  }
        else do
          failBecause MIRTransferNotCurrentlyAllowed
          pure ds
    DCertPool _ -> do
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure ds
