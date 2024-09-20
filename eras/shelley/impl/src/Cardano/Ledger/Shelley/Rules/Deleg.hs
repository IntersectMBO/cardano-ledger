{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Deleg (
  ShelleyDELEG,
  DelegEnv (..),
  PredicateFailure,
  ShelleyDelegPredFailure (..),
  ShelleyDelegEvent (..),
)
where

import Cardano.Ledger.BaseTypes (
  Globals (..),
  ShelleyBase,
  StrictMaybe (..),
  epochInfoPure,
  invalidKey,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
 )
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..), addDeltaCoin, toDeltaCoin)
import Cardano.Ledger.Credential (Credential, Ptr)
import Cardano.Ledger.Keys (
  GenDelegPair (..),
  GenDelegs (..),
  Hash,
  KeyHash,
  KeyRole (..),
  VerKeyVRF,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyDELEG, ShelleyEra)
import Cardano.Ledger.Shelley.HardForks as HardForks (allowMIRTransfer)
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
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
import Cardano.Ledger.Slot (
  Duration (..),
  EpochNo (..),
  SlotNo,
  epochInfoEpoch,
  epochInfoFirst,
  (*-),
  (+*),
 )
import Cardano.Ledger.UMap (RDPair (..), UView (..), compactCoinOrError)
import qualified Cardano.Ledger.UMap as UM
import Control.DeepSeq
import Control.Monad (guard)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, range, singleton, (∉), (∪), (⨃))
import Control.State.Transition
import Data.Foldable (fold)
import Data.Group (Group (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data DelegEnv era = DelegEnv
  { slotNo :: !SlotNo
  , ptr_ :: !Ptr
  , acnt_ :: !AccountState
  , ppDE :: !(PParams era) -- The protocol parameters are only used for the HardFork mechanism
  }
  deriving (Generic)

deriving instance Show (PParams era) => Show (DelegEnv era)

deriving instance Eq (PParams era) => Eq (DelegEnv era)

instance NFData (PParams era) => NFData (DelegEnv era)

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
  | WrongCertificateTypeDELEG -- The TxCertPool constructor should not be used by this transition
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

type instance EraRuleFailure "DELEG" (ShelleyEra c) = ShelleyDelegPredFailure (ShelleyEra c)

instance InjectRuleFailure "DELEG" ShelleyDelegPredFailure (ShelleyEra c)

newtype ShelleyDelegEvent era = DelegNewEpoch EpochNo
  deriving (Generic, Eq)

instance NFData (ShelleyDelegEvent era)

instance (EraPParams era, ShelleyEraTxCert era, ProtVerAtMost era 8) => STS (ShelleyDELEG era) where
  type State (ShelleyDELEG era) = DState era
  type Signal (ShelleyDELEG era) = TxCert era
  type Environment (ShelleyDELEG era) = DelegEnv era
  type BaseM (ShelleyDELEG era) = ShelleyBase
  type PredicateFailure (ShelleyDELEG era) = ShelleyDelegPredFailure era
  type Event (ShelleyDELEG era) = ShelleyDelegEvent era

  transitionRules = [delegationTransition]

instance NoThunks (ShelleyDelegPredFailure era)

instance NFData (ShelleyDelegPredFailure era)

instance
  (Era era, Typeable (Script era)) =>
  EncCBOR (ShelleyDelegPredFailure era)
  where
  encCBOR = \case
    StakeKeyAlreadyRegisteredDELEG cred ->
      encodeListLen 2 <> encCBOR (0 :: Word8) <> encCBOR cred
    StakeKeyNotRegisteredDELEG cred ->
      encodeListLen 2 <> encCBOR (1 :: Word8) <> encCBOR cred
    StakeKeyNonZeroAccountBalanceDELEG rewardBalance ->
      encodeListLen 2 <> encCBOR (2 :: Word8) <> encCBOR rewardBalance
    StakeDelegationImpossibleDELEG cred ->
      encodeListLen 2 <> encCBOR (3 :: Word8) <> encCBOR cred
    WrongCertificateTypeDELEG ->
      encodeListLen 1 <> encCBOR (4 :: Word8)
    GenesisKeyNotInMappingDELEG gkh ->
      encodeListLen 2 <> encCBOR (5 :: Word8) <> encCBOR gkh
    DuplicateGenesisDelegateDELEG kh ->
      encodeListLen 2 <> encCBOR (6 :: Word8) <> encCBOR kh
    InsufficientForInstantaneousRewardsDELEG pot needed potAmount ->
      encodeListLen 4
        <> encCBOR (7 :: Word8)
        <> encCBOR pot
        <> encCBOR needed
        <> encCBOR potAmount
    MIRCertificateTooLateinEpochDELEG sNow sTooLate ->
      encodeListLen 3 <> encCBOR (8 :: Word8) <> encCBOR sNow <> encCBOR sTooLate
    DuplicateGenesisVRFDELEG vrf ->
      encodeListLen 2 <> encCBOR (9 :: Word8) <> encCBOR vrf
    StakeKeyInRewardsDELEG cred ->
      encodeListLen 2 <> encCBOR (10 :: Word8) <> encCBOR cred
    MIRTransferNotCurrentlyAllowed ->
      encodeListLen 1 <> encCBOR (11 :: Word8)
    MIRNegativesNotCurrentlyAllowed ->
      encodeListLen 1 <> encCBOR (12 :: Word8)
    InsufficientForTransferDELEG pot needed available ->
      encodeListLen 4
        <> encCBOR (13 :: Word8)
        <> encCBOR pot
        <> encCBOR needed
        <> encCBOR available
    MIRProducesNegativeUpdate ->
      encodeListLen 1 <> encCBOR (14 :: Word8)
    MIRNegativeTransfer pot amt ->
      encodeListLen 3
        <> encCBOR (15 :: Word8)
        <> encCBOR pot
        <> encCBOR amt

instance
  (Era era, Typeable (Script era)) =>
  DecCBOR (ShelleyDelegPredFailure era)
  where
  decCBOR = decodeRecordSum "ShelleyDelegPredFailure" $
    \case
      0 -> do
        kh <- decCBOR
        pure (2, StakeKeyAlreadyRegisteredDELEG kh)
      1 -> do
        kh <- decCBOR
        pure (2, StakeKeyNotRegisteredDELEG kh)
      2 -> do
        b <- decCBOR
        pure (2, StakeKeyNonZeroAccountBalanceDELEG b)
      3 -> do
        kh <- decCBOR
        pure (2, StakeDelegationImpossibleDELEG kh)
      4 -> do
        pure (1, WrongCertificateTypeDELEG)
      5 -> do
        gkh <- decCBOR
        pure (2, GenesisKeyNotInMappingDELEG gkh)
      6 -> do
        kh <- decCBOR
        pure (2, DuplicateGenesisDelegateDELEG kh)
      7 -> do
        pot <- decCBOR
        needed <- decCBOR
        potAmount <- decCBOR
        pure (4, InsufficientForInstantaneousRewardsDELEG pot needed potAmount)
      8 -> do
        sNow <- decCBOR
        sTooLate <- decCBOR
        pure (3, MIRCertificateTooLateinEpochDELEG sNow sTooLate)
      9 -> do
        vrf <- decCBOR
        pure (2, DuplicateGenesisVRFDELEG vrf)
      10 -> do
        kh <- decCBOR
        pure (2, StakeKeyInRewardsDELEG kh)
      11 -> do
        pure (1, MIRTransferNotCurrentlyAllowed)
      12 -> do
        pure (1, MIRNegativesNotCurrentlyAllowed)
      13 -> do
        pot <- decCBOR
        needed <- decCBOR
        available <- decCBOR
        pure (4, InsufficientForTransferDELEG pot needed available)
      14 -> do
        pure (1, MIRProducesNegativeUpdate)
      15 -> do
        pot <- decCBOR
        amt <- decCBOR
        pure (3, MIRNegativeTransfer pot amt)
      k -> invalidKey k

delegationTransition ::
  (ShelleyEraTxCert era, EraPParams era, ProtVerAtMost era 8) =>
  TransitionRule (ShelleyDELEG era)
delegationTransition = do
  TRC (DelegEnv slot ptr acnt pp, ds, c) <- judgmentContext
  let pv = pp ^. ppProtocolVersionL
  case c of
    RegTxCert hk -> do
      -- (hk ∉ dom (rewards ds))
      UM.notMember hk (rewards ds) ?! StakeKeyAlreadyRegisteredDELEG hk
      let u1 = dsUnified ds
          deposit = compactCoinOrError (pp ^. ppKeyDepositL)
          u2 = RewDepUView u1 UM.∪ (hk, RDPair (UM.CompactCoin 0) deposit)
          u3 = PtrUView u2 UM.∪ (ptr, hk)
      pure (ds {dsUnified = u3})
    UnRegTxCert cred -> do
      -- (hk ∈ dom (rewards ds))
      let (mUMElem, umap) = UM.extractStakingCredential cred (dsUnified ds)
          checkStakeKeyHasZeroRewardBalance = do
            UM.UMElem (SJust rd) _ _ _ <- mUMElem
            guard (UM.rdReward rd /= mempty)
            Just $ UM.fromCompact (UM.rdReward rd)
      isJust mUMElem ?! StakeKeyNotRegisteredDELEG cred
      failOnJust checkStakeKeyHasZeroRewardBalance (StakeKeyNonZeroAccountBalanceDELEG . Just)
      pure $ ds {dsUnified = umap}
    DelegStakeTxCert hk dpool -> do
      -- note that pattern match is used instead of cwitness and dpool, as in the spec
      -- (hk ∈ dom (rewards ds))
      UM.member hk (rewards ds) ?! StakeDelegationImpossibleDELEG hk

      pure (ds {dsUnified = delegations ds UM.⨃ Map.singleton hk dpool})
    GenesisDelegTxCert gkh vkh vrf -> do
      sp <- liftSTS $ asks stabilityWindow
      -- note that pattern match is used instead of genesisDeleg, as in the spec
      let s' = slot +* Duration sp
          GenDelegs genDelegs = dsGenDelegs ds

      -- gkh ∈ dom genDelegs ?! GenesisKeyNotInMappingDELEG gkh
      isJust (Map.lookup gkh genDelegs) ?! GenesisKeyNotInMappingDELEG gkh

      let cod = range $ Map.delete gkh genDelegs
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
    RegPoolTxCert _ -> do
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure ds
    _ | Just (MIRCert targetPot mirTarget) <- getMirTxCert c -> do
      checkSlotNotTooLate slot
      case mirTarget of
        StakeAddressesMIR credCoinMap -> do
          let (potAmount, delta, instantaneousRewards) =
                case targetPot of
                  ReservesMIR ->
                    (asReserves acnt, deltaReserves $ dsIRewards ds, iRReserves $ dsIRewards ds)
                  TreasuryMIR ->
                    (asTreasury acnt, deltaTreasury $ dsIRewards ds, iRTreasury $ dsIRewards ds)
          let credCoinMap' = Map.map (\(DeltaCoin x) -> Coin x) credCoinMap
          (combinedMap, available) <-
            if HardForks.allowMIRTransfer pv
              then do
                let cm = Map.unionWith (<>) credCoinMap' instantaneousRewards
                all (>= mempty) cm ?! MIRProducesNegativeUpdate
                pure (cm, potAmount `addDeltaCoin` delta)
              else do
                all (>= mempty) credCoinMap ?! MIRNegativesNotCurrentlyAllowed
                pure (Map.union credCoinMap' instantaneousRewards, potAmount)
          updateReservesAndTreasury targetPot combinedMap available ds
        SendToOppositePotMIR coin ->
          if HardForks.allowMIRTransfer pv
            then do
              let available = availableAfterMIR targetPot acnt (dsIRewards ds)
              coin >= mempty ?! MIRNegativeTransfer targetPot coin
              coin <= available ?! InsufficientForTransferDELEG targetPot coin available

              let ir = dsIRewards ds
                  dr = deltaReserves ir
                  dt = deltaTreasury ir
              case targetPot of
                ReservesMIR ->
                  pure $
                    ds
                      { dsIRewards =
                          ir
                            { deltaReserves = dr <> invert (toDeltaCoin coin)
                            , deltaTreasury = dt <> toDeltaCoin coin
                            }
                      }
                TreasuryMIR ->
                  pure $
                    ds
                      { dsIRewards =
                          ir
                            { deltaReserves = dr <> toDeltaCoin coin
                            , deltaTreasury = dt <> invert (toDeltaCoin coin)
                            }
                      }
            else do
              failBecause MIRTransferNotCurrentlyAllowed
              pure ds
    _ -> do
      -- The impossible case
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure ds

checkSlotNotTooLate ::
  (ShelleyEraTxCert era, EraPParams era, ProtVerAtMost era 8) =>
  SlotNo ->
  Rule (ShelleyDELEG era) 'Transition ()
checkSlotNotTooLate slot = do
  sp <- liftSTS $ asks stabilityWindow
  ei <- liftSTS $ asks epochInfoPure
  EpochNo currEpoch <- liftSTS $ epochInfoEpoch ei slot
  let newEpoch = EpochNo (currEpoch + 1)
  tellEvent (DelegNewEpoch newEpoch)
  firstSlot <- liftSTS $ epochInfoFirst ei newEpoch
  let tooLate = firstSlot *- Duration sp
  slot < tooLate ?! MIRCertificateTooLateinEpochDELEG slot tooLate

updateReservesAndTreasury ::
  MIRPot ->
  Map.Map (Credential 'Staking (EraCrypto era)) Coin ->
  Coin ->
  DState era ->
  Rule (ShelleyDELEG era) 'Transition (DState era)
updateReservesAndTreasury targetPot combinedMap available ds = do
  let requiredForRewards = fold combinedMap
  requiredForRewards
    <= available
      ?! InsufficientForInstantaneousRewardsDELEG targetPot requiredForRewards available
  pure $
    case targetPot of
      ReservesMIR -> ds {dsIRewards = (dsIRewards ds) {iRReserves = combinedMap}}
      TreasuryMIR -> ds {dsIRewards = (dsIRewards ds) {iRTreasury = combinedMap}}
