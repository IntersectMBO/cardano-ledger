{-# LANGUAGE BangPatterns #-}
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
) where

import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  Globals (..),
  Mismatch (..),
  Relation (..),
  ShelleyBase,
  addEpochInterval,
  epochInfoPure,
  invalidKey,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
 )
import Cardano.Ledger.Coin (
  Coin (..),
  DeltaCoin (..),
  addDeltaCoin,
  compactCoinOrError,
  toDeltaCoin,
 )
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Credential (Credential, Ptr)
import Cardano.Ledger.Hashes (GenDelegPair (..), GenDelegs (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Era (ShelleyDELEG, ShelleyEra, hardforkAlonzoAllowMIRTransfer)
import Cardano.Ledger.Shelley.LedgerState (availableAfterMIR)
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Slot (
  Duration (..),
  EpochNo (..),
  SlotNo,
  epochInfoFirst,
  (*-),
  (+*),
 )
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
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data DelegEnv era = DelegEnv
  { slotNo :: SlotNo
  , deCurEpochNo :: EpochNo
  -- ^ Lazy on purpose, because not all certificates need to know the current EpochNo
  , ptr_ :: Ptr
  , deChainAccountState :: ChainAccountState
  , ppDE :: PParams era -- The protocol parameters are only used for the HardFork mechanism
  }
  deriving (Generic)

deriving instance Show (PParams era) => Show (DelegEnv era)

deriving instance Eq (PParams era) => Eq (DelegEnv era)

instance NFData (PParams era) => NFData (DelegEnv era)

data ShelleyDelegPredFailure era
  = StakeKeyAlreadyRegisteredDELEG
      (Credential 'Staking) -- Credential which is already registered
  | StakeKeyNotRegisteredDELEG
      (Credential 'Staking) -- Credential which is not registered
  | StakeKeyNonZeroAccountBalanceDELEG
      Coin -- The remaining reward account balance
  | StakeDelegationImpossibleDELEG
      (Credential 'Staking) -- Credential that is not registered
  | WrongCertificateTypeDELEG -- The TxCertPool constructor should not be used by this transition
  | GenesisKeyNotInMappingDELEG
      (KeyHash 'Genesis) -- Unknown Genesis KeyHash
  | DuplicateGenesisDelegateDELEG
      (KeyHash 'GenesisDelegate) -- Keyhash which is already delegated to
  | InsufficientForInstantaneousRewardsDELEG
      MIRPot -- which pot the rewards are to be drawn from, treasury or reserves
      (Mismatch 'RelLTEQ Coin)
  | MIRCertificateTooLateinEpochDELEG
      (Mismatch 'RelLT SlotNo)
  | DuplicateGenesisVRFDELEG
      (VRFVerKeyHash 'GenDelegVRF) -- VRF KeyHash which is already delegated to
  | MIRTransferNotCurrentlyAllowed
  | MIRNegativesNotCurrentlyAllowed
  | InsufficientForTransferDELEG
      MIRPot -- which pot the rewards are to be drawn from, treasury or reserves
      (Mismatch 'RelLTEQ Coin)
  | MIRProducesNegativeUpdate
  | MIRNegativeTransfer
      MIRPot -- which pot the rewards are to be drawn from, treasury or reserves
      Coin -- amount attempted to transfer
  deriving (Show, Eq, Generic)

type instance EraRuleFailure "DELEG" ShelleyEra = ShelleyDelegPredFailure ShelleyEra

instance InjectRuleFailure "DELEG" ShelleyDelegPredFailure ShelleyEra

newtype ShelleyDelegEvent era = DelegNewEpoch EpochNo
  deriving (Generic, Eq)

instance NFData (ShelleyDelegEvent era)

instance
  ( EraCertState era
  , EraPParams era
  , ShelleyEraAccounts era
  , ShelleyEraTxCert era
  , AtMostEra "Babbage" era
  ) =>
  STS (ShelleyDELEG era)
  where
  type State (ShelleyDELEG era) = CertState era
  type Signal (ShelleyDELEG era) = TxCert era
  type Environment (ShelleyDELEG era) = DelegEnv era
  type BaseM (ShelleyDELEG era) = ShelleyBase
  type PredicateFailure (ShelleyDELEG era) = ShelleyDelegPredFailure era
  type Event (ShelleyDELEG era) = ShelleyDelegEvent era

  transitionRules = [delegationTransition]

instance NoThunks (ShelleyDelegPredFailure era)

instance NFData (ShelleyDelegPredFailure era)

instance Era era => EncCBOR (ShelleyDelegPredFailure era) where
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
    InsufficientForInstantaneousRewardsDELEG pot m ->
      encodeListLen 3
        <> encCBOR (7 :: Word8)
        <> encCBOR pot
        <> encCBOR m
    MIRCertificateTooLateinEpochDELEG m ->
      encodeListLen 2 <> encCBOR (8 :: Word8) <> encCBOR m
    DuplicateGenesisVRFDELEG vrf ->
      encodeListLen 2 <> encCBOR (9 :: Word8) <> encCBOR vrf
    MIRTransferNotCurrentlyAllowed ->
      encodeListLen 1 <> encCBOR (11 :: Word8)
    MIRNegativesNotCurrentlyAllowed ->
      encodeListLen 1 <> encCBOR (12 :: Word8)
    InsufficientForTransferDELEG pot m ->
      encodeListLen 3
        <> encCBOR (13 :: Word8)
        <> encCBOR pot
        <> encCBOR m
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
        m <- decCBOR
        pure (3, InsufficientForInstantaneousRewardsDELEG pot m)
      8 -> do
        m <- decCBOR
        pure (2, MIRCertificateTooLateinEpochDELEG m)
      9 -> do
        vrf <- decCBOR
        pure (2, DuplicateGenesisVRFDELEG vrf)
      11 -> do
        pure (1, MIRTransferNotCurrentlyAllowed)
      12 -> do
        pure (1, MIRNegativesNotCurrentlyAllowed)
      13 -> do
        pot <- decCBOR
        m <- decCBOR
        pure (3, InsufficientForTransferDELEG pot m)
      14 -> do
        pure (1, MIRProducesNegativeUpdate)
      15 -> do
        pot <- decCBOR
        amt <- decCBOR
        pure (3, MIRNegativeTransfer pot amt)
      k -> invalidKey k

delegationTransition ::
  ( EraCertState era
  , ShelleyEraAccounts era
  , ShelleyEraTxCert era
  , EraPParams era
  , AtMostEra "Babbage" era
  ) =>
  TransitionRule (ShelleyDELEG era)
delegationTransition = do
  TRC (DelegEnv slot epochNo ptr chainAccountState pp, certState, c) <- judgmentContext
  let pv = pp ^. ppProtocolVersionL
      ds = certState ^. certDStateL
  case c of
    RegTxCert cred -> do
      -- (hk ∉ dom (rewards ds))
      not (isAccountRegistered cred (ds ^. accountsL)) ?! StakeKeyAlreadyRegisteredDELEG cred
      let compactDeposit = compactCoinOrError (pp ^. ppKeyDepositL)
      pure $ certState & certDStateL . accountsL %~ registerShelleyAccount cred ptr compactDeposit Nothing
    UnRegTxCert cred -> do
      let !(!mAccountState, !accounts) = unregisterShelleyAccount cred (ds ^. accountsL)
          checkStakeKeyHasZeroRewardBalance = do
            accountState <- mAccountState
            let accountBalance = accountState ^. balanceAccountStateL
            guard (accountBalance /= mempty)
            Just $ fromCompact accountBalance
      failOnJust checkStakeKeyHasZeroRewardBalance StakeKeyNonZeroAccountBalanceDELEG
      -- (hk ∈ dom (rewards ds))
      case mAccountState of
        Nothing -> do
          failBecause $ StakeKeyNotRegisteredDELEG cred
          pure certState
        Just accountState ->
          pure $
            certState
              & certDStateL . accountsL .~ accounts
              & certPStateL
                %~ unDelegReDelegStakePool cred accountState Nothing
    DelegStakeTxCert cred stakePool -> do
      -- note that pattern match is used instead of cwitness and dpool, as in the spec
      -- (hk ∈ dom (rewards ds))
      case lookupAccountState cred (ds ^. accountsL) of
        Nothing -> do
          failBecause $ StakeDelegationImpossibleDELEG cred
          pure certState
        Just accountState ->
          pure $
            certState
              & certDStateL . accountsL %~ adjustAccountState (stakePoolDelegationAccountStateL ?~ stakePool) cred
              & certPStateL %~ unDelegReDelegStakePool cred accountState (Just stakePool)
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
        certState
          & certDStateL . dsFutureGenDelegsL
            .~ eval (dsFutureGenDelegs ds ⨃ singleton (FutureGenDeleg s' gkh) (GenDelegPair vkh vrf))
    RegPoolTxCert _ -> do
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure certState
    _ | Just (MIRCert targetPot mirTarget) <- getMirTxCert c -> do
      checkSlotNotTooLate slot epochNo
      case mirTarget of
        StakeAddressesMIR credCoinMap -> do
          let (potAmount, delta, instantaneousRewards) =
                case targetPot of
                  ReservesMIR ->
                    ( casReserves chainAccountState
                    , deltaReserves $ dsIRewards ds
                    , iRReserves $ dsIRewards ds
                    )
                  TreasuryMIR ->
                    ( casTreasury chainAccountState
                    , deltaTreasury $ dsIRewards ds
                    , iRTreasury $ dsIRewards ds
                    )
          let credCoinMap' = Map.map (\(DeltaCoin x) -> Coin x) credCoinMap
          (combinedMap, available) <-
            if hardforkAlonzoAllowMIRTransfer pv
              then do
                let cm = Map.unionWith (<>) credCoinMap' instantaneousRewards
                all (>= mempty) cm ?! MIRProducesNegativeUpdate
                pure (cm, potAmount `addDeltaCoin` delta)
              else do
                all (>= mempty) credCoinMap ?! MIRNegativesNotCurrentlyAllowed
                pure (Map.union credCoinMap' instantaneousRewards, potAmount)
          updateReservesAndTreasury targetPot combinedMap available certState
        SendToOppositePotMIR coin ->
          if hardforkAlonzoAllowMIRTransfer pv
            then do
              let available = availableAfterMIR targetPot chainAccountState (dsIRewards ds)
              coin >= mempty ?! MIRNegativeTransfer targetPot coin
              coin <= available ?! InsufficientForTransferDELEG targetPot (Mismatch coin available)
              case targetPot of
                ReservesMIR ->
                  pure $
                    certState
                      & certDStateL . dsIRewardsL . iRDeltaReservesL <>~ invert (toDeltaCoin coin)
                      & certDStateL . dsIRewardsL . iRDeltaTreasuryL <>~ toDeltaCoin coin
                TreasuryMIR ->
                  pure $
                    certState
                      & certDStateL . dsIRewardsL . iRDeltaReservesL <>~ toDeltaCoin coin
                      & certDStateL . dsIRewardsL . iRDeltaTreasuryL <>~ invert (toDeltaCoin coin)
            else do
              failBecause MIRTransferNotCurrentlyAllowed
              pure certState
    _ -> do
      -- The impossible case
      failBecause WrongCertificateTypeDELEG -- this always fails
      pure certState

checkSlotNotTooLate ::
  ( EraCertState era
  , ShelleyEraAccounts era
  , ShelleyEraTxCert era
  , EraPParams era
  , AtMostEra "Babbage" era
  ) =>
  SlotNo ->
  EpochNo ->
  Rule (ShelleyDELEG era) 'Transition ()
checkSlotNotTooLate slot curEpochNo = do
  sp <- liftSTS $ asks stabilityWindow
  ei <- liftSTS $ asks epochInfoPure
  let firstSlot = epochInfoFirst ei newEpoch
      tooLate = firstSlot *- Duration sp
      newEpoch = addEpochInterval curEpochNo (EpochInterval 1)
  tellEvent (DelegNewEpoch newEpoch)
  slot < tooLate ?! MIRCertificateTooLateinEpochDELEG (Mismatch slot tooLate)

updateReservesAndTreasury ::
  EraCertState era =>
  MIRPot ->
  Map.Map (Credential 'Staking) Coin ->
  Coin ->
  CertState era ->
  Rule (ShelleyDELEG era) 'Transition (CertState era)
updateReservesAndTreasury targetPot combinedMap available certState = do
  let requiredForRewards = fold combinedMap
  requiredForRewards
    <= available
      ?! InsufficientForInstantaneousRewardsDELEG
        targetPot
        Mismatch
          { mismatchSupplied = requiredForRewards
          , mismatchExpected = available
          }
  pure $
    case targetPot of
      ReservesMIR -> certState & certDStateL . dsIRewardsL . iRReservesL .~ combinedMap
      TreasuryMIR -> certState & certDStateL . dsIRewardsL . iRTreasuryL .~ combinedMap
