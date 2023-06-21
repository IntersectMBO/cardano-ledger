{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Conway.Core (
  module X,
  ConwayEraTxBody (..),
  ConwayEraPParams (..),
  ppPoolVotingThresholdsL,
  ppDRepVotingThresholdsL,
  ppMinCommitteeSizeL,
  ppCommitteeTermLimitL,
  ppGovActionExpirationL,
  ppGovActionDepositL,
  ppDRepDepositL,
  ppDRepActivityL,
  ppuPoolVotingThresholdsL,
  ppuDRepVotingThresholdsL,
  ppuMinCommitteeSizeL,
  ppuCommitteeTermLimitL,
  ppuGovActionExpirationL,
  ppuGovActionDepositL,
  ppuDRepDepositL,
  ppuDRepActivityL,
  PoolVotingThresholds (..),
  DRepVotingThresholds (..),
)
where

import Cardano.Ledger.Babbage.Core as X
import Cardano.Ledger.BaseTypes (EpochNo, StrictMaybe, UnitInterval)
import Cardano.Ledger.Binary (DecCBOR, EncCBOR, decodeRecordNamed, encodeListLen)
import Cardano.Ledger.Binary.Decoding (DecCBOR (decCBOR))
import Cardano.Ledger.Binary.Encoding (EncCBOR (encCBOR))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Governance (ProposalProcedure, VotingProcedure)
import Cardano.Ledger.HKD (HKD, HKDFunctor)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Default.Class (Default)
import Data.Functor.Identity (Identity)
import Data.Sequence.Strict (StrictSeq (..))
import GHC.Generics (Generic)
import Lens.Micro (Lens')
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)

class BabbageEraTxBody era => ConwayEraTxBody era where
  votingProceduresTxBodyL :: Lens' (TxBody era) (StrictSeq (VotingProcedure era))
  proposalProceduresTxBodyL :: Lens' (TxBody era) (StrictSeq (ProposalProcedure era))

class BabbageEraPParams era => ConwayEraPParams era where
  hkdPoolVotingThresholdsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f PoolVotingThresholds)
  hkdDRepVotingThresholdsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f DRepVotingThresholds)
  hkdMinCommitteeSizeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)
  hkdCommitteeTermLimitL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)
  hkdGovActionExpirationL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)
  hkdGovActionDepositL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)
  hkdDRepDepositL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)
  hkdDRepActivityL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f EpochNo)

ppPoolVotingThresholdsL :: forall era. ConwayEraPParams era => Lens' (PParams era) PoolVotingThresholds
ppPoolVotingThresholdsL = ppLens . hkdPoolVotingThresholdsL @era @Identity

ppDRepVotingThresholdsL :: forall era. ConwayEraPParams era => Lens' (PParams era) DRepVotingThresholds
ppDRepVotingThresholdsL = ppLens . hkdDRepVotingThresholdsL @era @Identity

ppMinCommitteeSizeL :: forall era. ConwayEraPParams era => Lens' (PParams era) Natural
ppMinCommitteeSizeL = ppLens . hkdMinCommitteeSizeL @era @Identity

ppCommitteeTermLimitL :: forall era. ConwayEraPParams era => Lens' (PParams era) Natural
ppCommitteeTermLimitL = ppLens . hkdCommitteeTermLimitL @era @Identity

ppGovActionExpirationL :: forall era. ConwayEraPParams era => Lens' (PParams era) Natural
ppGovActionExpirationL = ppLens . hkdGovActionExpirationL @era @Identity

ppGovActionDepositL :: forall era. ConwayEraPParams era => Lens' (PParams era) Coin
ppGovActionDepositL = ppLens . hkdGovActionDepositL @era @Identity

ppDRepDepositL :: forall era. ConwayEraPParams era => Lens' (PParams era) Coin
ppDRepDepositL = ppLens . hkdDRepDepositL @era @Identity

ppDRepActivityL :: forall era. ConwayEraPParams era => Lens' (PParams era) EpochNo
ppDRepActivityL = ppLens . hkdDRepActivityL @era @Identity

ppuPoolVotingThresholdsL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe PoolVotingThresholds)
ppuPoolVotingThresholdsL = ppuLens . hkdPoolVotingThresholdsL @era @StrictMaybe

ppuDRepVotingThresholdsL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe DRepVotingThresholds)
ppuDRepVotingThresholdsL = ppuLens . hkdDRepVotingThresholdsL @era @StrictMaybe

ppuMinCommitteeSizeL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMinCommitteeSizeL = ppuLens . hkdMinCommitteeSizeL @era @StrictMaybe

ppuCommitteeTermLimitL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuCommitteeTermLimitL = ppuLens . hkdCommitteeTermLimitL @era @StrictMaybe

ppuGovActionExpirationL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuGovActionExpirationL = ppuLens . hkdGovActionExpirationL @era @StrictMaybe

ppuGovActionDepositL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuGovActionDepositL = ppuLens . hkdGovActionDepositL @era @StrictMaybe

ppuDRepDepositL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuDRepDepositL = ppuLens . hkdDRepDepositL @era @StrictMaybe

ppuDRepActivityL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe EpochNo)
ppuDRepActivityL = ppuLens . hkdDRepActivityL @era @StrictMaybe

data PoolVotingThresholds = PoolVotingThresholds
  { pvtMotionNoConfidence :: !UnitInterval
  , pvtCommitteeNormal :: !UnitInterval
  , pvtCommitteeNoConfidence :: !UnitInterval
  , pvtHardForkInitiation :: !UnitInterval
  }
  deriving (Eq, Ord, Show, Generic, Default, ToJSON, NFData, NoThunks)

instance EncCBOR PoolVotingThresholds where
  encCBOR PoolVotingThresholds {..} =
    encodeListLen 4
      <> encCBOR pvtMotionNoConfidence
      <> encCBOR pvtCommitteeNormal
      <> encCBOR pvtCommitteeNoConfidence
      <> encCBOR pvtHardForkInitiation

instance DecCBOR PoolVotingThresholds where
  decCBOR =
    decodeRecordNamed "PoolVotingThresholds" (const 4) $ do
      pvtMotionNoConfidence <- decCBOR
      pvtCommitteeNormal <- decCBOR
      pvtCommitteeNoConfidence <- decCBOR
      pvtHardForkInitiation <- decCBOR
      pure $ PoolVotingThresholds {..}

data DRepVotingThresholds = DRepVotingThresholds
  { dvtMotionNoConfidence :: !UnitInterval
  , dvtCommitteeNormal :: !UnitInterval
  , dvtCommitteeNoConfidence :: !UnitInterval
  , dvtUpdateToConstitution :: !UnitInterval
  , dvtHardForkInitiation :: !UnitInterval
  , dvtPPNetworkGroup :: !UnitInterval
  , dvtPPEconomicGroup :: !UnitInterval
  , dvtPPTechnicalGroup :: !UnitInterval
  , dvtPPGovernanceGroup :: !UnitInterval
  , dvtTreasuryWithdrawal :: !UnitInterval
  }
  deriving (Eq, Ord, Show, Generic, Default, ToJSON, NFData, NoThunks)

instance EncCBOR DRepVotingThresholds where
  encCBOR DRepVotingThresholds {..} =
    encodeListLen 10
      <> encCBOR dvtMotionNoConfidence
      <> encCBOR dvtCommitteeNormal
      <> encCBOR dvtCommitteeNoConfidence
      <> encCBOR dvtUpdateToConstitution
      <> encCBOR dvtHardForkInitiation
      <> encCBOR dvtPPNetworkGroup
      <> encCBOR dvtPPEconomicGroup
      <> encCBOR dvtPPTechnicalGroup
      <> encCBOR dvtPPGovernanceGroup
      <> encCBOR dvtTreasuryWithdrawal

instance DecCBOR DRepVotingThresholds where
  decCBOR =
    decodeRecordNamed "DRepVotingThresholds" (const 10) $ do
      dvtMotionNoConfidence <- decCBOR
      dvtCommitteeNormal <- decCBOR
      dvtCommitteeNoConfidence <- decCBOR
      dvtUpdateToConstitution <- decCBOR
      dvtHardForkInitiation <- decCBOR
      dvtPPNetworkGroup <- decCBOR
      dvtPPEconomicGroup <- decCBOR
      dvtPPTechnicalGroup <- decCBOR
      dvtPPGovernanceGroup <- decCBOR
      dvtTreasuryWithdrawal <- decCBOR
      pure $ DRepVotingThresholds {..}
