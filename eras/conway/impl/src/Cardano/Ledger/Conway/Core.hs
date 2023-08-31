{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
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
  PParamGroup (..),
  ParamGrouper,
  pGroup,
  pUngrouped,
  modifiedGroups,
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
  dvtPPNetworkGroupL,
  dvtPPGovGroupL,
  dvtPPTechnicalGroupL,
  dvtPPEconomicGroupL,
  dvtUpdateToConstitutionL,
)
where

import Cardano.Ledger.Ap (Ap, runAp_)
import Cardano.Ledger.Babbage.Core as X
import Cardano.Ledger.BaseTypes (EpochNo, StrictMaybe (..), UnitInterval)
import Cardano.Ledger.Binary (DecCBOR, EncCBOR, decodeRecordNamed, encodeListLen)
import Cardano.Ledger.Binary.Decoding (DecCBOR (decCBOR))
import Cardano.Ledger.Binary.Encoding (EncCBOR (encCBOR))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Governance.Procedures (ProposalProcedure, VotingProcedures)
import Cardano.Ledger.HKD (HKD, HKDFunctor)
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Default.Class (Default)
import Data.Functor.Identity (Identity)
import Data.Sequence.Strict (StrictSeq (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)

class BabbageEraTxBody era => ConwayEraTxBody era where
  -- | Lens for getting and setting number of `Coin` that is expected to be in the
  -- Treasury at the current Epoch
  currentTreasuryValueTxBodyL :: Lens' (TxBody era) (StrictMaybe Coin)

  -- | Lens for getting and setting `VotingProcedures`.
  votingProceduresTxBodyL :: Lens' (TxBody era) (VotingProcedures era)

  -- | Lens for getting and setting `ProposalProcedures`.
  proposalProceduresTxBodyL :: Lens' (TxBody era) (StrictSeq (ProposalProcedure era))

  treasuryDonationTxBodyL :: Lens' (TxBody era) Coin

data PParamGroup
  = EconomicGroup
  | NetworkGroup
  | TechnicalGroup
  | GovernanceGroup
  deriving (Eq, Ord)

newtype ParamGrouper a = ParamGrouper {unParamGrouper :: Set PParamGroup}
  deriving (Functor)

pGroup :: PParamGroup -> StrictMaybe a -> Ap f (ParamGrouper a)
pGroup pg (SJust _) = pure . ParamGrouper $ Set.singleton pg
pGroup _ SNothing = pure $ ParamGrouper mempty

pUngrouped :: Ap f (ParamGrouper a)
pUngrouped = pure $ ParamGrouper mempty

modifiedGroups ::
  forall era.
  ConwayEraPParams era =>
  PParamsUpdate era ->
  Set PParamGroup
modifiedGroups = runAp_ unParamGrouper . (pparamsGroups @era)

class BabbageEraPParams era => ConwayEraPParams era where
  pparamsGroups ::
    Functor f => PParamsUpdate era -> Ap f (PParamsHKD ParamGrouper era)
  ppuWellFormed :: PParamsUpdate era -> Bool

  hkdPoolVotingThresholdsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f PoolVotingThresholds)
  hkdDRepVotingThresholdsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f DRepVotingThresholds)
  hkdMinCommitteeSizeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)
  hkdCommitteeTermLimitL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)
  hkdGovActionExpirationL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f EpochNo)
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

ppGovActionExpirationL :: forall era. ConwayEraPParams era => Lens' (PParams era) EpochNo
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

ppuGovActionExpirationL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe EpochNo)
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

instance ToExpr PoolVotingThresholds

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
  , dvtPPGovGroup :: !UnitInterval
  , dvtTreasuryWithdrawal :: !UnitInterval
  }
  deriving (Eq, Ord, Show, Generic, Default, ToJSON, NFData, NoThunks)

instance ToExpr DRepVotingThresholds

dvtPPNetworkGroupL :: Lens' DRepVotingThresholds UnitInterval
dvtPPNetworkGroupL = lens dvtPPNetworkGroup (\x y -> x {dvtPPNetworkGroup = y})

dvtPPEconomicGroupL :: Lens' DRepVotingThresholds UnitInterval
dvtPPEconomicGroupL = lens dvtPPEconomicGroup (\x y -> x {dvtPPEconomicGroup = y})

dvtPPTechnicalGroupL :: Lens' DRepVotingThresholds UnitInterval
dvtPPTechnicalGroupL = lens dvtPPTechnicalGroup (\x y -> x {dvtPPTechnicalGroup = y})

dvtPPGovGroupL :: Lens' DRepVotingThresholds UnitInterval
dvtPPGovGroupL = lens dvtPPGovGroup (\x y -> x {dvtPPGovGroup = y})

dvtUpdateToConstitutionL :: Lens' DRepVotingThresholds UnitInterval
dvtUpdateToConstitutionL = lens dvtUpdateToConstitution (\x y -> x {dvtUpdateToConstitution = y})

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
      <> encCBOR dvtPPGovGroup
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
      dvtPPGovGroup <- decCBOR
      dvtTreasuryWithdrawal <- decCBOR
      pure $ DRepVotingThresholds {..}
