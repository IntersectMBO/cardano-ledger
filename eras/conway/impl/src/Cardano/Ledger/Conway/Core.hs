{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Conway.Core (
  module X,
  PoolVotingThresholds (..),
  DRepVotingThresholds (..),
  dvtPPNetworkGroupL,
  dvtPPGovGroupL,
  dvtPPTechnicalGroupL,
  dvtPPEconomicGroupL,
  dvtUpdateToConstitutionL,
)
where

import Cardano.Ledger.Babbage.Core as X
import Cardano.Ledger.BaseTypes (UnitInterval)
import Cardano.Ledger.Binary (DecCBOR, EncCBOR, decodeRecordNamed, encodeListLen)
import Cardano.Ledger.Binary.Decoding (DecCBOR (decCBOR))
import Cardano.Ledger.Binary.Encoding (EncCBOR (encCBOR))
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Default.Class (Default)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

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
  , dvtPPGovGroup :: !UnitInterval
  , dvtTreasuryWithdrawal :: !UnitInterval
  }
  deriving (Eq, Ord, Show, Generic, Default, ToJSON, NFData, NoThunks)

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
