{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains the type of protocol parameters and EraPParams instance
module Cardano.Ledger.Conway.PParams (
  ConwayEraPParams (..),
  ppCommitteeMaxTermLength,
  ppCommitteeMinSize,
  ppDRepActivity,
  ppDRepDeposit,
  ppDRepVotingThresholds,
  ppGovActionDeposit,
  ppGovActionLifetime,
  ppGovProtocolVersion,
  ppMinFeeRefScriptCostPerByte,
  ppPoolVotingThresholds,
  ppPoolVotingThresholdsL,
  ppDRepVotingThresholdsL,
  ppCommitteeMinSizeL,
  ppCommitteeMaxTermLengthL,
  ppGovActionLifetimeL,
  ppGovActionDepositL,
  ppDRepDepositL,
  ppDRepActivityL,
  ppMinFeeRefScriptCostPerByteL,
  ppuPoolVotingThresholdsL,
  ppuDRepVotingThresholdsL,
  ppuCommitteeMinSizeL,
  ppuCommitteeMaxTermLengthL,
  ppuGovActionLifetimeL,
  ppuGovActionDepositL,
  ppuDRepDepositL,
  ppuDRepActivityL,
  ppuMinFeeRefScriptCostPerByteL,
  PoolVotingThresholds (..),
  pvtCommitteeNoConfidenceL,
  pvtCommitteeNormalL,
  pvtPPSecurityGroupL,
  DRepVotingThresholds (..),
  dvtCommitteeNoConfidenceL,
  dvtCommitteeNormalL,
  dvtHardForkInitiationL,
  dvtMotionNoConfidenceL,
  dvtPPNetworkGroupL,
  dvtPPGovGroupL,
  dvtPPTechnicalGroupL,
  dvtPPEconomicGroupL,
  dvtTreasuryWithdrawalL,
  dvtUpdateToConstitutionL,
  ConwayPParams (..),
  getLanguageView,
  LangDepView (..),
  encodeLangViews,
  upgradeConwayPParams,
  UpgradeConwayPParams (..),
  toUpgradeConwayPParamsUpdatePairs,
  THKD (..),
  DRepGroup (..),
  PPGroups (..),
  StakePoolGroup (..),
  conwayModifiedPPGroups,
  pvtHardForkInitiationL,
  pvtMotionNoConfidenceL,
  conwayApplyPPUpdates,
  emptyConwayPParams,
  emptyConwayPParamsUpdate,
  asNaturalHKD,
  asBoundedIntegralHKD,
) where

import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Scripts (
  CostModels,
  ExUnits (..),
  Prices (Prices),
  emptyCostModels,
  updateCostModels,
 )
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  NonNegativeInterval,
  ProtVer (ProtVer),
  UnitInterval,
  integralToBounded,
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  encodeListLen,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Core (EraPParams (..))
import Cardano.Ledger.HKD (
  HKDApplicative (hkdLiftA2),
  HKDFunctor (..),
  HKDNoUpdate,
  NoUpdate (..),
 )
import Cardano.Ledger.Plutus.CostModels (
  CostModel,
  decodeCostModel,
  encodeCostModel,
  mkCostModel,
  mkCostModels,
 )
import Cardano.Ledger.Plutus.Language (Language (PlutusV3))
import Cardano.Ledger.Plutus.ToPlutusData (ToPlutusData (..))
import Cardano.Ledger.Shelley.HardForks (bootstrapPhase)
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Aeson hiding (Encoding, Value, decode, encode)
import qualified Data.Aeson as Aeson
import Data.Default (Default (def))
import Data.Foldable (foldlM)
import Data.Functor.Identity (Identity)
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.Common as P (Data (..))

class BabbageEraPParams era => ConwayEraPParams era where
  modifiedPPGroups :: PParamsUpdate era -> Set PPGroups
  ppuWellFormed :: ProtVer -> PParamsUpdate era -> Bool

  hkdPoolVotingThresholdsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f PoolVotingThresholds)
  hkdDRepVotingThresholdsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f DRepVotingThresholds)
  hkdCommitteeMinSizeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)
  hkdCommitteeMaxTermLengthL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f EpochInterval)
  hkdGovActionLifetimeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f EpochInterval)
  hkdGovActionDepositL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)
  hkdDRepDepositL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)
  hkdDRepActivityL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f EpochInterval)
  hkdMinFeeRefScriptCostPerByteL ::
    HKDFunctor f => Lens' (PParamsHKD f era) (HKD f NonNegativeInterval)

instance ConwayEraPParams era => ToPlutusData (PParamsUpdate era) where
  toPlutusData ppu = P.Map $ mapMaybe ppToData (eraPParams @era)
    where
      ppToData PParam {ppUpdate} = do
        PParamUpdate {ppuTag, ppuLens} <- ppUpdate
        t <- strictMaybeToMaybe $ ppu ^. ppuLens
        pure (P.I (toInteger @Word ppuTag), toPlutusData t)

  fromPlutusData (P.Map dataPairs) = foldlM accum emptyPParamsUpdate dataPairs
    where
      accum acc (dataKey, dataVal) = do
        tag <- fromPlutusData @Word dataKey
        PParam {ppUpdate} <-
          IntMap.lookup (fromIntegral tag) ppMap
        PParamUpdate {ppuLens} <- ppUpdate
        plutusData <- fromPlutusData dataVal
        pure $ set ppuLens (SJust plutusData) acc
      ppMap =
        IntMap.fromList
          [ (fromIntegral ppuTag, pp)
          | pp@PParam {ppUpdate = Just PParamUpdate {ppuTag}} <- eraPParams @era
          ]
  fromPlutusData _ = Nothing

ppPoolVotingThresholdsL ::
  forall era. ConwayEraPParams era => Lens' (PParams era) PoolVotingThresholds
ppPoolVotingThresholdsL = ppLensHKD . hkdPoolVotingThresholdsL @era @Identity

ppDRepVotingThresholdsL ::
  forall era. ConwayEraPParams era => Lens' (PParams era) DRepVotingThresholds
ppDRepVotingThresholdsL = ppLensHKD . hkdDRepVotingThresholdsL @era @Identity

ppCommitteeMinSizeL :: forall era. ConwayEraPParams era => Lens' (PParams era) Natural
ppCommitteeMinSizeL = ppLensHKD . hkdCommitteeMinSizeL @era @Identity

ppCommitteeMaxTermLengthL :: forall era. ConwayEraPParams era => Lens' (PParams era) EpochInterval
ppCommitteeMaxTermLengthL = ppLensHKD . hkdCommitteeMaxTermLengthL @era @Identity

ppGovActionLifetimeL :: forall era. ConwayEraPParams era => Lens' (PParams era) EpochInterval
ppGovActionLifetimeL = ppLensHKD . hkdGovActionLifetimeL @era @Identity

ppGovActionDepositL :: forall era. ConwayEraPParams era => Lens' (PParams era) Coin
ppGovActionDepositL = ppLensHKD . hkdGovActionDepositL @era @Identity

ppDRepDepositL :: forall era. ConwayEraPParams era => Lens' (PParams era) Coin
ppDRepDepositL = ppLensHKD . hkdDRepDepositL @era @Identity

ppDRepActivityL :: forall era. ConwayEraPParams era => Lens' (PParams era) EpochInterval
ppDRepActivityL = ppLensHKD . hkdDRepActivityL @era @Identity

ppMinFeeRefScriptCostPerByteL ::
  forall era. ConwayEraPParams era => Lens' (PParams era) NonNegativeInterval
ppMinFeeRefScriptCostPerByteL = ppLensHKD . hkdMinFeeRefScriptCostPerByteL @era @Identity

ppuPoolVotingThresholdsL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe PoolVotingThresholds)
ppuPoolVotingThresholdsL = ppuLensHKD . hkdPoolVotingThresholdsL @era @StrictMaybe

ppuDRepVotingThresholdsL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe DRepVotingThresholds)
ppuDRepVotingThresholdsL = ppuLensHKD . hkdDRepVotingThresholdsL @era @StrictMaybe

ppuCommitteeMinSizeL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuCommitteeMinSizeL = ppuLensHKD . hkdCommitteeMinSizeL @era @StrictMaybe

ppuCommitteeMaxTermLengthL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe EpochInterval)
ppuCommitteeMaxTermLengthL = ppuLensHKD . hkdCommitteeMaxTermLengthL @era @StrictMaybe

ppuGovActionLifetimeL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe EpochInterval)
ppuGovActionLifetimeL = ppuLensHKD . hkdGovActionLifetimeL @era @StrictMaybe

ppuGovActionDepositL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuGovActionDepositL = ppuLensHKD . hkdGovActionDepositL @era @StrictMaybe

ppuDRepDepositL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuDRepDepositL = ppuLensHKD . hkdDRepDepositL @era @StrictMaybe

ppuDRepActivityL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe EpochInterval)
ppuDRepActivityL = ppuLensHKD . hkdDRepActivityL @era @StrictMaybe

ppuMinFeeRefScriptCostPerByteL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe NonNegativeInterval)
ppuMinFeeRefScriptCostPerByteL = ppuLensHKD . hkdMinFeeRefScriptCostPerByteL @era @StrictMaybe

data PoolVotingThresholds = PoolVotingThresholds
  { pvtMotionNoConfidence :: !UnitInterval
  , pvtCommitteeNormal :: !UnitInterval
  , pvtCommitteeNoConfidence :: !UnitInterval
  , pvtHardForkInitiation :: !UnitInterval
  , pvtPPSecurityGroup :: !UnitInterval
  }
  deriving (Eq, Ord, Show, Generic)

pvtMotionNoConfidenceL :: Lens' PoolVotingThresholds UnitInterval
pvtMotionNoConfidenceL = lens pvtMotionNoConfidence (\x y -> x {pvtMotionNoConfidence = y})

pvtCommitteeNormalL :: Lens' PoolVotingThresholds UnitInterval
pvtCommitteeNormalL = lens pvtCommitteeNormal (\x y -> x {pvtCommitteeNormal = y})

pvtCommitteeNoConfidenceL :: Lens' PoolVotingThresholds UnitInterval
pvtCommitteeNoConfidenceL = lens pvtCommitteeNoConfidence (\x y -> x {pvtCommitteeNoConfidence = y})

pvtPPSecurityGroupL :: Lens' PoolVotingThresholds UnitInterval
pvtPPSecurityGroupL = lens pvtPPSecurityGroup (\x y -> x {pvtPPSecurityGroup = y})

pvtHardForkInitiationL :: Lens' PoolVotingThresholds UnitInterval
pvtHardForkInitiationL = lens pvtHardForkInitiation (\x y -> x {pvtHardForkInitiation = y})

instance NoThunks PoolVotingThresholds

instance NFData PoolVotingThresholds where
  rnf = rwhnf

instance Default PoolVotingThresholds where
  def = PoolVotingThresholds def def def def def

instance ToJSON PoolVotingThresholds where
  toJSON pvt@(PoolVotingThresholds _ _ _ _ _) =
    let PoolVotingThresholds {..} = pvt
     in object
          [ ("motionNoConfidence", toJSON pvtMotionNoConfidence)
          , ("committeeNormal", toJSON pvtCommitteeNormal)
          , ("committeeNoConfidence", toJSON pvtCommitteeNoConfidence)
          , ("hardForkInitiation", toJSON pvtHardForkInitiation)
          , ("ppSecurityGroup", toJSON pvtPPSecurityGroup)
          ]

instance FromJSON PoolVotingThresholds where
  parseJSON =
    withObject "PoolVotingThresholds" $ \o ->
      PoolVotingThresholds
        <$> o .: "motionNoConfidence"
        <*> o .: "committeeNormal"
        <*> o .: "committeeNoConfidence"
        <*> o .: "hardForkInitiation"
        <*> o .: "ppSecurityGroup"

instance EncCBOR PoolVotingThresholds where
  encCBOR PoolVotingThresholds {..} =
    encodeListLen 5
      <> encCBOR pvtMotionNoConfidence
      <> encCBOR pvtCommitteeNormal
      <> encCBOR pvtCommitteeNoConfidence
      <> encCBOR pvtHardForkInitiation
      <> encCBOR pvtPPSecurityGroup

instance DecCBOR PoolVotingThresholds where
  decCBOR =
    decodeRecordNamed "PoolVotingThresholds" (const 5) $ do
      pvtMotionNoConfidence <- decCBOR
      pvtCommitteeNormal <- decCBOR
      pvtCommitteeNoConfidence <- decCBOR
      pvtHardForkInitiation <- decCBOR
      pvtPPSecurityGroup <- decCBOR
      pure $ PoolVotingThresholds {..}

instance ToPlutusData PoolVotingThresholds where
  toPlutusData x =
    P.List
      [ toPlutusData (pvtMotionNoConfidence x)
      , toPlutusData (pvtCommitteeNormal x)
      , toPlutusData (pvtCommitteeNoConfidence x)
      , toPlutusData (pvtHardForkInitiation x)
      , toPlutusData (pvtPPSecurityGroup x)
      ]
  fromPlutusData (P.List [a, b, c, d, e]) =
    PoolVotingThresholds
      <$> fromPlutusData a
      <*> fromPlutusData b
      <*> fromPlutusData c
      <*> fromPlutusData d
      <*> fromPlutusData e
  fromPlutusData _ = Nothing

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
  deriving (Eq, Ord, Show, Generic)

instance NoThunks DRepVotingThresholds

instance NFData DRepVotingThresholds where
  rnf = rwhnf

instance Default DRepVotingThresholds where
  def = DRepVotingThresholds def def def def def def def def def def

instance ToJSON DRepVotingThresholds where
  toJSON pvt@(DRepVotingThresholds _ _ _ _ _ _ _ _ _ _) =
    let DRepVotingThresholds {..} = pvt
     in object
          [ ("motionNoConfidence", toJSON dvtMotionNoConfidence)
          , ("committeeNormal", toJSON dvtCommitteeNormal)
          , ("committeeNoConfidence", toJSON dvtCommitteeNoConfidence)
          , ("updateToConstitution", toJSON dvtUpdateToConstitution)
          , ("hardForkInitiation", toJSON dvtHardForkInitiation)
          , ("ppNetworkGroup", toJSON dvtPPNetworkGroup)
          , ("ppEconomicGroup", toJSON dvtPPEconomicGroup)
          , ("ppTechnicalGroup", toJSON dvtPPTechnicalGroup)
          , ("ppGovGroup", toJSON dvtPPGovGroup)
          , ("treasuryWithdrawal", toJSON dvtTreasuryWithdrawal)
          ]

instance FromJSON DRepVotingThresholds where
  parseJSON =
    withObject "DRepVotingThresholds" $ \o ->
      DRepVotingThresholds
        <$> o .: "motionNoConfidence"
        <*> o .: "committeeNormal"
        <*> o .: "committeeNoConfidence"
        <*> o .: "updateToConstitution"
        <*> o .: "hardForkInitiation"
        <*> o .: "ppNetworkGroup"
        <*> o .: "ppEconomicGroup"
        <*> o .: "ppTechnicalGroup"
        <*> o .: "ppGovGroup"
        <*> o .: "treasuryWithdrawal"

instance ToPlutusData DRepVotingThresholds where
  toPlutusData x =
    P.List
      [ toPlutusData (dvtMotionNoConfidence x)
      , toPlutusData (dvtCommitteeNormal x)
      , toPlutusData (dvtCommitteeNoConfidence x)
      , toPlutusData (dvtUpdateToConstitution x)
      , toPlutusData (dvtHardForkInitiation x)
      , toPlutusData (dvtPPNetworkGroup x)
      , toPlutusData (dvtPPEconomicGroup x)
      , toPlutusData (dvtPPTechnicalGroup x)
      , toPlutusData (dvtPPGovGroup x)
      , toPlutusData (dvtTreasuryWithdrawal x)
      ]
  fromPlutusData (P.List [a, b, c, d, e, f, g, h, i, j]) =
    DRepVotingThresholds
      <$> fromPlutusData a
      <*> fromPlutusData b
      <*> fromPlutusData c
      <*> fromPlutusData d
      <*> fromPlutusData e
      <*> fromPlutusData f
      <*> fromPlutusData g
      <*> fromPlutusData h
      <*> fromPlutusData i
      <*> fromPlutusData j
  fromPlutusData _ = Nothing

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

dvtCommitteeNoConfidenceL :: Lens' DRepVotingThresholds UnitInterval
dvtCommitteeNoConfidenceL = lens dvtCommitteeNoConfidence (\x y -> x {dvtCommitteeNoConfidence = y})

dvtCommitteeNormalL :: Lens' DRepVotingThresholds UnitInterval
dvtCommitteeNormalL = lens dvtCommitteeNormal (\x y -> x {dvtCommitteeNormal = y})

dvtMotionNoConfidenceL :: Lens' DRepVotingThresholds UnitInterval
dvtMotionNoConfidenceL = lens dvtMotionNoConfidence (\x y -> x {dvtMotionNoConfidence = y})

dvtHardForkInitiationL :: Lens' DRepVotingThresholds UnitInterval
dvtHardForkInitiationL = lens dvtHardForkInitiation (\x y -> x {dvtHardForkInitiation = y})

dvtTreasuryWithdrawalL :: Lens' DRepVotingThresholds UnitInterval
dvtTreasuryWithdrawalL = lens dvtTreasuryWithdrawal (\x y -> x {dvtTreasuryWithdrawal = y})

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

data PPGroups
  = PPGroups DRepGroup StakePoolGroup
  deriving (Eq, Ord, Show)

-- | Protocol parameter groups that dictate different thresholds for DReps.
data DRepGroup
  = NetworkGroup
  | EconomicGroup
  | TechnicalGroup
  | GovGroup
  deriving (Eq, Ord, Show)

data StakePoolGroup
  = SecurityGroup
  | NoStakePoolGroup
  deriving (Eq, Ord, Show)

class ToDRepGroup (t :: DRepGroup) where
  toDRepGroup :: DRepGroup

instance ToDRepGroup 'NetworkGroup where
  toDRepGroup = NetworkGroup

instance ToDRepGroup 'EconomicGroup where
  toDRepGroup = EconomicGroup

instance ToDRepGroup 'TechnicalGroup where
  toDRepGroup = TechnicalGroup

instance ToDRepGroup 'GovGroup where
  toDRepGroup = GovGroup

class ToStakePoolGroup (t :: StakePoolGroup) where
  toStakePoolGroup :: StakePoolGroup

instance ToStakePoolGroup 'SecurityGroup where
  toStakePoolGroup = SecurityGroup

instance ToStakePoolGroup 'NoStakePoolGroup where
  toStakePoolGroup = NoStakePoolGroup

-- | HKD that is tagged with a group
newtype THKD (t :: PPGroups) f a = THKD {unTHKD :: HKD f a}

instance Eq (HKD f a) => Eq (THKD t f a) where
  THKD x1 == THKD x2 = x1 == x2

instance Ord (HKD f a) => Ord (THKD t f a) where
  compare (THKD x1) (THKD x2) = compare x1 x2

instance Show (HKD f a) => Show (THKD t f a) where
  show = show . unTHKD

instance Semigroup (HKD f a) => Semigroup (THKD t f a) where
  a <> b = THKD $ unTHKD a <> unTHKD b

instance Monoid (HKD f a) => Monoid (THKD t f a) where
  mempty = THKD mempty

instance NoThunks (HKD f a) => NoThunks (THKD t f a) where
  noThunks ctx = noThunks ctx . unTHKD
  wNoThunks ctx = wNoThunks ctx . unTHKD
  showTypeOf _ = showTypeOf (Proxy @(HKD f a))

instance NFData (HKD f a) => NFData (THKD t f a) where
  rnf = rnf . unTHKD

instance (Typeable t, EncCBOR a) => EncCBOR (THKD t Identity a) where
  encCBOR = encCBOR . unTHKD

instance (Typeable t, DecCBOR a) => DecCBOR (THKD t Identity a) where
  decCBOR = THKD <$> decCBOR

instance (Typeable t, EncCBOR a) => EncCBOR (THKD t StrictMaybe a) where
  encCBOR = encCBOR . unTHKD

instance (Typeable t, DecCBOR a) => DecCBOR (THKD t StrictMaybe a) where
  decCBOR = THKD <$> decCBOR

instance (Typeable t, ToJSON a) => ToJSON (THKD t Identity a) where
  toJSON = toJSON . unTHKD

instance (Typeable t, FromJSON a) => FromJSON (THKD t Identity a) where
  parseJSON = fmap THKD . parseJSON

instance (Typeable t, ToJSON a) => ToJSON (THKD t StrictMaybe a) where
  toJSON = toJSON . unTHKD

instance (Typeable t, FromJSON a) => FromJSON (THKD t StrictMaybe a) where
  parseJSON = fmap THKD . parseJSON

ppGroup ::
  forall t s a.
  (ToDRepGroup t, ToStakePoolGroup s) =>
  THKD ('PPGroups t s) StrictMaybe a ->
  Set PPGroups
ppGroup = \case
  THKD SNothing -> Set.empty
  THKD SJust {} -> Set.singleton $ PPGroups (toDRepGroup @t) (toStakePoolGroup @s)

-- | Conway Protocol parameters. The following parameters have been added since Babbage:
-- * @poolVotingThresholds@
-- * @dRepVotingThresholds@
-- * @committeeMinSize@
-- * @committeeMaxTermLength@
-- * @govActionLifetime@
-- * @govActionDeposit@
-- * @dRepDeposit@
-- * @dRepActivity@
data ConwayPParams f era = ConwayPParams
  { cppMinFeeA :: !(THKD ('PPGroups 'EconomicGroup 'SecurityGroup) f Coin)
  -- ^ The linear factor for the minimum fee calculation
  , cppMinFeeB :: !(THKD ('PPGroups 'EconomicGroup 'SecurityGroup) f Coin)
  -- ^ The constant factor for the minimum fee calculation
  , cppMaxBBSize :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Word32)
  -- ^ Maximal block body size
  , cppMaxTxSize :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Word32)
  -- ^ Maximal transaction size
  , cppMaxBHSize :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Word16)
  -- ^ Maximal block header size
  , cppKeyDeposit :: !(THKD ('PPGroups 'EconomicGroup 'NoStakePoolGroup) f Coin)
  -- ^ The amount of a key registration deposit
  , cppPoolDeposit :: !(THKD ('PPGroups 'EconomicGroup 'NoStakePoolGroup) f Coin)
  -- ^ The amount of a pool registration deposit
  , cppEMax :: !(THKD ('PPGroups 'TechnicalGroup 'NoStakePoolGroup) f EpochInterval)
  -- ^ Maximum number of epochs in the future a pool retirement is allowed to
  -- be scheduled for.
  , cppNOpt :: !(THKD ('PPGroups 'TechnicalGroup 'NoStakePoolGroup) f Word16)
  -- ^ Desired number of pools
  , cppA0 :: !(THKD ('PPGroups 'TechnicalGroup 'NoStakePoolGroup) f NonNegativeInterval)
  -- ^ Pool influence
  , cppRho :: !(THKD ('PPGroups 'EconomicGroup 'NoStakePoolGroup) f UnitInterval)
  -- ^ Monetary expansion
  , cppTau :: !(THKD ('PPGroups 'EconomicGroup 'NoStakePoolGroup) f UnitInterval)
  -- ^ Treasury expansion
  , cppProtocolVersion :: !(HKDNoUpdate f ProtVer)
  -- ^ Protocol version
  , cppMinPoolCost :: !(THKD ('PPGroups 'EconomicGroup 'NoStakePoolGroup) f Coin)
  -- ^ Minimum Stake Pool Cost
  , cppCoinsPerUTxOByte :: !(THKD ('PPGroups 'EconomicGroup 'SecurityGroup) f CoinPerByte)
  -- ^ Cost in lovelace per byte of UTxO storage
  , cppCostModels :: !(THKD ('PPGroups 'TechnicalGroup 'NoStakePoolGroup) f CostModels)
  -- ^ Cost models for non-native script languages
  , cppPrices :: !(THKD ('PPGroups 'EconomicGroup 'NoStakePoolGroup) f Prices)
  -- ^ Prices of execution units (for non-native script languages)
  , cppMaxTxExUnits :: !(THKD ('PPGroups 'NetworkGroup 'NoStakePoolGroup) f OrdExUnits)
  -- ^ Max total script execution resources units allowed per tx
  , cppMaxBlockExUnits :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f OrdExUnits)
  -- ^ Max total script execution resources units allowed per block
  , cppMaxValSize :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Word32)
  -- ^ Max size of a Value in an output
  , cppCollateralPercentage :: !(THKD ('PPGroups 'TechnicalGroup 'NoStakePoolGroup) f Word16)
  -- ^ Percentage of the txfee which must be provided as collateral when
  -- including non-native scripts.
  , cppMaxCollateralInputs :: !(THKD ('PPGroups 'NetworkGroup 'NoStakePoolGroup) f Word16)
  -- ^ Maximum number of collateral inputs allowed in a transaction
  , -- New ones for Conway:
    cppPoolVotingThresholds :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f PoolVotingThresholds)
  -- ^ Thresholds for SPO votes
  , cppDRepVotingThresholds :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f DRepVotingThresholds)
  -- ^ Thresholds for DRep votes
  , cppCommitteeMinSize :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f Word16)
  -- ^ Minimum size of the Constitutional Committee
  , cppCommitteeMaxTermLength :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f EpochInterval)
  -- ^ The Constitutional Committee Term limit in number of Slots
  , cppGovActionLifetime :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f EpochInterval)
  -- ^ Gov action lifetime in number of Epochs
  , cppGovActionDeposit :: !(THKD ('PPGroups 'GovGroup 'SecurityGroup) f Coin)
  -- ^ The amount of the Gov Action deposit
  , cppDRepDeposit :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f Coin)
  -- ^ The amount of a DRep registration deposit
  , cppDRepActivity :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f EpochInterval)
  -- ^ The number of Epochs that a DRep can perform no activity without losing their @Active@ status.
  , cppMinFeeRefScriptCostPerByte ::
      !(THKD ('PPGroups 'EconomicGroup 'SecurityGroup) f NonNegativeInterval)
  -- ^ Reference scripts fee for the minimum fee calculation
  }
  deriving (Generic)

deriving instance Eq (ConwayPParams Identity era)

deriving instance Ord (ConwayPParams Identity era)

deriving instance Show (ConwayPParams Identity era)

instance NoThunks (ConwayPParams Identity era)

instance NFData (ConwayPParams Identity era)

deriving instance Eq (ConwayPParams StrictMaybe era)

deriving instance Ord (ConwayPParams StrictMaybe era)

deriving instance Show (ConwayPParams StrictMaybe era)

instance NoThunks (ConwayPParams StrictMaybe era)

instance NFData (ConwayPParams StrictMaybe era)

data UpgradeConwayPParams f = UpgradeConwayPParams
  { ucppPoolVotingThresholds :: !(HKD f PoolVotingThresholds)
  , ucppDRepVotingThresholds :: !(HKD f DRepVotingThresholds)
  , ucppCommitteeMinSize :: !(HKD f Word16)
  , ucppCommitteeMaxTermLength :: !(HKD f EpochInterval)
  , ucppGovActionLifetime :: !(HKD f EpochInterval)
  , ucppGovActionDeposit :: !(HKD f Coin)
  , ucppDRepDeposit :: !(HKD f Coin)
  , ucppDRepActivity :: !(HKD f EpochInterval)
  , ucppMinFeeRefScriptCostPerByte :: !(HKD f NonNegativeInterval)
  , ucppPlutusV3CostModel :: !(HKD f CostModel)
  }
  deriving (Generic)

deriving instance Eq (UpgradeConwayPParams Identity)

deriving instance Ord (UpgradeConwayPParams Identity)

deriving instance Show (UpgradeConwayPParams Identity)

instance NoThunks (UpgradeConwayPParams Identity)

instance NFData (UpgradeConwayPParams Identity)

deriving instance Eq (UpgradeConwayPParams StrictMaybe)

deriving instance Ord (UpgradeConwayPParams StrictMaybe)

deriving instance Show (UpgradeConwayPParams StrictMaybe)

instance NoThunks (UpgradeConwayPParams StrictMaybe)

instance NFData (UpgradeConwayPParams StrictMaybe)

instance Default (UpgradeConwayPParams StrictMaybe) where
  def =
    UpgradeConwayPParams
      { ucppPoolVotingThresholds = SNothing
      , ucppDRepVotingThresholds = SNothing
      , ucppCommitteeMinSize = SNothing
      , ucppCommitteeMaxTermLength = SNothing
      , ucppGovActionLifetime = SNothing
      , ucppGovActionDeposit = SNothing
      , ucppDRepDeposit = SNothing
      , ucppDRepActivity = SNothing
      , ucppMinFeeRefScriptCostPerByte = SNothing
      , ucppPlutusV3CostModel = SNothing
      }

instance EncCBOR (UpgradeConwayPParams Identity) where
  encCBOR UpgradeConwayPParams {..} =
    encode $
      Rec (UpgradeConwayPParams @Identity)
        !> To ucppPoolVotingThresholds
        !> To ucppDRepVotingThresholds
        !> To ucppCommitteeMinSize
        !> To ucppCommitteeMaxTermLength
        !> To ucppGovActionLifetime
        !> To ucppGovActionDeposit
        !> To ucppDRepDeposit
        !> To ucppDRepActivity
        !> To ucppMinFeeRefScriptCostPerByte
        !> E encodeCostModel ucppPlutusV3CostModel

instance DecCBOR (UpgradeConwayPParams Identity) where
  decCBOR =
    decode $
      RecD UpgradeConwayPParams
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! D (decodeCostModel PlutusV3)

instance EraPParams ConwayEra where
  type PParamsHKD f ConwayEra = ConwayPParams f ConwayEra
  type UpgradePParams f ConwayEra = UpgradeConwayPParams f
  type DowngradePParams f ConwayEra = ()

  applyPPUpdates (PParams pp) (PParamsUpdate ppu) =
    PParams $ conwayApplyPPUpdates pp ppu

  emptyPParamsIdentity = emptyConwayPParams
  emptyPParamsStrictMaybe = emptyConwayPParamsUpdate

  upgradePParamsHKD = upgradeConwayPParams
  downgradePParamsHKD () = downgradeConwayPParams

  hkdMinFeeAL = lens (unTHKD . cppMinFeeA) $ \pp x -> pp {cppMinFeeA = THKD x}
  hkdMinFeeBL = lens (unTHKD . cppMinFeeB) $ \pp x -> pp {cppMinFeeB = THKD x}
  hkdMaxBBSizeL = lens (unTHKD . cppMaxBBSize) $ \pp x -> pp {cppMaxBBSize = THKD x}
  hkdMaxTxSizeL = lens (unTHKD . cppMaxTxSize) $ \pp x -> pp {cppMaxTxSize = THKD x}
  hkdMaxBHSizeL = lens (unTHKD . cppMaxBHSize) $ \pp x -> pp {cppMaxBHSize = THKD x}
  hkdKeyDepositL = lens (unTHKD . cppKeyDeposit) $ \pp x -> pp {cppKeyDeposit = THKD x}
  hkdPoolDepositL = lens (unTHKD . cppPoolDeposit) $ \pp x -> pp {cppPoolDeposit = THKD x}
  hkdEMaxL = lens (unTHKD . cppEMax) $ \pp x -> pp {cppEMax = THKD x}
  hkdNOptL = lens (unTHKD . cppNOpt) $ \pp x -> pp {cppNOpt = THKD x}
  hkdA0L = lens (unTHKD . cppA0) $ \pp x -> pp {cppA0 = THKD x}
  hkdRhoL = lens (unTHKD . cppRho) $ \pp x -> pp {cppRho = THKD x}
  hkdTauL = lens (unTHKD . cppTau) $ \pp x -> pp {cppTau = THKD x}
  hkdProtocolVersionL = notSupportedInThisEraL
  hkdMinPoolCostL = lens (unTHKD . cppMinPoolCost) $ \pp x -> pp {cppMinPoolCost = THKD x}
  ppProtocolVersionL = ppLensHKD . lens cppProtocolVersion (\pp x -> pp {cppProtocolVersion = x})

  ppDG = to (const minBound)
  ppuProtocolVersionL = notSupportedInThisEraL
  hkdDL = notSupportedInThisEraL
  hkdExtraEntropyL = notSupportedInThisEraL
  hkdMinUTxOValueL = notSupportedInThisEraL

  eraPParams =
    [ ppMinFeeA
    , ppMinFeeB
    , ppMaxBBSize
    , ppMaxTxSize
    , ppMaxBHSize
    , ppKeyDeposit
    , ppPoolDeposit
    , ppEMax
    , ppNOpt
    , ppA0
    , ppRho
    , ppTau
    , ppGovProtocolVersion
    , ppMinPoolCost
    , ppCoinsPerUTxOByte
    , ppCostModels
    , ppPrices
    , ppMaxTxExUnits
    , ppMaxBlockExUnits
    , ppMaxValSize
    , ppCollateralPercentage
    , ppMaxCollateralInputs
    , ppPoolVotingThresholds
    , ppDRepVotingThresholds
    , ppCommitteeMinSize
    , ppCommitteeMaxTermLength
    , ppGovActionLifetime
    , ppGovActionDeposit
    , ppDRepDeposit
    , ppDRepActivity
    , ppMinFeeRefScriptCostPerByte
    ]

instance AlonzoEraPParams ConwayEra where
  hkdCoinsPerUTxOWordL = notSupportedInThisEraL
  hkdCostModelsL = lens (unTHKD . cppCostModels) $ \pp x -> pp {cppCostModels = THKD x}
  hkdPricesL = lens (unTHKD . cppPrices) $ \pp x -> pp {cppPrices = THKD x}

  hkdMaxTxExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f ConwayEra) (HKD f ExUnits)
  hkdMaxTxExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . unTHKD . cppMaxTxExUnits) $ \pp x ->
      pp {cppMaxTxExUnits = THKD $ hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxBlockExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f ConwayEra) (HKD f ExUnits)
  hkdMaxBlockExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . unTHKD . cppMaxBlockExUnits) $ \pp x ->
      pp {cppMaxBlockExUnits = THKD $ hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxValSizeL :: forall f. HKDFunctor f => Lens' (PParamsHKD f ConwayEra) (HKD f Natural)
  hkdMaxValSizeL =
    lens (asNaturalHKD @f @Word32 . (unTHKD . cppMaxValSize)) $
      \pp x -> pp {cppMaxValSize = THKD (asBoundedIntegralHKD @f @Natural @Word32 x)}
  hkdCollateralPercentageL ::
    forall f. HKDFunctor f => Lens' (PParamsHKD f ConwayEra) (HKD f Natural)
  hkdCollateralPercentageL =
    lens (asNaturalHKD @f @Word16 . (unTHKD . cppCollateralPercentage)) $
      \pp x -> pp {cppCollateralPercentage = THKD (asBoundedIntegralHKD @f @Natural @Word16 x)}
  hkdMaxCollateralInputsL ::
    forall f. HKDFunctor f => Lens' (PParamsHKD f ConwayEra) (HKD f Natural)
  hkdMaxCollateralInputsL =
    lens (asNaturalHKD @f @Word16 . (unTHKD . cppMaxCollateralInputs)) $
      \pp x -> pp {cppMaxCollateralInputs = THKD (asBoundedIntegralHKD @f @Natural @Word16 x)}

instance BabbageEraPParams ConwayEra where
  hkdCoinsPerUTxOByteL =
    lens (unTHKD . cppCoinsPerUTxOByte) $ \pp x -> pp {cppCoinsPerUTxOByte = THKD x}

instance ConwayEraPParams ConwayEra where
  modifiedPPGroups (PParamsUpdate ppu) = conwayModifiedPPGroups ppu
  ppuWellFormed pv ppu =
    and
      [ -- Numbers
        isValid (/= 0) ppuMaxBBSizeL
      , isValid (/= 0) ppuMaxTxSizeL
      , isValid (/= 0) ppuMaxBHSizeL
      , isValid (/= 0) ppuMaxValSizeL
      , isValid (/= 0) ppuCollateralPercentageL
      , isValid (/= EpochInterval 0) ppuCommitteeMaxTermLengthL
      , isValid (/= EpochInterval 0) ppuGovActionLifetimeL
      , -- Coins
        isValid (/= zero) ppuPoolDepositL
      , isValid (/= zero) ppuGovActionDepositL
      , isValid (/= zero) ppuDRepDepositL
      , bootstrapPhase pv
          || isValid ((/= zero) . unCoinPerByte) ppuCoinsPerUTxOByteL
      , ppu /= emptyPParamsUpdate
      ]
    where
      isValid ::
        (t -> Bool) ->
        Lens' (PParamsUpdate ConwayEra) (StrictMaybe t) ->
        Bool
      isValid p l = case ppu ^. l of
        SJust x -> p x
        SNothing -> True
  hkdPoolVotingThresholdsL =
    lens (unTHKD . cppPoolVotingThresholds) $ \pp x -> pp {cppPoolVotingThresholds = THKD x}
  hkdDRepVotingThresholdsL =
    lens (unTHKD . cppDRepVotingThresholds) $ \pp x -> pp {cppDRepVotingThresholds = THKD x}
  hkdCommitteeMinSizeL :: forall f. HKDFunctor f => Lens' (PParamsHKD f ConwayEra) (HKD f Natural)
  hkdCommitteeMinSizeL =
    lens (asNaturalHKD @f @Word16 . (unTHKD . cppCommitteeMinSize)) $
      \pp x -> pp {cppCommitteeMinSize = THKD (asBoundedIntegralHKD @f @Natural @Word16 x)}
  hkdCommitteeMaxTermLengthL =
    lens (unTHKD . cppCommitteeMaxTermLength) $ \pp x -> pp {cppCommitteeMaxTermLength = THKD x}
  hkdGovActionLifetimeL =
    lens (unTHKD . cppGovActionLifetime) $ \pp x -> pp {cppGovActionLifetime = THKD x}
  hkdGovActionDepositL =
    lens (unTHKD . cppGovActionDeposit) $ \pp x -> pp {cppGovActionDeposit = THKD x}
  hkdDRepDepositL =
    lens (unTHKD . cppDRepDeposit) $ \pp x -> pp {cppDRepDeposit = THKD x}
  hkdDRepActivityL =
    lens (unTHKD . cppDRepActivity) $ \pp x -> pp {cppDRepActivity = THKD x}
  hkdMinFeeRefScriptCostPerByteL =
    lens (unTHKD . cppMinFeeRefScriptCostPerByte) $ \pp x -> pp {cppMinFeeRefScriptCostPerByte = THKD x}

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyConwayPParams :: forall era. Era era => ConwayPParams Identity era
emptyConwayPParams =
  ConwayPParams
    { cppMinFeeA = THKD (Coin 0)
    , cppMinFeeB = THKD (Coin 0)
    , cppMaxBBSize = THKD 0
    , cppMaxTxSize = THKD 2048
    , cppMaxBHSize = THKD 0
    , cppKeyDeposit = THKD (Coin 0)
    , cppPoolDeposit = THKD (Coin 0)
    , cppEMax = THKD (EpochInterval 0)
    , cppNOpt = THKD 100
    , cppA0 = THKD minBound
    , cppRho = THKD minBound
    , cppTau = THKD minBound
    , cppProtocolVersion = ProtVer (eraProtVerLow @era) 0
    , cppMinPoolCost = THKD mempty
    , cppCoinsPerUTxOByte = THKD (CoinPerByte $ Coin 0)
    , cppCostModels = THKD emptyCostModels
    , cppPrices = THKD (Prices minBound minBound)
    , cppMaxTxExUnits = THKD (OrdExUnits $ ExUnits 0 0)
    , cppMaxBlockExUnits = THKD (OrdExUnits $ ExUnits 0 0)
    , cppMaxValSize = THKD 0
    , cppCollateralPercentage = THKD 150
    , cppMaxCollateralInputs = THKD 5
    , -- New in Conway
      cppPoolVotingThresholds = THKD def
    , cppDRepVotingThresholds = THKD def
    , cppCommitteeMinSize = THKD 0
    , cppCommitteeMaxTermLength = THKD (EpochInterval 0)
    , cppGovActionLifetime = THKD (EpochInterval 0)
    , cppGovActionDeposit = THKD (Coin 0)
    , cppDRepDeposit = THKD (Coin 0)
    , cppDRepActivity = THKD (EpochInterval 0)
    , cppMinFeeRefScriptCostPerByte = THKD minBound
    }

emptyConwayPParamsUpdate :: ConwayPParams StrictMaybe era
emptyConwayPParamsUpdate =
  ConwayPParams
    { cppMinFeeA = THKD SNothing
    , cppMinFeeB = THKD SNothing
    , cppMaxBBSize = THKD SNothing
    , cppMaxTxSize = THKD SNothing
    , cppMaxBHSize = THKD SNothing
    , cppKeyDeposit = THKD SNothing
    , cppPoolDeposit = THKD SNothing
    , cppEMax = THKD SNothing
    , cppNOpt = THKD SNothing
    , cppA0 = THKD SNothing
    , cppRho = THKD SNothing
    , cppTau = THKD SNothing
    , cppProtocolVersion = NoUpdate
    , cppMinPoolCost = THKD SNothing
    , cppCoinsPerUTxOByte = THKD SNothing
    , cppCostModels = THKD SNothing
    , cppPrices = THKD SNothing
    , cppMaxTxExUnits = THKD SNothing
    , cppMaxBlockExUnits = THKD SNothing
    , cppMaxValSize = THKD SNothing
    , cppCollateralPercentage = THKD SNothing
    , cppMaxCollateralInputs = THKD SNothing
    , -- New for Conway
      cppPoolVotingThresholds = THKD SNothing
    , cppDRepVotingThresholds = THKD SNothing
    , cppCommitteeMinSize = THKD SNothing
    , cppCommitteeMaxTermLength = THKD SNothing
    , cppGovActionLifetime = THKD SNothing
    , cppGovActionDeposit = THKD SNothing
    , cppDRepDeposit = THKD SNothing
    , cppDRepActivity = THKD SNothing
    , cppMinFeeRefScriptCostPerByte = THKD SNothing
    }

instance ToJSON (UpgradeConwayPParams Identity) where
  toJSON = object . toUpgradeConwayPParamsUpdatePairs
  toEncoding = pairs . mconcat . toUpgradeConwayPParamsUpdatePairs

toUpgradeConwayPParamsUpdatePairs :: KeyValue e a => UpgradeConwayPParams Identity -> [a]
toUpgradeConwayPParamsUpdatePairs upp =
  uncurry (.=) <$> upgradeConwayPParamsHKDPairs upp

upgradeConwayPParamsHKDPairs :: UpgradeConwayPParams Identity -> [(Key, Aeson.Value)]
upgradeConwayPParamsHKDPairs UpgradeConwayPParams {..} =
  [ ("poolVotingThresholds", (toJSON @PoolVotingThresholds) ucppPoolVotingThresholds)
  , ("dRepVotingThresholds", (toJSON @DRepVotingThresholds) ucppDRepVotingThresholds)
  , ("committeeMinSize", (toJSON @Word16) ucppCommitteeMinSize)
  , ("committeeMaxTermLength", (toJSON @EpochInterval) ucppCommitteeMaxTermLength)
  , ("govActionLifetime", (toJSON @EpochInterval) ucppGovActionLifetime)
  , ("govActionDeposit", (toJSON @Coin) ucppGovActionDeposit)
  , ("dRepDeposit", (toJSON @Coin) ucppDRepDeposit)
  , ("dRepActivity", (toJSON @EpochInterval) ucppDRepActivity)
  , ("minFeeRefScriptCostPerByte", (toJSON @NonNegativeInterval) ucppMinFeeRefScriptCostPerByte)
  , ("plutusV3CostModel", (toJSON @CostModel) ucppPlutusV3CostModel)
  ]

instance FromJSON (UpgradeConwayPParams Identity) where
  parseJSON =
    withObject "UpgradeConwayPParams" $ \o ->
      UpgradeConwayPParams
        <$> o .: "poolVotingThresholds"
        <*> o .: "dRepVotingThresholds"
        <*> o .: "committeeMinSize"
        <*> o .: "committeeMaxTermLength"
        <*> o .: "govActionLifetime"
        <*> o .: "govActionDeposit"
        <*> o .: "dRepDeposit"
        <*> o .: "dRepActivity"
        <*> o .: "minFeeRefScriptCostPerByte"
        <*> (either (fail . show) pure . mkCostModel PlutusV3 =<< o .: "plutusV3CostModel")

upgradeConwayPParams ::
  forall f.
  HKDApplicative f =>
  UpgradeConwayPParams f ->
  PParamsHKD f BabbageEra ->
  ConwayPParams f ConwayEra
upgradeConwayPParams UpgradeConwayPParams {..} BabbagePParams {..} =
  ConwayPParams
    { cppMinFeeA = THKD bppMinFeeA
    , cppMinFeeB = THKD bppMinFeeB
    , cppMaxBBSize = THKD bppMaxBBSize
    , cppMaxTxSize = THKD bppMaxTxSize
    , cppMaxBHSize = THKD bppMaxBHSize
    , cppKeyDeposit = THKD bppKeyDeposit
    , cppPoolDeposit = THKD bppPoolDeposit
    , cppEMax = THKD bppEMax
    , cppNOpt = THKD bppNOpt
    , cppA0 = THKD bppA0
    , cppRho = THKD bppRho
    , cppTau = THKD bppTau
    , cppProtocolVersion = toNoUpdate @f @ProtVer bppProtocolVersion
    , cppMinPoolCost = THKD bppMinPoolCost
    , cppCoinsPerUTxOByte = THKD bppCoinsPerUTxOByte
    , cppCostModels =
        THKD $
          -- We add the PlutusV3 CostModel from ConwayGenesis to the ConwayPParams here
          hkdLiftA2 @f
            updateCostModels
            bppCostModels
            ( hkdMap
                (Proxy @f)
                (mkCostModels . Map.singleton PlutusV3)
                ucppPlutusV3CostModel
            )
    , cppPrices = THKD bppPrices
    , cppMaxTxExUnits = THKD bppMaxTxExUnits
    , cppMaxBlockExUnits = THKD bppMaxBlockExUnits
    , cppMaxValSize =
        THKD (asBoundedIntegralHKD @f @Natural @Word32 bppMaxValSize)
    , cppCollateralPercentage =
        THKD (asBoundedIntegralHKD @f @Natural @Word16 bppCollateralPercentage)
    , cppMaxCollateralInputs =
        THKD (asBoundedIntegralHKD @f @Natural @Word16 bppMaxCollateralInputs)
    , -- New for Conway
      cppPoolVotingThresholds = THKD ucppPoolVotingThresholds
    , cppDRepVotingThresholds = THKD ucppDRepVotingThresholds
    , cppCommitteeMinSize = THKD ucppCommitteeMinSize
    , cppCommitteeMaxTermLength = THKD ucppCommitteeMaxTermLength
    , cppGovActionLifetime = THKD ucppGovActionLifetime
    , cppGovActionDeposit = THKD ucppGovActionDeposit
    , cppDRepDeposit = THKD ucppDRepDeposit
    , cppDRepActivity = THKD ucppDRepActivity
    , cppMinFeeRefScriptCostPerByte = THKD ucppMinFeeRefScriptCostPerByte
    }

downgradeConwayPParams ::
  forall f.
  HKDFunctor f =>
  ConwayPParams f ConwayEra ->
  PParamsHKD f BabbageEra
downgradeConwayPParams ConwayPParams {..} =
  BabbagePParams
    { bppMinFeeA = unTHKD cppMinFeeA
    , bppMinFeeB = unTHKD cppMinFeeB
    , bppMaxBBSize = unTHKD cppMaxBBSize
    , bppMaxTxSize = unTHKD cppMaxTxSize
    , bppMaxBHSize = unTHKD cppMaxBHSize
    , bppKeyDeposit = unTHKD cppKeyDeposit
    , bppPoolDeposit = unTHKD cppPoolDeposit
    , bppEMax = unTHKD cppEMax
    , bppNOpt = unTHKD cppNOpt
    , bppA0 = unTHKD cppA0
    , bppRho = unTHKD cppRho
    , bppTau = unTHKD cppTau
    , bppProtocolVersion = fromNoUpdate @f @ProtVer cppProtocolVersion
    , bppMinPoolCost = unTHKD cppMinPoolCost
    , bppCoinsPerUTxOByte = unTHKD cppCoinsPerUTxOByte
    , bppCostModels = unTHKD cppCostModels
    , bppPrices = unTHKD cppPrices
    , bppMaxTxExUnits = unTHKD cppMaxTxExUnits
    , bppMaxBlockExUnits = unTHKD cppMaxBlockExUnits
    , bppMaxValSize = asNaturalHKD @f @Word32 (unTHKD cppMaxValSize)
    , bppCollateralPercentage = asNaturalHKD @f @Word16 (unTHKD cppCollateralPercentage)
    , bppMaxCollateralInputs = asNaturalHKD @f @Word16 (unTHKD cppMaxCollateralInputs)
    }

conwayApplyPPUpdates ::
  ConwayPParams Identity era ->
  ConwayPParams StrictMaybe era ->
  ConwayPParams Identity era
conwayApplyPPUpdates pp ppu =
  ConwayPParams
    { cppMinFeeA = ppUpdate (cppMinFeeA pp) (cppMinFeeA ppu)
    , cppMinFeeB = ppUpdate (cppMinFeeB pp) (cppMinFeeB ppu)
    , cppMaxBBSize = ppUpdate (cppMaxBBSize pp) (cppMaxBBSize ppu)
    , cppMaxTxSize = ppUpdate (cppMaxTxSize pp) (cppMaxTxSize ppu)
    , cppMaxBHSize = ppUpdate (cppMaxBHSize pp) (cppMaxBHSize ppu)
    , cppKeyDeposit = ppUpdate (cppKeyDeposit pp) (cppKeyDeposit ppu)
    , cppPoolDeposit = ppUpdate (cppPoolDeposit pp) (cppPoolDeposit ppu)
    , cppEMax = ppUpdate (cppEMax pp) (cppEMax ppu)
    , cppNOpt = ppUpdate (cppNOpt pp) (cppNOpt ppu)
    , cppA0 = ppUpdate (cppA0 pp) (cppA0 ppu)
    , cppRho = ppUpdate (cppRho pp) (cppRho ppu)
    , cppTau = ppUpdate (cppTau pp) (cppTau ppu)
    , cppProtocolVersion = cppProtocolVersion pp
    , cppMinPoolCost = ppUpdate (cppMinPoolCost pp) (cppMinPoolCost ppu)
    , cppCoinsPerUTxOByte = ppUpdate (cppCoinsPerUTxOByte pp) (cppCoinsPerUTxOByte ppu)
    , cppCostModels = ppUpdateCostModels (cppCostModels pp) (cppCostModels ppu)
    , cppPrices = ppUpdate (cppPrices pp) (cppPrices ppu)
    , cppMaxTxExUnits = ppUpdate (cppMaxTxExUnits pp) (cppMaxTxExUnits ppu)
    , cppMaxBlockExUnits = ppUpdate (cppMaxBlockExUnits pp) (cppMaxBlockExUnits ppu)
    , cppMaxValSize = ppUpdate (cppMaxValSize pp) (cppMaxValSize ppu)
    , cppCollateralPercentage = ppUpdate (cppCollateralPercentage pp) (cppCollateralPercentage ppu)
    , cppMaxCollateralInputs = ppUpdate (cppMaxCollateralInputs pp) (cppMaxCollateralInputs ppu)
    , cppPoolVotingThresholds = ppUpdate (cppPoolVotingThresholds pp) (cppPoolVotingThresholds ppu)
    , cppDRepVotingThresholds = ppUpdate (cppDRepVotingThresholds pp) (cppDRepVotingThresholds ppu)
    , cppCommitteeMinSize = ppUpdate (cppCommitteeMinSize pp) (cppCommitteeMinSize ppu)
    , cppCommitteeMaxTermLength =
        ppUpdate (cppCommitteeMaxTermLength pp) (cppCommitteeMaxTermLength ppu)
    , cppGovActionLifetime = ppUpdate (cppGovActionLifetime pp) (cppGovActionLifetime ppu)
    , cppGovActionDeposit = ppUpdate (cppGovActionDeposit pp) (cppGovActionDeposit ppu)
    , cppDRepDeposit = ppUpdate (cppDRepDeposit pp) (cppDRepDeposit ppu)
    , cppDRepActivity = ppUpdate (cppDRepActivity pp) (cppDRepActivity ppu)
    , cppMinFeeRefScriptCostPerByte =
        ppUpdate (cppMinFeeRefScriptCostPerByte pp) (cppMinFeeRefScriptCostPerByte ppu)
    }
  where
    ppUpdate ::
      THKD f Identity a ->
      THKD f StrictMaybe a ->
      THKD f Identity a
    ppUpdate (THKD ppCurValue) (THKD ppuValue) =
      case ppuValue of
        SNothing -> THKD ppCurValue
        SJust ppNewValue -> THKD ppNewValue

    ppUpdateCostModels ::
      THKD f Identity CostModels ->
      THKD f StrictMaybe CostModels ->
      THKD f Identity CostModels
    ppUpdateCostModels (THKD curCostModel) (THKD ppuCostModel) =
      case ppuCostModel of
        SNothing -> THKD curCostModel
        SJust costModelUpdate -> THKD $ updateCostModels curCostModel costModelUpdate

conwayModifiedPPGroups :: ConwayPParams StrictMaybe era -> Set PPGroups
conwayModifiedPPGroups
  ( ConwayPParams
      p01
      p02
      p03
      p04
      p05
      p06
      p07
      p08
      p09
      p10
      p11
      p12
      _protocolVersion
      p14
      p15
      p16
      p17
      p18
      p19
      p20
      p21
      p22
      p23
      p24
      p25
      p26
      p27
      p28
      p29
      p30
      p31
    ) =
    mconcat
      [ ppGroup p01
      , ppGroup p02
      , ppGroup p03
      , ppGroup p04
      , ppGroup p05
      , ppGroup p06
      , ppGroup p07
      , ppGroup p08
      , ppGroup p09
      , ppGroup p10
      , ppGroup p11
      , ppGroup p12
      , ppGroup p14
      , ppGroup p15
      , ppGroup p16
      , ppGroup p17
      , ppGroup p18
      , ppGroup p19
      , ppGroup p20
      , ppGroup p21
      , ppGroup p22
      , ppGroup p23
      , ppGroup p24
      , ppGroup p25
      , ppGroup p26
      , ppGroup p27
      , ppGroup p28
      , ppGroup p29
      , ppGroup p30
      , ppGroup p31
      ]

asNaturalHKD :: forall f i. (HKDFunctor f, Integral i) => HKD f i -> HKD f Natural
asNaturalHKD = hkdMap (Proxy @f) (fromIntegral @i @Natural)

asBoundedIntegralHKD ::
  forall f i b.
  (HKDFunctor f, Integral i, Integral b, Bounded b, HasCallStack) =>
  HKD f i ->
  HKD f b
asBoundedIntegralHKD = hkdMap (Proxy @f) $ \x ->
  case integralToBounded @i @b @Maybe x of
    Just b -> b
    Nothing ->
      error $
        "Value: "
          <> show (toInteger x)
          <> " is out of the valid range: ["
          <> show (toInteger (minBound @b))
          <> ","
          <> show (toInteger (maxBound @b))
          <> "]"

ppPoolVotingThresholds :: ConwayEraPParams era => PParam era
ppPoolVotingThresholds =
  PParam
    { ppName = "poolVotingThresholds"
    , ppLens = ppPoolVotingThresholdsL
    , ppUpdate = Just $ PParamUpdate 25 ppuPoolVotingThresholdsL
    }

ppDRepVotingThresholds :: ConwayEraPParams era => PParam era
ppDRepVotingThresholds =
  PParam
    { ppName = "dRepVotingThresholds"
    , ppLens = ppDRepVotingThresholdsL
    , ppUpdate = Just $ PParamUpdate 26 ppuDRepVotingThresholdsL
    }

ppCommitteeMinSize :: ConwayEraPParams era => PParam era
ppCommitteeMinSize =
  PParam
    { ppName = "committeeMinSize"
    , ppLens = ppCommitteeMinSizeL
    , ppUpdate = Just $ PParamUpdate 27 ppuCommitteeMinSizeL
    }

ppCommitteeMaxTermLength :: ConwayEraPParams era => PParam era
ppCommitteeMaxTermLength =
  PParam
    { ppName = "committeeMaxTermLength"
    , ppLens = ppCommitteeMaxTermLengthL
    , ppUpdate = Just $ PParamUpdate 28 ppuCommitteeMaxTermLengthL
    }

ppGovActionLifetime :: ConwayEraPParams era => PParam era
ppGovActionLifetime =
  PParam
    { ppName = "govActionLifetime"
    , ppLens = ppGovActionLifetimeL
    , ppUpdate = Just $ PParamUpdate 29 ppuGovActionLifetimeL
    }

ppGovActionDeposit :: ConwayEraPParams era => PParam era
ppGovActionDeposit =
  PParam
    { ppName = "govActionDeposit"
    , ppLens = ppGovActionDepositL
    , ppUpdate = Just $ PParamUpdate 30 ppuGovActionDepositL
    }

ppDRepDeposit :: ConwayEraPParams era => PParam era
ppDRepDeposit =
  PParam
    { ppName = "dRepDeposit"
    , ppLens = ppDRepDepositL
    , ppUpdate = Just $ PParamUpdate 31 ppuDRepDepositL
    }

ppDRepActivity :: ConwayEraPParams era => PParam era
ppDRepActivity =
  PParam
    { ppName = "dRepActivity"
    , ppLens = ppDRepActivityL
    , ppUpdate = Just $ PParamUpdate 32 ppuDRepActivityL
    }

ppMinFeeRefScriptCostPerByte :: ConwayEraPParams era => PParam era
ppMinFeeRefScriptCostPerByte =
  PParam
    { ppName = "minFeeRefScriptCostPerByte"
    , ppLens = ppMinFeeRefScriptCostPerByteL
    , ppUpdate = Just $ PParamUpdate 33 ppuMinFeeRefScriptCostPerByteL
    }

ppGovProtocolVersion :: ConwayEraPParams era => PParam era
ppGovProtocolVersion =
  PParam
    { ppName = "protocolVersion"
    , ppLens = ppProtocolVersionL
    , ppUpdate = Nothing
    }
