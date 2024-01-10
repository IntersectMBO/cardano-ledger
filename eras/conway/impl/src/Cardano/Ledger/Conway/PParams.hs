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
  ppPoolVotingThresholdsL,
  ppDRepVotingThresholdsL,
  ppCommitteeMinSizeL,
  ppCommitteeMaxTermLengthL,
  ppGovActionLifetimeL,
  ppGovActionDepositL,
  ppDRepDepositL,
  ppDRepActivityL,
  ppuPoolVotingThresholdsL,
  ppuDRepVotingThresholdsL,
  ppuCommitteeMinSizeL,
  ppuCommitteeMaxTermLengthL,
  ppuGovActionLifetimeL,
  ppuGovActionDepositL,
  ppuDRepDepositL,
  ppuDRepActivityL,
  PoolVotingThresholds (..),
  pvtCommitteeNoConfidenceL,
  pvtCommitteeNormalL,
  pvtPPSecurityGroupL,
  DRepVotingThresholds (..),
  dvtCommitteeNoConfidenceL,
  dvtPPNetworkGroupL,
  dvtPPGovGroupL,
  dvtPPTechnicalGroupL,
  dvtPPEconomicGroupL,
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
)
where

import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams (..), OrdExUnits (..))
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
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  Encoding,
  FromCBOR (..),
  ToCBOR (..),
  encodeListLen,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Core (EraPParams (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.HKD (HKD, HKDFunctor (..), HKDNoUpdate, NoUpdate (..))
import Cardano.Ledger.Plutus.CostModels (decodeValidAndUnknownCostModels)
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Aeson hiding (Encoding, Value, decode, encode)
import qualified Data.Aeson as Aeson
import Data.Default.Class (Default (def))
import Data.Functor.Identity (Identity)
import Data.Maybe.Strict (StrictMaybe (..), isSNothing)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

class BabbageEraPParams era => ConwayEraPParams era where
  modifiedPPGroups :: PParamsUpdate era -> Set PPGroups
  ppuWellFormed :: PParamsUpdate era -> Bool

  hkdPoolVotingThresholdsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f PoolVotingThresholds)
  hkdDRepVotingThresholdsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f DRepVotingThresholds)
  hkdCommitteeMinSizeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)
  hkdCommitteeMaxTermLengthL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f EpochInterval)
  hkdGovActionLifetimeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f EpochInterval)
  hkdGovActionDepositL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)
  hkdDRepDepositL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)
  hkdDRepActivityL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f EpochInterval)

ppPoolVotingThresholdsL ::
  forall era. ConwayEraPParams era => Lens' (PParams era) PoolVotingThresholds
ppPoolVotingThresholdsL = ppLens . hkdPoolVotingThresholdsL @era @Identity

ppDRepVotingThresholdsL ::
  forall era. ConwayEraPParams era => Lens' (PParams era) DRepVotingThresholds
ppDRepVotingThresholdsL = ppLens . hkdDRepVotingThresholdsL @era @Identity

ppCommitteeMinSizeL :: forall era. ConwayEraPParams era => Lens' (PParams era) Natural
ppCommitteeMinSizeL = ppLens . hkdCommitteeMinSizeL @era @Identity

ppCommitteeMaxTermLengthL :: forall era. ConwayEraPParams era => Lens' (PParams era) EpochInterval
ppCommitteeMaxTermLengthL = ppLens . hkdCommitteeMaxTermLengthL @era @Identity

ppGovActionLifetimeL :: forall era. ConwayEraPParams era => Lens' (PParams era) EpochInterval
ppGovActionLifetimeL = ppLens . hkdGovActionLifetimeL @era @Identity

ppGovActionDepositL :: forall era. ConwayEraPParams era => Lens' (PParams era) Coin
ppGovActionDepositL = ppLens . hkdGovActionDepositL @era @Identity

ppDRepDepositL :: forall era. ConwayEraPParams era => Lens' (PParams era) Coin
ppDRepDepositL = ppLens . hkdDRepDepositL @era @Identity

ppDRepActivityL :: forall era. ConwayEraPParams era => Lens' (PParams era) EpochInterval
ppDRepActivityL = ppLens . hkdDRepActivityL @era @Identity

ppuPoolVotingThresholdsL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe PoolVotingThresholds)
ppuPoolVotingThresholdsL = ppuLens . hkdPoolVotingThresholdsL @era @StrictMaybe

ppuDRepVotingThresholdsL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe DRepVotingThresholds)
ppuDRepVotingThresholdsL = ppuLens . hkdDRepVotingThresholdsL @era @StrictMaybe

ppuCommitteeMinSizeL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuCommitteeMinSizeL = ppuLens . hkdCommitteeMinSizeL @era @StrictMaybe

ppuCommitteeMaxTermLengthL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe EpochInterval)
ppuCommitteeMaxTermLengthL = ppuLens . hkdCommitteeMaxTermLengthL @era @StrictMaybe

ppuGovActionLifetimeL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe EpochInterval)
ppuGovActionLifetimeL = ppuLens . hkdGovActionLifetimeL @era @StrictMaybe

ppuGovActionDepositL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuGovActionDepositL = ppuLens . hkdGovActionDepositL @era @StrictMaybe

ppuDRepDepositL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuDRepDepositL = ppuLens . hkdDRepDepositL @era @StrictMaybe

ppuDRepActivityL ::
  forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe EpochInterval)
ppuDRepActivityL = ppuLens . hkdDRepActivityL @era @StrictMaybe

data PoolVotingThresholds = PoolVotingThresholds
  { pvtMotionNoConfidence :: !UnitInterval
  , pvtCommitteeNormal :: !UnitInterval
  , pvtCommitteeNoConfidence :: !UnitInterval
  , pvtHardForkInitiation :: !UnitInterval
  , pvtPPSecurityGroup :: !UnitInterval
  }
  deriving (Eq, Ord, Show, Generic)

pvtCommitteeNormalL :: Lens' PoolVotingThresholds UnitInterval
pvtCommitteeNormalL = lens pvtCommitteeNormal (\x y -> x {pvtCommitteeNormal = y})

pvtCommitteeNoConfidenceL :: Lens' PoolVotingThresholds UnitInterval
pvtCommitteeNoConfidenceL = lens pvtCommitteeNoConfidence (\x y -> x {pvtCommitteeNoConfidence = y})

pvtPPSecurityGroupL :: Lens' PoolVotingThresholds UnitInterval
pvtPPSecurityGroupL = lens pvtPPSecurityGroup (\x y -> x {pvtPPSecurityGroup = y})

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

ppGroup :: forall t s a. (ToDRepGroup t, ToStakePoolGroup s) => THKD ('PPGroups t s) StrictMaybe a -> Set PPGroups
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
  , cppNOpt :: !(THKD ('PPGroups 'TechnicalGroup 'NoStakePoolGroup) f Natural)
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
  , cppMaxValSize :: !(THKD ('PPGroups 'NetworkGroup 'SecurityGroup) f Natural)
  -- ^ Max size of a Value in an output
  , cppCollateralPercentage :: !(THKD ('PPGroups 'TechnicalGroup 'NoStakePoolGroup) f Natural)
  -- ^ Percentage of the txfee which must be provided as collateral when
  -- including non-native scripts.
  , cppMaxCollateralInputs :: !(THKD ('PPGroups 'NetworkGroup 'NoStakePoolGroup) f Natural)
  -- ^ Maximum number of collateral inputs allowed in a transaction
  , -- New ones for Conway:
    cppPoolVotingThresholds :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f PoolVotingThresholds)
  -- ^ Thresholds for SPO votes
  , cppDRepVotingThresholds :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f DRepVotingThresholds)
  -- ^ Thresholds for DRep votes
  , cppCommitteeMinSize :: !(THKD ('PPGroups 'GovGroup 'NoStakePoolGroup) f Natural)
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
  , ucppCommitteeMinSize :: !(HKD f Natural)
  , ucppCommitteeMaxTermLength :: !(HKD f EpochInterval)
  , ucppGovActionLifetime :: !(HKD f EpochInterval)
  , ucppGovActionDeposit :: !(HKD f Coin)
  , ucppDRepDeposit :: !(HKD f Coin)
  , ucppDRepActivity :: !(HKD f EpochInterval)
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

instance Default (UpgradeConwayPParams Identity) where
  def =
    UpgradeConwayPParams
      { ucppPoolVotingThresholds = def
      , ucppDRepVotingThresholds = def
      , ucppCommitteeMinSize = 0
      , ucppCommitteeMaxTermLength = EpochInterval 0
      , ucppGovActionLifetime = EpochInterval 0
      , ucppGovActionDeposit = Coin 0
      , ucppDRepDeposit = Coin 0
      , ucppDRepActivity = EpochInterval 0
      }

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

instance Crypto c => EraPParams (ConwayEra c) where
  type PParamsHKD f (ConwayEra c) = ConwayPParams f (ConwayEra c)
  type UpgradePParams f (ConwayEra c) = UpgradeConwayPParams f
  type DowngradePParams f (ConwayEra c) = ()

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
  ppProtocolVersionL = ppLens . lens cppProtocolVersion (\pp x -> pp {cppProtocolVersion = x})

  ppDG = to (const minBound)
  ppuProtocolVersionL = notSupportedInThisEraL
  hkdDL = notSupportedInThisEraL
  hkdExtraEntropyL = notSupportedInThisEraL
  hkdMinUTxOValueL = notSupportedInThisEraL

instance Crypto c => AlonzoEraPParams (ConwayEra c) where
  hkdCoinsPerUTxOWordL = notSupportedInThisEraL
  hkdCostModelsL = lens (unTHKD . cppCostModels) $ \pp x -> pp {cppCostModels = THKD x}
  hkdPricesL = lens (unTHKD . cppPrices) $ \pp x -> pp {cppPrices = THKD x}

  hkdMaxTxExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f (ConwayEra c)) (HKD f ExUnits)
  hkdMaxTxExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . unTHKD . cppMaxTxExUnits) $ \pp x ->
      pp {cppMaxTxExUnits = THKD $ hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxBlockExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f (ConwayEra c)) (HKD f ExUnits)
  hkdMaxBlockExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . unTHKD . cppMaxBlockExUnits) $ \pp x ->
      pp {cppMaxBlockExUnits = THKD $ hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxValSizeL = lens (unTHKD . cppMaxValSize) $ \pp x -> pp {cppMaxValSize = THKD x}
  hkdCollateralPercentageL =
    lens (unTHKD . cppCollateralPercentage) $ \pp x -> pp {cppCollateralPercentage = THKD x}
  hkdMaxCollateralInputsL =
    lens (unTHKD . cppMaxCollateralInputs) $ \pp x -> pp {cppMaxCollateralInputs = THKD x}

instance Crypto c => BabbageEraPParams (ConwayEra c) where
  hkdCoinsPerUTxOByteL =
    lens (unTHKD . cppCoinsPerUTxOByte) $ \pp x -> pp {cppCoinsPerUTxOByte = THKD x}

instance Crypto c => ConwayEraPParams (ConwayEra c) where
  modifiedPPGroups (PParamsUpdate ppu) = conwayModifiedPPGroups ppu
  ppuWellFormed ppu =
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
      , ppu /= emptyPParamsUpdate
      ]
    where
      isValid ::
        (t -> Bool) ->
        Lens' (PParamsUpdate (ConwayEra c)) (StrictMaybe t) ->
        Bool
      isValid p l = case ppu ^. l of
        SJust x -> p x
        SNothing -> True

  hkdPoolVotingThresholdsL =
    lens (unTHKD . cppPoolVotingThresholds) $ \pp x -> pp {cppPoolVotingThresholds = THKD x}
  hkdDRepVotingThresholdsL =
    lens (unTHKD . cppDRepVotingThresholds) $ \pp x -> pp {cppDRepVotingThresholds = THKD x}
  hkdCommitteeMinSizeL =
    lens (unTHKD . cppCommitteeMinSize) $ \pp x -> pp {cppCommitteeMinSize = THKD x}
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

instance Era era => EncCBOR (ConwayPParams Identity era) where
  encCBOR ConwayPParams {..} =
    encode $
      Rec (ConwayPParams @Identity)
        !> To cppMinFeeA
        !> To cppMinFeeB
        !> To cppMaxBBSize
        !> To cppMaxTxSize
        !> To cppMaxBHSize
        !> To cppKeyDeposit
        !> To cppPoolDeposit
        !> To cppEMax
        !> To cppNOpt
        !> To cppA0
        !> To cppRho
        !> To cppTau
        !> To cppProtocolVersion
        !> To cppMinPoolCost
        !> To cppCoinsPerUTxOByte
        !> To cppCostModels
        !> To cppPrices
        !> To cppMaxTxExUnits
        !> To cppMaxBlockExUnits
        !> To cppMaxValSize
        !> To cppCollateralPercentage
        !> To cppMaxCollateralInputs
        -- New for Conway
        !> To cppPoolVotingThresholds
        !> To cppDRepVotingThresholds
        !> To cppCommitteeMinSize
        !> To cppCommitteeMaxTermLength
        !> To cppGovActionLifetime
        !> To cppGovActionDeposit
        !> To cppDRepDeposit
        !> To cppDRepActivity

instance Era era => ToCBOR (ConwayPParams Identity era) where
  toCBOR = toEraCBOR @era

instance Era era => DecCBOR (ConwayPParams Identity era) where
  decCBOR =
    decode $
      RecD (ConwayPParams @Identity)
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        --  -- New for Conway
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance Era era => FromCBOR (ConwayPParams Identity era) where
  fromCBOR = fromEraCBOR @era

instance Crypto c => ToJSON (ConwayPParams Identity (ConwayEra c)) where
  toJSON = object . conwayPParamsPairs
  toEncoding = pairs . mconcat . conwayPParamsPairs

conwayPParamsPairs ::
  forall era a e.
  (ConwayEraPParams era, KeyValue e a) =>
  PParamsHKD Identity era ->
  [a]
conwayPParamsPairs pp =
  uncurry (.=)
    <$> conwayPParamsHKDPairs (Proxy @Identity) pp
      <> [("protocolVersion", toJSON $ PParams pp ^. ppProtocolVersionL)]

instance Era era => FromJSON (ConwayPParams Identity era) where
  parseJSON =
    withObject "PParams" $ \obj ->
      ConwayPParams
        <$> obj .: "minFeeA"
        <*> obj .: "minFeeB"
        <*> obj .: "maxBlockBodySize"
        <*> obj .: "maxTxSize"
        <*> obj .: "maxBlockHeaderSize"
        <*> obj .: "keyDeposit"
        <*> obj .: "poolDeposit"
        <*> obj .: "eMax"
        <*> obj .: "nOpt"
        <*> obj .: "a0"
        <*> obj .: "rho"
        <*> obj .: "tau"
        <*> obj .: "protocolVersion"
        <*> obj .: "minPoolCost" .!= mempty
        <*> obj .: "coinsPerUTxOByte"
        <*> obj .: "costmdls"
        <*> obj .: "prices"
        <*> obj .: "maxTxExUnits"
        <*> obj .: "maxBlockExUnits"
        <*> obj .: "maxValSize"
        <*> obj .: "collateralPercentage"
        <*> obj .: "maxCollateralInputs"
        <*> obj .: "poolVotingThresholds"
        <*> obj .: "dRepVotingThresholds"
        <*> obj .: "committeeMinSize"
        <*> obj .: "committeeMaxTermLength"
        <*> obj .: "govActionLifetime"
        <*> obj .: "govActionDeposit"
        <*> obj .: "dRepDeposit"
        <*> obj .: "dRepActivity"

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
    }

encodePParamsUpdate ::
  ConwayPParams StrictMaybe era ->
  Encode ('Closed 'Sparse) (ConwayPParams StrictMaybe era)
encodePParamsUpdate ppu =
  Keyed ConwayPParams
    !> omitStrictMaybe 0 (cppMinFeeA ppu) encCBOR
    !> omitStrictMaybe 1 (cppMinFeeB ppu) encCBOR
    !> omitStrictMaybe 2 (cppMaxBBSize ppu) encCBOR
    !> omitStrictMaybe 3 (cppMaxTxSize ppu) encCBOR
    !> omitStrictMaybe 4 (cppMaxBHSize ppu) encCBOR
    !> omitStrictMaybe 5 (cppKeyDeposit ppu) encCBOR
    !> omitStrictMaybe 6 (cppPoolDeposit ppu) encCBOR
    !> omitStrictMaybe 7 (cppEMax ppu) encCBOR
    !> omitStrictMaybe 8 (cppNOpt ppu) encCBOR
    !> omitStrictMaybe 9 (cppA0 ppu) encCBOR
    !> omitStrictMaybe 10 (cppRho ppu) encCBOR
    !> omitStrictMaybe 11 (cppTau ppu) encCBOR
    !> OmitC NoUpdate
    !> omitStrictMaybe 16 (cppMinPoolCost ppu) encCBOR
    !> omitStrictMaybe 17 (cppCoinsPerUTxOByte ppu) encCBOR
    !> omitStrictMaybe 18 (cppCostModels ppu) encCBOR
    !> omitStrictMaybe 19 (cppPrices ppu) encCBOR
    !> omitStrictMaybe 20 (cppMaxTxExUnits ppu) encCBOR
    !> omitStrictMaybe 21 (cppMaxBlockExUnits ppu) encCBOR
    !> omitStrictMaybe 22 (cppMaxValSize ppu) encCBOR
    !> omitStrictMaybe 23 (cppCollateralPercentage ppu) encCBOR
    !> omitStrictMaybe 24 (cppMaxCollateralInputs ppu) encCBOR
    -- New for Conway
    !> omitStrictMaybe 25 (cppPoolVotingThresholds ppu) encCBOR
    !> omitStrictMaybe 26 (cppDRepVotingThresholds ppu) encCBOR
    !> omitStrictMaybe 27 (cppCommitteeMinSize ppu) encCBOR
    !> omitStrictMaybe 28 (cppCommitteeMaxTermLength ppu) encCBOR
    !> omitStrictMaybe 29 (cppGovActionLifetime ppu) encCBOR
    !> omitStrictMaybe 30 (cppGovActionDeposit ppu) encCBOR
    !> omitStrictMaybe 31 (cppDRepDeposit ppu) encCBOR
    !> omitStrictMaybe 32 (cppDRepActivity ppu) encCBOR
  where
    omitStrictMaybe ::
      Word ->
      THKD t StrictMaybe a ->
      (a -> Encoding) ->
      Encode ('Closed 'Sparse) (THKD t StrictMaybe a)
    omitStrictMaybe key x enc =
      Omit (isSNothing . unTHKD) (Key key (E (enc . fromSJust . unTHKD) x))

    fromSJust :: StrictMaybe a -> a
    fromSJust (SJust x) = x
    fromSJust SNothing =
      error "SNothing in fromSJust. This should never happen, it is guarded by isSNothing."

instance Era era => EncCBOR (ConwayPParams StrictMaybe era) where
  encCBOR ppup = encode (encodePParamsUpdate ppup)

updateField :: Word -> Field (ConwayPParams StrictMaybe era)
updateField = \case
  0 -> field (\x up -> up {cppMinFeeA = THKD (SJust x)}) From
  1 -> field (\x up -> up {cppMinFeeB = THKD (SJust x)}) From
  2 -> field (\x up -> up {cppMaxBBSize = THKD (SJust x)}) From
  3 -> field (\x up -> up {cppMaxTxSize = THKD (SJust x)}) From
  4 -> field (\x up -> up {cppMaxBHSize = THKD (SJust x)}) From
  5 -> field (\x up -> up {cppKeyDeposit = THKD (SJust x)}) From
  6 -> field (\x up -> up {cppPoolDeposit = THKD (SJust x)}) From
  7 -> field (\x up -> up {cppEMax = THKD (SJust x)}) From
  8 -> field (\x up -> up {cppNOpt = THKD (SJust x)}) From
  9 -> field (\x up -> up {cppA0 = THKD (SJust x)}) From
  10 -> field (\x up -> up {cppRho = THKD (SJust x)}) From
  11 -> field (\x up -> up {cppTau = THKD (SJust x)}) From
  16 -> field (\x up -> up {cppMinPoolCost = THKD (SJust x)}) From
  17 -> field (\x up -> up {cppCoinsPerUTxOByte = THKD (SJust x)}) From
  18 -> field (\x up -> up {cppCostModels = THKD (SJust x)}) $ D decodeValidAndUnknownCostModels
  19 -> field (\x up -> up {cppPrices = THKD (SJust x)}) From
  20 -> field (\x up -> up {cppMaxTxExUnits = THKD (SJust x)}) From
  21 -> field (\x up -> up {cppMaxBlockExUnits = THKD (SJust x)}) From
  22 -> field (\x up -> up {cppMaxValSize = THKD (SJust x)}) From
  23 -> field (\x up -> up {cppCollateralPercentage = THKD (SJust x)}) From
  24 -> field (\x up -> up {cppMaxCollateralInputs = THKD (SJust x)}) From
  -- New for Conway
  25 -> field (\x up -> up {cppPoolVotingThresholds = THKD (SJust x)}) From
  26 -> field (\x up -> up {cppDRepVotingThresholds = THKD (SJust x)}) From
  27 -> field (\x up -> up {cppCommitteeMinSize = THKD (SJust x)}) From
  28 -> field (\x up -> up {cppCommitteeMaxTermLength = THKD (SJust x)}) From
  29 -> field (\x up -> up {cppGovActionLifetime = THKD (SJust x)}) From
  30 -> field (\x up -> up {cppGovActionDeposit = THKD (SJust x)}) From
  31 -> field (\x up -> up {cppDRepDeposit = THKD (SJust x)}) From
  32 -> field (\x up -> up {cppDRepActivity = THKD (SJust x)}) From
  k -> field (\_x up -> up) (Invalid k)

instance Era era => DecCBOR (ConwayPParams StrictMaybe era) where
  decCBOR = decode (SparseKeyed "PParamsUpdate" emptyConwayPParamsUpdate updateField [])

instance Era era => ToCBOR (ConwayPParams StrictMaybe era) where
  toCBOR = toEraCBOR @era

instance Era era => FromCBOR (ConwayPParams StrictMaybe era) where
  fromCBOR = fromEraCBOR @era

instance
  ( ConwayEraPParams era
  , PParamsHKD StrictMaybe era ~ ConwayPParams StrictMaybe era
  ) =>
  ToJSON (ConwayPParams StrictMaybe era)
  where
  toJSON = object . conwayPParamsUpdatePairs
  toEncoding = pairs . mconcat . conwayPParamsUpdatePairs

conwayPParamsUpdatePairs ::
  forall era a e.
  (ConwayEraPParams era, KeyValue e a) =>
  PParamsHKD StrictMaybe era ->
  [a]
conwayPParamsUpdatePairs pp =
  [ k .= v
  | (k, SJust v) <- conwayPParamsHKDPairs (Proxy @StrictMaybe) pp
  ]

conwayPParamsHKDPairs ::
  forall era f.
  (ConwayEraPParams era, HKDFunctor f) =>
  Proxy f ->
  PParamsHKD f era ->
  [(Key, HKD f Aeson.Value)]
conwayPParamsHKDPairs px pp =
  babbageCommonPParamsHKDPairs px pp
    <> conwayUpgradePParamsHKDPairs px pp

conwayUpgradePParamsHKDPairs ::
  forall era f.
  (ConwayEraPParams era, HKDFunctor f) =>
  Proxy f ->
  PParamsHKD f era ->
  [(Key, HKD f Aeson.Value)]
conwayUpgradePParamsHKDPairs px pp =
  [ ("poolVotingThresholds", hkdMap px (toJSON @PoolVotingThresholds) (pp ^. hkdPoolVotingThresholdsL @era @f))
  , ("dRepVotingThresholds", hkdMap px (toJSON @DRepVotingThresholds) (pp ^. hkdDRepVotingThresholdsL @era @f))
  , ("committeeMinSize", hkdMap px (toJSON @Natural) (pp ^. hkdCommitteeMinSizeL @era @f))
  , ("committeeMaxTermLength", hkdMap px (toJSON @EpochInterval) (pp ^. hkdCommitteeMaxTermLengthL @era @f))
  , ("govActionLifetime", hkdMap px (toJSON @EpochInterval) (pp ^. hkdGovActionLifetimeL @era @f))
  , ("govActionDeposit", hkdMap px (toJSON @Coin) (pp ^. hkdGovActionDepositL @era @f))
  , ("dRepDeposit", hkdMap px (toJSON @Coin) (pp ^. hkdDRepDepositL @era @f))
  , ("dRepActivity", hkdMap px (toJSON @EpochInterval) (pp ^. hkdDRepActivityL @era @f))
  ]

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
  , ("committeeMinSize", (toJSON @Natural) ucppCommitteeMinSize)
  , ("committeeMaxTermLength", (toJSON @EpochInterval) ucppCommitteeMaxTermLength)
  , ("govActionLifetime", (toJSON @EpochInterval) ucppGovActionLifetime)
  , ("govActionDeposit", (toJSON @Coin) ucppGovActionDeposit)
  , ("dRepDeposit", (toJSON @Coin) ucppDRepDeposit)
  , ("dRepActivity", (toJSON @EpochInterval) ucppDRepActivity)
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

upgradeConwayPParams ::
  forall f c.
  HKDFunctor f =>
  UpgradeConwayPParams f ->
  PParamsHKD f (BabbageEra c) ->
  ConwayPParams f (ConwayEra c)
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
    , cppCostModels = THKD bppCostModels
    , cppPrices = THKD bppPrices
    , cppMaxTxExUnits = THKD bppMaxTxExUnits
    , cppMaxBlockExUnits = THKD bppMaxBlockExUnits
    , cppMaxValSize = THKD bppMaxValSize
    , cppCollateralPercentage = THKD bppCollateralPercentage
    , cppMaxCollateralInputs = THKD bppMaxCollateralInputs
    , -- New for Conway
      cppPoolVotingThresholds = THKD ucppPoolVotingThresholds
    , cppDRepVotingThresholds = THKD ucppDRepVotingThresholds
    , cppCommitteeMinSize = THKD ucppCommitteeMinSize
    , cppCommitteeMaxTermLength = THKD ucppCommitteeMaxTermLength
    , cppGovActionLifetime = THKD ucppGovActionLifetime
    , cppGovActionDeposit = THKD ucppGovActionDeposit
    , cppDRepDeposit = THKD ucppDRepDeposit
    , cppDRepActivity = THKD ucppDRepActivity
    }

downgradeConwayPParams ::
  forall f c.
  HKDFunctor f =>
  ConwayPParams f (ConwayEra c) ->
  PParamsHKD f (BabbageEra c)
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
    , bppMaxValSize = unTHKD cppMaxValSize
    , bppCollateralPercentage = unTHKD cppCollateralPercentage
    , bppMaxCollateralInputs = unTHKD cppMaxCollateralInputs
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
      ]
