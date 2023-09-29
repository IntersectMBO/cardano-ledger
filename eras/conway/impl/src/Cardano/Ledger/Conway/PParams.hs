{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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
  BabbagePParams (..),
  ConwayPParams (..),
  getLanguageView,
  LangDepView (..),
  encodeLangViews,
  upgradeConwayPParams,
  UpgradeConwayPParams (..),
  toUpgradeConwayPParamsUpdatePairs,
  PoolVotingThresholds (..),
  DRepVotingThresholds (..),
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
  PParamGroup (..),
  modifiedGroups,
)
where

import Cardano.Ledger.Alonzo.PParams (OrdExUnits (..))
import Cardano.Ledger.Alonzo.Scripts (CostModels, ExUnits (..), Prices (Prices), emptyCostModels, updateCostModels)
import Cardano.Ledger.Ap (Ap, runAp_)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.BaseTypes (EpochNo (EpochNo), NonNegativeInterval, ProtVer (ProtVer), UnitInterval)
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway.Core hiding (Value)
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Crypto
import Cardano.Ledger.HKD (HKD, HKDFunctor (..))
import Cardano.Ledger.TreeDiff (ToExpr)
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData)
import Data.Aeson hiding (Encoding, decode, encode)
import qualified Data.Aeson as Aeson
import Data.Default.Class (Default (def))
import Data.Functor.Identity (Identity)
import Data.Maybe.Strict (StrictMaybe (..), fromSMaybe, isSNothing)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)
import Numeric.Natural (Natural)

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
  { cppMinFeeA :: !(HKD f Coin)
  -- ^ The linear factor for the minimum fee calculation
  , cppMinFeeB :: !(HKD f Coin)
  -- ^ The constant factor for the minimum fee calculation
  , cppMaxBBSize :: !(HKD f Natural)
  -- ^ Maximal block body size
  , cppMaxTxSize :: !(HKD f Natural)
  -- ^ Maximal transaction size
  , cppMaxBHSize :: !(HKD f Natural)
  -- ^ Maximal block header size
  , cppKeyDeposit :: !(HKD f Coin)
  -- ^ The amount of a key registration deposit
  , cppPoolDeposit :: !(HKD f Coin)
  -- ^ The amount of a pool registration deposit
  , cppEMax :: !(HKD f EpochNo)
  -- ^ Maximum number of epochs in the future a pool retirement is allowed to
  -- be scheduled for.
  , cppNOpt :: !(HKD f Natural)
  -- ^ Desired number of pools
  , cppA0 :: !(HKD f NonNegativeInterval)
  -- ^ Pool influence
  , cppRho :: !(HKD f UnitInterval)
  -- ^ Monetary expansion
  , cppTau :: !(HKD f UnitInterval)
  -- ^ Treasury expansion
  , cppProtocolVersion :: !(HKD f ProtVer)
  -- ^ Protocol version
  , cppMinPoolCost :: !(HKD f Coin)
  -- ^ Minimum Stake Pool Cost
  , cppCoinsPerUTxOByte :: !(HKD f CoinPerByte)
  -- ^ Cost in lovelace per byte of UTxO storage
  , cppCostModels :: !(HKD f CostModels)
  -- ^ Cost models for non-native script languages
  , cppPrices :: !(HKD f Prices)
  -- ^ Prices of execution units (for non-native script languages)
  , cppMaxTxExUnits :: !(HKD f OrdExUnits)
  -- ^ Max total script execution resources units allowed per tx
  , cppMaxBlockExUnits :: !(HKD f OrdExUnits)
  -- ^ Max total script execution resources units allowed per block
  , cppMaxValSize :: !(HKD f Natural)
  -- ^ Max size of a Value in an output
  , cppCollateralPercentage :: !(HKD f Natural)
  -- ^ Percentage of the txfee which must be provided as collateral when
  -- including non-native scripts.
  , cppMaxCollateralInputs :: !(HKD f Natural)
  -- ^ Maximum number of collateral inputs allowed in a transaction
  --
  -- New ones for Conway
  , cppPoolVotingThresholds :: !(HKD f PoolVotingThresholds)
  -- ^ Thresholds for SPO votes
  , cppDRepVotingThresholds :: !(HKD f DRepVotingThresholds)
  -- ^ Thresholds for DRep votes
  , cppCommitteeMinSize :: !(HKD f Natural)
  -- ^ Minimum size of the Constitutional Committee
  , cppCommitteeMaxTermLength :: !(HKD f Natural) -- TODO: This too should be EpochNo

  -- ^ The Constitutional Committee Term limit in number of Slots
  , cppGovActionLifetime :: !(HKD f EpochNo)
  -- ^ Gov action lifetime in number of Epochs
  , cppGovActionDeposit :: !(HKD f Coin)
  -- ^ The amount of the Gov Action deposit
  , cppDRepDeposit :: !(HKD f Coin)
  -- ^ The amount of a DRep registration deposit
  , cppDRepActivity :: !(HKD f EpochNo)
  -- ^ The number of Epochs that a DRep can perform no activity without losing their @Active@ status.
  }
  deriving (Generic)

instance ToExpr (ConwayPParams Identity era)

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
  , ucppCommitteeMaxTermLength :: !(HKD f Natural)
  , ucppGovActionLifetime :: !(HKD f EpochNo)
  , ucppGovActionDeposit :: !(HKD f Coin)
  , ucppDRepDeposit :: !(HKD f Coin)
  , ucppDRepActivity :: !(HKD f EpochNo)
  }
  deriving (Generic)

deriving instance Eq (UpgradeConwayPParams Identity)

deriving instance Ord (UpgradeConwayPParams Identity)

deriving instance Show (UpgradeConwayPParams Identity)

instance NoThunks (UpgradeConwayPParams Identity)

instance NFData (UpgradeConwayPParams Identity)

instance ToExpr (ConwayPParams StrictMaybe era)

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
      , ucppCommitteeMaxTermLength = 0
      , ucppGovActionLifetime = EpochNo 0
      , ucppGovActionDeposit = Coin 0
      , ucppDRepDeposit = Coin 0
      , ucppDRepActivity = EpochNo 0
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

  applyPPUpdates :: PParams (ConwayEra c) -> PParamsUpdate (ConwayEra c) -> PParams (ConwayEra c)
  applyPPUpdates pp ppu =
    genericApplyPPUpdates pp ppu
      & ppCostModelsL
        .~ updateCostModels
          (pp ^. ppCostModelsL)
          (fromSMaybe emptyCostModels $ ppu ^. ppuCostModelsL)

  emptyPParamsIdentity = emptyConwayPParams
  emptyPParamsStrictMaybe = emptyConwayPParamsUpdate

  upgradePParamsHKD = upgradeConwayPParams
  downgradePParamsHKD () = downgradeConwayPParams

  hkdMinFeeAL = lens cppMinFeeA $ \pp x -> pp {cppMinFeeA = x}
  hkdMinFeeBL = lens cppMinFeeB $ \pp x -> pp {cppMinFeeB = x}
  hkdMaxBBSizeL = lens cppMaxBBSize $ \pp x -> pp {cppMaxBBSize = x}
  hkdMaxTxSizeL = lens cppMaxTxSize $ \pp x -> pp {cppMaxTxSize = x}
  hkdMaxBHSizeL = lens cppMaxBHSize $ \pp x -> pp {cppMaxBHSize = x}
  hkdKeyDepositL = lens cppKeyDeposit $ \pp x -> pp {cppKeyDeposit = x}
  hkdPoolDepositL = lens cppPoolDeposit $ \pp x -> pp {cppPoolDeposit = x}
  hkdEMaxL = lens cppEMax $ \pp x -> pp {cppEMax = x}
  hkdNOptL = lens cppNOpt $ \pp x -> pp {cppNOpt = x}
  hkdA0L = lens cppA0 $ \pp x -> pp {cppA0 = x}
  hkdRhoL = lens cppRho $ \pp x -> pp {cppRho = x}
  hkdTauL = lens cppTau $ \pp x -> pp {cppTau = x}
  hkdProtocolVersionL = lens cppProtocolVersion $ \pp x -> pp {cppProtocolVersion = x}
  hkdMinPoolCostL = lens cppMinPoolCost $ \pp x -> pp {cppMinPoolCost = x}

  ppDG = to (const minBound)
  hkdDL = notSupportedInThisEraL
  hkdExtraEntropyL = notSupportedInThisEraL
  hkdMinUTxOValueL = notSupportedInThisEraL

instance Crypto c => AlonzoEraPParams (ConwayEra c) where
  hkdCoinsPerUTxOWordL = notSupportedInThisEraL
  hkdCostModelsL = lens cppCostModels $ \pp x -> pp {cppCostModels = x}
  hkdPricesL = lens cppPrices $ \pp x -> pp {cppPrices = x}
  hkdMaxTxExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f (ConwayEra c)) (HKD f ExUnits)
  hkdMaxTxExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . cppMaxTxExUnits) $ \pp x ->
      pp {cppMaxTxExUnits = hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxBlockExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f (ConwayEra c)) (HKD f ExUnits)
  hkdMaxBlockExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . cppMaxBlockExUnits) $ \pp x ->
      pp {cppMaxBlockExUnits = hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxValSizeL = lens cppMaxValSize $ \pp x -> pp {cppMaxValSize = x}
  hkdCollateralPercentageL =
    lens cppCollateralPercentage $ \pp x -> pp {cppCollateralPercentage = x}
  hkdMaxCollateralInputsL =
    lens cppMaxCollateralInputs $ \pp x -> pp {cppMaxCollateralInputs = x}

instance Crypto c => BabbageEraPParams (ConwayEra c) where
  hkdCoinsPerUTxOByteL = lens cppCoinsPerUTxOByte (\pp x -> pp {cppCoinsPerUTxOByte = x})

instance Crypto c => ConwayEraPParams (ConwayEra c) where
  pparamsGroups (PParamsUpdate ConwayPParams {..}) =
    ConwayPParams
      <$> pGroup EconomicGroup cppMinFeeA
      <*> pGroup EconomicGroup cppMinFeeB
      <*> pGroup NetworkGroup cppMaxBBSize
      <*> pGroup NetworkGroup cppMaxTxSize
      <*> pGroup NetworkGroup cppMaxBHSize
      <*> pGroup EconomicGroup cppKeyDeposit
      <*> pGroup EconomicGroup cppPoolDeposit
      <*> pGroup TechnicalGroup cppEMax
      <*> pGroup TechnicalGroup cppNOpt
      <*> pGroup TechnicalGroup cppA0
      <*> pGroup EconomicGroup cppRho
      <*> pGroup EconomicGroup cppTau
      <*> pUngrouped
      <*> pGroup EconomicGroup cppMinPoolCost
      <*> pGroup EconomicGroup cppCoinsPerUTxOByte
      <*> pGroup TechnicalGroup cppCostModels
      <*> pGroup EconomicGroup cppPrices
      <*> pGroup NetworkGroup cppMaxTxExUnits
      <*> pGroup NetworkGroup cppMaxBlockExUnits
      <*> pGroup NetworkGroup cppMaxValSize
      <*> pGroup TechnicalGroup cppCollateralPercentage
      <*> pGroup NetworkGroup cppMaxCollateralInputs
      <*> pGroup GovernanceGroup cppPoolVotingThresholds
      <*> pGroup GovernanceGroup cppDRepVotingThresholds
      <*> pGroup GovernanceGroup cppCommitteeMinSize
      <*> pGroup GovernanceGroup cppCommitteeMaxTermLength
      <*> pGroup GovernanceGroup cppGovActionLifetime
      <*> pGroup GovernanceGroup cppGovActionDeposit
      <*> pGroup GovernanceGroup cppDRepDeposit
      <*> pGroup GovernanceGroup cppDRepActivity

  ppuWellFormed ppu =
    and
      [ -- Numbers
        isValid (/= 0) ppuMaxBBSizeL
      , isValid (/= 0) ppuMaxTxSizeL
      , isValid (/= 0) ppuMaxBHSizeL
      , isValid (/= 0) ppuMaxValSizeL
      , isValid (/= 0) ppuCollateralPercentageL
      , isValid (/= 0) ppuCommitteeMaxTermLengthL
      , isValid (/= EpochNo 0) ppuGovActionLifetimeL
      , -- Coins
        isValid (/= zero) ppuPoolDepositL
      , isValid (/= zero) ppuGovActionDepositL
      , isValid (/= zero) ppuDRepDepositL
      ]
    where
      isValid ::
        (t -> Bool) ->
        Lens' (PParamsUpdate (ConwayEra c)) (StrictMaybe t) ->
        Bool
      isValid p l = case ppu ^. l of
        SJust x -> p x
        SNothing -> True

  hkdPoolVotingThresholdsL = lens cppPoolVotingThresholds (\pp x -> pp {cppPoolVotingThresholds = x})
  hkdDRepVotingThresholdsL = lens cppDRepVotingThresholds (\pp x -> pp {cppDRepVotingThresholds = x})
  hkdCommitteeMinSizeL = lens cppCommitteeMinSize (\pp x -> pp {cppCommitteeMinSize = x})
  hkdCommitteeMaxTermLengthL = lens cppCommitteeMaxTermLength (\pp x -> pp {cppCommitteeMaxTermLength = x})
  hkdGovActionLifetimeL = lens cppGovActionLifetime (\pp x -> pp {cppGovActionLifetime = x})
  hkdGovActionDepositL = lens cppGovActionDeposit (\pp x -> pp {cppGovActionDeposit = x})
  hkdDRepDepositL = lens cppDRepDeposit (\pp x -> pp {cppDRepDeposit = x})
  hkdDRepActivityL = lens cppDRepActivity (\pp x -> pp {cppDRepActivity = x})

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
  forall era a.
  (ConwayEraPParams era, KeyValue a) =>
  PParamsHKD Identity era ->
  [a]
conwayPParamsPairs pp =
  uncurry (.=) <$> conwayPParamsHKDPairs (Proxy @Identity) pp

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyConwayPParams :: forall era. Era era => ConwayPParams Identity era
emptyConwayPParams =
  ConwayPParams
    { cppMinFeeA = Coin 0
    , cppMinFeeB = Coin 0
    , cppMaxBBSize = 0
    , cppMaxTxSize = 2048
    , cppMaxBHSize = 0
    , cppKeyDeposit = Coin 0
    , cppPoolDeposit = Coin 0
    , cppEMax = EpochNo 0
    , cppNOpt = 100
    , cppA0 = minBound
    , cppRho = minBound
    , cppTau = minBound
    , cppProtocolVersion = ProtVer (eraProtVerLow @era) 0
    , cppMinPoolCost = mempty
    , cppCoinsPerUTxOByte = CoinPerByte $ Coin 0
    , cppCostModels = emptyCostModels
    , cppPrices = Prices minBound minBound
    , cppMaxTxExUnits = OrdExUnits $ ExUnits 0 0
    , cppMaxBlockExUnits = OrdExUnits $ ExUnits 0 0
    , cppMaxValSize = 0
    , cppCollateralPercentage = 150
    , cppMaxCollateralInputs = 5
    , -- New in Conway
      cppPoolVotingThresholds = def
    , cppDRepVotingThresholds = def
    , cppCommitteeMinSize = 0
    , cppCommitteeMaxTermLength = 0
    , cppGovActionLifetime = EpochNo 0
    , cppGovActionDeposit = Coin 0
    , cppDRepDeposit = Coin 0
    , cppDRepActivity = EpochNo 0
    }

emptyConwayPParamsUpdate :: ConwayPParams StrictMaybe era
emptyConwayPParamsUpdate =
  ConwayPParams
    { cppMinFeeA = SNothing
    , cppMinFeeB = SNothing
    , cppMaxBBSize = SNothing
    , cppMaxTxSize = SNothing
    , cppMaxBHSize = SNothing
    , cppKeyDeposit = SNothing
    , cppPoolDeposit = SNothing
    , cppEMax = SNothing
    , cppNOpt = SNothing
    , cppA0 = SNothing
    , cppRho = SNothing
    , cppTau = SNothing
    , cppProtocolVersion = SNothing
    , cppMinPoolCost = SNothing
    , cppCoinsPerUTxOByte = SNothing
    , cppCostModels = SNothing
    , cppPrices = SNothing
    , cppMaxTxExUnits = SNothing
    , cppMaxBlockExUnits = SNothing
    , cppMaxValSize = SNothing
    , cppCollateralPercentage = SNothing
    , cppMaxCollateralInputs = SNothing
    , -- New for Conway
      cppPoolVotingThresholds = SNothing
    , cppDRepVotingThresholds = SNothing
    , cppCommitteeMinSize = SNothing
    , cppCommitteeMaxTermLength = SNothing
    , cppGovActionLifetime = SNothing
    , cppGovActionDeposit = SNothing
    , cppDRepDeposit = SNothing
    , cppDRepActivity = SNothing
    }

encodePParamsUpdate ::
  ConwayPParams StrictMaybe era ->
  Encode ('Closed 'Sparse) (ConwayPParams StrictMaybe era)
encodePParamsUpdate ppup =
  Keyed ConwayPParams
    !> omitStrictMaybe 0 (cppMinFeeA ppup) encCBOR
    !> omitStrictMaybe 1 (cppMinFeeB ppup) encCBOR
    !> omitStrictMaybe 2 (cppMaxBBSize ppup) encCBOR
    !> omitStrictMaybe 3 (cppMaxTxSize ppup) encCBOR
    !> omitStrictMaybe 4 (cppMaxBHSize ppup) encCBOR
    !> omitStrictMaybe 5 (cppKeyDeposit ppup) encCBOR
    !> omitStrictMaybe 6 (cppPoolDeposit ppup) encCBOR
    !> omitStrictMaybe 7 (cppEMax ppup) encCBOR
    !> omitStrictMaybe 8 (cppNOpt ppup) encCBOR
    !> omitStrictMaybe 9 (cppA0 ppup) encCBOR
    !> omitStrictMaybe 10 (cppRho ppup) encCBOR
    !> omitStrictMaybe 11 (cppTau ppup) encCBOR
    !> omitStrictMaybe 14 SNothing encCBOR
    !> omitStrictMaybe 16 (cppMinPoolCost ppup) encCBOR
    !> omitStrictMaybe 17 (cppCoinsPerUTxOByte ppup) encCBOR
    !> omitStrictMaybe 18 (cppCostModels ppup) encCBOR
    !> omitStrictMaybe 19 (cppPrices ppup) encCBOR
    !> omitStrictMaybe 20 (cppMaxTxExUnits ppup) encCBOR
    !> omitStrictMaybe 21 (cppMaxBlockExUnits ppup) encCBOR
    !> omitStrictMaybe 22 (cppMaxValSize ppup) encCBOR
    !> omitStrictMaybe 23 (cppCollateralPercentage ppup) encCBOR
    !> omitStrictMaybe 24 (cppMaxCollateralInputs ppup) encCBOR
    -- New for Conway
    !> omitStrictMaybe 25 (cppPoolVotingThresholds ppup) encCBOR
    !> omitStrictMaybe 26 (cppDRepVotingThresholds ppup) encCBOR
    !> omitStrictMaybe 27 (cppCommitteeMinSize ppup) encCBOR
    !> omitStrictMaybe 28 (cppCommitteeMaxTermLength ppup) encCBOR
    !> omitStrictMaybe 29 (cppGovActionLifetime ppup) encCBOR
    !> omitStrictMaybe 30 (cppGovActionDeposit ppup) encCBOR
    !> omitStrictMaybe 31 (cppDRepDeposit ppup) encCBOR
    !> omitStrictMaybe 32 (cppDRepActivity ppup) encCBOR
  where
    omitStrictMaybe ::
      Word -> StrictMaybe a -> (a -> Encoding) -> Encode ('Closed 'Sparse) (StrictMaybe a)
    omitStrictMaybe key x enc = Omit isSNothing (Key key (E (enc . fromSJust) x))

    fromSJust :: StrictMaybe a -> a
    fromSJust (SJust x) = x
    fromSJust SNothing = error "SNothing in fromSJust. This should never happen, it is guarded by isSNothing."

instance Era era => EncCBOR (ConwayPParams StrictMaybe era) where
  encCBOR ppup = encode (encodePParamsUpdate ppup)

updateField :: Word -> Field (ConwayPParams StrictMaybe era)
updateField = \case
  0 -> field (\x up -> up {cppMinFeeA = SJust x}) From
  1 -> field (\x up -> up {cppMinFeeB = SJust x}) From
  2 -> field (\x up -> up {cppMaxBBSize = SJust x}) From
  3 -> field (\x up -> up {cppMaxTxSize = SJust x}) From
  4 -> field (\x up -> up {cppMaxBHSize = SJust x}) From
  5 -> field (\x up -> up {cppKeyDeposit = SJust x}) From
  6 -> field (\x up -> up {cppPoolDeposit = SJust x}) From
  7 -> field (\x up -> up {cppEMax = SJust x}) From
  8 -> field (\x up -> up {cppNOpt = SJust x}) From
  9 -> field (\x up -> up {cppA0 = SJust x}) From
  10 -> field (\x up -> up {cppRho = SJust x}) From
  11 -> field (\x up -> up {cppTau = SJust x}) From
  16 -> field (\x up -> up {cppMinPoolCost = SJust x}) From
  17 -> field (\x up -> up {cppCoinsPerUTxOByte = SJust x}) From
  18 -> field (\x up -> up {cppCostModels = SJust x}) From
  19 -> field (\x up -> up {cppPrices = SJust x}) From
  20 -> field (\x up -> up {cppMaxTxExUnits = SJust x}) From
  21 -> field (\x up -> up {cppMaxBlockExUnits = SJust x}) From
  22 -> field (\x up -> up {cppMaxValSize = SJust x}) From
  23 -> field (\x up -> up {cppCollateralPercentage = SJust x}) From
  24 -> field (\x up -> up {cppMaxCollateralInputs = SJust x}) From
  -- New for Conway
  25 -> field (\x up -> up {cppPoolVotingThresholds = SJust x}) From
  26 -> field (\x up -> up {cppDRepVotingThresholds = SJust x}) From
  27 -> field (\x up -> up {cppCommitteeMinSize = SJust x}) From
  28 -> field (\x up -> up {cppCommitteeMaxTermLength = SJust x}) From
  29 -> field (\x up -> up {cppGovActionLifetime = SJust x}) From
  30 -> field (\x up -> up {cppGovActionDeposit = SJust x}) From
  31 -> field (\x up -> up {cppDRepDeposit = SJust x}) From
  32 -> field (\x up -> up {cppDRepActivity = SJust x}) From
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
  forall era a.
  (ConwayEraPParams era, KeyValue a) =>
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
  [(Key, HKD f Value)]
conwayPParamsHKDPairs px pp = babbagePParamsHKDPairs px pp <> conwayUpgradePParamsHKDPairs px pp

conwayUpgradePParamsHKDPairs ::
  forall era f.
  (ConwayEraPParams era, HKDFunctor f) =>
  Proxy f ->
  PParamsHKD f era ->
  [(Key, HKD f Value)]
conwayUpgradePParamsHKDPairs px pp =
  [ ("poolVotingThresholds", hkdMap px (toJSON @PoolVotingThresholds) (pp ^. hkdPoolVotingThresholdsL @era @f))
  , ("dRepVotingThresholds", hkdMap px (toJSON @DRepVotingThresholds) (pp ^. hkdDRepVotingThresholdsL @era @f))
  , ("committeeMinSize", hkdMap px (toJSON @Natural) (pp ^. hkdCommitteeMinSizeL @era @f))
  , ("committeeMaxTermLength", hkdMap px (toJSON @Natural) (pp ^. hkdCommitteeMaxTermLengthL @era @f))
  , ("govActionLifetime", hkdMap px (toJSON @EpochNo) (pp ^. hkdGovActionLifetimeL @era @f))
  , ("govActionDeposit", hkdMap px (toJSON @Coin) (pp ^. hkdGovActionDepositL @era @f))
  , ("dRepDeposit", hkdMap px (toJSON @Coin) (pp ^. hkdDRepDepositL @era @f))
  , ("dRepActivity", hkdMap px (toJSON @EpochNo) (pp ^. hkdDRepActivityL @era @f))
  ]

instance ToJSON (UpgradeConwayPParams Identity) where
  toJSON = object . toUpgradeConwayPParamsUpdatePairs
  toEncoding = pairs . mconcat . toUpgradeConwayPParamsUpdatePairs

toUpgradeConwayPParamsUpdatePairs :: KeyValue a => UpgradeConwayPParams Identity -> [a]
toUpgradeConwayPParamsUpdatePairs upp =
  uncurry (.=) <$> upgradeConwayPParamsHKDPairs upp

upgradeConwayPParamsHKDPairs :: UpgradeConwayPParams Identity -> [(Key, Aeson.Value)]
upgradeConwayPParamsHKDPairs UpgradeConwayPParams {..} =
  [ ("poolVotingThresholds", (toJSON @PoolVotingThresholds) ucppPoolVotingThresholds)
  , ("dRepVotingThresholds", (toJSON @DRepVotingThresholds) ucppDRepVotingThresholds)
  , ("committeeMinSize", (toJSON @Natural) ucppCommitteeMinSize)
  , ("committeeMaxTermLength", (toJSON @Natural) ucppCommitteeMaxTermLength)
  , ("govActionLifetime", (toJSON @EpochNo) ucppGovActionLifetime)
  , ("govActionDeposit", (toJSON @Coin) ucppGovActionDeposit)
  , ("dRepDeposit", (toJSON @Coin) ucppDRepDeposit)
  , ("dRepActivity", (toJSON @EpochNo) ucppDRepActivity)
  ]

instance FromJSON PoolVotingThresholds

instance FromJSON DRepVotingThresholds

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
  UpgradeConwayPParams f ->
  PParamsHKD f (BabbageEra c) ->
  ConwayPParams f (ConwayEra c)
upgradeConwayPParams UpgradeConwayPParams {..} BabbagePParams {..} =
  ConwayPParams
    { cppMinFeeA = bppMinFeeA
    , cppMinFeeB = bppMinFeeB
    , cppMaxBBSize = bppMaxBBSize
    , cppMaxTxSize = bppMaxTxSize
    , cppMaxBHSize = bppMaxBHSize
    , cppKeyDeposit = bppKeyDeposit
    , cppPoolDeposit = bppPoolDeposit
    , cppEMax = bppEMax
    , cppNOpt = bppNOpt
    , cppA0 = bppA0
    , cppRho = bppRho
    , cppTau = bppTau
    , cppProtocolVersion = bppProtocolVersion
    , cppMinPoolCost = bppMinPoolCost
    , cppCoinsPerUTxOByte = bppCoinsPerUTxOByte
    , cppCostModels = bppCostModels
    , cppPrices = bppPrices
    , cppMaxTxExUnits = bppMaxTxExUnits
    , cppMaxBlockExUnits = bppMaxBlockExUnits
    , cppMaxValSize = bppMaxValSize
    , cppCollateralPercentage = bppCollateralPercentage
    , cppMaxCollateralInputs = bppMaxCollateralInputs
    , -- New for Conway
      cppPoolVotingThresholds = ucppPoolVotingThresholds
    , cppDRepVotingThresholds = ucppDRepVotingThresholds
    , cppCommitteeMinSize = ucppCommitteeMinSize
    , cppCommitteeMaxTermLength = ucppCommitteeMaxTermLength
    , cppGovActionLifetime = ucppGovActionLifetime
    , cppGovActionDeposit = ucppGovActionDeposit
    , cppDRepDeposit = ucppDRepDeposit
    , cppDRepActivity = ucppDRepActivity
    }

downgradeConwayPParams ::
  forall f c.
  ConwayPParams f (ConwayEra c) ->
  PParamsHKD f (BabbageEra c)
downgradeConwayPParams ConwayPParams {..} =
  BabbagePParams
    { bppMinFeeA = cppMinFeeA
    , bppMinFeeB = cppMinFeeB
    , bppMaxBBSize = cppMaxBBSize
    , bppMaxTxSize = cppMaxTxSize
    , bppMaxBHSize = cppMaxBHSize
    , bppKeyDeposit = cppKeyDeposit
    , bppPoolDeposit = cppPoolDeposit
    , bppEMax = cppEMax
    , bppNOpt = cppNOpt
    , bppA0 = cppA0
    , bppRho = cppRho
    , bppTau = cppTau
    , bppProtocolVersion = cppProtocolVersion
    , bppMinPoolCost = cppMinPoolCost
    , bppCoinsPerUTxOByte = cppCoinsPerUTxOByte
    , bppCostModels = cppCostModels
    , bppPrices = cppPrices
    , bppMaxTxExUnits = cppMaxTxExUnits
    , bppMaxBlockExUnits = cppMaxBlockExUnits
    , bppMaxValSize = cppMaxValSize
    , bppCollateralPercentage = cppCollateralPercentage
    , bppMaxCollateralInputs = cppMaxCollateralInputs
    }

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
  hkdCommitteeMinSizeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)
  hkdCommitteeMaxTermLengthL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)
  hkdGovActionLifetimeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f EpochNo)
  hkdGovActionDepositL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)
  hkdDRepDepositL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Coin)
  hkdDRepActivityL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f EpochNo)

ppPoolVotingThresholdsL :: forall era. ConwayEraPParams era => Lens' (PParams era) PoolVotingThresholds
ppPoolVotingThresholdsL = ppLens . hkdPoolVotingThresholdsL @era @Identity

ppDRepVotingThresholdsL :: forall era. ConwayEraPParams era => Lens' (PParams era) DRepVotingThresholds
ppDRepVotingThresholdsL = ppLens . hkdDRepVotingThresholdsL @era @Identity

ppCommitteeMinSizeL :: forall era. ConwayEraPParams era => Lens' (PParams era) Natural
ppCommitteeMinSizeL = ppLens . hkdCommitteeMinSizeL @era @Identity

ppCommitteeMaxTermLengthL :: forall era. ConwayEraPParams era => Lens' (PParams era) Natural
ppCommitteeMaxTermLengthL = ppLens . hkdCommitteeMaxTermLengthL @era @Identity

ppGovActionLifetimeL :: forall era. ConwayEraPParams era => Lens' (PParams era) EpochNo
ppGovActionLifetimeL = ppLens . hkdGovActionLifetimeL @era @Identity

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

ppuCommitteeMinSizeL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuCommitteeMinSizeL = ppuLens . hkdCommitteeMinSizeL @era @StrictMaybe

ppuCommitteeMaxTermLengthL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuCommitteeMaxTermLengthL = ppuLens . hkdCommitteeMaxTermLengthL @era @StrictMaybe

ppuGovActionLifetimeL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe EpochNo)
ppuGovActionLifetimeL = ppuLens . hkdGovActionLifetimeL @era @StrictMaybe

ppuGovActionDepositL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuGovActionDepositL = ppuLens . hkdGovActionDepositL @era @StrictMaybe

ppuDRepDepositL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuDRepDepositL = ppuLens . hkdDRepDepositL @era @StrictMaybe

ppuDRepActivityL :: forall era. ConwayEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe EpochNo)
ppuDRepActivityL = ppuLens . hkdDRepActivityL @era @StrictMaybe
