{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains just the type of protocol parameters.
module Cardano.Ledger.Alonzo.PParams (
  -- * Era Agnostic
  AlonzoEraPParams (..),
  ppCoinsPerUTxOWordL,
  ppCostModelsL,
  ppPricesL,
  ppMaxTxExUnitsL,
  ppMaxBlockExUnitsL,
  ppMaxValSizeL,
  ppCollateralPercentageL,
  ppMaxCollateralInputsL,
  ppuCoinsPerUTxOWordL,
  ppuCostModelsL,
  ppuPricesL,
  ppuMaxTxExUnitsL,
  ppuMaxBlockExUnitsL,
  ppuMaxValSizeL,
  ppuCollateralPercentageL,
  ppuMaxCollateralInputsL,

  -- * Alonzo specific
  AlonzoPParams (..),
  UpgradeAlonzoPParams (..),
  DowngradeAlonzoPParams (..),
  emptyAlonzoPParams,
  emptyAlonzoPParamsUpdate,
  upgradeAlonzoPParams,
  downgradeAlonzoPParams,
  getLanguageView,
  LangDepView (..),
  encodeLangViews,
  OrdExUnits (..),
  CoinPerWord (..),

  -- * PParam
  ppCollateralPercentage,
  ppCostModels,
  ppMaxBlockExUnits,
  ppMaxCollateralInputs,
  ppMaxTxExUnits,
  ppMaxValSize,
  ppPrices,
)
where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  NonNegativeInterval,
  Nonce (NeutralNonce),
  StrictMaybe (..),
  UnitInterval,
 )
import qualified Cardano.Ledger.BaseTypes as BT (ProtVer (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  Encoding,
  encodeFoldableAsDefLenList,
  encodeFoldableAsIndefLenList,
  encodeMapLen,
  encodeNull,
  encodePreEncoded,
  serialize',
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraPParams (..))
import Cardano.Ledger.HKD (HKDFunctor (..))
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Plutus.CostModels (
  CostModel,
  CostModels,
  costModelsValid,
  emptyCostModels,
  getCostModelLanguage,
  getCostModelParams,
 )
import Cardano.Ledger.Plutus.ExUnits (
  ExUnits (..),
  Prices (..),
  zipSemiExUnits,
 )
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Plutus.ToPlutusData (ToPlutusData (..))
import Cardano.Ledger.Shelley.PParams
import Control.DeepSeq (NFData)
import Data.Aeson as Aeson (
  FromJSON,
  ToJSON (..),
 )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Default (Default (def))
import Data.Function (on)
import Data.Functor.Identity (Identity (..))
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.))
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

class EraPParams era => AlonzoEraPParams era where
  hkdCoinsPerUTxOWordL ::
    (HKDFunctor f, ExactEra AlonzoEra era) =>
    Lens' (PParamsHKD f era) (HKD f CoinPerWord)

  hkdCostModelsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f CostModels)

  hkdPricesL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Prices)

  hkdMaxTxExUnitsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f ExUnits)

  hkdMaxBlockExUnitsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f ExUnits)

  hkdMaxValSizeL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)

  hkdCollateralPercentageL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)

  hkdMaxCollateralInputsL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f Natural)

ppCoinsPerUTxOWordL ::
  forall era.
  (AlonzoEraPParams era, ExactEra AlonzoEra era) =>
  Lens' (PParams era) CoinPerWord
ppCoinsPerUTxOWordL = ppLens . hkdCoinsPerUTxOWordL @era @Identity

ppCostModelsL :: forall era. AlonzoEraPParams era => Lens' (PParams era) CostModels
ppCostModelsL = ppLens . hkdCostModelsL @era @Identity

ppPricesL :: forall era. AlonzoEraPParams era => Lens' (PParams era) Prices
ppPricesL = ppLens . hkdPricesL @era @Identity

ppMaxTxExUnitsL :: forall era. AlonzoEraPParams era => Lens' (PParams era) ExUnits
ppMaxTxExUnitsL = ppLens . hkdMaxTxExUnitsL @era @Identity

ppMaxBlockExUnitsL :: forall era. AlonzoEraPParams era => Lens' (PParams era) ExUnits
ppMaxBlockExUnitsL = ppLens . hkdMaxBlockExUnitsL @era @Identity

ppMaxValSizeL :: forall era. AlonzoEraPParams era => Lens' (PParams era) Natural
ppMaxValSizeL = ppLens . hkdMaxValSizeL @era @Identity

ppCollateralPercentageL :: forall era. AlonzoEraPParams era => Lens' (PParams era) Natural
ppCollateralPercentageL = ppLens . hkdCollateralPercentageL @era @Identity

ppMaxCollateralInputsL :: forall era. AlonzoEraPParams era => Lens' (PParams era) Natural
ppMaxCollateralInputsL = ppLens . hkdMaxCollateralInputsL @era @Identity

ppuCoinsPerUTxOWordL ::
  forall era.
  (AlonzoEraPParams era, ExactEra AlonzoEra era) =>
  Lens' (PParamsUpdate era) (StrictMaybe CoinPerWord)
ppuCoinsPerUTxOWordL = ppuLens . hkdCoinsPerUTxOWordL @era @StrictMaybe

ppuCostModelsL ::
  forall era.
  AlonzoEraPParams era =>
  Lens' (PParamsUpdate era) (StrictMaybe CostModels)
ppuCostModelsL = ppuLens . hkdCostModelsL @era @StrictMaybe

ppuPricesL ::
  forall era.
  AlonzoEraPParams era =>
  Lens' (PParamsUpdate era) (StrictMaybe Prices)
ppuPricesL = ppuLens . hkdPricesL @era @StrictMaybe

ppuMaxTxExUnitsL ::
  forall era.
  AlonzoEraPParams era =>
  Lens' (PParamsUpdate era) (StrictMaybe ExUnits)
ppuMaxTxExUnitsL = ppuLens . hkdMaxTxExUnitsL @era @StrictMaybe

ppuMaxBlockExUnitsL ::
  forall era.
  AlonzoEraPParams era =>
  Lens' (PParamsUpdate era) (StrictMaybe ExUnits)
ppuMaxBlockExUnitsL = ppuLens . hkdMaxBlockExUnitsL @era @StrictMaybe

ppuMaxValSizeL ::
  forall era.
  AlonzoEraPParams era =>
  Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMaxValSizeL = ppuLens . hkdMaxValSizeL @era @StrictMaybe

ppuCollateralPercentageL ::
  forall era.
  AlonzoEraPParams era =>
  Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuCollateralPercentageL = ppuLens . hkdCollateralPercentageL @era @StrictMaybe

ppuMaxCollateralInputsL ::
  forall era.
  AlonzoEraPParams era =>
  Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMaxCollateralInputsL = ppuLens . hkdMaxCollateralInputsL @era @StrictMaybe

-- | Protocol parameters.
-- Shelley parameters + additional ones
data AlonzoPParams f era = AlonzoPParams
  { appMinFeeA :: !(HKD f Coin)
  -- ^ The linear factor for the minimum fee calculation
  , appMinFeeB :: !(HKD f Coin)
  -- ^ The constant factor for the minimum fee calculation
  , appMaxBBSize :: !(HKD f Word32)
  -- ^ Maximal block body size
  , appMaxTxSize :: !(HKD f Word32)
  -- ^ Maximal transaction size
  , appMaxBHSize :: !(HKD f Word16)
  -- ^ Maximal block header size
  , appKeyDeposit :: !(HKD f Coin)
  -- ^ The amount of a key registration deposit
  , appPoolDeposit :: !(HKD f Coin)
  -- ^ The amount of a pool registration deposit
  , appEMax :: !(HKD f EpochInterval)
  -- ^ Maximum number of epochs in the future a pool retirement is allowed to
  -- be scheduled for.
  , appNOpt :: !(HKD f Word16)
  -- ^ Desired number of pools
  , appA0 :: !(HKD f NonNegativeInterval)
  -- ^ Pool influence
  , appRho :: !(HKD f UnitInterval)
  -- ^ Monetary expansion
  , appTau :: !(HKD f UnitInterval)
  -- ^ Treasury expansion
  , appD :: !(HKD f UnitInterval)
  -- ^ Decentralization parameter. Note that the scale is inverted here - a
  -- value of 0 indicates full decentralisation, where 1 indicates full
  -- federalisation.
  , appExtraEntropy :: !(HKD f Nonce)
  -- ^ Extra entropy
  , appProtocolVersion :: !(HKD f BT.ProtVer)
  -- ^ Protocol version
  , appMinPoolCost :: !(HKD f Coin)
  -- ^ Minimum Stake Pool Cost
  , -- new/updated for alonzo

    appCoinsPerUTxOWord :: !(HKD f CoinPerWord)
  -- ^ Cost in lovelace per word (8 bytes) of UTxO storage (instead of appMinUTxOValue)
  , appCostModels :: !(HKD f CostModels)
  -- ^ Cost models for non-native script languages
  , appPrices :: !(HKD f Prices)
  -- ^ Prices of execution units (for non-native script languages)
  , appMaxTxExUnits :: !(HKD f OrdExUnits)
  -- ^ Max total script execution resources units allowed per tx
  , appMaxBlockExUnits :: !(HKD f OrdExUnits)
  -- ^ Max total script execution resources units allowed per block
  , appMaxValSize :: !(HKD f Natural)
  -- ^ Max size of a Value in an output
  , appCollateralPercentage :: !(HKD f Natural)
  -- ^ Percentage of the txfee which must be provided as collateral when
  -- including non-native scripts.
  , appMaxCollateralInputs :: !(HKD f Natural)
  -- ^ Maximum number of collateral inputs allowed in a transaction
  }
  deriving (Generic)

deriving instance Eq (AlonzoPParams Identity era)

deriving instance Ord (AlonzoPParams Identity era)

deriving instance Show (AlonzoPParams Identity era)

instance NoThunks (AlonzoPParams Identity era)

instance NFData (AlonzoPParams Identity era)

deriving instance Eq (AlonzoPParams StrictMaybe era)

deriving instance Ord (AlonzoPParams StrictMaybe era)

deriving instance Show (AlonzoPParams StrictMaybe era)

instance NoThunks (AlonzoPParams StrictMaybe era)

instance NFData (AlonzoPParams StrictMaybe era)

instance EraPParams AlonzoEra where
  type PParamsHKD f AlonzoEra = AlonzoPParams f AlonzoEra
  type UpgradePParams f AlonzoEra = UpgradeAlonzoPParams f
  type DowngradePParams f AlonzoEra = DowngradeAlonzoPParams f

  emptyPParamsIdentity = emptyAlonzoPParams
  emptyPParamsStrictMaybe = emptyAlonzoPParamsUpdate

  upgradePParamsHKD = upgradeAlonzoPParams
  downgradePParamsHKD = downgradeAlonzoPParams

  hkdMinFeeAL = lens appMinFeeA $ \pp x -> pp {appMinFeeA = x}
  hkdMinFeeBL = lens appMinFeeB $ \pp x -> pp {appMinFeeB = x}
  hkdMaxBBSizeL = lens appMaxBBSize $ \pp x -> pp {appMaxBBSize = x}
  hkdMaxTxSizeL = lens appMaxTxSize $ \pp x -> pp {appMaxTxSize = x}
  hkdMaxBHSizeL = lens appMaxBHSize $ \pp x -> pp {appMaxBHSize = x}
  hkdKeyDepositL = lens appKeyDeposit $ \pp x -> pp {appKeyDeposit = x}
  hkdPoolDepositL = lens appPoolDeposit $ \pp x -> pp {appPoolDeposit = x}
  hkdEMaxL = lens appEMax $ \pp x -> pp {appEMax = x}
  hkdNOptL = lens appNOpt $ \pp x -> pp {appNOpt = x}
  hkdA0L = lens appA0 $ \pp x -> pp {appA0 = x}
  hkdRhoL = lens appRho $ \pp x -> pp {appRho = x}
  hkdTauL = lens appTau $ \pp x -> pp {appTau = x}
  hkdDL = lens appD $ \pp x -> pp {appD = x}
  hkdExtraEntropyL = lens appExtraEntropy $ \pp x -> pp {appExtraEntropy = x}
  hkdProtocolVersionL = lens appProtocolVersion $ \pp x -> pp {appProtocolVersion = x}
  hkdMinUTxOValueL = notSupportedInThisEraL
  hkdMinPoolCostL = lens appMinPoolCost $ \pp x -> pp {appMinPoolCost = x}

  pparams =
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
    , ppD
    , ppExtraEntropy
    , ppProtocolVersion
    , ppMinPoolCost
    , ppCoinsPerUTxOWord
    , ppCostModels
    , ppPrices
    , ppMaxTxExUnits
    , ppMaxBlockExUnits
    , ppMaxValSize
    , ppCollateralPercentage
    , ppMaxCollateralInputs
    ]

instance AlonzoEraPParams AlonzoEra where
  hkdCoinsPerUTxOWordL = lens appCoinsPerUTxOWord $ \pp x -> pp {appCoinsPerUTxOWord = x}
  hkdCostModelsL = lens appCostModels $ \pp x -> pp {appCostModels = x}
  hkdPricesL = lens appPrices $ \pp x -> pp {appPrices = x}
  hkdMaxTxExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f AlonzoEra) (HKD f ExUnits)
  hkdMaxTxExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . appMaxTxExUnits) $ \pp x ->
      pp {appMaxTxExUnits = hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxBlockExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f AlonzoEra) (HKD f ExUnits)
  hkdMaxBlockExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . appMaxBlockExUnits) $ \pp x ->
      pp {appMaxBlockExUnits = hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxValSizeL = lens appMaxValSize $ \pp x -> pp {appMaxValSize = x}
  hkdCollateralPercentageL =
    lens appCollateralPercentage $ \pp x -> pp {appCollateralPercentage = x}
  hkdMaxCollateralInputsL =
    lens appMaxCollateralInputs $ \pp x -> pp {appMaxCollateralInputs = x}

instance EraGov AlonzoEra where
  type GovState AlonzoEra = ShelleyGovState AlonzoEra
  emptyGovState = emptyShelleyGovState

  curPParamsGovStateL = curPParamsShelleyGovStateL

  prevPParamsGovStateL = prevPParamsShelleyGovStateL

  futurePParamsGovStateL = futurePParamsShelleyGovStateL

  obligationGovState = const mempty

newtype CoinPerWord = CoinPerWord {unCoinPerWord :: Coin}
  deriving stock (Eq, Ord)
  deriving newtype (EncCBOR, DecCBOR, ToJSON, FromJSON, NFData, NoThunks, Show)

-- | This is a helper type that allows us to define an `Ord` instance for executions units
-- without affecting the `ExUnits` type. This is needed in order to derive an `Ord` instance`
-- for PParams. This is just a helper type and should not be used directly. Both lenses
-- that operate on TxExUnits and BlockExUnits use the `ExUnits` type, not this one.
newtype OrdExUnits = OrdExUnits {unOrdExUnits :: ExUnits}
  deriving (Eq)
  deriving newtype (Show, NoThunks, NFData, DecCBOR, EncCBOR, FromJSON, ToJSON)

instance Ord OrdExUnits where
  compare = coerce (zipSemiExUnits compare)

-- | Parameters that were added in Alonzo
data UpgradeAlonzoPParams f = UpgradeAlonzoPParams
  { uappCoinsPerUTxOWord :: !(HKD f CoinPerWord)
  , uappCostModels :: !(HKD f CostModels)
  , uappPrices :: !(HKD f Prices)
  , uappMaxTxExUnits :: !(HKD f ExUnits)
  , uappMaxBlockExUnits :: !(HKD f ExUnits)
  , uappMaxValSize :: !(HKD f Natural)
  , uappCollateralPercentage :: !(HKD f Natural)
  , uappMaxCollateralInputs :: !(HKD f Natural)
  }
  deriving (Generic)

deriving instance Eq (UpgradeAlonzoPParams Identity)

deriving instance Show (UpgradeAlonzoPParams Identity)

instance NoThunks (UpgradeAlonzoPParams Identity)

instance NFData (UpgradeAlonzoPParams Identity)

instance Default (UpgradeAlonzoPParams StrictMaybe) where
  def =
    UpgradeAlonzoPParams
      { uappCoinsPerUTxOWord = SNothing
      , uappCostModels = SNothing
      , uappPrices = SNothing
      , uappMaxTxExUnits = SNothing
      , uappMaxBlockExUnits = SNothing
      , uappMaxValSize = SNothing
      , uappCollateralPercentage = SNothing
      , uappMaxCollateralInputs = SNothing
      }

-- | Parameters that were removed in Alonzo
newtype DowngradeAlonzoPParams f = DowngradeAlonzoPParams
  { dappMinUTxOValue :: HKD f Coin
  }
  deriving (Generic)

deriving instance Eq (DowngradeAlonzoPParams Identity)

deriving instance Show (DowngradeAlonzoPParams Identity)

instance NoThunks (DowngradeAlonzoPParams Identity)

instance NFData (DowngradeAlonzoPParams Identity)

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyAlonzoPParams :: forall era. Era era => AlonzoPParams Identity era
emptyAlonzoPParams =
  AlonzoPParams
    { appMinFeeA = Coin 0
    , appMinFeeB = Coin 0
    , appMaxBBSize = 0
    , appMaxTxSize = 2048
    , appMaxBHSize = 0
    , appKeyDeposit = Coin 0
    , appPoolDeposit = Coin 0
    , appEMax = EpochInterval 0
    , appNOpt = 100
    , appA0 = minBound
    , appRho = minBound
    , appTau = minBound
    , appD = minBound
    , appExtraEntropy = NeutralNonce
    , appProtocolVersion = BT.ProtVer (eraProtVerLow @era) 0
    , appMinPoolCost = mempty
    , -- new/updated for alonzo
      appCoinsPerUTxOWord = CoinPerWord (Coin 0)
    , appCostModels = emptyCostModels
    , appPrices = Prices minBound minBound
    , appMaxTxExUnits = OrdExUnits $ ExUnits 0 0
    , appMaxBlockExUnits = OrdExUnits $ ExUnits 0 0
    , appMaxValSize = 0
    , appCollateralPercentage = 150
    , appMaxCollateralInputs = 5
    }

emptyAlonzoPParamsUpdate :: AlonzoPParams StrictMaybe era
emptyAlonzoPParamsUpdate =
  AlonzoPParams
    { appMinFeeA = SNothing
    , appMinFeeB = SNothing
    , appMaxBBSize = SNothing
    , appMaxTxSize = SNothing
    , appMaxBHSize = SNothing
    , appKeyDeposit = SNothing
    , appPoolDeposit = SNothing
    , appEMax = SNothing
    , appNOpt = SNothing
    , appA0 = SNothing
    , appRho = SNothing
    , appTau = SNothing
    , appD = SNothing
    , appExtraEntropy = SNothing
    , appProtocolVersion = SNothing
    , appMinPoolCost = SNothing
    , -- new/updated for alonzo
      appCoinsPerUTxOWord = SNothing
    , appCostModels = SNothing
    , appPrices = SNothing
    , appMaxTxExUnits = SNothing
    , appMaxBlockExUnits = SNothing
    , appMaxValSize = SNothing
    , appCollateralPercentage = SNothing
    , appMaxCollateralInputs = SNothing
    }

-- ===================================================
-- Figure 1: "Definitions Used in Protocol Parameters"

-- The LangDepView is a key value pair. The key is the (canonically) encoded
-- language tag and the value is the (canonically) encoded set of relevant
-- protocol parameters
data LangDepView = LangDepView {tag :: ByteString, params :: ByteString}
  deriving (Eq, Show, Ord, Generic, NoThunks)

encodeCostModel :: CostModel -> Encoding
encodeCostModel cm =
  case getCostModelLanguage cm of
    -- In the Alonzo era, the map of languages to cost models was mistakenly encoded
    -- using an indefinite CBOR map (contrary to canonical CBOR, as intended) when
    -- computing the script integrity hash.
    -- For this reason, PlutusV1 remains with this encoding.
    -- Future versions of Plutus, starting with PlutusV2 in the Babbage era, will
    -- use the intended definite length encoding.
    PlutusV1 -> encodeFoldableAsIndefLenList encCBOR $ getCostModelParams cm
    -- Since cost model serializations need to be independently reproduced,
    -- we use the 'canonical' serialization with definite list length.
    PlutusV2 -> encodeFoldableAsDefLenList encCBOR $ getCostModelParams cm
    PlutusV3 -> encodeFoldableAsDefLenList encCBOR $ getCostModelParams cm

getLanguageView ::
  AlonzoEraPParams era =>
  PParams era ->
  Language ->
  LangDepView
getLanguageView pp lang =
  case lang of
    PlutusV1 ->
      LangDepView -- The silly double bagging is to keep compatibility with a past bug
        (serialize' version (serialize' version lang))
        (serialize' version costModelEncoding)
    PlutusV2 -> latestLangDepView
    PlutusV3 -> latestLangDepView
  where
    -- LangDepView for PlutusV1 differs from the rest
    latestLangDepView = LangDepView (serialize' version lang) costModelEncoding
    costModel = Map.lookup lang (costModelsValid $ pp ^. ppCostModelsL)
    costModelEncoding = serialize' version $ maybe encodeNull encodeCostModel costModel
    version = BT.pvMajor $ pp ^. ppProtocolVersionL

encodeLangViews :: Set LangDepView -> Encoding
encodeLangViews views = encodeMapLen n <> foldMap encPair ascending
  where
    n = fromIntegral (Set.size views) :: Word
    ascending = sortBy (shortLex `on` tag) $ Set.toList views
    encPair (LangDepView k v) = encodePreEncoded k <> encodePreEncoded v
    shortLex :: ByteString -> ByteString -> Ordering
    shortLex a b
      | BS.length a < BS.length b = LT
      | BS.length a > BS.length b = GT
      | otherwise = compare a b

-- | Given the missing pieces, turn a ShelleyPParams into a AlonzoPParams
upgradeAlonzoPParams ::
  forall f era1 era2.
  HKDFunctor f =>
  UpgradeAlonzoPParams f ->
  ShelleyPParams f era1 ->
  AlonzoPParams f era2
upgradeAlonzoPParams UpgradeAlonzoPParams {..} ShelleyPParams {..} =
  AlonzoPParams
    { appMinFeeA = sppMinFeeA
    , appMinFeeB = sppMinFeeB
    , appMaxBBSize = sppMaxBBSize
    , appMaxTxSize = sppMaxTxSize
    , appMaxBHSize = sppMaxBHSize
    , appKeyDeposit = sppKeyDeposit
    , appPoolDeposit = sppPoolDeposit
    , appEMax = sppEMax
    , appNOpt = sppNOpt
    , appA0 = sppA0
    , appRho = sppRho
    , appTau = sppTau
    , appD = sppD
    , appExtraEntropy = sppExtraEntropy
    , appProtocolVersion = sppProtocolVersion
    , appMinPoolCost = sppMinPoolCost
    , -- new in alonzo
      appCoinsPerUTxOWord = uappCoinsPerUTxOWord
    , appCostModels = uappCostModels
    , appPrices = uappPrices
    , appMaxTxExUnits = hkdMap (Proxy @f) OrdExUnits uappMaxTxExUnits
    , appMaxBlockExUnits = hkdMap (Proxy @f) OrdExUnits uappMaxBlockExUnits
    , appMaxValSize = uappMaxValSize
    , appCollateralPercentage = uappCollateralPercentage
    , appMaxCollateralInputs = uappMaxCollateralInputs
    }

-- | Turn an AlonzoPParams into a ShelleyParams
downgradeAlonzoPParams :: DowngradeAlonzoPParams f -> AlonzoPParams f era2 -> ShelleyPParams f era1
downgradeAlonzoPParams DowngradeAlonzoPParams {dappMinUTxOValue} AlonzoPParams {..} =
  ShelleyPParams
    { sppMinFeeA = appMinFeeA
    , sppMinFeeB = appMinFeeB
    , sppMaxBBSize = appMaxBBSize
    , sppMaxTxSize = appMaxTxSize
    , sppMaxBHSize = appMaxBHSize
    , sppKeyDeposit = appKeyDeposit
    , sppPoolDeposit = appPoolDeposit
    , sppEMax = appEMax
    , sppNOpt = appNOpt
    , sppA0 = appA0
    , sppRho = appRho
    , sppTau = appTau
    , sppD = appD
    , sppExtraEntropy = appExtraEntropy
    , sppProtocolVersion = appProtocolVersion
    , sppMinUTxOValue = dappMinUTxOValue -- <- parameter that was dropped in Alonzo
    , sppMinPoolCost = appMinPoolCost
    }

ppCoinsPerUTxOWord :: (AlonzoEraPParams era, ExactEra AlonzoEra era) => PParam' era
ppCoinsPerUTxOWord =
  PParam'
    { ppName = "utxoCostPerByte"
    , ppTag = 17
    , ppLens' = ppCoinsPerUTxOWordL
    , ppUpdateLens = ppuCoinsPerUTxOWordL
    , ppToPlutusData = Nothing
    , ppFromPlutusData = Nothing
    }

ppCollateralPercentage :: AlonzoEraPParams era => PParam' era
ppCollateralPercentage =
  PParam'
    { ppName = "collateralPercentage"
    , ppTag = 23
    , ppLens' = ppCollateralPercentageL
    , ppUpdateLens = ppuCollateralPercentageL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppCostModels :: AlonzoEraPParams era => PParam' era
ppCostModels =
  PParam'
    { ppName = "costModels"
    , ppTag = 18
    , ppLens' = ppCostModelsL
    , ppUpdateLens = ppuCostModelsL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppMaxBlockExUnits :: AlonzoEraPParams era => PParam' era
ppMaxBlockExUnits =
  PParam'
    { ppName = "maxBlockExecutionUnits"
    , ppTag = 21
    , ppLens' = ppMaxBlockExUnitsL
    , ppUpdateLens = ppuMaxBlockExUnitsL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppMaxCollateralInputs :: AlonzoEraPParams era => PParam' era
ppMaxCollateralInputs =
  PParam'
    { ppName = "maxCollateralInputs"
    , ppTag = 24
    , ppLens' = ppMaxCollateralInputsL
    , ppUpdateLens = ppuMaxCollateralInputsL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppMaxTxExUnits :: AlonzoEraPParams era => PParam' era
ppMaxTxExUnits =
  PParam'
    { ppName = "maxTxExecutionUnits"
    , ppTag = 20
    , ppLens' = ppMaxTxExUnitsL
    , ppUpdateLens = ppuMaxTxExUnitsL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppMaxValSize :: AlonzoEraPParams era => PParam' era
ppMaxValSize =
  PParam'
    { ppName = "maxValueSize"
    , ppTag = 22
    , ppLens' = ppMaxValSizeL
    , ppUpdateLens = ppuMaxValSizeL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }

ppPrices :: AlonzoEraPParams era => PParam' era
ppPrices =
  PParam'
    { ppName = "executionUnitPrices"
    , ppTag = 19
    , ppLens' = ppPricesL
    , ppUpdateLens = ppuPricesL
    , ppToPlutusData = Just toPlutusData
    , ppFromPlutusData = Just fromPlutusData
    }
