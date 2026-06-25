{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
module Cardano.Ledger.Babbage.PParams (
  BabbageEraPParams (..),
  ppCoinsPerUTxOByteL,
  ppuCoinsPerUTxOByteL,
  BabbagePParams (..),
  emptyBabbagePParams,
  emptyBabbagePParamsUpdate,
  DowngradeBabbagePParams (..),
  upgradeBabbagePParams,
  getLanguageView,
  LangDepView (..),
  encodeLangViews,
  coinsPerUTxOWordToCoinsPerUTxOByte,
  coinsPerUTxOByteToCoinsPerUTxOWord,
  ppCoinsPerUTxOByte,

  -- * Deprecated
  bppMinFeeA,
  bppMinFeeB,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Scripts (
  CostModels,
  ExUnits (..),
  Prices (..),
  emptyCostModels,
 )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  NonNegativeInterval,
  Nonce,
  ProtVer (..),
  StrictMaybe (..),
  UnitInterval,
 )
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible (toCompactPartial)
import Cardano.Ledger.Core (EraPParams (..))
import Cardano.Ledger.HKD (HKDFunctor (..))
import Cardano.Ledger.Orphans ()
import Cardano.Ledger.Shelley.PParams
import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

class AlonzoEraPParams era => BabbageEraPParams era where
  hkdCoinsPerUTxOByteL :: HKDFunctor f => Lens' (PParamsHKD f era) (HKD f CoinPerByte)

ppCoinsPerUTxOByteL ::
  forall era. BabbageEraPParams era => Lens' (PParams era) CoinPerByte
ppCoinsPerUTxOByteL = ppLensHKD . hkdCoinsPerUTxOByteL @era @Identity

ppuCoinsPerUTxOByteL ::
  forall era. BabbageEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe CoinPerByte)
ppuCoinsPerUTxOByteL = ppuLensHKD . hkdCoinsPerUTxOByteL @era @StrictMaybe

-- | Babbage Protocol parameters. Ways in which parameters have changed from Alonzo: lack
-- of @d@, @extraEntropy@ and replacement of @coinsPerUTxOWord@ with @coinsPerUTxOByte@
data BabbagePParams f era = BabbagePParams
  { bppTxFeePerByte :: !(HKD f CoinPerByte)
  -- ^ The linear factor for the minimum fee calculation
  , bppTxFeeFixed :: !(HKD f (CompactForm Coin))
  -- ^ The constant factor for the minimum fee calculation
  , bppMaxBBSize :: !(HKD f Word32)
  -- ^ Maximal block body size
  , bppMaxTxSize :: !(HKD f Word32)
  -- ^ Maximal transaction size
  , bppMaxBHSize :: !(HKD f Word16)
  -- ^ Maximal block header size
  , bppKeyDeposit :: !(HKD f (CompactForm Coin))
  -- ^ The amount of a key registration deposit
  , bppPoolDeposit :: !(HKD f (CompactForm Coin))
  -- ^ The amount of a pool registration deposit
  , bppEMax :: !(HKD f EpochInterval)
  -- ^ Maximum number of epochs in the future a pool retirement is allowed to
  -- be scheduled for.
  , bppNOpt :: !(HKD f Word16)
  -- ^ Desired number of pools
  , bppA0 :: !(HKD f NonNegativeInterval)
  -- ^ Pool influence
  , bppRho :: !(HKD f UnitInterval)
  -- ^ Monetary expansion
  , bppTau :: !(HKD f UnitInterval)
  -- ^ Treasury expansion
  , bppProtocolVersion :: !(HKD f ProtVer)
  -- ^ Protocol version
  , bppMinPoolCost :: !(HKD f (CompactForm Coin))
  -- ^ Minimum Stake Pool Cost
  , bppCoinsPerUTxOByte :: !(HKD f CoinPerByte)
  -- ^ Cost in lovelace per byte of UTxO storage (instead of bppCoinsPerUTxOByte)
  , bppCostModels :: !(HKD f CostModels)
  -- ^ Cost models for non-native script languages
  , bppPrices :: !(HKD f Prices)
  -- ^ Prices of execution units (for non-native script languages)
  , bppMaxTxExUnits :: !(HKD f OrdExUnits)
  -- ^ Max total script execution resources units allowed per tx
  , bppMaxBlockExUnits :: !(HKD f OrdExUnits)
  -- ^ Max total script execution resources units allowed per block
  , bppMaxValSize :: !(HKD f Word32)
  -- ^ Max size of a Value in an output
  , bppCollateralPercentage :: !(HKD f Word16)
  -- ^ Percentage of the txfee which must be provided as collateral when
  -- including non-native scripts.
  , bppMaxCollateralInputs :: !(HKD f Word16)
  -- ^ Maximum number of collateral inputs allowed in a transaction
  }
  deriving (Generic)

bppMinFeeA :: forall era f. HKDFunctor f => BabbagePParams f era -> HKD f Coin
bppMinFeeA p = bppTxFeePerByte p ^. hkdCoinPerByteL @f . hkdPartialCompactCoinL @f
{-# DEPRECATED bppMinFeeA "In favor of `bppTxFeePerByte`" #-}

bppMinFeeB :: forall era f. HKDFunctor f => BabbagePParams f era -> HKD f Coin
bppMinFeeB p = bppTxFeeFixed p ^. hkdPartialCompactCoinL @f
{-# DEPRECATED bppMinFeeB "In favor of `bppTxFeeFixed`" #-}

deriving instance Eq (BabbagePParams Identity era)

deriving instance Ord (BabbagePParams Identity era)

deriving instance Show (BabbagePParams Identity era)

instance NoThunks (BabbagePParams Identity era)

instance NFData (BabbagePParams Identity era)

deriving instance Eq (BabbagePParams StrictMaybe era)

deriving instance Ord (BabbagePParams StrictMaybe era)

deriving instance Show (BabbagePParams StrictMaybe era)

instance NoThunks (BabbagePParams StrictMaybe era)

instance NFData (BabbagePParams StrictMaybe era)

data DowngradeBabbagePParams f = DowngradeBabbagePParams
  { dbppD :: !(HKD f UnitInterval)
  , dbppExtraEntropy :: !(HKD f Nonce)
  }

instance EraPParams BabbageEra where
  type PParamsHKD f BabbageEra = BabbagePParams f BabbageEra
  type UpgradePParams f BabbageEra = ()
  type DowngradePParams f BabbageEra = DowngradeBabbagePParams f

  emptyPParamsIdentity = emptyBabbagePParams
  emptyPParamsStrictMaybe = emptyBabbagePParamsUpdate

  upgradePParamsHKD () = upgradeBabbagePParams True
  downgradePParamsHKD = downgradeBabbagePParams

  hkdTxFeePerByteL = lens bppTxFeePerByte $ \pp x -> pp {bppTxFeePerByte = x}
  hkdTxFeeFixedCompactL = lens bppTxFeeFixed $ \pp x -> pp {bppTxFeeFixed = x}
  hkdMaxBBSizeL = lens bppMaxBBSize $ \pp x -> pp {bppMaxBBSize = x}
  hkdMaxTxSizeL = lens bppMaxTxSize $ \pp x -> pp {bppMaxTxSize = x}
  hkdMaxBHSizeL = lens bppMaxBHSize $ \pp x -> pp {bppMaxBHSize = x}
  hkdKeyDepositCompactL = lens bppKeyDeposit $ \pp x -> pp {bppKeyDeposit = x}
  hkdPoolDepositCompactL = lens bppPoolDeposit $ \pp x -> pp {bppPoolDeposit = x}
  hkdEMaxL = lens bppEMax $ \pp x -> pp {bppEMax = x}
  hkdNOptL = lens bppNOpt $ \pp x -> pp {bppNOpt = x}
  hkdA0L = lens bppA0 $ \pp x -> pp {bppA0 = x}
  hkdRhoL = lens bppRho $ \pp x -> pp {bppRho = x}
  hkdTauL = lens bppTau $ \pp x -> pp {bppTau = x}
  hkdProtocolVersionL = lens bppProtocolVersion $ \pp x -> pp {bppProtocolVersion = x}
  hkdMinPoolCostCompactL = lens bppMinPoolCost $ \pp x -> pp {bppMinPoolCost = x}

  ppDG = to (const minBound)
  hkdDL = notSupportedInThisEraL
  hkdExtraEntropyL = notSupportedInThisEraL
  hkdMinUTxOValueCompactL = notSupportedInThisEraL

  eraPParams =
    [ ppTxFeePerByte
    , ppTxFeeFixed
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
    , ppProtocolVersion
    , ppMinPoolCost
    , ppCoinsPerUTxOByte
    , ppCostModels
    , ppPrices
    , ppMaxTxExUnits
    , ppMaxBlockExUnits
    , ppMaxValSize
    , ppCollateralPercentage
    , ppMaxCollateralInputs
    ]

instance AlonzoEraPParams BabbageEra where
  hkdCoinsPerUTxOWordL = notSupportedInThisEraL
  hkdCostModelsL = lens bppCostModels $ \pp x -> pp {bppCostModels = x}
  hkdPricesL = lens bppPrices $ \pp x -> pp {bppPrices = x}
  hkdMaxTxExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f BabbageEra) (HKD f ExUnits)
  hkdMaxTxExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . bppMaxTxExUnits) $ \pp x ->
      pp {bppMaxTxExUnits = hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxBlockExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f BabbageEra) (HKD f ExUnits)
  hkdMaxBlockExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . bppMaxBlockExUnits) $ \pp x ->
      pp {bppMaxBlockExUnits = hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxValSizeL = lens bppMaxValSize $ \pp x -> pp {bppMaxValSize = x}
  hkdCollateralPercentageL =
    lens bppCollateralPercentage $ \pp x -> pp {bppCollateralPercentage = x}
  hkdMaxCollateralInputsL =
    lens bppMaxCollateralInputs $ \pp x -> pp {bppMaxCollateralInputs = x}

instance BabbageEraPParams BabbageEra where
  hkdCoinsPerUTxOByteL = lens bppCoinsPerUTxOByte (\pp x -> pp {bppCoinsPerUTxOByte = x})

instance EraGov BabbageEra where
  type GovState BabbageEra = ShelleyGovState BabbageEra
  emptyGovState = emptyShelleyGovState

  curPParamsGovStateL = curPParamsShelleyGovStateL

  prevPParamsGovStateL = prevPParamsShelleyGovStateL

  futurePParamsGovStateL = futurePParamsShelleyGovStateL

  obligationGovState = const mempty

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyBabbagePParams :: forall era. Era era => BabbagePParams Identity era
emptyBabbagePParams =
  BabbagePParams
    { bppTxFeePerByte = CoinPerByte $ CompactCoin 0
    , bppTxFeeFixed = CompactCoin 0
    , bppMaxBBSize = 0
    , bppMaxTxSize = 2048
    , bppMaxBHSize = 0
    , bppKeyDeposit = CompactCoin 0
    , bppPoolDeposit = CompactCoin 0
    , bppEMax = EpochInterval 0
    , bppNOpt = 100
    , bppA0 = minBound
    , bppRho = minBound
    , bppTau = minBound
    , bppProtocolVersion = ProtVer (eraProtVerLow @era) 0
    , bppMinPoolCost = mempty
    , bppCoinsPerUTxOByte = CoinPerByte $ CompactCoin 0
    , bppCostModels = emptyCostModels
    , bppPrices = Prices minBound minBound
    , bppMaxTxExUnits = OrdExUnits $ ExUnits 0 0
    , bppMaxBlockExUnits = OrdExUnits $ ExUnits 0 0
    , bppMaxValSize = 0
    , bppCollateralPercentage = 150
    , bppMaxCollateralInputs = 5
    }

emptyBabbagePParamsUpdate :: BabbagePParams StrictMaybe era
emptyBabbagePParamsUpdate =
  BabbagePParams
    { bppTxFeePerByte = SNothing
    , bppTxFeeFixed = SNothing
    , bppMaxBBSize = SNothing
    , bppMaxTxSize = SNothing
    , bppMaxBHSize = SNothing
    , bppKeyDeposit = SNothing
    , bppPoolDeposit = SNothing
    , bppEMax = SNothing
    , bppNOpt = SNothing
    , bppA0 = SNothing
    , bppRho = SNothing
    , bppTau = SNothing
    , bppProtocolVersion = SNothing
    , bppMinPoolCost = SNothing
    , bppCoinsPerUTxOByte = SNothing
    , bppCostModels = SNothing
    , bppPrices = SNothing
    , bppMaxTxExUnits = SNothing
    , bppMaxBlockExUnits = SNothing
    , bppMaxValSize = SNothing
    , bppCollateralPercentage = SNothing
    , bppMaxCollateralInputs = SNothing
    }

upgradeBabbagePParams ::
  forall f.
  HKDFunctor f =>
  Bool ->
  PParamsHKD f AlonzoEra ->
  BabbagePParams f BabbageEra
upgradeBabbagePParams updateCoinsPerUTxOWord AlonzoPParams {..} =
  BabbagePParams
    { bppTxFeePerByte = appTxFeePerByte
    , bppTxFeeFixed = appTxFeeFixed
    , bppMaxBBSize = appMaxBBSize
    , bppMaxTxSize = appMaxTxSize
    , bppMaxBHSize = appMaxBHSize
    , bppKeyDeposit = appKeyDeposit
    , bppPoolDeposit = appPoolDeposit
    , bppEMax = appEMax
    , bppNOpt = appNOpt
    , bppA0 = appA0
    , bppRho = appRho
    , bppTau = appTau
    , bppProtocolVersion = appProtocolVersion
    , bppMinPoolCost = appMinPoolCost
    , bppCoinsPerUTxOByte =
        hkdMap
          (Proxy @f)
          ( if updateCoinsPerUTxOWord
              then coinsPerUTxOWordToCoinsPerUTxOByte
              else coinsPerUTxOWordToCoinsPerUTxOByteInTx
          )
          appCoinsPerUTxOWord
    , bppCostModels = appCostModels
    , bppPrices = appPrices
    , bppMaxTxExUnits = appMaxTxExUnits
    , bppMaxBlockExUnits = appMaxBlockExUnits
    , bppMaxValSize = appMaxValSize
    , bppCollateralPercentage = appCollateralPercentage
    , bppMaxCollateralInputs = appMaxCollateralInputs
    }

downgradeBabbagePParams ::
  forall f.
  HKDFunctor f =>
  DowngradeBabbagePParams f ->
  BabbagePParams f BabbageEra ->
  PParamsHKD f AlonzoEra
downgradeBabbagePParams DowngradeBabbagePParams {..} BabbagePParams {..} =
  AlonzoPParams
    { appTxFeePerByte = bppTxFeePerByte
    , appTxFeeFixed = bppTxFeeFixed
    , appMaxBBSize = bppMaxBBSize
    , appMaxTxSize = bppMaxTxSize
    , appMaxBHSize = bppMaxBHSize
    , appKeyDeposit = bppKeyDeposit
    , appPoolDeposit = bppPoolDeposit
    , appEMax = bppEMax
    , appNOpt = bppNOpt
    , appA0 = bppA0
    , appRho = bppRho
    , appTau = bppTau
    , appD = dbppD
    , appExtraEntropy = dbppExtraEntropy
    , appProtocolVersion = bppProtocolVersion
    , appMinPoolCost = bppMinPoolCost
    , appCoinsPerUTxOWord = hkdMap (Proxy @f) coinsPerUTxOByteToCoinsPerUTxOWord bppCoinsPerUTxOByte
    , appCostModels = bppCostModels
    , appPrices = bppPrices
    , appMaxTxExUnits = bppMaxTxExUnits
    , appMaxBlockExUnits = bppMaxBlockExUnits
    , appMaxValSize = bppMaxValSize
    , appCollateralPercentage = bppCollateralPercentage
    , appMaxCollateralInputs = bppMaxCollateralInputs
    }

-- | A word is 8 bytes, so convert from coinsPerUTxOWord to coinsPerUTxOByte, rounding down.
coinsPerUTxOWordToCoinsPerUTxOByte :: CoinPerWord -> CoinPerByte
coinsPerUTxOWordToCoinsPerUTxOByte (CoinPerWord (Coin c)) = CoinPerByte . CompactCoin $ fromIntegral (c `div` 8)

-- | A word is 8 bytes, so convert from coinsPerUTxOByte to coinsPerUTxOWord.
coinsPerUTxOByteToCoinsPerUTxOWord :: CoinPerByte -> CoinPerWord
coinsPerUTxOByteToCoinsPerUTxOWord (CoinPerByte (CompactCoin c)) = CoinPerWord . Coin $ fromIntegral c * 8

-- | Naively convert coins per UTxO word to coins per byte. This function only
-- exists to support the very unusual case of translating a transaction
-- containing an update to the 'coinsPerUTxOWord' field, in which case we must
-- not do the translation above, since this would render the transaction
-- invalid.
coinsPerUTxOWordToCoinsPerUTxOByteInTx :: CoinPerWord -> CoinPerByte
coinsPerUTxOWordToCoinsPerUTxOByteInTx (CoinPerWord (Coin c)) = CoinPerByte . toCompactPartial $ Coin c

ppCoinsPerUTxOByte :: BabbageEraPParams era => PParam era
ppCoinsPerUTxOByte =
  PParam
    { ppName = "utxoCostPerByte"
    , ppLens = ppCoinsPerUTxOByteL
    , ppUpdate = Just $ PParamUpdate 17 ppuCoinsPerUTxOByteL
    }
