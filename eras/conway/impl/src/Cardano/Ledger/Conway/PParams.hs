{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains the type of protocol parameters and EraPParams instance
module Cardano.Ledger.Conway.PParams (
  BabbagePParams (..),
  getLanguageView,
  LangDepView (..),
  encodeLangViews,
)
where

import Cardano.Ledger.Alonzo.PParams (OrdExUnits (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Crypto
import Cardano.Ledger.HKD (HKD, HKDFunctor (..))
import Data.Coerce
import Data.Proxy
import Lens.Micro

instance Crypto c => EraPParams (ConwayEra c) where
  type PParamsHKD f (ConwayEra c) = BabbagePParams f (ConwayEra c)
  type UpgradePParams f (ConwayEra c) = ()
  type DowngradePParams f (ConwayEra c) = ()

  emptyPParamsIdentity = emptyBabbagePParams
  emptyPParamsStrictMaybe = emptyBabbagePParamsUpdate

  upgradePParamsHKD () = coerce
  downgradePParamsHKD () = coerce

  hkdMinFeeAL = lens bppMinFeeA $ \pp x -> pp {bppMinFeeA = x}
  hkdMinFeeBL = lens bppMinFeeB $ \pp x -> pp {bppMinFeeB = x}
  hkdMaxBBSizeL = lens bppMaxBBSize $ \pp x -> pp {bppMaxBBSize = x}
  hkdMaxTxSizeL = lens bppMaxTxSize $ \pp x -> pp {bppMaxTxSize = x}
  hkdMaxBHSizeL = lens bppMaxBHSize $ \pp x -> pp {bppMaxBHSize = x}
  hkdKeyDepositL = lens bppKeyDeposit $ \pp x -> pp {bppKeyDeposit = x}
  hkdPoolDepositL = lens bppPoolDeposit $ \pp x -> pp {bppPoolDeposit = x}
  hkdEMaxL = lens bppEMax $ \pp x -> pp {bppEMax = x}
  hkdNOptL = lens bppNOpt $ \pp x -> pp {bppNOpt = x}
  hkdA0L = lens bppA0 $ \pp x -> pp {bppA0 = x}
  hkdRhoL = lens bppRho $ \pp x -> pp {bppRho = x}
  hkdTauL = lens bppTau $ \pp x -> pp {bppTau = x}
  hkdProtocolVersionL = lens bppProtocolVersion $ \pp x -> pp {bppProtocolVersion = x}
  hkdMinPoolCostL = lens bppMinPoolCost $ \pp x -> pp {bppMinPoolCost = x}

  ppDG = to (const minBound)
  hkdDL = notSupportedInThisEraL
  hkdExtraEntropyL = notSupportedInThisEraL
  hkdMinUTxOValueL = notSupportedInThisEraL

instance Crypto c => AlonzoEraPParams (ConwayEra c) where
  hkdCoinsPerUTxOWordL = notSupportedInThisEraL
  hkdCostModelsL = lens bppCostModels $ \pp x -> pp {bppCostModels = x}
  hkdPricesL = lens bppPrices $ \pp x -> pp {bppPrices = x}
  hkdMaxTxExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f (ConwayEra c)) (HKD f ExUnits)
  hkdMaxTxExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . bppMaxTxExUnits) $ \pp x ->
      pp {bppMaxTxExUnits = hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxBlockExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f (ConwayEra c)) (HKD f ExUnits)
  hkdMaxBlockExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . bppMaxBlockExUnits) $ \pp x ->
      pp {bppMaxBlockExUnits = hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxValSizeL = lens bppMaxValSize $ \pp x -> pp {bppMaxValSize = x}
  hkdCollateralPercentageL =
    lens bppCollateralPercentage $ \pp x -> pp {bppCollateralPercentage = x}
  hkdMaxCollateralInputsL =
    lens bppMaxCollateralInputs $ \pp x -> pp {bppMaxCollateralInputs = x}

instance Crypto c => BabbageEraPParams (ConwayEra c) where
  hkdCoinsPerUTxOByteL = lens bppCoinsPerUTxOByte (\pp x -> pp {bppCoinsPerUTxOByte = x})
