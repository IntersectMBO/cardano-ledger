{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains the type of protocol parameters and EraPParams instance
module Cardano.Ledger.Conway.PParams
  ( BabbagePParamsHKD (..),
    BabbagePParams,
    BabbagePParamsUpdate,
    updatePParams,
    getLanguageView,
    LangDepView (..),
    encodeLangViews,
    retractPP,
    extendPP,
  )
where

import Cardano.Ledger.Alonzo.PParams.Class (AlonzoEraPParams (..))
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.Babbage.PParams.Class (BabbageEraPParams (..))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Data.Default (Default (..))
import Lens.Micro (lens, to)

instance CC.Crypto c => EraPParams (ConwayEra c) where
  type PParamsHKD f (ConwayEra c) = BabbagePParamsHKD f (ConwayEra c)

  emptyPParams = def
  emptyPParamsUpdate = def

  hkdMinFeeAL = lens _minfeeA (\pp x -> pp {_minfeeA = x})
  hkdMinFeeBL = lens _minfeeB (\pp x -> pp {_minfeeB = x})
  hkdMaxBBSizeL = lens _maxBBSize (\pp x -> pp {_maxBBSize = x})
  hkdMaxTxSizeL = lens _maxTxSize (\pp x -> pp {_maxTxSize = x})
  hkdMaxBHSizeL = lens _maxBHSize (\pp x -> pp {_maxBHSize = x})
  hkdKeyDepositL = lens _keyDeposit (\pp x -> pp {_keyDeposit = x})
  hkdPoolDepositL = lens _poolDeposit (\pp x -> pp {_poolDeposit = x})
  hkdEMaxL = lens _eMax (\pp x -> pp {_eMax = x})
  hkdNOptL = lens _nOpt (\pp x -> pp {_nOpt = x})
  hkdA0L = lens _a0 (\pp x -> pp {_a0 = x})
  hkdRhoL = lens _rho (\pp x -> pp {_rho = x})
  hkdTauL = lens _tau (\pp x -> pp {_tau = x})
  hkdProtocolVersionL = lens _protocolVersion (\pp x -> pp {_protocolVersion = x})
  hkdMinPoolCostL = lens _minPoolCost (\pp x -> pp {_minPoolCost = x})
  ppDG = ppLens . to (const minBound)

  hkdDL = notSupportedInThisEraL
  hkdExtraEntropyL = notSupportedInThisEraL
  hkdMinUTxOValueL = notSupportedInThisEraL

instance CC.Crypto c => AlonzoEraPParams (ConwayEra c) where
  hkdCostmdlsL = lens _costmdls (\pp x -> pp {_costmdls = x})
  hkdPricesL = lens _prices (\pp x -> pp {_prices = x})
  hkdMaxTxExUnitsL = lens _maxTxExUnits (\pp x -> pp {_maxTxExUnits = x})
  hkdMaxBlockExUnitsL = lens _maxBlockExUnits (\pp x -> pp {_maxBlockExUnits = x})
  hkdMaxValSizeL = lens _maxValSize (\pp x -> pp {_maxValSize = x})
  hkdCollateralPercentageL = lens _collateralPercentage (\pp x -> pp {_collateralPercentage = x})
  hkdMaxCollateralInputsL = lens _maxCollateralInputs (\pp x -> pp {_maxCollateralInputs = x})

  hkdCoinsPerUTxOWordL = notSupportedInThisEraL

instance CC.Crypto c => BabbageEraPParams (ConwayEra c) where
  hkdCoinsPerUTxOByteL = lens _coinsPerUTxOByte (\pp x -> pp {_coinsPerUTxOByte = x})
