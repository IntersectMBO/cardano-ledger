{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Forecast () where

import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley.API.Forecast (EraForecast (..), ShelleyEraForecast (..))
import Cardano.Ledger.Shelley.Forecast

instance EraForecast MaryEra where
  type Forecast t MaryEra = ShelleyForecast t MaryEra
  mkForecast = mkShelleyForecast
  poolDistrForecastL = sfPoolDistrL
  maxBlockHeaderSizeForecastL = sfMaxBlockHeaderSizeL
  maxBlockBodySizeForecastL = sfMaxBlockBodySizeL
  protocolVersionForecastL = sfProtocolVersionL

instance ShelleyEraForecast MaryEra where
  genDelegsForecastL = sfGenDelegsL
  decentralizationForecastL = sfDecentralizationL
  extraEntropyForecastL = sfExtraEntropyL
