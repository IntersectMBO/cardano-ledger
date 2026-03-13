{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Forecast () where

import Cardano.Ledger.Babbage.Forecast
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Shelley.API.Forecast (BabbageEraForecast, EraForecast (..))

instance EraForecast ConwayEra where
  type Forecast t ConwayEra = BabbageForecast t ConwayEra
  mkForecast = mkBabbageForecast
  poolDistrForecastL = bfPoolDistrL
  maxBlockHeaderSizeForecastL = bfMaxBlockHeaderSizeL
  maxBlockBodySizeForecastL = bfMaxBlockBodySizeL
  protocolVersionForecastL = bfProtocolVersionL

instance BabbageEraForecast ConwayEra
