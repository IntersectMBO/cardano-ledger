{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Forecast () where

import Cardano.Ledger.Babbage.Forecast
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Shelley.API.Forecast (BabbageEraForecast, EraForecast (..))

instance EraForecast DijkstraEra where
  type Forecast t DijkstraEra = BabbageForecast t DijkstraEra
  mkForecast = mkBabbageForecast
  poolDistrForecastL = bfPoolDistrL
  maxBlockHeaderSizeForecastL = bfMaxBlockHeaderSizeL
  maxBlockBodySizeForecastL = bfMaxBlockBodySizeL
  protocolVersionForecastL = bfProtocolVersionL

instance BabbageEraForecast DijkstraEra
