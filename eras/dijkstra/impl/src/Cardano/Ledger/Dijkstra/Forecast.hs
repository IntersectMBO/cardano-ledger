{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Forecast () where

import Cardano.Ledger.Babbage (
  BabbageForecast (..),
  bfMaxBlockBodySizeL,
  bfMaxBlockHeaderSizeL,
  bfPoolDistrL,
  bfProtocolVersionL,
  mkBabbageForecast,
 )
import Cardano.Ledger.Conway.Rules ()
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Governance ()
import Cardano.Ledger.Dijkstra.State.CertState ()
import Cardano.Ledger.Shelley.API.Forecast (EraForecast (..))

instance EraForecast DijkstraEra where
  type Forecast t DijkstraEra = BabbageForecast t DijkstraEra
  mkForecast = mkBabbageForecast
  poolDistrForecastL = bfPoolDistrL
  maxBlockHeaderSizeForecastL = bfMaxBlockHeaderSizeL
  maxBlockBodySizeForecastL = bfMaxBlockBodySizeL
  protocolVersionForecastL = bfProtocolVersionL
