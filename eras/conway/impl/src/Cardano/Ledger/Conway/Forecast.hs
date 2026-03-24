{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Forecast () where

import Cardano.Ledger.Babbage (
  BabbageForecast (..),
  bfMaxBlockBodySizeL,
  bfMaxBlockHeaderSizeL,
  bfPoolDistrL,
  bfProtocolVersionL,
  mkBabbageForecast,
 )
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance ()
import Cardano.Ledger.Conway.Rules ()
import Cardano.Ledger.Conway.State.CertState ()
import Cardano.Ledger.Shelley.API.Forecast (EraForecast (..))

instance EraForecast ConwayEra where
  type Forecast t ConwayEra = BabbageForecast t ConwayEra
  mkForecast = mkBabbageForecast
  poolDistrForecastL = bfPoolDistrL
  maxBlockHeaderSizeForecastL = bfMaxBlockHeaderSizeL
  maxBlockBodySizeForecastL = bfMaxBlockBodySizeL
  protocolVersionForecastL = bfProtocolVersionL
