{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Forecast () where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams ()
import Cardano.Ledger.Alonzo.State.CertState ()
import Cardano.Ledger.Shelley (
  ShelleyForecast (..),
  mkShelleyForecast,
  sfDecentralizationL,
  sfExtraEntropyL,
  sfGenDelegsL,
  sfMaxBlockBodySizeL,
  sfMaxBlockHeaderSizeL,
  sfPoolDistrL,
  sfProtocolVersionL,
 )
import Cardano.Ledger.Shelley.API.Forecast (EraForecast (..), ShelleyEraForecast (..))

instance EraForecast AlonzoEra where
  type Forecast t AlonzoEra = ShelleyForecast t AlonzoEra
  mkForecast = mkShelleyForecast
  poolDistrForecastL = sfPoolDistrL
  maxBlockHeaderSizeForecastL = sfMaxBlockHeaderSizeL
  maxBlockBodySizeForecastL = sfMaxBlockBodySizeL
  protocolVersionForecastL = sfProtocolVersionL

instance ShelleyEraForecast AlonzoEra where
  genDelegsForecastL = sfGenDelegsL
  decentralizationForecastL = sfDecentralizationL
  extraEntropyForecastL = sfExtraEntropyL
