{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Forecast () where

import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.PParams ()
import Cardano.Ledger.Mary.State.CertState ()
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
