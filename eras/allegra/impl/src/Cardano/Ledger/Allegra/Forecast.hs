{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Forecast () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.PParams ()
import Cardano.Ledger.Allegra.State.CertState ()
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

instance EraForecast AllegraEra where
  type Forecast t AllegraEra = ShelleyForecast t AllegraEra
  mkForecast = mkShelleyForecast
  poolDistrForecastL = sfPoolDistrL
  maxBlockHeaderSizeForecastL = sfMaxBlockHeaderSizeL
  maxBlockBodySizeForecastL = sfMaxBlockBodySizeL
  protocolVersionForecastL = sfProtocolVersionL

instance ShelleyEraForecast AllegraEra where
  genDelegsForecastL = sfGenDelegsL
  decentralizationForecastL = sfDecentralizationL
  extraEntropyForecastL = sfExtraEntropyL
