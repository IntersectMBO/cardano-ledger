{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Cardano.Ledger.Shelley.API.Forecast (
  EraForecast (..),
  ShelleyEraForecast (..),
) where

import Data.Kind (Type)
import Lens.Micro

type data Timeline
  = Current
  | Future

class EraForecast era where
  type Forecast era = (r :: Timeline -> Type) | r -> era

  currentForecast :: NewEpochState era -> Forecast Current era

  futureForecast :: NewEpochState era -> Forecast Future era

  poolDistrForecastL :: Lens' (Forecast t era) PoolDistr

  maxBlockHeaderSizeForecastL :: Lens' (Forecast t era) Word16

  maxBlockBodySizeForecastL :: Lens' (Forecast t era) Word32

  protocolVersionForecastL :: Lens' (Forecast t era) ProtVer

class EraForecast era => ShelleyEraForecast era where
  genDelegsForecastL :: Lens' (Forecast t era) GenDelegs

  decentralizationForecastL :: Lens' (Forecast t era) UnitInterval

  extraEntropyForecastL :: Lens' (Forecast t era) Nonce

-- | Forecast for TPraos protocol that started with Shelley era and ended with Babbage era.
data ShelleyForecast (t :: Timeline) era = ShelleyForecast
  { sfPoolDistr :: !PoolDistr
  , sfMaxBlockHeaderSize :: !Word16
  , sfMaxBlockBodySize :: !Word32
  , sfProtocolVersion :: !ProtVer
  , sfGenDelegs :: !GenDelegs
  , sfDecentralization :: !UnitInterval
  , sfExtraEntropy :: !Nonce
  }
  deriving (Eq, Show)


-- | Forecast for Praos protocol that started with Shelley era and ended with Babbage era.
data BabbageForecast (t :: Timeline) era = BabbageForecast
  { bfPoolDistr :: !PoolDistr
  , bfMaxBlockHeaderSize :: !Word16
  , bfMaxBlockBodySize :: !Word32
  , bfProtocolVersion :: !ProtVer
  }
  deriving (Eq, Show)




