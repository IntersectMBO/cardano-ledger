{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Forecast (
  ShelleyForecast (..),
  mkShelleyForecast,
  sfPoolDistrL,
  sfMaxBlockHeaderSizeL,
  sfMaxBlockBodySizeL,
  sfProtocolVersionL,
  sfGenDelegsL,
  sfDecentralizationL,
  sfExtraEntropyL,
) where

import Cardano.Ledger.BaseTypes (
  Nonce,
  ProtVer,
  UnitInterval,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (GenDelegs)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.Forecast (
  EraForecast (..),
  ShelleyEraForecast (..),
  Timeline (..),
 )
import Cardano.Ledger.Shelley.Core (EraGov)
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState (..),
  curPParamsEpochStateL,
  dsGenDelegsL,
  esLStateL,
  lsCertStateL,
  nesEsL,
  nesPdL,
 )
import Cardano.Ledger.State (EraCertState (..), PoolDistr (..))
import Control.DeepSeq (NFData)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.))
import NoThunks.Class (NoThunks (..))

-- | Forecast data for TPraos eras.
data ShelleyForecast (t :: Timeline) era = ShelleyForecast
  { sfPoolDistr :: !PoolDistr
  , sfMaxBlockHeaderSize :: !Word16
  , sfMaxBlockBodySize :: !Word32
  , sfProtocolVersion :: !ProtVer
  , sfGenDelegs :: !GenDelegs
  , sfDecentralization :: !UnitInterval
  , sfExtraEntropy :: !Nonce
  }
  deriving (Eq, Show, Generic)

type role ShelleyForecast nominal nominal

instance NFData (ShelleyForecast t era)

instance NoThunks (ShelleyForecast t era)

-- | Helper to build a 'ShelleyForecast' from any TPraos-era 'NewEpochState'.
mkShelleyForecast ::
  (AtMostEra "Alonzo" era, EraGov era, EraCertState era) =>
  NewEpochState era ->
  ShelleyForecast t era
mkShelleyForecast nes =
  ShelleyForecast
    { sfPoolDistr = nes ^. nesPdL
    , sfMaxBlockHeaderSize = nes ^. nesEsL . curPParamsEpochStateL . ppMaxBHSizeL
    , sfMaxBlockBodySize = nes ^. nesEsL . curPParamsEpochStateL . ppMaxBBSizeL
    , sfProtocolVersion = nes ^. nesEsL . curPParamsEpochStateL . ppProtocolVersionL
    , sfGenDelegs = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . dsGenDelegsL
    , sfDecentralization = nes ^. nesEsL . curPParamsEpochStateL . ppDG
    , sfExtraEntropy = nes ^. nesEsL . curPParamsEpochStateL . ppExtraEntropyL
    }

sfPoolDistrL :: Lens' (ShelleyForecast t era) PoolDistr
sfPoolDistrL = lens sfPoolDistr $ \s x -> s {sfPoolDistr = x}

sfMaxBlockHeaderSizeL :: Lens' (ShelleyForecast t era) Word16
sfMaxBlockHeaderSizeL = lens sfMaxBlockHeaderSize $ \s x -> s {sfMaxBlockHeaderSize = x}

sfMaxBlockBodySizeL :: Lens' (ShelleyForecast t era) Word32
sfMaxBlockBodySizeL = lens sfMaxBlockBodySize $ \s x -> s {sfMaxBlockBodySize = x}

sfProtocolVersionL :: Lens' (ShelleyForecast t era) ProtVer
sfProtocolVersionL = lens sfProtocolVersion $ \s x -> s {sfProtocolVersion = x}

sfGenDelegsL :: Lens' (ShelleyForecast t era) GenDelegs
sfGenDelegsL = lens sfGenDelegs $ \s x -> s {sfGenDelegs = x}

sfDecentralizationL :: Lens' (ShelleyForecast t era) UnitInterval
sfDecentralizationL = lens sfDecentralization $ \s x -> s {sfDecentralization = x}

sfExtraEntropyL :: Lens' (ShelleyForecast t era) Nonce
sfExtraEntropyL = lens sfExtraEntropy $ \s x -> s {sfExtraEntropy = x}

instance EraForecast ShelleyEra where
  type Forecast t ShelleyEra = ShelleyForecast t ShelleyEra
  mkForecast = mkShelleyForecast
  poolDistrForecastL = sfPoolDistrL
  maxBlockHeaderSizeForecastL = sfMaxBlockHeaderSizeL
  maxBlockBodySizeForecastL = sfMaxBlockBodySizeL
  protocolVersionForecastL = sfProtocolVersionL

instance ShelleyEraForecast ShelleyEra where
  genDelegsForecastL = sfGenDelegsL
  decentralizationForecastL = sfDecentralizationL
  extraEntropyForecastL = sfExtraEntropyL
