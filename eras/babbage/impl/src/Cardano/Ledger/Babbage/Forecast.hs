{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Forecast (
  BabbageForecast (..),
  mkBabbageForecast,
  bfPoolDistrL,
  bfMaxBlockHeaderSizeL,
  bfMaxBlockBodySizeL,
  bfProtocolVersionL,
) where

import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.PParams ()
import Cardano.Ledger.Babbage.State.CertState ()
import Cardano.Ledger.BaseTypes (ProtVer)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API.Forecast (
  EraForecast (..),
  Timeline (..),
 )
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState (..),
  curPParamsEpochStateL,
  nesEsL,
  nesPdL,
 )
import Cardano.Ledger.Shelley.Rules ()
import Cardano.Ledger.State (EraGov, PoolDistr (..))
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.))
import NoThunks.Class (NoThunks (..))

-- | Forecast data for Praos eras.
data BabbageForecast (t :: Timeline) era = BabbageForecast
  { bfPoolDistr :: !PoolDistr
  , bfMaxBlockHeaderSize :: !Word16
  , bfMaxBlockBodySize :: !Word32
  , bfProtocolVersion :: !ProtVer
  }
  deriving (Eq, Show, Generic)

type role BabbageForecast phantom phantom

instance NoThunks (BabbageForecast t era)

-- | Helper to build a 'BabbageForecast' from any Praos-era 'NewEpochState'.
mkBabbageForecast ::
  EraGov era =>
  NewEpochState era ->
  BabbageForecast t era
mkBabbageForecast nes =
  BabbageForecast
    { bfPoolDistr = nes ^. nesPdL
    , bfMaxBlockHeaderSize = nes ^. nesEsL . curPParamsEpochStateL . ppMaxBHSizeL
    , bfMaxBlockBodySize = nes ^. nesEsL . curPParamsEpochStateL . ppMaxBBSizeL
    , bfProtocolVersion = nes ^. nesEsL . curPParamsEpochStateL . ppProtocolVersionL
    }

bfPoolDistrL :: Lens' (BabbageForecast t era) PoolDistr
bfPoolDistrL = lens bfPoolDistr $ \s x -> s {bfPoolDistr = x}

bfMaxBlockHeaderSizeL :: Lens' (BabbageForecast t era) Word16
bfMaxBlockHeaderSizeL = lens bfMaxBlockHeaderSize $ \s x -> s {bfMaxBlockHeaderSize = x}

bfMaxBlockBodySizeL :: Lens' (BabbageForecast t era) Word32
bfMaxBlockBodySizeL = lens bfMaxBlockBodySize $ \s x -> s {bfMaxBlockBodySize = x}

bfProtocolVersionL :: Lens' (BabbageForecast t era) ProtVer
bfProtocolVersionL = lens bfProtocolVersion $ \s x -> s {bfProtocolVersion = x}

instance EraForecast BabbageEra where
  type Forecast t BabbageEra = BabbageForecast t BabbageEra
  mkForecast = mkBabbageForecast
  poolDistrForecastL = bfPoolDistrL
  maxBlockHeaderSizeForecastL = bfMaxBlockHeaderSizeL
  maxBlockBodySizeForecastL = bfMaxBlockBodySizeL
  protocolVersionForecastL = bfProtocolVersionL
