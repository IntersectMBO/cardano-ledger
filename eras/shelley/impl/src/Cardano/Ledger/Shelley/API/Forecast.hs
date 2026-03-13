{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Forecast API for the consensus layer.
--
-- This module provides a type-safe interface for extracting protocol-relevant
-- data from the ledger state, both for the current slot and for future slots
-- within the stability window.
module Cardano.Ledger.Shelley.API.Forecast (
  -- * Core forecast class
  EraForecast (..),
  Timeline (..),

  -- * TPraos-era extension (Shelley through Alonzo)
  ShelleyEraForecast (..),

  -- * Praos-eras (Babbage onwards)
  BabbageEraForecast,

  -- * Main functions
  currentForecast,
  futureForecast,

  -- * Conversion helpers
  forecastChainChecks,
) where

import Cardano.Ledger.BaseTypes (
  Globals,
  Nonce,
  ProtVer,
  ShelleyBase,
  SlotNo,
  UnitInterval,
 )
import Cardano.Ledger.Chain (ChainChecksPParams (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (GenDelegs)
import Cardano.Ledger.Shelley.LedgerState (NewEpochState)
import Cardano.Ledger.State (EraCertState, EraGov, PoolDistr)
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
import qualified Data.List.NonEmpty as NE (head)
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32)
import Lens.Micro (Lens', (^.))

-- | Type-level tag distinguishing current from future forecasts.
type data Timeline = Current | Future

-- | Core class for extracting forecast data from the ledger state.
class
  ( STS (EraRule "TICKF" era)
  , BaseM (EraRule "TICKF" era) ~ ShelleyBase
  , Environment (EraRule "TICKF" era) ~ ()
  , State (EraRule "TICKF" era) ~ NewEpochState era
  , Signal (EraRule "TICKF" era) ~ SlotNo
  , PredicateFailure (EraRule "TICKF" era) ~ Void
  , EraGov era
  , EraCertState era
  ) =>
  EraForecast era
  where
  -- | Per-era forecast type.
  type Forecast (t :: Timeline) era = r | r -> era

  -- | Extract a forecast from a `NewEpochState`
  mkForecast :: NewEpochState era -> Forecast t era

  poolDistrForecastL :: Lens' (Forecast t era) PoolDistr
  maxBlockHeaderSizeForecastL :: Lens' (Forecast t era) Word16
  maxBlockBodySizeForecastL :: Lens' (Forecast t era) Word32
  protocolVersionForecastL :: Lens' (Forecast t era) ProtVer

-- | Extract the forecast for the current slot.
currentForecast :: forall era. EraForecast era => NewEpochState era -> Forecast Current era
currentForecast = mkForecast @era @Current

-- | `TICKF` a `NewEpochState` and then extract the forecast from it.
futureForecast ::
  forall era.
  EraForecast era =>
  Globals ->
  SlotNo ->
  NewEpochState era ->
  Forecast Future era
futureForecast globals slot nes =
  either (absurd . NE.head) (mkForecast @era @Future) $
    flip runReader globals $
      applySTS @(EraRule "TICKF" era) (TRC ((), nes, slot))

-- | Forecast fields available in Praos eras.
-- For now, does not have unique fields beyond EraForecast.
class (EraForecast era, AtLeastEra "Babbage" era) => BabbageEraForecast era

-- | Additional forecast fields available only in TPraos eras.
class (EraForecast era, AtMostEra "Alonzo" era) => ShelleyEraForecast era where
  genDelegsForecastL :: Lens' (Forecast t era) GenDelegs
  decentralizationForecastL :: Lens' (Forecast t era) UnitInterval
  extraEntropyForecastL :: Lens' (Forecast t era) Nonce

-- | Construct 'ChainChecksPParams' from any forecast.
forecastChainChecks :: forall t era. EraForecast era => Forecast t era -> ChainChecksPParams
forecastChainChecks f =
  ChainChecksPParams
    { ccMaxBHSize = f ^. maxBlockHeaderSizeForecastL @era @t
    , ccMaxBBSize = f ^. maxBlockBodySizeForecastL @era @t
    , ccProtocolVersion = f ^. protocolVersionForecastL @era @t
    }
