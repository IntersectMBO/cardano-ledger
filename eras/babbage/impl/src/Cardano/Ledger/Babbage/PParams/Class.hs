{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Babbage.PParams.Class
  ( BabbageEraPParams (..),
    ppCoinsPerUTxOByteL,
    ppuCoinsPerUTxOByteL,
  )
where

import Cardano.Ledger.Alonzo.PParams.Class (AlonzoEraPParams)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraPParams (..), PParams (..), PParamsUpdate (..), ppLens, ppuLens)
import Cardano.Ledger.HKD (HKD)
import Data.Functor.Identity (Identity)
import Data.Maybe.Strict (StrictMaybe)
import Lens.Micro (Lens')

class AlonzoEraPParams era => BabbageEraPParams era where
  hkdCoinsPerUTxOByteL :: Lens' (PParamsHKD f era) (HKD f Coin)

ppCoinsPerUTxOByteL :: forall era. BabbageEraPParams era => Lens' (PParams era) Coin
ppCoinsPerUTxOByteL = ppLens . hkdCoinsPerUTxOByteL @era @Identity

ppuCoinsPerUTxOByteL :: forall era. BabbageEraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuCoinsPerUTxOByteL = ppuLens . hkdCoinsPerUTxOByteL @era @StrictMaybe
