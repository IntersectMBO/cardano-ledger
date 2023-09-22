{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Transition (TransitionConfig (..)) where

import Cardano.Ledger.Allegra.Era
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.Transition
import Data.Aeson (FromJSON (..), ToJSON (..))
import Lens.Micro
import NoThunks.Class (NoThunks (..))

instance Crypto c => EraTransition (AllegraEra c) where
  newtype TransitionConfig (AllegraEra c) = AllegraTransitionConfig
    { atcShelleyTransitionConfig :: TransitionConfig (ShelleyEra c)
    }
    deriving (Show, Eq, NoThunks, ToJSON, FromJSON)

  mkTransitionConfig () = AllegraTransitionConfig

  tcPreviousEraConfigL =
    lens atcShelleyTransitionConfig (\atc pc -> atc {atcShelleyTransitionConfig = pc})

  tcTranslationContextL = lens (const ()) (const . id)
