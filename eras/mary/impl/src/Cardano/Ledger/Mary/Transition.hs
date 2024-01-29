{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Transition (TransitionConfig (..)) where

import Cardano.Ledger.Allegra
import Cardano.Ledger.Allegra.Transition
import Cardano.Ledger.Crypto
import Cardano.Ledger.Mary.Era
import Cardano.Ledger.Mary.Translation ()
import Cardano.Ledger.Shelley.Transition
import Data.Aeson (FromJSON (..), ToJSON (..))
import Lens.Micro
import NoThunks.Class (NoThunks (..))

instance Crypto c => EraTransition (MaryEra c) where
  newtype TransitionConfig (MaryEra c) = MaryTransitionConfig
    { mtcAllegraTransitionConfig :: TransitionConfig (AllegraEra c)
    }
    deriving (Show, Eq, NoThunks, ToJSON, FromJSON)

  mkTransitionConfig () = MaryTransitionConfig

  registerInState = registerInitialFundsThenStaking

  tcPreviousEraConfigL =
    lens mtcAllegraTransitionConfig (\mtc pc -> mtc {mtcAllegraTransitionConfig = pc})

  tcTranslationContextL = lens (const ()) (const . id)
