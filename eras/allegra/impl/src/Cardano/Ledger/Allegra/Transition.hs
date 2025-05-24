{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Transition (TransitionConfig (..)) where

import Cardano.Ledger.Allegra.Era
import Cardano.Ledger.Allegra.State ()
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.Transition
import Lens.Micro
import NoThunks.Class (NoThunks (..))

instance EraTransition AllegraEra where
  newtype TransitionConfig AllegraEra = AllegraTransitionConfig
    { atcShelleyTransitionConfig :: TransitionConfig ShelleyEra
    }
    deriving (Show, Eq, NoThunks)

  mkTransitionConfig NoGenesis = AllegraTransitionConfig

  injectIntoTestState = shelleyRegisterInitialFundsThenStaking

  tcPreviousEraConfigL =
    lens atcShelleyTransitionConfig (\atc pc -> atc {atcShelleyTransitionConfig = pc})

  tcTranslationContextL = lens (const NoGenesis) const
