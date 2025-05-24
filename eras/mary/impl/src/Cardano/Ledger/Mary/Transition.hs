{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Transition (TransitionConfig (..)) where

import Cardano.Ledger.Allegra
import Cardano.Ledger.Allegra.Transition
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Mary.Era
import Cardano.Ledger.Mary.State ()
import Cardano.Ledger.Mary.Translation ()
import Cardano.Ledger.Shelley.Transition
import Lens.Micro
import NoThunks.Class (NoThunks (..))

instance EraTransition MaryEra where
  newtype TransitionConfig MaryEra = MaryTransitionConfig
    { mtcAllegraTransitionConfig :: TransitionConfig AllegraEra
    }
    deriving (Show, Eq, NoThunks)

  mkTransitionConfig NoGenesis = MaryTransitionConfig

  injectIntoTestState = shelleyRegisterInitialFundsThenStaking

  tcPreviousEraConfigL =
    lens mtcAllegraTransitionConfig (\mtc pc -> mtc {mtcAllegraTransitionConfig = pc})

  tcTranslationContextL = lens (const NoGenesis) const
