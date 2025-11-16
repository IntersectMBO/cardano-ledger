{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Transition (TransitionConfig (..), alonzoInjectCostModels) where

import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Transition
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Babbage.State ()
import Cardano.Ledger.Babbage.Translation ()
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Shelley.Transition
import Lens.Micro
import NoThunks.Class (NoThunks (..))

instance EraTransition BabbageEra where
  newtype TransitionConfig BabbageEra = BabbageTransitionConfig
    { btcAlonzoTransitionConfig :: TransitionConfig AlonzoEra
    }
    deriving (Show, Eq, NoThunks)

  mkTransitionConfig NoGenesis = BabbageTransitionConfig

  injectIntoTestState cfg = shelleyRegisterInitialFundsThenStaking cfg . alonzoInjectCostModels (cfg ^. tcPreviousEraConfigL)

  tcPreviousEraConfigL =
    lens btcAlonzoTransitionConfig (\btc pc -> btc {btcAlonzoTransitionConfig = pc})

  tcTranslationContextL = lens (const NoGenesis) const
