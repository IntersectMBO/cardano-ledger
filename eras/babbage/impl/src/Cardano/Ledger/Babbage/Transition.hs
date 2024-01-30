{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Transition (TransitionConfig (..)) where

import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Transition
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Babbage.Translation ()
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley.Transition
import Data.Aeson (FromJSON (..), ToJSON (..))
import Lens.Micro
import NoThunks.Class (NoThunks (..))

instance Crypto c => EraTransition (BabbageEra c) where
  newtype TransitionConfig (BabbageEra c) = BabbageTransitionConfig
    { btcAlonzoTransitionConfig :: TransitionConfig (AlonzoEra c)
    }
    deriving (Show, Eq, NoThunks, ToJSON, FromJSON)

  mkTransitionConfig () = BabbageTransitionConfig

  injectIntoTestState = registerInitialFundsThenStaking

  tcPreviousEraConfigL =
    lens btcAlonzoTransitionConfig (\btc pc -> btc {btcAlonzoTransitionConfig = pc})

  tcTranslationContextL = lens (const ()) (const . id)
