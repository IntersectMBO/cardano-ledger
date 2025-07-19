{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Transition (
  TransitionConfig (..),
  toAlonzoTransitionConfigPairs,
) where

import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
import Cardano.Ledger.Alonzo.Translation ()
import Cardano.Ledger.BaseTypes (toKeyValuePairs)
import Cardano.Ledger.Mary
import Cardano.Ledger.Mary.Transition (TransitionConfig (MaryTransitionConfig))
import Cardano.Ledger.Shelley.Transition
import Data.Aeson (KeyValue (..))
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks (..))

instance EraTransition AlonzoEra where
  data TransitionConfig AlonzoEra = AlonzoTransitionConfig
    { atcAlonzoGenesis :: !AlonzoGenesis
    , atcMaryTransitionConfig :: !(TransitionConfig MaryEra)
    }
    deriving (Show, Eq, Generic)

  mkTransitionConfig = AlonzoTransitionConfig

  injectIntoTestState = shelleyRegisterInitialFundsThenStaking

  tcPreviousEraConfigL =
    lens atcMaryTransitionConfig (\atc pc -> atc {atcMaryTransitionConfig = pc})

  tcTranslationContextL =
    lens atcAlonzoGenesis (\atc ag -> atc {atcAlonzoGenesis = ag})

instance NoThunks (TransitionConfig AlonzoEra)

toAlonzoTransitionConfigPairs :: KeyValue e a => TransitionConfig AlonzoEra -> [a]
toAlonzoTransitionConfigPairs = toKeyValuePairs
{-# DEPRECATED toAlonzoTransitionConfigPairs "In favor of `toKeyValuePairs`" #-}
