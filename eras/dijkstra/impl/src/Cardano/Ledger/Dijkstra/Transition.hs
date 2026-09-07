{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Transition (
  TransitionConfig (..),
) where

import Cardano.Ledger.Alonzo.Transition (AlonzoEraTransition)
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Transition (
  ConwayEraTransition,
  conwayInjectIntoTestState,
 )
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Genesis
import Cardano.Ledger.Dijkstra.Translation ()
import Cardano.Ledger.Shelley.Transition
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks (..))

instance EraTransition DijkstraEra where
  data TransitionConfig DijkstraEra = DijkstraTransitionConfig
    { dtcDijkstraGenesis :: !DijkstraGenesis
    , dtcConwayTransitionConfig :: !(TransitionConfig ConwayEra)
    }
    deriving (Show, Eq, Generic)

  mkTransitionConfig = DijkstraTransitionConfig

  injectIntoTestState = conwayInjectIntoTestState

  tcPreviousEraConfigL =
    lens dtcConwayTransitionConfig (\dtc pc -> dtc {dtcConwayTransitionConfig = pc})

  tcTranslationContextL =
    lens dtcDijkstraGenesis (\dtc ag -> dtc {dtcDijkstraGenesis = ag})

instance AlonzoEraTransition DijkstraEra

instance ConwayEraTransition DijkstraEra

instance NoThunks (TransitionConfig DijkstraEra)
