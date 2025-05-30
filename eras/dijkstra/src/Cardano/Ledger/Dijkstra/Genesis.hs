{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Genesis () where

import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Genesis (EraGenesis (..), NoGenesis)

instance EraGenesis DijkstraEra where
  type Genesis DijkstraEra = NoGenesis DijkstraEra
