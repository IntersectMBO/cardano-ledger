{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Dijkstra.Era () where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (Era (..), Value)
import Cardano.Ledger.Mary (MaryValue)

data DijkstraEra

instance Era DijkstraEra where
  type PreviousEra DijkstraEra = ConwayEra
  type ProtVerLow DijkstraEra = 12
  type ProtVerHigh DijkstraEra = 12

  eraName = "Dijkstra"

type instance Value DijkstraEra = MaryValue
