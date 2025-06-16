{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Pool () where

import Cardano.Ledger.Dijkstra.Core (EraRuleEvent, EraRuleFailure)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Shelley.Rules (PoolEvent, ShelleyPoolPredFailure)

type instance EraRuleFailure "POOL" DijkstraEra = ShelleyPoolPredFailure DijkstraEra

type instance EraRuleEvent "POOL" DijkstraEra = PoolEvent DijkstraEra
