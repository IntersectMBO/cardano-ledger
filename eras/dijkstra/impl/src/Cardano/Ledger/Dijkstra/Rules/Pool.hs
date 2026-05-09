{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Pool () where

import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "POOL" DijkstraEra = Shelley.ShelleyPoolPredFailure DijkstraEra

type instance EraRuleEvent "POOL" DijkstraEra = Shelley.PoolEvent DijkstraEra

instance InjectRuleFailure "POOL" Shelley.ShelleyPoolPredFailure DijkstraEra

instance InjectRuleEvent "POOL" Shelley.PoolEvent DijkstraEra
