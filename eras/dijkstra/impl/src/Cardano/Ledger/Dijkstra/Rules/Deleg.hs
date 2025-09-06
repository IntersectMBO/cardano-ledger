{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Deleg () where

import Cardano.Ledger.Conway.Rules (ConwayDelegPredFailure)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)

type instance EraRuleFailure "DELEG" DijkstraEra = ConwayDelegPredFailure DijkstraEra

type instance EraRuleEvent "DELEG" DijkstraEra = VoidEraRule "DELEG" DijkstraEra

instance InjectRuleFailure "DELEG" ConwayDelegPredFailure DijkstraEra
