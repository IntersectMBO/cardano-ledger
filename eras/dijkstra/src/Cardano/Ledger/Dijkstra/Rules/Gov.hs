{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Gov () where

import Cardano.Ledger.Conway.Rules (ConwayGovEvent, ConwayGovPredFailure)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)

type instance EraRuleFailure "GOV" DijkstraEra = ConwayGovPredFailure DijkstraEra

type instance EraRuleEvent "GOV" DijkstraEra = ConwayGovEvent DijkstraEra

instance InjectRuleFailure "GOV" ConwayGovPredFailure DijkstraEra
