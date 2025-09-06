{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.GovCert () where

import Cardano.Ledger.Conway.Rules (ConwayGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)

type instance EraRuleFailure "GOVCERT" DijkstraEra = ConwayGovCertPredFailure DijkstraEra

type instance EraRuleEvent "GOVCERT" DijkstraEra = VoidEraRule "GOVCERT" DijkstraEra

instance InjectRuleFailure "GOVCERT" ConwayGovCertPredFailure DijkstraEra
