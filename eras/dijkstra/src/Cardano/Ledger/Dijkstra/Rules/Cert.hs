{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Cert () where

import Cardano.Ledger.Conway.Rules (
  ConwayCertEvent,
  ConwayCertPredFailure (..),
  ConwayDelegPredFailure,
  ConwayGovCertPredFailure,
 )
import Cardano.Ledger.Dijkstra.Core (EraRuleEvent, EraRuleFailure, InjectRuleFailure (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Shelley.Rules (ShelleyPoolPredFailure)

type instance EraRuleFailure "CERT" DijkstraEra = ConwayCertPredFailure DijkstraEra

type instance EraRuleEvent "CERT" DijkstraEra = ConwayCertEvent DijkstraEra

instance InjectRuleFailure "CERT" ConwayCertPredFailure DijkstraEra

instance InjectRuleFailure "CERT" ConwayDelegPredFailure DijkstraEra where
  injectFailure = DelegFailure

instance InjectRuleFailure "CERT" ShelleyPoolPredFailure DijkstraEra where
  injectFailure = PoolFailure

instance InjectRuleFailure "CERT" ConwayGovCertPredFailure DijkstraEra where
  injectFailure = GovCertFailure
