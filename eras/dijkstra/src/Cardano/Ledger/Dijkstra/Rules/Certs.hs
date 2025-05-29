{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Certs () where

import Cardano.Ledger.Conway.Rules (
  ConwayCertPredFailure,
  ConwayCertsEvent,
  ConwayCertsPredFailure (..),
  ConwayDelegPredFailure,
  ConwayGovCertPredFailure,
 )
import Cardano.Ledger.Dijkstra.Core (EraRuleEvent, EraRuleFailure, InjectRuleFailure (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules.Cert ()
import Cardano.Ledger.Shelley.Rules (ShelleyPoolPredFailure)

type instance EraRuleFailure "CERTS" DijkstraEra = ConwayCertsPredFailure DijkstraEra

type instance EraRuleEvent "CERTS" DijkstraEra = ConwayCertsEvent DijkstraEra

instance InjectRuleFailure "CERTS" ConwayCertsPredFailure DijkstraEra

instance InjectRuleFailure "CERTS" ConwayCertPredFailure DijkstraEra where
  injectFailure = CertFailure

instance InjectRuleFailure "CERTS" ConwayDelegPredFailure DijkstraEra where
  injectFailure = CertFailure . injectFailure

instance InjectRuleFailure "CERTS" ShelleyPoolPredFailure DijkstraEra where
  injectFailure = CertFailure . injectFailure

instance InjectRuleFailure "CERTS" ConwayGovCertPredFailure DijkstraEra where
  injectFailure = CertFailure . injectFailure
