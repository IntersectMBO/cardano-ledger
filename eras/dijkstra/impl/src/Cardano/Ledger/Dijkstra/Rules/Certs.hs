{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Certs () where

import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Rules.Cert ()
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.State.Transition.Extended
import GHC.Base (absurd)

type instance EraRuleFailure "CERTS" DijkstraEra = Conway.ConwayCertsPredFailure DijkstraEra

type instance EraRuleEvent "CERTS" DijkstraEra = Conway.ConwayCertsEvent DijkstraEra

instance InjectRuleFailure "CERTS" Conway.ConwayCertsPredFailure DijkstraEra

instance InjectRuleFailure "CERTS" Conway.ConwayCertPredFailure DijkstraEra where
  injectFailure = Conway.CertFailure

instance InjectRuleFailure "CERTS" Conway.ConwayDelegPredFailure DijkstraEra where
  injectFailure = Conway.CertFailure . injectFailure

instance InjectRuleFailure "CERTS" Shelley.ShelleyPoolPredFailure DijkstraEra where
  injectFailure = Conway.CertFailure . injectFailure

instance InjectRuleFailure "CERTS" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = Conway.CertFailure . injectFailure

instance InjectRuleFailure "CERTS" Conway.ConwayGovCertPredFailure DijkstraEra where
  injectFailure = Conway.CertFailure . injectFailure

instance
  ( Era era
  , STS (DijkstraCERT era)
  , Event (EraRule "CERT" era) ~ Conway.ConwayCertEvent era
  , PredicateFailure (EraRule "CERT" era) ~ Conway.ConwayCertPredFailure era
  ) =>
  Embed (DijkstraCERT era) (Conway.ConwayCERTS era)
  where
  wrapFailed = Conway.CertFailure
  wrapEvent = Conway.CertEvent

instance
  ( STS (Conway.ConwayDELEG era)
  , PredicateFailure (EraRule "DELEG" era) ~ Conway.ConwayDelegPredFailure era
  ) =>
  Embed (Conway.ConwayDELEG era) (DijkstraCERT era)
  where
  wrapFailed = Conway.DelegFailure
  wrapEvent = absurd

instance
  ( STS (Shelley.ShelleyPOOL era)
  , PredicateFailure (EraRule "POOL" era) ~ Shelley.ShelleyPoolPredFailure era
  , Event (EraRule "POOL" era) ~ Shelley.PoolEvent era
  ) =>
  Embed (Shelley.ShelleyPOOL era) (DijkstraCERT era)
  where
  wrapFailed = Conway.PoolFailure
  wrapEvent = Conway.PoolEvent

instance
  ( STS (Conway.ConwayGOVCERT era)
  , PredicateFailure (EraRule "GOVCERT" era) ~ Conway.ConwayGovCertPredFailure era
  ) =>
  Embed (Conway.ConwayGOVCERT era) (DijkstraCERT era)
  where
  wrapFailed = Conway.GovCertFailure
  wrapEvent = absurd
