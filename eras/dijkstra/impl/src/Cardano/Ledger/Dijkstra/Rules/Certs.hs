{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Certs () where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Rules (
  ConwayCERTS,
  ConwayCertEvent (..),
  ConwayCertPredFailure (..),
  ConwayCertsEvent (..),
  ConwayCertsPredFailure (..),
  ConwayDELEG,
  ConwayDelegPredFailure,
  ConwayGOVCERT,
  ConwayGovCertPredFailure,
 )
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Rules.Cert ()
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Shelley.Rules (PoolEvent, ShelleyPOOL, ShelleyPoolPredFailure)
import Control.State.Transition.Extended
import GHC.Base (absurd)

type instance EraRuleFailure "CERTS" DijkstraEra = ConwayCertsPredFailure DijkstraEra

type instance EraRuleEvent "CERTS" DijkstraEra = ConwayCertsEvent DijkstraEra

instance InjectRuleFailure "CERTS" ConwayCertsPredFailure DijkstraEra

instance InjectRuleFailure "CERTS" ConwayCertPredFailure DijkstraEra where
  injectFailure = CertFailure

instance InjectRuleFailure "CERTS" ConwayDelegPredFailure DijkstraEra where
  injectFailure = CertFailure . injectFailure

instance InjectRuleFailure "CERTS" ShelleyPoolPredFailure DijkstraEra where
  injectFailure = CertFailure . injectFailure

instance InjectRuleFailure "CERTS" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = CertFailure . injectFailure

instance InjectRuleFailure "CERTS" ConwayGovCertPredFailure DijkstraEra where
  injectFailure = CertFailure . injectFailure

instance
  ( Era era
  , STS (DijkstraCERT era)
  , BaseM (EraRule "CERT" era) ~ ShelleyBase
  , Event (EraRule "CERT" era) ~ ConwayCertEvent era
  , PredicateFailure (EraRule "CERT" era) ~ ConwayCertPredFailure era
  ) =>
  Embed (DijkstraCERT era) (ConwayCERTS era)
  where
  wrapFailed = CertFailure
  wrapEvent = CertEvent

instance
  ( Era era
  , STS (ConwayDELEG era)
  , PredicateFailure (EraRule "DELEG" era) ~ ConwayDelegPredFailure era
  ) =>
  Embed (ConwayDELEG era) (DijkstraCERT era)
  where
  wrapFailed = DelegFailure
  wrapEvent = absurd

instance
  ( Era era
  , STS (ShelleyPOOL era)
  , Event (EraRule "POOL" era) ~ PoolEvent era
  , PredicateFailure (EraRule "POOL" era) ~ ShelleyPoolPredFailure era
  , PredicateFailure (ShelleyPOOL era) ~ ShelleyPoolPredFailure era
  , BaseM (ShelleyPOOL era) ~ ShelleyBase
  ) =>
  Embed (ShelleyPOOL era) (DijkstraCERT era)
  where
  wrapFailed = PoolFailure
  wrapEvent = PoolEvent

instance
  ( Era era
  , STS (ConwayGOVCERT era)
  , PredicateFailure (EraRule "GOVCERT" era) ~ ConwayGovCertPredFailure era
  ) =>
  Embed (ConwayGOVCERT era) (DijkstraCERT era)
  where
  wrapFailed = GovCertFailure
  wrapEvent = absurd
