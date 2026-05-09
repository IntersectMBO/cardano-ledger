{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Cert (
  DijkstraCERT,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Dijkstra.TxCert
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.State.Transition.Extended
import Data.Void (absurd)
import Lens.Micro ((&), (.~), (^.))

type instance EraRuleFailure "CERT" DijkstraEra = Conway.ConwayCertPredFailure DijkstraEra

type instance EraRuleEvent "CERT" DijkstraEra = Conway.ConwayCertEvent DijkstraEra

instance InjectRuleFailure "CERT" Conway.ConwayCertPredFailure DijkstraEra

instance InjectRuleFailure "CERT" Conway.ConwayDelegPredFailure DijkstraEra where
  injectFailure = Conway.DelegFailure

instance InjectRuleFailure "CERT" Shelley.ShelleyPoolPredFailure DijkstraEra where
  injectFailure = Conway.PoolFailure

instance InjectRuleFailure "CERT" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = Conway.GovCertFailure

instance InjectRuleFailure "CERT" Conway.ConwayGovCertPredFailure DijkstraEra where
  injectFailure = Conway.GovCertFailure . injectFailure

instance
  ( Era era
  , State (EraRule "DELEG" era) ~ CertState era
  , State (EraRule "POOL" era) ~ PState era
  , State (EraRule "GOVCERT" era) ~ CertState era
  , Environment (EraRule "DELEG" era) ~ Conway.ConwayDelegEnv era
  , Environment (EraRule "POOL" era) ~ Shelley.PoolEnv era
  , Environment (EraRule "GOVCERT" era) ~ Conway.ConwayGovCertEnv era
  , Signal (EraRule "DELEG" era) ~ ConwayDelegCert
  , Signal (EraRule "POOL" era) ~ PoolCert
  , Signal (EraRule "GOVCERT" era) ~ ConwayGovCert
  , Embed (EraRule "DELEG" era) (DijkstraCERT era)
  , Embed (EraRule "POOL" era) (DijkstraCERT era)
  , Embed (EraRule "GOVCERT" era) (DijkstraCERT era)
  , TxCert era ~ DijkstraTxCert era
  , EraCertState era
  ) =>
  STS (DijkstraCERT era)
  where
  type State (DijkstraCERT era) = CertState era
  type Signal (DijkstraCERT era) = TxCert era
  type Environment (DijkstraCERT era) = Conway.CertEnv era
  type BaseM (DijkstraCERT era) = ShelleyBase
  type PredicateFailure (DijkstraCERT era) = Conway.ConwayCertPredFailure era
  type Event (DijkstraCERT era) = Conway.ConwayCertEvent era

  transitionRules = [certTransition @era]

certTransition ::
  forall era.
  ( State (EraRule "DELEG" era) ~ CertState era
  , State (EraRule "POOL" era) ~ PState era
  , State (EraRule "GOVCERT" era) ~ CertState era
  , Environment (EraRule "DELEG" era) ~ Conway.ConwayDelegEnv era
  , Environment (EraRule "POOL" era) ~ Shelley.PoolEnv era
  , Environment (EraRule "GOVCERT" era) ~ Conway.ConwayGovCertEnv era
  , Signal (EraRule "DELEG" era) ~ ConwayDelegCert
  , Signal (EraRule "POOL" era) ~ PoolCert
  , Signal (EraRule "GOVCERT" era) ~ ConwayGovCert
  , Embed (EraRule "DELEG" era) (DijkstraCERT era)
  , Embed (EraRule "POOL" era) (DijkstraCERT era)
  , Embed (EraRule "GOVCERT" era) (DijkstraCERT era)
  , TxCert era ~ DijkstraTxCert era
  , EraCertState era
  ) =>
  TransitionRule (DijkstraCERT era)
certTransition = do
  TRC (Conway.CertEnv pp currentEpoch committee committeeProposals, certState, c) <- judgmentContext
  let
    certPState = certState ^. certPStateL
    pools = psStakePools certPState
  case c of
    DijkstraTxCertDeleg delegCert ->
      trans @(EraRule "DELEG" era) $
        TRC (Conway.ConwayDelegEnv pp pools, certState, dijkstraToConwayDelegCert delegCert)
    DijkstraTxCertPool poolCert -> do
      newPState <-
        trans @(EraRule "POOL" era) $ TRC (Shelley.PoolEnv currentEpoch pp, certPState, poolCert)
      pure $ certState & certPStateL .~ newPState
    DijkstraTxCertGov govCert -> do
      trans @(EraRule "GOVCERT" era) $
        TRC (Conway.ConwayGovCertEnv pp currentEpoch committee committeeProposals, certState, govCert)

instance
  ( STS (DijkstraGOVCERT era)
  , PredicateFailure (EraRule "GOVCERT" era) ~ DijkstraGovCertPredFailure era
  ) =>
  Embed (DijkstraGOVCERT era) (DijkstraCERT era)
  where
  wrapFailed = Conway.GovCertFailure
  wrapEvent = absurd
