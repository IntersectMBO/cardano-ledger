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

import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe (..))
import Cardano.Ledger.Conway.Rules (
  CertEnv (..),
  ConwayCertEvent,
  ConwayCertPredFailure (..),
  ConwayDelegEnv (..),
  ConwayDelegPredFailure,
  ConwayGovCertEnv (..),
  ConwayGovCertPredFailure,
 )
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Dijkstra.TxCert
import Cardano.Ledger.Shelley.Rules (PoolEnv (..), ShelleyPoolPredFailure)
import Control.State.Transition.Extended
import Data.Void (absurd)
import Lens.Micro ((&), (.~), (^.))

type instance EraRuleFailure "CERT" DijkstraEra = ConwayCertPredFailure DijkstraEra

type instance EraRuleEvent "CERT" DijkstraEra = ConwayCertEvent DijkstraEra

instance InjectRuleFailure "CERT" ConwayCertPredFailure DijkstraEra

instance InjectRuleFailure "CERT" ConwayDelegPredFailure DijkstraEra where
  injectFailure = DelegFailure

instance InjectRuleFailure "CERT" ShelleyPoolPredFailure DijkstraEra where
  injectFailure = PoolFailure

instance InjectRuleFailure "CERT" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = GovCertFailure

instance InjectRuleFailure "CERT" ConwayGovCertPredFailure DijkstraEra where
  injectFailure = GovCertFailure . injectFailure

instance
  ( Era era
  , State (EraRule "DELEG" era) ~ CertState era
  , State (EraRule "POOL" era) ~ PState era
  , State (EraRule "GOVCERT" era) ~ CertState era
  , Environment (EraRule "DELEG" era) ~ ConwayDelegEnv era
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , Environment (EraRule "GOVCERT" era) ~ ConwayGovCertEnv era
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
  type Environment (DijkstraCERT era) = CertEnv era
  type BaseM (DijkstraCERT era) = ShelleyBase
  type PredicateFailure (DijkstraCERT era) = ConwayCertPredFailure era
  type Event (DijkstraCERT era) = ConwayCertEvent era

  transitionRules = [certTransition @era]

certTransition ::
  forall era.
  ( State (EraRule "DELEG" era) ~ CertState era
  , State (EraRule "POOL" era) ~ PState era
  , State (EraRule "GOVCERT" era) ~ CertState era
  , Environment (EraRule "DELEG" era) ~ ConwayDelegEnv era
  , Environment (EraRule "POOL" era) ~ PoolEnv era
  , Environment (EraRule "GOVCERT" era) ~ ConwayGovCertEnv era
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
  TRC (CertEnv pp currentEpoch committee committeeProposals, certState, c) <- judgmentContext
  let
    certPState = certState ^. certPStateL
    pools = psStakePools certPState
  case c of
    DijkstraTxCertDeleg delegCert ->
      let conwayDelegCert = case delegCert of
            DijkstraRegCert cred coin -> ConwayRegCert cred (SJust coin)
            DijkstraUnRegCert cred coin -> ConwayUnRegCert cred (SJust coin)
            DijkstraDelegCert cred d -> ConwayDelegCert cred d
            DijkstraRegDelegCert sc d coin -> ConwayRegDelegCert sc d coin
       in trans @(EraRule "DELEG" era) $
            TRC (ConwayDelegEnv pp pools, certState, conwayDelegCert)
    DijkstraTxCertPool poolCert -> do
      newPState <- trans @(EraRule "POOL" era) $ TRC (PoolEnv currentEpoch pp, certPState, poolCert)
      pure $ certState & certPStateL .~ newPState
    DijkstraTxCertGov govCert -> do
      trans @(EraRule "GOVCERT" era) $
        TRC (ConwayGovCertEnv pp currentEpoch committee committeeProposals, certState, govCert)

instance
  ( Era era
  , STS (DijkstraGOVCERT era)
  , PredicateFailure (EraRule "GOVCERT" era) ~ DijkstraGovCertPredFailure era
  ) =>
  Embed (DijkstraGOVCERT era) (DijkstraCERT era)
  where
  wrapFailed = GovCertFailure
  wrapEvent = absurd
