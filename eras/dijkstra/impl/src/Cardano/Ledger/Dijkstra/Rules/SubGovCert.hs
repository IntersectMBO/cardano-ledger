{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.SubGovCert (
  SUBGOVCERT,
  DijkstraSubGovCertPredFailure (..),
) where

import Cardano.Ledger.BaseTypes (
  ShelleyBase,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Conway.Core
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert (ConwayGovCert)
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  SUBGOVCERT,
 )
import Cardano.Ledger.Dijkstra.Rules.GovCert (
  DijkstraGovCertPredFailure,
  conwayToDijkstraGovCertPredFailure,
 )
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS,
  Signal,
  State,
  transitionRules,
 )
import Data.Void (Void)
import GHC.Generics (Generic)

newtype DijkstraSubGovCertPredFailure era
  = DijkstraSubGovCertPredFailure (DijkstraGovCertPredFailure era)
  deriving (Show, Eq, Generic, NFData, EncCBOR, DecCBOR)

type instance EraRuleFailure "SUBGOVCERT" DijkstraEra = DijkstraSubGovCertPredFailure DijkstraEra

type instance EraRuleEvent "SUBGOVCERT" DijkstraEra = VoidEraRule "SUBGOVCERT" DijkstraEra

instance InjectRuleFailure "SUBGOVCERT" Conway.ConwayGovCertPredFailure DijkstraEra where
  injectFailure = DijkstraSubGovCertPredFailure . conwayToDijkstraGovCertPredFailure

instance InjectRuleFailure "SUBGOVCERT" DijkstraSubGovCertPredFailure DijkstraEra

instance InjectRuleFailure "SUBGOVCERT" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = DijkstraSubGovCertPredFailure

instance
  ( EraGov era
  , ConwayEraPParams era
  , ConwayEraCertState era
  , EraRule "SUBGOVCERT" era ~ SUBGOVCERT era
  , InjectRuleFailure "SUBGOVCERT" Conway.ConwayGovCertPredFailure era
  , InjectRuleFailure "SUBGOVCERT" DijkstraSubGovCertPredFailure era
  ) =>
  STS (SUBGOVCERT era)
  where
  type State (SUBGOVCERT era) = CertState era
  type Signal (SUBGOVCERT era) = ConwayGovCert
  type Environment (SUBGOVCERT era) = Conway.ConwayGovCertEnv era
  type BaseM (SUBGOVCERT era) = ShelleyBase
  type PredicateFailure (SUBGOVCERT era) = DijkstraSubGovCertPredFailure era
  type Event (SUBGOVCERT era) = Void

  transitionRules = [Conway.conwayGovCertTransition]
