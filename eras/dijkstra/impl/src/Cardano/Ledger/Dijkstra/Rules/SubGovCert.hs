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
  DijkstraSUBGOVCERT,
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
import Cardano.Ledger.Conway.Rules (
  ConwayGovCertEnv,
  ConwayGovCertPredFailure,
  conwayGovCertTransition,
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert (ConwayGovCert)
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBGOVCERT,
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
import NoThunks.Class (NoThunks (..))

newtype DijkstraSubGovCertPredFailure era
  = DijkstraSubGovCertPredFailure (DijkstraGovCertPredFailure era)
  deriving (Show, Eq, Generic, NFData, NoThunks, EncCBOR, DecCBOR)

type instance EraRuleFailure "SUBGOVCERT" DijkstraEra = DijkstraSubGovCertPredFailure DijkstraEra

type instance EraRuleEvent "SUBGOVCERT" DijkstraEra = VoidEraRule "SUBGOVCERT" DijkstraEra

instance InjectRuleFailure "SUBGOVCERT" ConwayGovCertPredFailure DijkstraEra where
  injectFailure = DijkstraSubGovCertPredFailure . conwayToDijkstraGovCertPredFailure

instance InjectRuleFailure "SUBGOVCERT" DijkstraSubGovCertPredFailure DijkstraEra

instance
  ( EraGov era
  , ConwayEraPParams era
  , ConwayEraCertState era
  , EraRule "SUBGOVCERT" era ~ DijkstraSUBGOVCERT era
  , InjectRuleFailure "SUBGOVCERT" ConwayGovCertPredFailure era
  , InjectRuleFailure "SUBGOVCERT" DijkstraSubGovCertPredFailure era
  ) =>
  STS (DijkstraSUBGOVCERT era)
  where
  type State (DijkstraSUBGOVCERT era) = CertState era
  type Signal (DijkstraSUBGOVCERT era) = ConwayGovCert
  type Environment (DijkstraSUBGOVCERT era) = ConwayGovCertEnv era
  type BaseM (DijkstraSUBGOVCERT era) = ShelleyBase
  type PredicateFailure (DijkstraSUBGOVCERT era) = DijkstraSubGovCertPredFailure era
  type Event (DijkstraSUBGOVCERT era) = Void

  transitionRules = [conwayGovCertTransition]
