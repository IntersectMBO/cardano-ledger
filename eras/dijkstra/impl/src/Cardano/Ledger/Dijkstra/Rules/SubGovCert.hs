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
import Cardano.Ledger.Conway.Rules (ConwayGovCertEnv)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert (ConwayGovCert)
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBGOVCERT,
 )
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS,
  Signal,
  State,
  TRC (TRC),
  TransitionRule,
  judgmentContext,
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

instance InjectRuleFailure "SUBGOVCERT" DijkstraSubGovCertPredFailure DijkstraEra

instance
  ( EraGov era
  , EraCertState era
  , EraRule "SUBGOVCERT" era ~ DijkstraSUBGOVCERT era
  ) =>
  STS (DijkstraSUBGOVCERT era)
  where
  type State (DijkstraSUBGOVCERT era) = CertState era
  type Signal (DijkstraSUBGOVCERT era) = ConwayGovCert
  type Environment (DijkstraSUBGOVCERT era) = ConwayGovCertEnv era
  type BaseM (DijkstraSUBGOVCERT era) = ShelleyBase
  type PredicateFailure (DijkstraSUBGOVCERT era) = DijkstraSubGovCertPredFailure era
  type Event (DijkstraSUBGOVCERT era) = Void

  transitionRules = [dijkstraSubGovCertTransition @era]

dijkstraSubGovCertTransition :: TransitionRule (EraRule "SUBGOVCERT" era)
dijkstraSubGovCertTransition = do
  TRC (_, st, _) <- judgmentContext
  pure st
