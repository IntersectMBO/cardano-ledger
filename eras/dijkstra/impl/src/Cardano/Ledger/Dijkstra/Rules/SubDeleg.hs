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

module Cardano.Ledger.Dijkstra.Rules.SubDeleg (
  DijkstraSUBDELEG,
  DijkstraSubDelegPredFailure (..),
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
  ConwayDelegEnv,
  ConwayDelegPredFailure,
  conwayDelegTransition,
 )
import Cardano.Ledger.Conway.TxCert (ConwayDelegCert)
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBDELEG,
 )
import Cardano.Ledger.Dijkstra.State
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

newtype DijkstraSubDelegPredFailure era = DijkstraSubDelegPredFailure (ConwayDelegPredFailure era)
  deriving (Generic, Eq, Show, NFData, NoThunks, EncCBOR, DecCBOR)

type instance EraRuleFailure "SUBDELEG" DijkstraEra = DijkstraSubDelegPredFailure DijkstraEra

type instance EraRuleEvent "SUBDELEG" DijkstraEra = VoidEraRule "SUBDELEG" DijkstraEra

instance InjectRuleFailure "SUBDELEG" DijkstraSubDelegPredFailure DijkstraEra

instance InjectRuleFailure "SUBDELEG" ConwayDelegPredFailure DijkstraEra where
  injectFailure = DijkstraSubDelegPredFailure

instance
  ( EraGov era
  , ConwayEraCertState era
  , EraRule "SUBDELEG" era ~ DijkstraSUBDELEG era
  , InjectRuleFailure "SUBDELEG" ConwayDelegPredFailure era
  ) =>
  STS (DijkstraSUBDELEG era)
  where
  type State (DijkstraSUBDELEG era) = CertState era
  type Signal (DijkstraSUBDELEG era) = ConwayDelegCert
  type Environment (DijkstraSUBDELEG era) = ConwayDelegEnv era
  type BaseM (DijkstraSUBDELEG era) = ShelleyBase
  type PredicateFailure (DijkstraSUBDELEG era) = DijkstraSubDelegPredFailure era
  type Event (DijkstraSUBDELEG era) = Void

  transitionRules = [conwayDelegTransition]
