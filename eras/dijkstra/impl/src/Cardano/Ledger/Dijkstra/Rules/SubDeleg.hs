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
  SUBDELEG,
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
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.TxCert (ConwayDelegCert)
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  SUBDELEG,
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

newtype DijkstraSubDelegPredFailure era = DijkstraSubDelegPredFailure (Conway.ConwayDelegPredFailure era)
  deriving (Generic, Eq, Show, NFData, EncCBOR, DecCBOR)

type instance EraRuleFailure "SUBDELEG" DijkstraEra = DijkstraSubDelegPredFailure DijkstraEra

type instance EraRuleEvent "SUBDELEG" DijkstraEra = VoidEraRule "SUBDELEG" DijkstraEra

instance InjectRuleFailure "SUBDELEG" DijkstraSubDelegPredFailure DijkstraEra

instance InjectRuleFailure "SUBDELEG" Conway.ConwayDelegPredFailure DijkstraEra where
  injectFailure = DijkstraSubDelegPredFailure

instance
  ( EraGov era
  , ConwayEraCertState era
  , EraRule "SUBDELEG" era ~ SUBDELEG era
  , InjectRuleFailure "SUBDELEG" Conway.ConwayDelegPredFailure era
  ) =>
  STS (SUBDELEG era)
  where
  type State (SUBDELEG era) = CertState era
  type Signal (SUBDELEG era) = ConwayDelegCert
  type Environment (SUBDELEG era) = Conway.ConwayDelegEnv era
  type BaseM (SUBDELEG era) = ShelleyBase
  type PredicateFailure (SUBDELEG era) = DijkstraSubDelegPredFailure era
  type Event (SUBDELEG era) = Void

  transitionRules = [Conway.conwayDelegTransition]
