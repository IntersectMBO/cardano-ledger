{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.SubGov (
  DijkstraSUBGOV,
  DijkstraSubGovPredFailure (..),
  DijkstraSubGovEvent (..),
) where

import Cardano.Ledger.BaseTypes (
  ShelleyBase,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State (ConwayEraCertState)
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBGOV,
 )
import Cardano.Ledger.Dijkstra.Rules.Gov (DijkstraGovPredFailure, conwayToDijkstraGovPredFailure)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import GHC.Generics (Generic)

newtype DijkstraSubGovPredFailure era = DijkstraSubGovPredFailure (DijkstraGovPredFailure era)
  deriving (Generic, Eq, Show, NFData, EncCBOR, DecCBOR)

type instance EraRuleFailure "SUBGOV" DijkstraEra = DijkstraSubGovPredFailure DijkstraEra

type instance EraRuleEvent "SUBGOV" DijkstraEra = DijkstraSubGovEvent DijkstraEra

instance InjectRuleFailure "SUBGOV" Conway.ConwayGovPredFailure DijkstraEra where
  injectFailure = DijkstraSubGovPredFailure . conwayToDijkstraGovPredFailure

instance InjectRuleFailure "SUBGOV" DijkstraSubGovPredFailure DijkstraEra

instance InjectRuleFailure "SUBGOV" DijkstraGovPredFailure DijkstraEra where
  injectFailure = DijkstraSubGovPredFailure

newtype DijkstraSubGovEvent era = DijkstraSubGovEvent (Conway.ConwayGovEvent era)
  deriving (Generic, Eq, NFData)

instance InjectRuleEvent "SUBGOV" DijkstraSubGovEvent DijkstraEra

instance InjectRuleEvent "SUBGOV" Conway.ConwayGovEvent DijkstraEra where
  injectEvent = DijkstraSubGovEvent

instance
  ( ConwayEraCertState era
  , ConwayEraTxCert era
  , ConwayEraPParams era
  , ConwayEraGov era
  , EraRule "SUBGOV" era ~ DijkstraSUBGOV era
  , InjectRuleEvent "SUBGOV" DijkstraSubGovEvent era
  , InjectRuleEvent "SUBGOV" Conway.ConwayGovEvent era
  , InjectRuleFailure "SUBGOV" DijkstraSubGovPredFailure era
  , InjectRuleFailure "SUBGOV" Conway.ConwayGovPredFailure era
  ) =>
  STS (DijkstraSUBGOV era)
  where
  type State (DijkstraSUBGOV era) = Proposals era
  type Signal (DijkstraSUBGOV era) = Conway.GovSignal era
  type Environment (DijkstraSUBGOV era) = Conway.GovEnv era
  type BaseM (DijkstraSUBGOV era) = ShelleyBase
  type PredicateFailure (DijkstraSUBGOV era) = DijkstraSubGovPredFailure era
  type Event (DijkstraSUBGOV era) = DijkstraSubGovEvent era

  transitionRules = [Conway.conwayGovTransition]
