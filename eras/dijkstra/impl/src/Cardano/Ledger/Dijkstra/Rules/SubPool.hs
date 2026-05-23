{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.SubPool (
  SUBPOOL,
  DijkstraSubPoolPredFailure (..),
  DijkstraSubPoolEvent (..),
) where

import Cardano.Ledger.BaseTypes (
  ShelleyBase,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  SUBPOOL,
 )
import Cardano.Ledger.Dijkstra.State
import qualified Cardano.Ledger.Shelley.Rules as Shelley
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
import GHC.Generics (Generic)

newtype DijkstraSubPoolPredFailure era = DijkstraSubPoolPredFailure (Shelley.ShelleyPoolPredFailure era)
  deriving (Eq, Show, Generic, DecCBOR, EncCBOR, NFData)

type instance EraRuleFailure "SUBPOOL" DijkstraEra = DijkstraSubPoolPredFailure DijkstraEra

type instance EraRuleEvent "SUBPOOL" DijkstraEra = DijkstraSubPoolEvent DijkstraEra

instance InjectRuleFailure "SUBPOOL" DijkstraSubPoolPredFailure DijkstraEra

instance InjectRuleFailure "SUBPOOL" Shelley.ShelleyPoolPredFailure DijkstraEra where
  injectFailure = DijkstraSubPoolPredFailure

instance InjectRuleEvent "SUBPOOL" DijkstraSubPoolEvent DijkstraEra

instance InjectRuleEvent "SUBPOOL" Shelley.PoolEvent DijkstraEra where
  injectEvent = DijkstraSubPoolEvent

newtype DijkstraSubPoolEvent era = DijkstraSubPoolEvent (Shelley.PoolEvent era)
  deriving (Generic, Eq, NFData)

instance
  ( EraGov era
  , EraRule "SUBPOOL" era ~ SUBPOOL era
  , InjectRuleEvent "SUBPOOL" DijkstraSubPoolEvent era
  , InjectRuleEvent "SUBPOOL" Shelley.PoolEvent era
  , InjectRuleFailure "SUBPOOL" DijkstraSubPoolPredFailure era
  , InjectRuleFailure "SUBPOOL" Shelley.ShelleyPoolPredFailure era
  ) =>
  STS (SUBPOOL era)
  where
  type State (SUBPOOL era) = PState era
  type Signal (SUBPOOL era) = PoolCert
  type Environment (SUBPOOL era) = Shelley.PoolEnv era
  type BaseM (SUBPOOL era) = ShelleyBase
  type PredicateFailure (SUBPOOL era) = DijkstraSubPoolPredFailure era
  type Event (SUBPOOL era) = DijkstraSubPoolEvent era

  transitionRules = [Shelley.poolTransition]
