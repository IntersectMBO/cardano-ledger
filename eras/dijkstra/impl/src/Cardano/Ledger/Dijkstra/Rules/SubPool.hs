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
  DijkstraSUBPOOL,
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
  DijkstraSUBPOOL,
 )
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Shelley.Rules (
  PoolEnv,
  PoolEvent (..),
  ShelleyPoolPredFailure,
  poolTransition,
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
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

newtype DijkstraSubPoolPredFailure era = DijkstraSubPoolPredFailure (ShelleyPoolPredFailure era)
  deriving (Eq, Show, Generic, DecCBOR, EncCBOR, NFData, NoThunks)

type instance EraRuleFailure "SUBPOOL" DijkstraEra = DijkstraSubPoolPredFailure DijkstraEra

type instance EraRuleEvent "SUBPOOL" DijkstraEra = DijkstraSubPoolEvent DijkstraEra

instance InjectRuleFailure "SUBPOOL" DijkstraSubPoolPredFailure DijkstraEra

instance InjectRuleFailure "SUBPOOL" ShelleyPoolPredFailure DijkstraEra where
  injectFailure = DijkstraSubPoolPredFailure

instance InjectRuleEvent "SUBPOOL" DijkstraSubPoolEvent DijkstraEra

instance InjectRuleEvent "SUBPOOL" PoolEvent DijkstraEra where
  injectEvent = DijkstraSubPoolEvent

newtype DijkstraSubPoolEvent era = DijkstraSubPoolEvent (PoolEvent era)
  deriving (Generic, Eq, NFData)

instance
  ( EraGov era
  , EraRule "SUBPOOL" era ~ DijkstraSUBPOOL era
  , InjectRuleEvent "SUBPOOL" DijkstraSubPoolEvent era
  , InjectRuleEvent "SUBPOOL" PoolEvent era
  , InjectRuleFailure "SUBPOOL" DijkstraSubPoolPredFailure era
  , InjectRuleFailure "SUBPOOL" ShelleyPoolPredFailure era
  ) =>
  STS (DijkstraSUBPOOL era)
  where
  type State (DijkstraSUBPOOL era) = PState era
  type Signal (DijkstraSUBPOOL era) = PoolCert
  type Environment (DijkstraSUBPOOL era) = PoolEnv era
  type BaseM (DijkstraSUBPOOL era) = ShelleyBase
  type PredicateFailure (DijkstraSUBPOOL era) = DijkstraSubPoolPredFailure era
  type Event (DijkstraSUBPOOL era) = DijkstraSubPoolEvent era

  transitionRules = [poolTransition]
