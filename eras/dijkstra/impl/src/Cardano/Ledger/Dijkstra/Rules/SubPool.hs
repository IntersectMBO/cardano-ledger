{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Cardano.Ledger.Shelley.Rules (PoolEnv, PoolEvent (..))
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
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data DijkstraSubPoolPredFailure era = DijkstraSubPoolPredFailure
  deriving (Show, Eq, Generic)

instance NoThunks (DijkstraSubPoolPredFailure era)

instance NFData (DijkstraSubPoolPredFailure era)

instance Era era => EncCBOR (DijkstraSubPoolPredFailure era) where
  encCBOR _ = encCBOR ()

instance Typeable era => DecCBOR (DijkstraSubPoolPredFailure era) where
  decCBOR = decCBOR @() *> pure DijkstraSubPoolPredFailure

type instance EraRuleFailure "SUBPOOL" DijkstraEra = DijkstraSubPoolPredFailure DijkstraEra

type instance EraRuleEvent "SUBPOOL" DijkstraEra = DijkstraSubPoolEvent DijkstraEra

instance InjectRuleFailure "SUBPOOL" DijkstraSubPoolPredFailure DijkstraEra

instance InjectRuleEvent "SUBPOOL" DijkstraSubPoolEvent DijkstraEra

newtype DijkstraSubPoolEvent era = DijkstraSubPoolEvent (PoolEvent era)
  deriving (Generic, Eq, NFData)

instance
  ( EraGov era
  , EraRule "SUBPOOL" era ~ DijkstraSUBPOOL era
  ) =>
  STS (DijkstraSUBPOOL era)
  where
  type State (DijkstraSUBPOOL era) = PState era
  type Signal (DijkstraSUBPOOL era) = PoolCert
  type Environment (DijkstraSUBPOOL era) = PoolEnv era
  type BaseM (DijkstraSUBPOOL era) = ShelleyBase
  type PredicateFailure (DijkstraSUBPOOL era) = DijkstraSubPoolPredFailure era
  type Event (DijkstraSUBPOOL era) = DijkstraSubPoolEvent era

  transitionRules = [dijkstraSubPoolTransition @era]

dijkstraSubPoolTransition :: TransitionRule (EraRule "SUBPOOL" era)
dijkstraSubPoolTransition = do
  TRC (_, st, _) <- judgmentContext
  pure st
