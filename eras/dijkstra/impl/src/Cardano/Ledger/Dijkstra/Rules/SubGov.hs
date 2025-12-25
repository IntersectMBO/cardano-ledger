{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.SubGov (
  DijkstraSUBGOV,
  DijkstraSubGovPredFailure (..),
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
import Cardano.Ledger.Conway.Rules (GovEnv, GovSignal)
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBGOV,
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
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  transitionRules,
 )
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data DijkstraSubGovPredFailure era = DijkstraSubGovPredFailure
  deriving (Show, Eq, Generic)

instance NoThunks (DijkstraSubGovPredFailure era)

instance NFData (DijkstraSubGovPredFailure era)

instance Era era => EncCBOR (DijkstraSubGovPredFailure era) where
  encCBOR _ = encCBOR ()

instance Typeable era => DecCBOR (DijkstraSubGovPredFailure era) where
  decCBOR = decCBOR @() *> pure DijkstraSubGovPredFailure

type instance EraRuleFailure "SUBGOV" DijkstraEra = DijkstraSubGovPredFailure DijkstraEra

type instance EraRuleEvent "SUBGOV" DijkstraEra = VoidEraRule "SUBGOV" DijkstraEra

instance InjectRuleFailure "SUBGOV" DijkstraSubGovPredFailure DijkstraEra

instance
  ( EraGov era
  , EraRule "SUBGOV" era ~ DijkstraSUBGOV era
  ) =>
  STS (DijkstraSUBGOV era)
  where
  type State (DijkstraSUBGOV era) = Proposals era
  type Signal (DijkstraSUBGOV era) = GovSignal era
  type Environment (DijkstraSUBGOV era) = GovEnv era
  type BaseM (DijkstraSUBGOV era) = ShelleyBase
  type PredicateFailure (DijkstraSUBGOV era) = DijkstraSubGovPredFailure era
  type Event (DijkstraSUBGOV era) = Void

  transitionRules = [dijkstraSubGovTransition @era]

dijkstraSubGovTransition :: TransitionRule (EraRule "SUBGOV" era)
dijkstraSubGovTransition = do
  TRC (_, st, _) <- judgmentContext
  pure st
