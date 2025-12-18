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

module Cardano.Ledger.Dijkstra.Rules.SubUtxow (
  DijkstraSUBUTXOW,
  DijkstraSubUtxowPredFailure (..),
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
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBUTXOW,
 )
import Cardano.Ledger.Shelley.LedgerState (UTxOState)
import Cardano.Ledger.Shelley.Rules (UtxoEnv)
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

data DijkstraSubUtxowPredFailure era = DijkstraSubUtxowPredFailure
  deriving (Show, Eq, Generic)

instance NoThunks (DijkstraSubUtxowPredFailure era)

instance NFData (DijkstraSubUtxowPredFailure era)

instance Era era => EncCBOR (DijkstraSubUtxowPredFailure era) where
  encCBOR _ = encCBOR ()

instance Typeable era => DecCBOR (DijkstraSubUtxowPredFailure era) where
  decCBOR = decCBOR @() *> pure DijkstraSubUtxowPredFailure

type instance EraRuleFailure "SUBUTXOW" DijkstraEra = DijkstraSubUtxowPredFailure DijkstraEra

type instance EraRuleEvent "SUBUTXOW" DijkstraEra = VoidEraRule "SUBUTXOW" DijkstraEra

instance InjectRuleFailure "SUBUTXOW" DijkstraSubUtxowPredFailure DijkstraEra

instance
  ( ConwayEraGov era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  ) =>
  STS (DijkstraSUBUTXOW era)
  where
  type State (DijkstraSUBUTXOW era) = UTxOState era
  type Signal (DijkstraSUBUTXOW era) = Tx SubTx era
  type Environment (DijkstraSUBUTXOW era) = UtxoEnv era
  type BaseM (DijkstraSUBUTXOW era) = ShelleyBase
  type PredicateFailure (DijkstraSUBUTXOW era) = DijkstraSubUtxowPredFailure era
  type Event (DijkstraSUBUTXOW era) = Void

  transitionRules = [dijkstraSubUtxowTransition @era]

dijkstraSubUtxowTransition :: TransitionRule (EraRule "SUBUTXOW" era)
dijkstraSubUtxowTransition = do
  TRC (_, st, _) <- judgmentContext
  pure st
