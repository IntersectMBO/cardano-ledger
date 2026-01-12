{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

module Cardano.Ledger.Dijkstra.Rules.SubUtxos (
  DijkstraSUBUTXOS,
  DijkstraSubUtxosEvent (..),
  DijkstraSubUtxosPredFailure (..),
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
import Cardano.Ledger.Conway.Rules (ConwayUtxosEvent)
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBUTXOS,
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
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data DijkstraSubUtxosPredFailure era = DijkstraSubUtxosPredFailure
  deriving (Show, Eq, Generic)

instance NoThunks (DijkstraSubUtxosPredFailure era)

instance NFData (DijkstraSubUtxosPredFailure era)

instance Era era => EncCBOR (DijkstraSubUtxosPredFailure era) where
  encCBOR _ = encCBOR ()

instance Typeable era => DecCBOR (DijkstraSubUtxosPredFailure era) where
  decCBOR = decCBOR @() *> pure DijkstraSubUtxosPredFailure

type instance EraRuleFailure "SUBUTXOS" DijkstraEra = DijkstraSubUtxosPredFailure DijkstraEra

type instance EraRuleEvent "SUBUTXOS" DijkstraEra = DijkstraSubUtxosEvent DijkstraEra

instance InjectRuleFailure "SUBUTXOS" DijkstraSubUtxosPredFailure DijkstraEra

newtype DijkstraSubUtxosEvent era = DijkstraSubUtxosEvent (ConwayUtxosEvent era)
  deriving (Generic)

deriving instance Eq (ConwayUtxosEvent era) => Eq (DijkstraSubUtxosEvent era)

instance NFData (ConwayUtxosEvent era) => NFData (DijkstraSubUtxosEvent era)

instance InjectRuleEvent "SUBUTXOS" DijkstraSubUtxosEvent DijkstraEra

instance
  ( ConwayEraGov era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  ) =>
  STS (DijkstraSUBUTXOS era)
  where
  type State (DijkstraSUBUTXOS era) = UTxOState era
  type Signal (DijkstraSUBUTXOS era) = Tx SubTx era
  type Environment (DijkstraSUBUTXOS era) = UtxoEnv era
  type BaseM (DijkstraSUBUTXOS era) = ShelleyBase
  type PredicateFailure (DijkstraSUBUTXOS era) = DijkstraSubUtxosPredFailure era
  type Event (DijkstraSUBUTXOS era) = DijkstraSubUtxosEvent era

  transitionRules = [dijkstraSubUtxosTransition @era]

dijkstraSubUtxosTransition :: TransitionRule (EraRule "SUBUTXOS" era)
dijkstraSubUtxosTransition = do
  TRC (_, st, _) <- judgmentContext
  pure st
