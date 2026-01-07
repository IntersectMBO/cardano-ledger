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

module Cardano.Ledger.Dijkstra.Rules.SubUtxos (
  DijkstraSUBUTXOS,
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
import Data.Void (Void)
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

type instance EraRuleEvent "SUBUTXOS" DijkstraEra = VoidEraRule "SUBUTXOS" DijkstraEra

instance InjectRuleFailure "SUBUTXOS" DijkstraSubUtxosPredFailure DijkstraEra

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
  type Event (DijkstraSUBUTXOS era) = Void

  transitionRules = [dijkstraSubUtxosTransition @era]

dijkstraSubUtxosTransition :: TransitionRule (EraRule "SUBUTXOS" era)
dijkstraSubUtxosTransition = do
  TRC (_, st, _) <- judgmentContext
  pure st
