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

module Cardano.Ledger.Dijkstra.Rules.SubUtxos (
  DijkstraSUBUTXOS,
  DijkstraSubUtxosEvent (..),
  DijkstraSubUtxosPredFailure (..),
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.BaseTypes (
  ShelleyBase,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (ConwayUtxosEvent, ConwayUtxosPredFailure)
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
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

newtype DijkstraSubUtxosPredFailure era = DijkstraSubUtxosPredFailure (ConwayUtxosPredFailure era)
  deriving (Generic)

deriving newtype instance
  Eq (ConwayUtxosPredFailure era) =>
  Eq (DijkstraSubUtxosPredFailure era)

deriving newtype instance
  Show (ConwayUtxosPredFailure era) =>
  Show (DijkstraSubUtxosPredFailure era)

instance
  NoThunks (ConwayUtxosPredFailure era) =>
  NoThunks (DijkstraSubUtxosPredFailure era)

instance NFData (ConwayUtxosPredFailure era) => NFData (DijkstraSubUtxosPredFailure era)

deriving newtype instance
  EncCBOR (ConwayUtxosPredFailure era) =>
  EncCBOR (DijkstraSubUtxosPredFailure era)

deriving newtype instance
  (Era era, DecCBOR (ConwayUtxosPredFailure era)) =>
  DecCBOR (DijkstraSubUtxosPredFailure era)

type instance EraRuleFailure "SUBUTXOS" DijkstraEra = DijkstraSubUtxosPredFailure DijkstraEra

type instance EraRuleEvent "SUBUTXOS" DijkstraEra = DijkstraSubUtxosEvent DijkstraEra

instance InjectRuleFailure "SUBUTXOS" DijkstraSubUtxosPredFailure DijkstraEra

newtype DijkstraSubUtxosEvent era = DijkstraSubUtxosEvent (ConwayUtxosEvent era)
  deriving (Generic)

deriving instance Eq (DijkstraSubUtxosEvent era)

instance NFData (ConwayUtxosEvent era) => NFData (DijkstraSubUtxosEvent era)

instance InjectRuleEvent "SUBUTXOS" DijkstraSubUtxosEvent DijkstraEra

instance
  ( ConwayEraGov era
  , ConwayEraTxBody era
  , EraPlutusContext era
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
