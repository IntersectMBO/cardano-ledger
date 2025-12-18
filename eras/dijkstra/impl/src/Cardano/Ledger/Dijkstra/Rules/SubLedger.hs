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

module Cardano.Ledger.Dijkstra.Rules.SubLedger (
  DijkstraSUBLEDGER,
  DijkstraSubLedgerPredFailure (..),
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
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, DijkstraSUBLEDGER)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (LedgerEnv)
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

data DijkstraSubLedgerPredFailure era = DijkstraSubLedgerPredFailure
  deriving (Show, Eq, Generic)

instance NoThunks (DijkstraSubLedgerPredFailure era)

instance NFData (DijkstraSubLedgerPredFailure era)

instance Era era => EncCBOR (DijkstraSubLedgerPredFailure era) where
  encCBOR _ = encCBOR ()

instance Typeable era => DecCBOR (DijkstraSubLedgerPredFailure era) where
  decCBOR = decCBOR @() *> pure DijkstraSubLedgerPredFailure

type instance EraRuleFailure "SUBLEDGER" DijkstraEra = DijkstraSubLedgerPredFailure DijkstraEra

type instance EraRuleEvent "SUBLEDGER" DijkstraEra = VoidEraRule "SUBLEDGER" DijkstraEra

instance InjectRuleFailure "SUBLEDGER" DijkstraSubLedgerPredFailure DijkstraEra

instance
  ( Era era
  , ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBLEDGER" era ~ DijkstraSUBLEDGER era
  ) =>
  STS (DijkstraSUBLEDGER era)
  where
  type State (DijkstraSUBLEDGER era) = LedgerState era
  type Signal (DijkstraSUBLEDGER era) = Tx SubTx era
  type Environment (DijkstraSUBLEDGER era) = LedgerEnv era
  type BaseM (DijkstraSUBLEDGER era) = ShelleyBase
  type PredicateFailure (DijkstraSUBLEDGER era) = DijkstraSubLedgerPredFailure era
  type Event (DijkstraSUBLEDGER era) = Void

  transitionRules = [dijkstraSubLedgersTransition @era]

dijkstraSubLedgersTransition :: TransitionRule (EraRule "SUBLEDGER" era)
dijkstraSubLedgersTransition = do
  TRC (_, st, _) <- judgmentContext
  pure st
