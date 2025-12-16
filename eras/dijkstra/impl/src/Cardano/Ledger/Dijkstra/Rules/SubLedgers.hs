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

module Cardano.Ledger.Dijkstra.Rules.SubLedgers (
  DijkstraSUBLEDGERS,
  DijkstraSubLedgersPredFailure (..),
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
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, DijkstraSUBLEDGERS)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (LedgerEnv)
import Cardano.Ledger.TxIn (TxId)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.OMap.Strict (OMap)
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data DijkstraSubLedgersPredFailure era = DijkstraSubLedgersPredFailure
  deriving (Show, Eq, Generic)

instance NoThunks (DijkstraSubLedgersPredFailure era)

instance NFData (DijkstraSubLedgersPredFailure era)

instance Era era => EncCBOR (DijkstraSubLedgersPredFailure era) where
  encCBOR _ = encCBOR ()

instance Typeable era => DecCBOR (DijkstraSubLedgersPredFailure era) where
  decCBOR = decCBOR @() *> pure DijkstraSubLedgersPredFailure

type instance EraRuleFailure "SUBLEDGERS" DijkstraEra = DijkstraSubLedgersPredFailure DijkstraEra

type instance EraRuleEvent "SUBLEDGERS" DijkstraEra = VoidEraRule "SUBLEDGERS" DijkstraEra

instance InjectRuleFailure "SUBLEDGERS" DijkstraSubLedgersPredFailure DijkstraEra

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBLEDGERS" era ~ DijkstraSUBLEDGERS era
  ) =>
  STS (DijkstraSUBLEDGERS era)
  where
  type State (DijkstraSUBLEDGERS era) = LedgerState era
  type Signal (DijkstraSUBLEDGERS era) = OMap TxId (Tx SubTx era)
  type Environment (DijkstraSUBLEDGERS era) = LedgerEnv era
  type BaseM (DijkstraSUBLEDGERS era) = ShelleyBase
  type PredicateFailure (DijkstraSUBLEDGERS era) = DijkstraSubLedgersPredFailure era
  type Event (DijkstraSUBLEDGERS era) = Void

  transitionRules = [dijkstraSubLedgersTransition @era]

dijkstraSubLedgersTransition :: TransitionRule (EraRule "SUBLEDGERS" era)
dijkstraSubLedgersTransition = do
  TRC (_, st, _) <- judgmentContext
  pure st
