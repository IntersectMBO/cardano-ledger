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

module Cardano.Ledger.Dijkstra.Rules.SubLedgers (
  DijkstraSUBLEDGERS,
  DijkstraSubLedgersPredFailure (..),
  DijkstraSubLedgersEvent (..),
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
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBLEDGER,
  DijkstraSUBLEDGERS,
 )
import Cardano.Ledger.Dijkstra.Rules.SubLedger (
  DijkstraSubLedgerEvent,
  DijkstraSubLedgerPredFailure (..),
  SubLedgerEnv (..),
 )
import Cardano.Ledger.Dijkstra.Rules.SubPool (DijkstraSubPoolEvent, DijkstraSubPoolPredFailure (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (
  PoolEvent,
  ShelleyPoolPredFailure,
 )
import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Control.State.Transition.Extended
import GHC.Generics (Generic)

newtype DijkstraSubLedgersPredFailure era
  = SubLedgerFailure (PredicateFailure (EraRule "SUBLEDGER" era))
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "SUBLEDGER" era)) => Eq (DijkstraSubLedgersPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "SUBLEDGER" era)) => Show (DijkstraSubLedgersPredFailure era)

instance NFData (PredicateFailure (EraRule "SUBLEDGER" era)) => NFData (DijkstraSubLedgersPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "SUBLEDGER" era))
  ) =>
  EncCBOR (DijkstraSubLedgersPredFailure era)
  where
  encCBOR (SubLedgerFailure e) = encCBOR e

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "SUBLEDGER" era))
  ) =>
  DecCBOR (DijkstraSubLedgersPredFailure era)
  where
  decCBOR = SubLedgerFailure <$> decCBOR

type instance EraRuleFailure "SUBLEDGERS" DijkstraEra = DijkstraSubLedgersPredFailure DijkstraEra

type instance EraRuleEvent "SUBLEDGERS" DijkstraEra = DijkstraSubLedgersEvent DijkstraEra

instance InjectRuleFailure "SUBLEDGERS" DijkstraSubLedgersPredFailure DijkstraEra

instance InjectRuleFailure "SUBLEDGERS" DijkstraSubLedgerPredFailure DijkstraEra where
  injectFailure = SubLedgerFailure

instance InjectRuleEvent "SUBLEDGERS" DijkstraSubLedgersEvent DijkstraEra

newtype DijkstraSubLedgersEvent era
  = SubLedgerEvent (Event (EraRule "SUBLEDGER" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "SUBLEDGER" era)) => Eq (DijkstraSubLedgersEvent era)

instance NFData (Event (EraRule "SUBLEDGER" era)) => NFData (DijkstraSubLedgersEvent era)

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraPlutusContext era
  , EraRule "SUBLEDGERS" era ~ DijkstraSUBLEDGERS era
  , EraRule "SUBLEDGER" era ~ DijkstraSUBLEDGER era
  , Embed (EraRule "SUBLEDGER" era) (DijkstraSUBLEDGERS era)
  , InjectRuleEvent "SUBPOOL" PoolEvent era
  , InjectRuleEvent "SUBPOOL" DijkstraSubPoolEvent era
  , InjectRuleFailure "SUBPOOL" ShelleyPoolPredFailure era
  , InjectRuleFailure "SUBPOOL" DijkstraSubPoolPredFailure era
  ) =>
  STS (DijkstraSUBLEDGERS era)
  where
  type State (DijkstraSUBLEDGERS era) = LedgerState era
  type Signal (DijkstraSUBLEDGERS era) = [StAnnTx SubTx era]
  type Environment (DijkstraSUBLEDGERS era) = SubLedgerEnv era
  type BaseM (DijkstraSUBLEDGERS era) = ShelleyBase
  type PredicateFailure (DijkstraSUBLEDGERS era) = DijkstraSubLedgersPredFailure era
  type Event (DijkstraSUBLEDGERS era) = DijkstraSubLedgersEvent era

  transitionRules = [dijkstraSubLedgersTransition @era]

dijkstraSubLedgersTransition ::
  forall era.
  ( EraRule "SUBLEDGERS" era ~ DijkstraSUBLEDGERS era
  , EraRule "SUBLEDGER" era ~ DijkstraSUBLEDGER era
  , Embed (EraRule "SUBLEDGER" era) (DijkstraSUBLEDGERS era)
  ) =>
  TransitionRule (EraRule "SUBLEDGERS" era)
dijkstraSubLedgersTransition = do
  TRC (env, ledgerState, subTxs) <- judgmentContext
  foldM
    ( \ls subTx ->
        trans @(EraRule "SUBLEDGER" era) $ TRC (env, ls, subTx)
    )
    ledgerState
    subTxs

instance
  ( STS (DijkstraSUBLEDGER era)
  , PredicateFailure (EraRule "SUBLEDGER" era) ~ DijkstraSubLedgerPredFailure era
  , Event (EraRule "SUBLEDGER" era) ~ DijkstraSubLedgerEvent era
  ) =>
  Embed (DijkstraSUBLEDGER era) (DijkstraSUBLEDGERS era)
  where
  wrapFailed = SubLedgerFailure
  wrapEvent = SubLedgerEvent
