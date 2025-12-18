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
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBGOV,
  DijkstraSUBLEDGER,
  DijkstraSUBLEDGERS,
 )
import Cardano.Ledger.Dijkstra.Rules.SubLedger (DijkstraSubLedgerPredFailure (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (LedgerEnv)
import Cardano.Ledger.TxIn (TxId)
import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Control.State.Transition.Extended
import Data.OMap.Strict (OMap)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

newtype DijkstraSubLedgersPredFailure era
  = SubLedgerFailure (PredicateFailure (EraRule "SUBLEDGER" era))
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "SUBLEDGER" era)) => Eq (DijkstraSubLedgersPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "SUBLEDGER" era)) => Show (DijkstraSubLedgersPredFailure era)

instance
  NoThunks (PredicateFailure (EraRule "SUBLEDGER" era)) =>
  NoThunks (DijkstraSubLedgersPredFailure era)

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

type instance EraRuleEvent "SUBLEDGERS" DijkstraEra = VoidEraRule "SUBLEDGERS" DijkstraEra

instance InjectRuleFailure "SUBLEDGERS" DijkstraSubLedgersPredFailure DijkstraEra

instance InjectRuleFailure "SUBLEDGERS" DijkstraSubLedgerPredFailure DijkstraEra where
  injectFailure = SubLedgerFailure

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBLEDGERS" era ~ DijkstraSUBLEDGERS era
  , EraRule "SUBLEDGER" era ~ DijkstraSUBLEDGER era
  , Embed (EraRule "SUBLEDGER" era) (DijkstraSUBLEDGERS era)
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
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBLEDGER" era ~ DijkstraSUBLEDGER era
  , EraRule "SUBGOV" era ~ DijkstraSUBGOV era
  ) =>
  Embed (DijkstraSUBLEDGER era) (DijkstraSUBLEDGERS era)
  where
  wrapFailed = SubLedgerFailure
  wrapEvent = absurd
