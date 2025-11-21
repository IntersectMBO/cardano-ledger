{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Mempool (
  DijkstraMEMPOOL,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov,
  ConwayGovState,
  Proposals,
 )
import Cardano.Ledger.Conway.Rules (
  CertsEnv,
  ConwayLedgerEvent,
  ConwayLedgerPredFailure,
  GovEnv,
  GovSignal,
 )
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (
  DijkstraLEDGER,
  DijkstraMEMPOOL,
 )
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerPredFailure (..))
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..), ShelleyLedgerPredFailure, UtxoEnv)
import Control.State.Transition (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS (..),
  Signal,
  State,
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  transitionRules,
  whenFailureFreeDefault,
  (?!),
 )
import Control.State.Transition.Extended (Embed (..), trans)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import Lens.Micro ((^.))

instance
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , ConwayEraCertState era
  , EraStake era
  , EraCertState era
  , Embed (EraRule "LEDGER" era) (DijkstraMEMPOOL era)
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Eq (PredicateFailure (EraRule "CERTS" era))
  , Eq (PredicateFailure (EraRule "GOV" era))
  , Eq (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "CERTS" era))
  , Show (PredicateFailure (EraRule "GOV" era))
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Tx TopTx era ~ Signal (EraRule "LEDGER" era)
  ) =>
  STS (DijkstraMEMPOOL era)
  where
  type State (DijkstraMEMPOOL era) = LedgerState era
  type Signal (DijkstraMEMPOOL era) = Tx TopTx era
  type Environment (DijkstraMEMPOOL era) = LedgerEnv era
  type BaseM (DijkstraMEMPOOL era) = ShelleyBase
  type PredicateFailure (DijkstraMEMPOOL era) = DijkstraLedgerPredFailure era
  type Event (DijkstraMEMPOOL era) = ConwayLedgerEvent era

  transitionRules = [mempoolTransition @era]

mempoolTransition ::
  forall era.
  ( EraTx era
  , Embed (EraRule "LEDGER" era) (DijkstraMEMPOOL era)
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Tx TopTx era ~ Signal (EraRule "LEDGER" era)
  ) =>
  TransitionRule (DijkstraMEMPOOL era)
mempoolTransition = do
  TRC trc@(_ledgerEnv, ledgerState, tx) <-
    judgmentContext

  -- This rule only gets invoked on transactions within the mempool.
  -- Add checks here that sanitize undesired transactions.

  -- Detect whether the transaction is probably a duplicate
  let
    inputs = tx ^. bodyTxL . inputsTxBodyL
    UTxO utxo = ledgerState ^. utxoG
    notAllSpent = any (`Map.member` utxo) inputs
  notAllSpent
    ?! DijkstraMempoolFailure
      "All inputs are spent. Transaction has probably already been included"

  -- Continue with LEDGER rules if the transaction is not a duplicate,
  whenFailureFreeDefault ledgerState $ do
    trans @(EraRule "LEDGER" era) $ TRC trc

instance
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , BaseM (EraRule "CERTS" era) ~ ShelleyBase
  , BaseM (EraRule "GOV" era) ~ ShelleyBase
  , BaseM (EraRule "UTXOW" era) ~ ShelleyBase
  , Embed (EraRule "CERTS" era) (DijkstraLEDGER era)
  , Embed (EraRule "GOV" era) (DijkstraLEDGER era)
  , Embed (EraRule "UTXOW" era) (DijkstraLEDGER era)
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , GovState era ~ ConwayGovState era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , Signal (EraRule "UTXOW" era) ~ Tx TopTx era
  , Signal (EraRule "LEDGER" era) ~ Tx TopTx era
  , ConwayEraCertState era
  , EraRule "LEDGER" era ~ DijkstraLEDGER era
  , EraRuleFailure "LEDGER" era ~ DijkstraLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  , InjectRuleFailure "LEDGER" DijkstraLedgerPredFailure era
  ) =>
  Embed (DijkstraLEDGER era) (DijkstraMEMPOOL era)
  where
  wrapFailed = id
  wrapEvent = id
