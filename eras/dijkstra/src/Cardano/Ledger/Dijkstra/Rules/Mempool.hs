{-# LANGUAGE BangPatterns #-}
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
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraLEDGER, DijkstraMEMPOOL)
import Cardano.Ledger.Dijkstra.Governance (
  DijkstraEraGov,
  DijkstraGovState,
  Proposals,
  Voter (..),
  authorizedElectedHotCommitteeCredentials,
  unVotingProcedures,
 )
import Cardano.Ledger.Dijkstra.Rules.Certs (CertsEnv)
import Cardano.Ledger.Dijkstra.Rules.Gov (GovEnv, GovSignal)
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerEvent, DijkstraLedgerPredFailure (..))
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..), UtxoEnv)
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
  failOnNonEmpty,
  judgmentContext,
  transitionRules,
  whenFailureFreeDefault,
  (?!),
 )
import Control.State.Transition.Extended (Embed (..), trans)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Text as T (pack)
import Lens.Micro ((^.))

instance
  ( EraTx era
  , DijkstraEraTxBody era
  , DijkstraEraGov era
  , DijkstraEraCertState era
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
  , Tx era ~ Signal (EraRule "LEDGER" era)
  ) =>
  STS (DijkstraMEMPOOL era)
  where
  type State (DijkstraMEMPOOL era) = LedgerState era
  type Signal (DijkstraMEMPOOL era) = Tx era
  type Environment (DijkstraMEMPOOL era) = LedgerEnv era
  type BaseM (DijkstraMEMPOOL era) = ShelleyBase
  type PredicateFailure (DijkstraMEMPOOL era) = DijkstraLedgerPredFailure era
  type Event (DijkstraMEMPOOL era) = DijkstraLedgerEvent era

  transitionRules = [mempoolTransition @era]

mempoolTransition ::
  forall era.
  ( EraTx era
  , DijkstraEraTxBody era
  , DijkstraEraGov era
  , DijkstraEraCertState era
  , Embed (EraRule "LEDGER" era) (DijkstraMEMPOOL era)
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Tx era ~ Signal (EraRule "LEDGER" era)
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

  -- Skip all other checks if the transaction is probably a duplicate
  whenFailureFreeDefault ledgerState $ do
    -- Disallow votes by unelected committee members
    let
      authorizedElectedHotCreds = authorizedElectedHotCommitteeCredentials ledgerState
      collectUnelectedCommitteeVotes !unelectedHotCreds voter _ =
        case voter of
          CommitteeVoter hotCred
            | hotCred `Set.notMember` authorizedElectedHotCreds ->
                Set.insert hotCred unelectedHotCreds
          _ -> unelectedHotCreds
      unelectedCommitteeVoters =
        Map.foldlWithKey' collectUnelectedCommitteeVotes Set.empty $
          unVotingProcedures (tx ^. bodyTxL . votingProceduresTxBodyL)
      addPrefix =
        ("Unelected committee members are not allowed to cast votes: " <>)
    failOnNonEmpty unelectedCommitteeVoters $
      DijkstraMempoolFailure . addPrefix . T.pack . show . NE.toList

    -- Continue with LEDGER rules
    trans @(EraRule "LEDGER" era) $ TRC trc

instance
  ( AlonzoEraTx era
  , DijkstraEraTxBody era
  , DijkstraEraGov era
  , BaseM (EraRule "CERTS" era) ~ ShelleyBase
  , BaseM (EraRule "GOV" era) ~ ShelleyBase
  , BaseM (EraRule "UTXOW" era) ~ ShelleyBase
  , Embed (EraRule "CERTS" era) (DijkstraLEDGER era)
  , Embed (EraRule "GOV" era) (DijkstraLEDGER era)
  , Embed (EraRule "UTXOW" era) (DijkstraLEDGER era)
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , GovState era ~ DijkstraGovState era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , EraCertState era
  ) =>
  Embed (DijkstraLEDGER era) (DijkstraMEMPOOL era)
  where
  wrapFailed = id
  wrapEvent = id
