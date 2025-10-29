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

module Cardano.Ledger.Conway.Rules.Mempool (
  ConwayMEMPOOL,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (
  ConwayLEDGER,
  ConwayMEMPOOL,
  hardforkConwayDisallowUnelectedCommitteeFromVoting,
 )
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov,
  ConwayGovState,
  Proposals,
  committeeGovStateL,
 )
import Cardano.Ledger.Conway.Rules.Certs (CertsEnv)
import Cardano.Ledger.Conway.Rules.Gov (GovEnv, GovSignal, unelectedCommitteeVoters)
import Cardano.Ledger.Conway.Rules.Ledger (ConwayLedgerEvent, ConwayLedgerPredFailure (..))
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..), ShelleyLedgerPredFailure, UtxoEnv, ledgerPpL)
import Control.Monad (unless)
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
import Data.Text as T (pack)
import Lens.Micro ((^.))

instance
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , ConwayEraCertState era
  , EraStake era
  , EraCertState era
  , Embed (EraRule "LEDGER" era) (ConwayMEMPOOL era)
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
  STS (ConwayMEMPOOL era)
  where
  type State (ConwayMEMPOOL era) = LedgerState era
  type Signal (ConwayMEMPOOL era) = Tx TopTx era
  type Environment (ConwayMEMPOOL era) = LedgerEnv era
  type BaseM (ConwayMEMPOOL era) = ShelleyBase
  type PredicateFailure (ConwayMEMPOOL era) = ConwayLedgerPredFailure era
  type Event (ConwayMEMPOOL era) = ConwayLedgerEvent era

  transitionRules = [mempoolTransition @era]

mempoolTransition ::
  forall era.
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , ConwayEraCertState era
  , Embed (EraRule "LEDGER" era) (ConwayMEMPOOL era)
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Tx TopTx era ~ Signal (EraRule "LEDGER" era)
  ) =>
  TransitionRule (ConwayMEMPOOL era)
mempoolTransition = do
  TRC trc@(ledgerEnv, ledgerState, tx) <-
    judgmentContext

  -- This rule only gets invoked on transactions within the mempool.
  -- Add checks here that sanitize undesired transactions.

  -- Detect whether the transaction is probably a duplicate
  let
    inputs = tx ^. bodyTxL . inputsTxBodyL
    UTxO utxo = ledgerState ^. utxoG
    notAllSpent = any (`Map.member` utxo) inputs
  notAllSpent
    ?! ConwayMempoolFailure
      "All inputs are spent. Transaction has probably already been included"

  -- Skip all other checks if the transaction is probably a duplicate
  whenFailureFreeDefault ledgerState $ do
    let protVer = ledgerEnv ^. ledgerPpL . ppProtocolVersionL
    unless (hardforkConwayDisallowUnelectedCommitteeFromVoting protVer) $
      -- This check can completely be removed once mainnet switches to protocol
      -- version 11, since the same check has been implemented in the GOV rule.
      --
      -- Disallow votes by unelected committee members
      let addPrefix = ("Unelected committee members are not allowed to cast votes: " <>)
       in failOnNonEmpty
            ( unelectedCommitteeVoters
                (ledgerState ^. lsUTxOStateL . utxosGovStateL . committeeGovStateL)
                (ledgerState ^. lsCertStateL . certVStateL . vsCommitteeStateL)
                (tx ^. bodyTxL . votingProceduresTxBodyL)
            )
            (ConwayMempoolFailure . addPrefix . T.pack . show . NE.toList)

    -- Continue with LEDGER rules
    trans @(EraRule "LEDGER" era) $ TRC trc

instance
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , BaseM (EraRule "CERTS" era) ~ ShelleyBase
  , BaseM (EraRule "GOV" era) ~ ShelleyBase
  , BaseM (EraRule "UTXOW" era) ~ ShelleyBase
  , Embed (EraRule "CERTS" era) (ConwayLEDGER era)
  , Embed (EraRule "GOV" era) (ConwayLEDGER era)
  , Embed (EraRule "UTXOW" era) (ConwayLEDGER era)
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
  , EraRule "LEDGER" era ~ ConwayLEDGER era
  , EraRuleFailure "LEDGER" era ~ ConwayLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure era
  ) =>
  Embed (ConwayLEDGER era) (ConwayMEMPOOL era)
  where
  wrapFailed = id
  wrapEvent = id
