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
  MEMPOOL,
) where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (
  LEDGER,
  MEMPOOL,
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
import qualified Cardano.Ledger.Shelley.Rules as Shelley
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
  , Embed (EraRule "LEDGER" era) (MEMPOOL era)
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Eq (PredicateFailure (EraRule "CERTS" era))
  , Eq (PredicateFailure (EraRule "GOV" era))
  , Eq (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "CERTS" era))
  , Show (PredicateFailure (EraRule "GOV" era))
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Environment (EraRule "LEDGER" era) ~ Shelley.LedgerEnv era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  ) =>
  STS (MEMPOOL era)
  where
  type State (MEMPOOL era) = LedgerState era
  type Signal (MEMPOOL era) = StAnnTx TopTx era
  type Environment (MEMPOOL era) = Shelley.LedgerEnv era
  type BaseM (MEMPOOL era) = ShelleyBase
  type PredicateFailure (MEMPOOL era) = ConwayLedgerPredFailure era
  type Event (MEMPOOL era) = ConwayLedgerEvent era

  transitionRules = [mempoolTransition @era]

mempoolTransition ::
  forall era.
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , ConwayEraCertState era
  , Embed (EraRule "LEDGER" era) (MEMPOOL era)
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ Shelley.LedgerEnv era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  ) =>
  TransitionRule (MEMPOOL era)
mempoolTransition = do
  TRC trc@(ledgerEnv, ledgerState, stAnnTx) <-
    judgmentContext
  let tx = stAnnTx ^. txStAnnTxG

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
    let protVer = ledgerEnv ^. Shelley.ledgerPpL . ppProtocolVersionL
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
  , Embed (EraRule "CERTS" era) (LEDGER era)
  , Embed (EraRule "GOV" era) (LEDGER era)
  , Embed (EraRule "UTXOW" era) (LEDGER era)
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Environment (EraRule "UTXOW" era) ~ Shelley.UtxoEnv era
  , Environment (EraRule "LEDGER" era) ~ Shelley.LedgerEnv era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , GovState era ~ ConwayGovState era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , Signal (EraRule "UTXOW" era) ~ StAnnTx TopTx era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  , ConwayEraCertState era
  , EraRule "LEDGER" era ~ LEDGER era
  , EraRuleFailure "LEDGER" era ~ ConwayLedgerPredFailure era
  , InjectRuleFailure "LEDGER" Shelley.ShelleyLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  ) =>
  Embed (LEDGER era) (MEMPOOL era)
  where
  wrapFailed = id
  wrapEvent = id
