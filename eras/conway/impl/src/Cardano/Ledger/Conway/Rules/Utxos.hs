{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Utxos (ConwayUTXOS) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent (..),
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosEvent (..),
  AlonzoUtxosPredFailure (..),
  validBegin,
  validEnd,
 )
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO,
  AlonzoScriptsNeeded,
 )
import Cardano.Ledger.Babbage.Rules (
  BabbageUTXO,
  BabbageUtxoPredFailure (..),
  babbageEvalScriptsTxInvalid,
  expectScriptsToPass,
 )
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayUTXOS)
import Cardano.Ledger.Conway.Governance (ConwayGovState (..))
import Cardano.Ledger.Conway.TxInfo ()
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.LedgerState (
  PPUPPredFailure,
  UTxOState (..),
  utxosDonationL,
 )
import Cardano.Ledger.Shelley.Rules (
  UtxoEnv (..),
  updateUTxOState,
 )
import Cardano.Ledger.UTxO (EraUTxO (..))
import Control.State.Transition.Extended
import Debug.Trace (traceEvent)
import Lens.Micro

instance
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ConwayEraTxBody era
  , ConwayEraPParams era
  , EraGov era
  , EraPlutusContext era
  , GovState era ~ ConwayGovState era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (ConwayUTXOS era) ~ Tx era
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  ) =>
  STS (ConwayUTXOS era)
  where
  type BaseM (ConwayUTXOS era) = Cardano.Ledger.BaseTypes.ShelleyBase
  type Environment (ConwayUTXOS era) = UtxoEnv era
  type State (ConwayUTXOS era) = UTxOState era
  type Signal (ConwayUTXOS era) = AlonzoTx era
  type PredicateFailure (ConwayUTXOS era) = AlonzoUtxosPredFailure era
  type Event (ConwayUTXOS era) = AlonzoUtxosEvent era

  transitionRules = [utxosTransition]

instance
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ConwayEraTxBody era
  , ConwayEraPParams era
  , EraGov era
  , EraPlutusContext era
  , Event (EraRule "UTXOS" era) ~ AlonzoUtxosEvent era
  , GovState era ~ ConwayGovState era
  , PredicateFailure (EraRule "UTXOS" era) ~ AlonzoUtxosPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (ConwayUTXOS era) ~ Tx era
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  ) =>
  Embed (ConwayUTXOS era) (BabbageUTXO era)
  where
  wrapFailed = AlonzoInBabbageUtxoPredFailure . UtxosFailure
  wrapEvent = UtxosEvent

utxosTransition ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ConwayEraTxBody era
  , EraPlutusContext era
  , EraGov era
  , GovState era ~ ConwayGovState era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (ConwayUTXOS era) ~ Tx era
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  ) =>
  TransitionRule (ConwayUTXOS era)
utxosTransition =
  judgmentContext >>= \(TRC (_, _, tx)) -> do
    case tx ^. isValidTxL of
      IsValid True -> conwayEvalScriptsTxValid
      IsValid False -> babbageEvalScriptsTxInvalid

conwayEvalScriptsTxValid ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ConwayEraTxBody era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (ConwayUTXOS era) ~ Tx era
  , STS (ConwayUTXOS era)
  ) =>
  TransitionRule (ConwayUTXOS era)
conwayEvalScriptsTxValid = do
  TRC (UtxoEnv _ pp certState _, utxos@(UTxOState utxo _ _ govState _ _), tx) <-
    judgmentContext
  let txBody = tx ^. bodyTxL

  () <- pure $! traceEvent validBegin ()
  expectScriptsToPass pp tx utxo
  () <- pure $! traceEvent validEnd ()

  utxos' <-
    updateUTxOState
      pp
      utxos
      txBody
      certState
      govState
      (tellEvent . TotalDeposits (hashAnnotated txBody))
      (\a b -> tellEvent $ TxUTxODiff a b)
  pure $! utxos' & utxosDonationL <>~ txBody ^. treasuryDonationTxBodyL
