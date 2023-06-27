{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Utxos (ConwayUTXOS) where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (
  collectTwoPhaseScriptInputs,
  evalScripts,
 )
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent (..),
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosEvent (..),
  AlonzoUtxosPredFailure (..),
  TagMismatchDescription (..),
  invalidBegin,
  invalidEnd,
  scriptFailuresToPlutusDebug,
  scriptFailuresToPredicateFailure,
  validBegin,
  validEnd,
  when2Phase,
 )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.Alonzo.TxInfo (EraPlutusContext, ExtendedUTxO, ScriptResult (Fails, Passes))
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage.Collateral (collAdaBalance, collOuts)
import Cardano.Ledger.Babbage.Rules (BabbageUTXO, BabbageUtxoPredFailure (..))
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.BaseTypes (
  ShelleyBase,
  epochInfo,
  systemStart,
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayUTXOS)
import Cardano.Ledger.Conway.Governance (ConwayGovernance (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.LedgerState (
  PPUPPredFailure,
  UTxOState (..),
  keyTxRefunds,
  totalTxDeposits,
  updateStakeDistribution,
 )
import Cardano.Ledger.Shelley.Rules (
  UtxoEnv (..),
  updateUTxOState,
 )
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..))
import Cardano.Ledger.Val ((<->))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Strict as Map
import Data.MapExtras (extractKeys)
import Debug.Trace (traceEvent)
import Lens.Micro

instance
  ( AlonzoEraTx era
  , AlonzoEraPParams era
  , BabbageEraTxBody era
  , EraTxOut era
  , EraGovernance era
  , EraTxCert era
  , EraUTxO era
  , ExtendedUTxO era
  , EraPlutusContext 'PlutusV1 era
  , GovernanceState era ~ ConwayGovernance era
  , Script era ~ AlonzoScript era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Tx era ~ AlonzoTx era
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  ) =>
  STS (ConwayUTXOS era)
  where
  type BaseM (ConwayUTXOS era) = ShelleyBase
  type Environment (ConwayUTXOS era) = UtxoEnv era
  type State (ConwayUTXOS era) = UTxOState era
  type Signal (ConwayUTXOS era) = AlonzoTx era
  type PredicateFailure (ConwayUTXOS era) = AlonzoUtxosPredFailure era
  type Event (ConwayUTXOS era) = AlonzoUtxosEvent era

  transitionRules = [utxosTransition]

instance
  ( AlonzoEraTx era
  , BabbageEraTxBody era
  , EraGovernance era
  , EraPlutusContext 'PlutusV1 era
  , EraTxOut era
  , EraTxCert era
  , EraUTxO era
  , ExtendedUTxO era
  , Event (EraRule "UTXOS" era) ~ AlonzoUtxosEvent era
  , GovernanceState era ~ ConwayGovernance era
  , PredicateFailure (EraRule "UTXOS" era) ~ AlonzoUtxosPredFailure era
  , Script era ~ AlonzoScript era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Tx era ~ AlonzoTx era
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
  , BabbageEraTxBody era
  , EraGovernance era
  , EraPlutusContext 'PlutusV1 era
  , EraUTxO era
  , ExtendedUTxO era
  , GovernanceState era ~ ConwayGovernance era
  , Script era ~ AlonzoScript era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Tx era ~ AlonzoTx era
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  ) =>
  TransitionRule (ConwayUTXOS era)
utxosTransition =
  judgmentContext >>= \(TRC (_, _, tx)) -> do
    case tx ^. isValidTxL of
      IsValid True -> scriptsYes
      IsValid False -> scriptsNo

scriptsYes ::
  forall era.
  ( AlonzoEraTx era
  , EraUTxO era
  , EraPlutusContext 'PlutusV1 era
  , ExtendedUTxO era
  , Script era ~ AlonzoScript era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Tx era ~ AlonzoTx era
  , STS (ConwayUTXOS era)
  ) =>
  TransitionRule (ConwayUTXOS era)
scriptsYes = do
  TRC (UtxoEnv _ pp dpstate _, u@(UTxOState utxo _ _ pup _), tx) <-
    judgmentContext
  let txBody = body tx
      {- refunded := keyRefunds pp txb -}
      refunded = keyTxRefunds pp dpstate txBody
      {- depositChange := (totalDeposits pp poolParams txcerts txb) − refunded -}
      protVer = pp ^. ppProtocolVersionL
      depositChange = totalTxDeposits pp dpstate txBody <-> refunded
  tellEvent $ TotalDeposits (hashAnnotated txBody) depositChange
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  let !_ = traceEvent validBegin ()

  -- {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
  case collectTwoPhaseScriptInputs ei sysSt pp tx utxo of
    Right sLst ->
      {- isValid tx = evalScripts tx sLst = True -}
      whenFailureFree $
        when2Phase $
          case evalScripts @era protVer tx sLst of
            Fails _ fs ->
              failBecause $
                ValidationTagMismatch
                  (tx ^. isValidTxL)
                  (FailedUnexpectedly (scriptFailuresToPredicateFailure protVer fs))
            Passes ps -> mapM_ (tellEvent . SuccessfulPlutusScriptsEvent) (nonEmpty ps)
    Left info -> failBecause (CollectErrors info)

  let !_ = traceEvent validEnd ()

  pure $! updateUTxOState pp u txBody depositChange pup

scriptsNo ::
  forall era.
  ( AlonzoEraTx era
  , BabbageEraTxBody era
  , EraPlutusContext 'PlutusV1 era
  , EraUTxO era
  , ExtendedUTxO era
  , Script era ~ AlonzoScript era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , STS (ConwayUTXOS era)
  , Tx era ~ AlonzoTx era
  ) =>
  TransitionRule (ConwayUTXOS era)
scriptsNo = do
  TRC (UtxoEnv _ pp _ _, us@(UTxOState utxo _ fees _ _), tx) <- judgmentContext
  {- txb := txbody tx -}
  let txBody = tx ^. bodyTxL
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  () <- pure $! traceEvent invalidBegin ()

  case collectTwoPhaseScriptInputs ei sysSt pp tx utxo of
    Right sLst ->
      {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
      {- isValid tx = evalScripts tx sLst = False -}
      whenFailureFree $
        when2Phase $ case evalScripts @era (pp ^. ppProtocolVersionL) tx sLst of
          Passes _ -> failBecause $ ValidationTagMismatch (tx ^. isValidTxL) PassedUnexpectedly
          Fails ps fs -> do
            mapM_ (tellEvent . SuccessfulPlutusScriptsEvent) (nonEmpty ps)
            tellEvent (FailedPlutusScriptsEvent (scriptFailuresToPlutusDebug fs))
    Left info -> failBecause (CollectErrors info)

  () <- pure $! traceEvent invalidEnd ()

  {- utxoKeep = txBody ^. collateralInputsTxBodyL ⋪ utxo -}
  {- utxoDel  = txBody ^. collateralInputsTxBodyL ◁ utxo -}
  let !(utxoKeep, utxoDel) = extractKeys (unUTxO utxo) (txBody ^. collateralInputsTxBodyL)
      UTxO collouts = collOuts txBody
      collateralFees = collAdaBalance txBody utxoDel -- NEW to Babbage
  pure $!
    us {- (collInputs txb ⋪ utxo) ∪ collouts tx -}
      { utxosUtxo = UTxO (Map.union utxoKeep collouts) -- NEW to Babbage
      {- fees + collateralFees -}
      , utxosFees = fees <> collateralFees -- NEW to Babbage
      , utxosStakeDistr = updateStakeDistribution pp (utxosStakeDistr us) (UTxO utxoDel) (UTxO collouts)
      }
