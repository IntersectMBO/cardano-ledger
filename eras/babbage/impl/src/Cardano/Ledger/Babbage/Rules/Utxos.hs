{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Utxos (
  BabbageUTXOS,
  utxosTransition,
) where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (
  collectTwoPhaseScriptInputs,
  evalScripts,
 )
import Cardano.Ledger.Alonzo.Rules (
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
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageUTXOS)
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.BaseTypes (ShelleyBase, epochInfo, strictMaybeToMaybe, systemStart)
import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.Shelley.Delegation (ShelleyDCert)
import Cardano.Ledger.Shelley.LedgerState (
  PPUPPredFailure,
  UTxOState (..),
  keyTxRefunds,
  totalTxDeposits,
  updateStakeDistribution,
 )
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules (
  PpupEnv (..),
  ShelleyPPUP,
  ShelleyPpupPredFailure,
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

-- =====================================================

instance
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraPParams era
  , BabbageEraTxBody era
  , ExtendedUTxO era
  , EraUTxO era
  , EraPlutusContext 'PlutusV1 era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Tx era ~ AlonzoTx era
  , DCert era ~ ShelleyDCert era
  , Script era ~ AlonzoScript era
  , EraGovernance era
  , GovernanceState era ~ ShelleyPPUPState era
  , Embed (EraRule "PPUP" era) (BabbageUTXOS era)
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ Maybe (Update era)
  , State (EraRule "PPUP" era) ~ ShelleyPPUPState era
  , EncCBOR (PPUPPredFailure era) -- Serializing the PredicateFailure
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  ) =>
  STS (BabbageUTXOS era)
  where
  type BaseM (BabbageUTXOS era) = ShelleyBase
  type Environment (BabbageUTXOS era) = UtxoEnv era
  type State (BabbageUTXOS era) = UTxOState era
  type Signal (BabbageUTXOS era) = AlonzoTx era
  type PredicateFailure (BabbageUTXOS era) = AlonzoUtxosPredFailure era
  type Event (BabbageUTXOS era) = AlonzoUtxosEvent era
  transitionRules = [utxosTransition]

instance
  ( Era era
  , STS (ShelleyPPUP era)
  , PPUPPredFailure era ~ ShelleyPpupPredFailure era
  , Event (EraRule "PPUP" era) ~ Event (ShelleyPPUP era)
  ) =>
  Embed (ShelleyPPUP era) (BabbageUTXOS era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = AlonzoPpupToUtxosEvent

utxosTransition ::
  forall era.
  ( AlonzoEraTx era
  , ExtendedUTxO era
  , BabbageEraTxBody era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Tx era ~ AlonzoTx era
  , DCert era ~ ShelleyDCert era
  , Script era ~ AlonzoScript era
  , EraGovernance era
  , GovernanceState era ~ ShelleyPPUPState era
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ Maybe (Update era)
  , Embed (EraRule "PPUP" era) (BabbageUTXOS era)
  , State (EraRule "PPUP" era) ~ ShelleyPPUPState era
  , EncCBOR (PPUPPredFailure era)
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  , EraPlutusContext 'PlutusV1 era
  ) =>
  TransitionRule (BabbageUTXOS era)
utxosTransition =
  judgmentContext >>= \(TRC (_, _, tx)) -> do
    case tx ^. isValidTxL of
      IsValid True -> scriptsYes
      IsValid False -> scriptsNo

-- ===================================================================

scriptsYes ::
  forall era.
  ( ExtendedUTxO era
  , AlonzoEraTx era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Tx era ~ AlonzoTx era
  , Script era ~ AlonzoScript era
  , STS (BabbageUTXOS era)
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ Maybe (Update era)
  , Embed (EraRule "PPUP" era) (BabbageUTXOS era)
  , GovernanceState era ~ ShelleyPPUPState era
  , State (EraRule "PPUP" era) ~ ShelleyPPUPState era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  TransitionRule (BabbageUTXOS era)
scriptsYes = do
  TRC (UtxoEnv slot pp dpstate genDelegs, u@(UTxOState utxo _ _ pup _), tx) <-
    judgmentContext
  let txBody = body tx
      {- refunded := keyRefunds pp txb -}
      refunded = keyTxRefunds pp dpstate txBody
      {- depositChange := (totalDeposits pp poolParams txcerts txb) − refunded -}
      protVer = pp ^. ppProtocolVersionL
      depositChange = totalTxDeposits pp dpstate txBody <-> refunded
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  -- We intentionally run the PPUP rule before evaluating any Plutus scripts.
  -- We do not want to waste computation running plutus scripts if the
  -- transaction will fail due to `PPUP`
  ppup' <-
    trans @(EraRule "PPUP" era) $
      TRC
        (PPUPEnv slot pp genDelegs, pup, strictMaybeToMaybe $ txBody ^. updateTxBodyG)

  let !_ = traceEvent validBegin ()

  {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
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

  pure $! updateUTxOState pp u txBody depositChange ppup'

scriptsNo ::
  forall era.
  ( AlonzoEraTx era
  , ExtendedUTxO era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , STS (BabbageUTXOS era)
  , BabbageEraTxBody era
  , Tx era ~ AlonzoTx era
  , Script era ~ AlonzoScript era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  TransitionRule (BabbageUTXOS era)
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
