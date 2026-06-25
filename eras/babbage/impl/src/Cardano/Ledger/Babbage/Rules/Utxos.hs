{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Utxos (
  BabbageUTXOS,
  utxosTransition,
  expectScriptsToPass,
  babbageEvalScriptsTxInvalid,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  collectPlutusScriptsWithContext,
  evalPlutusScripts,
 )
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosEvent (..),
  AlonzoUtxosPredFailure (..),
  TagMismatchDescription (..),
  UtxosEnv (..),
  invalidBegin,
  invalidEnd,
  scriptFailureToFailureDescription,
  validBegin,
  validEnd,
  when2Phase,
 )
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  AlonzoScriptsNeeded,
 )
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageEra, BabbageUTXOS)
import Cardano.Ledger.Babbage.Rules.Ppup ()
import Cardano.Ledger.Babbage.State
import Cardano.Ledger.BaseTypes (
  ShelleyBase,
  StrictMaybe,
  epochInfo,
  systemStart,
 )
import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.Plutus.Evaluate (
  ScriptFailure (..),
  ScriptResult (..),
 )
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules (
  PpupEnv (..),
  PpupEvent,
  ShelleyPPUP,
  ShelleyPpupPredFailure,
 )
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.List.NonEmpty (nonEmpty)
import qualified Debug.Trace as Debug
import Lens.Micro

type instance EraRuleFailure "UTXOS" BabbageEra = AlonzoUtxosPredFailure BabbageEra

type instance EraRuleEvent "UTXOS" BabbageEra = AlonzoUtxosEvent BabbageEra

instance InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure BabbageEra

instance InjectRuleEvent "UTXOS" AlonzoUtxosEvent BabbageEra

instance InjectRuleFailure "UTXOS" ShelleyPpupPredFailure BabbageEra where
  injectFailure = UpdateFailure

-- =====================================================

instance
  ( AlonzoEraTx era
  , AlonzoEraPParams era
  , ShelleyEraTxBody era
  , BabbageEraTxBody era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , EraStake era
  , EraCertState era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , EraGov era
  , GovState era ~ ShelleyGovState era
  , Embed (EraRule "PPUP" era) (BabbageUTXOS era)
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ StrictMaybe (Update era)
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , Signal (BabbageUTXOS era) ~ Tx TopTx era
  , EncCBOR (EraRuleFailure "PPUP" era)
  , Eq (EraRuleFailure "PPUP" era)
  , Show (EraRuleFailure "PPUP" era)
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , EraRule "UTXOS" era ~ BabbageUTXOS era
  ) =>
  STS (BabbageUTXOS era)
  where
  type BaseM (BabbageUTXOS era) = ShelleyBase
  type Environment (BabbageUTXOS era) = UtxosEnv era
  type State (BabbageUTXOS era) = ShelleyGovState era
  type Signal (BabbageUTXOS era) = Tx TopTx era
  type PredicateFailure (BabbageUTXOS era) = AlonzoUtxosPredFailure era
  type Event (BabbageUTXOS era) = AlonzoUtxosEvent era
  transitionRules = [utxosTransition]

instance
  ( Era era
  , STS (ShelleyPPUP era)
  , EraRuleFailure "PPUP" era ~ ShelleyPpupPredFailure era
  , EraRuleEvent "PPUP" era ~ PpupEvent era
  ) =>
  Embed (ShelleyPPUP era) (BabbageUTXOS era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = AlonzoPpupToUtxosEvent

utxosTransition ::
  forall era.
  ( AlonzoEraTx era
  , ShelleyEraTxBody era
  , BabbageEraTxBody era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , EraCertState era
  , EraStake era
  , EraGov era
  , GovState era ~ ShelleyGovState era
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ StrictMaybe (Update era)
  , Embed (EraRule "PPUP" era) (BabbageUTXOS era)
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , EncCBOR (EraRuleFailure "PPUP" era)
  , Eq (EraRuleFailure "PPUP" era)
  , Show (EraRuleFailure "PPUP" era)
  , EraPlutusContext era
  , EraRule "UTXOS" era ~ BabbageUTXOS era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  ) =>
  TransitionRule (BabbageUTXOS era)
utxosTransition =
  judgmentContext >>= \(TRC (UtxosEnv _ pp _ utxo, pup, tx)) -> do
    case tx ^. isValidTxL of
      IsValid True -> babbageEvalScriptsTxValid
      IsValid False -> do
        babbageEvalScriptsTxInvalid @era pp tx utxo
        pure pup

-- ===================================================================

expectScriptsToPass ::
  forall era.
  ( AlonzoEraTx era
  , EraPlutusContext era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , STS (EraRule "UTXOS" era)
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  ) =>
  PParams era ->
  Tx TopTx era ->
  UTxO era ->
  Rule (EraRule "UTXOS" era) 'Transition ()
expectScriptsToPass pp tx utxo = do
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo
  {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
  case collectPlutusScriptsWithContext ei sysSt pp tx utxo of
    Right sLst -> do
      {- isValid tx = evalScripts tx sLst = True -}
      whenFailureFree $
        when2Phase $ case evalPlutusScripts sLst of
          Fails _ fs ->
            failBecause $
              injectFailure $
                ValidationTagMismatch
                  (tx ^. isValidTxL)
                  (FailedUnexpectedly (scriptFailureToFailureDescription <$> fs))
          Passes ps -> mapM_ (tellEvent . injectEvent . SuccessfulPlutusScriptsEvent) (nonEmpty ps)
    Left info -> failBecause (injectFailure $ CollectErrors info)

babbageEvalScriptsTxValid ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ShelleyEraTxBody era
  , EraCertState era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , STS (BabbageUTXOS era)
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ StrictMaybe (Update era)
  , Embed (EraRule "PPUP" era) (BabbageUTXOS era)
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , EraPlutusContext era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , EraRule "UTXOS" era ~ BabbageUTXOS era
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  ) =>
  TransitionRule (BabbageUTXOS era)
babbageEvalScriptsTxValid = do
  TRC (UtxosEnv slot pp certState utxo, pup, tx) <-
    judgmentContext
  let txBody = tx ^. bodyTxL
      genDelegs = certState ^. certDStateL . dsGenDelegsL

  -- We intentionally run the PPUP rule before evaluating any Plutus scripts.
  -- We do not want to waste computation running plutus scripts if the
  -- transaction will fail due to `PPUP`
  updatedGovState <-
    trans @(EraRule "PPUP" era) $
      TRC (PPUPEnv slot pp genDelegs, pup, txBody ^. updateTxBodyL)

  () <- pure $! Debug.traceEvent validBegin ()
  expectScriptsToPass pp tx utxo
  () <- pure $! Debug.traceEvent validEnd ()

  pure updatedGovState

babbageEvalScriptsTxInvalid ::
  forall era.
  ( AlonzoEraTx era
  , EraPlutusContext era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , STS (EraRule "UTXOS" era)
  ) =>
  PParams era ->
  Tx TopTx era ->
  UTxO era ->
  Rule (EraRule "UTXOS" era) 'Transition ()
babbageEvalScriptsTxInvalid pp tx utxo = do
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo
  () <- pure $! Debug.traceEvent invalidBegin ()
  case collectPlutusScriptsWithContext @era ei sysSt pp tx utxo of
    Right sLst ->
      {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
      {- isValid tx = evalScripts tx sLst = False -}
      whenFailureFree $
        when2Phase $ case evalPlutusScripts sLst of
          Passes _ ->
            failBecause $
              injectFailure $
                ValidationTagMismatch (tx ^. isValidTxL) PassedUnexpectedly
          Fails ps fs -> do
            mapM_
              (tellEvent . injectEvent . SuccessfulPlutusScriptsEvent @era)
              (nonEmpty ps)
            tellEvent . injectEvent $
              FailedPlutusScriptsEvent (scriptFailurePlutus <$> fs)
    Left info -> failBecause (injectFailure (CollectErrors info))
  pure $! Debug.traceEvent invalidEnd ()
