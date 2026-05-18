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
import Cardano.Ledger.Alonzo.Plutus.Evaluate (evalPlutusScripts)
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.UTxO (
  AlonzoEraUTxO (..),
  AlonzoScriptsNeeded,
 )
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageEra, BabbageUTXOS)
import Cardano.Ledger.Babbage.Rules.Ppup ()
import Cardano.Ledger.Babbage.State
import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe)
import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.Plutus.Evaluate (
  ScriptFailure (..),
  ScriptResult (..),
 )
import Cardano.Ledger.Shelley.PParams (Update)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.Monad (forM_)
import Control.State.Transition.Extended
import Data.List.NonEmpty (nonEmpty)
import qualified Debug.Trace as Debug
import Lens.Micro

type instance EraRuleFailure "UTXOS" BabbageEra = Alonzo.AlonzoUtxosPredFailure BabbageEra

type instance EraRuleEvent "UTXOS" BabbageEra = Alonzo.AlonzoUtxosEvent BabbageEra

instance InjectRuleFailure "UTXOS" Alonzo.AlonzoUtxosPredFailure BabbageEra

instance InjectRuleEvent "UTXOS" Alonzo.AlonzoUtxosEvent BabbageEra

instance InjectRuleFailure "UTXOS" Shelley.ShelleyPpupPredFailure BabbageEra where
  injectFailure = Alonzo.UpdateFailure

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
  , Environment (EraRule "PPUP" era) ~ Shelley.PpupEnv era
  , Signal (EraRule "PPUP" era) ~ StrictMaybe (Update era)
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , Signal (BabbageUTXOS era) ~ StAnnTx TopTx era
  , EncCBOR (EraRuleFailure "PPUP" era)
  , Eq (EraRuleFailure "PPUP" era)
  , Show (EraRuleFailure "PPUP" era)
  , InjectRuleFailure "UTXOS" Alonzo.AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" Alonzo.AlonzoUtxosEvent era
  , EraRule "UTXOS" era ~ BabbageUTXOS era
  ) =>
  STS (BabbageUTXOS era)
  where
  type BaseM (BabbageUTXOS era) = ShelleyBase
  type Environment (BabbageUTXOS era) = Alonzo.UtxosEnv era
  type State (BabbageUTXOS era) = ShelleyGovState era
  type Signal (BabbageUTXOS era) = StAnnTx TopTx era
  type PredicateFailure (BabbageUTXOS era) = Alonzo.AlonzoUtxosPredFailure era
  type Event (BabbageUTXOS era) = Alonzo.AlonzoUtxosEvent era
  transitionRules = [utxosTransition]

instance
  ( Era era
  , STS (Shelley.ShelleyPPUP era)
  , EraRuleFailure "PPUP" era ~ Shelley.ShelleyPpupPredFailure era
  , EraRuleEvent "PPUP" era ~ Shelley.PpupEvent era
  ) =>
  Embed (Shelley.ShelleyPPUP era) (BabbageUTXOS era)
  where
  wrapFailed = Alonzo.UpdateFailure
  wrapEvent = Alonzo.AlonzoPpupToUtxosEvent

utxosTransition ::
  forall era.
  ( AlonzoEraTx era
  , ShelleyEraTxBody era
  , AlonzoEraUTxO era
  , EraCertState era
  , Environment (EraRule "PPUP" era) ~ Shelley.PpupEnv era
  , Signal (EraRule "PPUP" era) ~ StrictMaybe (Update era)
  , Embed (EraRule "PPUP" era) (BabbageUTXOS era)
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , EraRule "UTXOS" era ~ BabbageUTXOS era
  , InjectRuleFailure "UTXOS" Alonzo.AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" Alonzo.AlonzoUtxosEvent era
  ) =>
  TransitionRule (BabbageUTXOS era)
utxosTransition =
  judgmentContext >>= \(TRC (_, pup, stAnnTx)) -> do
    let tx = stAnnTx ^. txStAnnTxG
    case tx ^. isValidTxL of
      IsValid True -> babbageEvalScriptsTxValid
      IsValid False -> do
        babbageEvalScriptsTxInvalid @era stAnnTx
        pure pup

-- ===================================================================

expectScriptsToPass ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , InjectRuleFailure "UTXOS" Alonzo.AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" Alonzo.AlonzoUtxosEvent era
  ) =>
  StAnnTx TopTx era ->
  Rule (EraRule "UTXOS" era) 'Transition ()
expectScriptsToPass stAnnTx = do
  let tx = stAnnTx ^. txStAnnTxG
  {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
  let scriptsWithContextEither = plutusScriptsWithContextStAnnTx stAnnTx
  (() <$ scriptsWithContextEither) ?!: (injectFailure . Alonzo.CollectErrors)
  {- isValid tx = evalScripts tx sLst = True -}
  Alonzo.when2Phase $
    whenFailureFree $
      forM_ scriptsWithContextEither $ \scriptsWithContext ->
        case evalPlutusScripts scriptsWithContext of
          Fails _ fs ->
            failBecause $
              injectFailure $
                Alonzo.ValidationTagMismatch
                  (tx ^. isValidTxL)
                  (Alonzo.FailedUnexpectedly (Alonzo.scriptFailureToFailureDescription <$> fs))
          Passes ps -> mapM_ (tellEvent . injectEvent . Alonzo.SuccessfulPlutusScriptsEvent) (nonEmpty ps)

babbageEvalScriptsTxValid ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ShelleyEraTxBody era
  , EraCertState era
  , Environment (EraRule "PPUP" era) ~ Shelley.PpupEnv era
  , Signal (EraRule "PPUP" era) ~ StrictMaybe (Update era)
  , Embed (EraRule "PPUP" era) (BabbageUTXOS era)
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , InjectRuleFailure "UTXOS" Alonzo.AlonzoUtxosPredFailure era
  , EraRule "UTXOS" era ~ BabbageUTXOS era
  , InjectRuleEvent "UTXOS" Alonzo.AlonzoUtxosEvent era
  ) =>
  TransitionRule (BabbageUTXOS era)
babbageEvalScriptsTxValid = do
  TRC (Alonzo.UtxosEnv slot pp certState _utxo, pup, stAnnTx) <-
    judgmentContext
  let tx = stAnnTx ^. txStAnnTxG
      txBody = tx ^. bodyTxL
      genDelegs = certState ^. certDStateL . dsGenDelegsL

  -- We intentionally run the PPUP rule before evaluating any Plutus scripts.
  -- We do not want to waste computation running plutus scripts if the
  -- transaction will fail due to `PPUP`
  updatedGovState <-
    trans @(EraRule "PPUP" era) $
      TRC (Shelley.PPUPEnv slot pp genDelegs, pup, txBody ^. updateTxBodyL)

  () <- pure $! Debug.traceEvent Alonzo.validBegin ()
  expectScriptsToPass stAnnTx
  () <- pure $! Debug.traceEvent Alonzo.validEnd ()

  pure updatedGovState

babbageEvalScriptsTxInvalid ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , InjectRuleFailure "UTXOS" Alonzo.AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" Alonzo.AlonzoUtxosEvent era
  ) =>
  StAnnTx TopTx era ->
  Rule (EraRule "UTXOS" era) 'Transition ()
babbageEvalScriptsTxInvalid stAnnTx = do
  let tx = stAnnTx ^. txStAnnTxG
  () <- pure $! Debug.traceEvent Alonzo.invalidBegin ()
  {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
  let scriptsWithContextEither = plutusScriptsWithContextStAnnTx stAnnTx
  (() <$ scriptsWithContextEither) ?!: (injectFailure . Alonzo.CollectErrors)
  {- isValid tx = evalScripts tx sLst = False -}
  Alonzo.when2Phase $
    whenFailureFree $
      forM_ scriptsWithContextEither $ \scriptsWithContext ->
        case evalPlutusScripts scriptsWithContext of
          Passes _ ->
            failBecause $
              injectFailure $
                Alonzo.ValidationTagMismatch (tx ^. isValidTxL) Alonzo.PassedUnexpectedly
          Fails ps fs -> do
            mapM_
              (tellEvent . injectEvent . Alonzo.SuccessfulPlutusScriptsEvent @era)
              (nonEmpty ps)
            tellEvent . injectEvent $
              Alonzo.FailedPlutusScriptsEvent (scriptFailurePlutus <$> fs)
  pure $! Debug.traceEvent Alonzo.invalidEnd ()
