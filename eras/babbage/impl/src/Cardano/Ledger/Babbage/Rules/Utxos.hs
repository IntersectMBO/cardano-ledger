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
import Cardano.Ledger.Babbage.Collateral (
  collAdaBalance,
  collOuts,
 )
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageUTXOS)
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.BaseTypes (
  ShelleyBase,
  epochInfo,
  strictMaybeToMaybe,
  systemStart,
 )
import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.CertState (certDState, dsGenDelegs)
import Cardano.Ledger.Plutus.Evaluate (
  ScriptFailure (..),
  ScriptResult (..),
 )
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.LedgerState (
  PPUPPredFailure,
  UTxOState (..),
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
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Strict as Map
import Data.MapExtras (extractKeys)
import Debug.Trace (traceEvent)
import Lens.Micro

-- =====================================================

instance
  ( AlonzoEraTx era
  , AlonzoEraPParams era
  , BabbageEraTxBody era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , EraGov era
  , GovState era ~ ShelleyGovState era
  , Embed (EraRule "PPUP" era) (BabbageUTXOS era)
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ Maybe (Update era)
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , Signal (BabbageUTXOS era) ~ Tx era
  , EncCBOR (PPUPPredFailure era) -- Serializing the PredicateFailure
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  , ProtVerAtMost era 8
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
  , BabbageEraTxBody era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , EraGov era
  , GovState era ~ ShelleyGovState era
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ Maybe (Update era)
  , Embed (EraRule "PPUP" era) (BabbageUTXOS era)
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , Signal (BabbageUTXOS era) ~ Tx era
  , EncCBOR (PPUPPredFailure era)
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  , EraPlutusContext era
  , ProtVerAtMost era 8
  ) =>
  TransitionRule (BabbageUTXOS era)
utxosTransition =
  judgmentContext >>= \(TRC (_, _, tx)) -> do
    case tx ^. isValidTxL of
      IsValid True -> babbageEvalScriptsTxValid
      IsValid False -> babbageEvalScriptsTxInvalid

-- ===================================================================

expectScriptsToPass ::
  forall era s.
  ( AlonzoEraTx era
  , EraPlutusContext era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , STS (s era)
  , Event (s era) ~ AlonzoUtxosEvent era
  , PredicateFailure (s era) ~ AlonzoUtxosPredFailure era
  , BaseM (s era) ~ ShelleyBase
  ) =>
  PParams era ->
  Tx era ->
  UTxO era ->
  Rule (s era) 'Transition ()
expectScriptsToPass pp tx utxo = do
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo
  {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
  case collectPlutusScriptsWithContext ei sysSt pp tx utxo of
    Right sLst -> do
      {- isValid tx = evalScripts tx sLst = True -}
      whenFailureFree $
        when2Phase $ case evalPlutusScripts tx sLst of
          Fails _ fs ->
            failBecause $
              ValidationTagMismatch
                (tx ^. isValidTxL)
                (FailedUnexpectedly (scriptFailureToFailureDescription <$> fs))
          Passes ps -> mapM_ (tellEvent . SuccessfulPlutusScriptsEvent) (nonEmpty ps)
    Left info -> failBecause (CollectErrors info)

babbageEvalScriptsTxValid ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , STS (BabbageUTXOS era)
  , Signal (BabbageUTXOS era) ~ Tx era
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ Maybe (Update era)
  , Embed (EraRule "PPUP" era) (BabbageUTXOS era)
  , GovState era ~ ShelleyGovState era
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , EraPlutusContext era
  , ProtVerAtMost era 8
  ) =>
  TransitionRule (BabbageUTXOS era)
babbageEvalScriptsTxValid = do
  TRC (UtxoEnv slot pp certState, utxos@(UTxOState utxo _ _ pup _ _), tx) <-
    judgmentContext
  let txBody = tx ^. bodyTxL
      genDelegs = dsGenDelegs (certDState certState)

  -- We intentionally run the PPUP rule before evaluating any Plutus scripts.
  -- We do not want to waste computation running plutus scripts if the
  -- transaction will fail due to `PPUP`
  ppup' <-
    trans @(EraRule "PPUP" era) $
      TRC
        (PPUPEnv slot pp genDelegs, pup, strictMaybeToMaybe $ txBody ^. updateTxBodyL)

  () <- pure $! traceEvent validBegin ()
  expectScriptsToPass pp tx utxo
  () <- pure $! traceEvent validEnd ()

  updateUTxOState
    pp
    utxos
    txBody
    certState
    ppup'
    (tellEvent . TotalDeposits (hashAnnotated txBody))
    (\a b -> tellEvent $ TxUTxODiff a b)

babbageEvalScriptsTxInvalid ::
  forall s era.
  ( AlonzoEraTx era
  , BabbageEraTxBody era
  , EraPlutusContext era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , STS (s era)
  , Environment (s era) ~ UtxoEnv era
  , Signal (s era) ~ Tx era
  , Event (s era) ~ AlonzoUtxosEvent era
  , PredicateFailure (s era) ~ AlonzoUtxosPredFailure era
  , State (s era) ~ UTxOState era
  , BaseM (s era) ~ ShelleyBase
  ) =>
  TransitionRule (s era)
babbageEvalScriptsTxInvalid = do
  TRC (UtxoEnv _ pp _, us@(UTxOState utxo _ fees _ _ _), tx) <- judgmentContext
  {- txb := txbody tx -}
  let txBody = tx ^. bodyTxL
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  () <- pure $! traceEvent invalidBegin ()

  case collectPlutusScriptsWithContext ei sysSt pp tx utxo of
    Right sLst ->
      {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
      {- isValid tx = evalScripts tx sLst = False -}
      whenFailureFree $
        when2Phase $ case evalPlutusScripts tx sLst of
          Passes _ -> failBecause $ ValidationTagMismatch (tx ^. isValidTxL) PassedUnexpectedly
          Fails ps fs -> do
            mapM_ (tellEvent . SuccessfulPlutusScriptsEvent) (nonEmpty ps)
            tellEvent (FailedPlutusScriptsEvent (scriptFailurePlutus <$> fs))
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
