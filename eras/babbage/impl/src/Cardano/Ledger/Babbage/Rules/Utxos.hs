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

module Cardano.Ledger.Babbage.Rules.Utxos
  ( BabbageUTXOS,
    utxosTransition,
  )
where

import Cardano.Binary (ToCBOR (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi
  ( collectTwoPhaseScriptInputs,
    evalScripts,
  )
import Cardano.Ledger.Alonzo.Rules
  ( AlonzoUtxosEvent (..),
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
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript, CostModels)
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO, ScriptResult (Fails, Passes))
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (..))
import Cardano.Ledger.Babbage.Collateral (collBalance, collOuts)
import Cardano.Ledger.Babbage.Era (BabbageUTXOS)
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.Babbage.TxBody
  ( AlonzoEraTxBody (collateralInputsTxBodyL),
    BabbageEraTxBody,
    BabbageTxOut,
    ShelleyEraTxBody (certsTxBodyL, updateTxBodyL),
  )
import Cardano.Ledger.BaseTypes (ProtVer, ShelleyBase, epochInfo, strictMaybeToMaybe, systemStart)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState
  ( PPUPState (..),
    UTxOState (..),
    keyRefunds,
    updateStakeDistribution,
  )
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules.Ppup (ShelleyPPUP, ShelleyPPUPEnv (..), ShelleyPpupPredFailure)
import Cardano.Ledger.Shelley.Rules.Utxo (ShelleyUtxoEnv (..), updateUTxOState)
import Cardano.Ledger.Shelley.UTxO (UTxO (..), totalDeposits)
import qualified Cardano.Ledger.Val as Val
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.Foldable (toList)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Strict as Map
import Data.MapExtras (extractKeys)
import Debug.Trace (traceEvent)
import GHC.Records (HasField (..))
import Lens.Micro

-- =====================================================

instance
  forall era.
  ( AlonzoEraTx era,
    BabbageEraTxBody era,
    ExtendedUTxO era,
    Show (Script era),
    Eq (PParamsUpdate era),
    Show (PParamsUpdate era),
    Tx era ~ AlonzoTx era,
    TxOut era ~ BabbageTxOut era,
    TxBody era ~ BabbageTxBody era,
    TxWits era ~ TxWitness era,
    Script era ~ AlonzoScript era,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin,
    HasField "_costmdls" (PParams era) CostModels,
    HasField "_protocolVersion" (PParams era) ProtVer,
    Embed (EraRule "PPUP" era) (BabbageUTXOS era),
    Environment (EraRule "PPUP" era) ~ ShelleyPPUPEnv era,
    State (EraRule "PPUP" era) ~ PPUPState era,
    Signal (EraRule "PPUP" era) ~ Maybe (Update era),
    ToCBOR (PredicateFailure (EraRule "PPUP" era)) -- Serializing the PredicateFailure
  ) =>
  STS (BabbageUTXOS era)
  where
  type BaseM (BabbageUTXOS era) = ShelleyBase
  type Environment (BabbageUTXOS era) = ShelleyUtxoEnv era
  type State (BabbageUTXOS era) = UTxOState era
  type Signal (BabbageUTXOS era) = AlonzoTx era
  type PredicateFailure (BabbageUTXOS era) = AlonzoUtxosPredFailure era
  type Event (BabbageUTXOS era) = AlonzoUtxosEvent era
  transitionRules = [utxosTransition]

instance
  ( Era era,
    STS (ShelleyPPUP era),
    PredicateFailure (EraRule "PPUP" era) ~ ShelleyPpupPredFailure era,
    Event (EraRule "PPUP" era) ~ Event (ShelleyPPUP era)
  ) =>
  Embed (ShelleyPPUP era) (BabbageUTXOS era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = AlonzoPpupToUtxosEvent

utxosTransition ::
  forall era.
  ( AlonzoEraTx era,
    ExtendedUTxO era,
    BabbageEraTxBody era,
    Tx era ~ AlonzoTx era,
    TxOut era ~ BabbageTxOut era,
    TxBody era ~ BabbageTxBody era,
    TxWits era ~ TxWitness era,
    Script era ~ AlonzoScript era,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin,
    HasField "_costmdls" (PParams era) CostModels,
    HasField "_protocolVersion" (PParams era) ProtVer,
    Environment (EraRule "PPUP" era) ~ ShelleyPPUPEnv era,
    State (EraRule "PPUP" era) ~ PPUPState era,
    Signal (EraRule "PPUP" era) ~ Maybe (Update era),
    Embed (EraRule "PPUP" era) (BabbageUTXOS era),
    ToCBOR (PredicateFailure (EraRule "PPUP" era))
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
  ( ExtendedUTxO era,
    AlonzoEraTx era,
    Tx era ~ AlonzoTx era,
    Script era ~ AlonzoScript era,
    TxWits era ~ TxWitness era,
    STS (BabbageUTXOS era),
    Environment (EraRule "PPUP" era) ~ ShelleyPPUPEnv era,
    State (EraRule "PPUP" era) ~ PPUPState era,
    Signal (EraRule "PPUP" era) ~ Maybe (Update era),
    Embed (EraRule "PPUP" era) (BabbageUTXOS era),
    HasField "_poolDeposit" (PParams era) Coin,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_costmdls" (PParams era) CostModels,
    HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  TransitionRule (BabbageUTXOS era)
scriptsYes = do
  TRC (UtxoEnv slot pp poolParams genDelegs, u@(UTxOState utxo _ _ pup _), tx) <-
    judgmentContext
  let {- txb := txbody tx -}
      txBody = body tx
      {- refunded := keyRefunds pp txb -}
      refunded = keyRefunds pp txBody
      txCerts = toList $ txBody ^. certsTxBodyL
      {- depositChange := (totalDeposits pp poolParams txcerts txb) − refunded -}
      depositChange =
        totalDeposits pp (`Map.notMember` poolParams) txCerts Val.<-> refunded
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  -- We intentionally run the PPUP rule before evaluating any Plutus scripts.
  -- We do not want to waste computation running plutus scripts if the
  -- transaction will fail due to `PPUP`
  ppup' <-
    trans @(EraRule "PPUP" era) $
      TRC
        (PPUPEnv slot pp genDelegs, pup, strictMaybeToMaybe $ txBody ^. updateTxBodyL)

  let !_ = traceEvent validBegin ()

  {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
  case collectTwoPhaseScriptInputs ei sysSt pp tx utxo of
    Right sLst ->
      {- isValid tx = evalScripts tx sLst = True -}
      whenFailureFree $
        when2Phase $
          case evalScripts @era (getField @"_protocolVersion" pp) tx sLst of
            Fails _ fs ->
              failBecause $
                ValidationTagMismatch
                  (tx ^. isValidTxL)
                  (FailedUnexpectedly (scriptFailuresToPredicateFailure fs))
            Passes ps -> mapM_ (tellEvent . SuccessfulPlutusScriptsEvent) (nonEmpty ps)
    Left info -> failBecause (CollectErrors info)

  let !_ = traceEvent validEnd ()

  pure $! updateUTxOState u txBody depositChange ppup'

scriptsNo ::
  forall era.
  ( AlonzoEraTx era,
    ExtendedUTxO era,
    STS (BabbageUTXOS era),
    BabbageEraTxBody era,
    Tx era ~ AlonzoTx era,
    TxOut era ~ BabbageTxOut era,
    TxBody era ~ BabbageTxBody era,
    TxWits era ~ TxWitness era,
    Script era ~ AlonzoScript era,
    HasField "_protocolVersion" (PParams era) ProtVer,
    HasField "_costmdls" (PParams era) CostModels
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
        when2Phase $ case evalScripts @era (getField @"_protocolVersion" pp) tx sLst of
          Passes _ -> failBecause $ ValidationTagMismatch (tx ^. isValidTxL) PassedUnexpectedly
          Fails ps fs -> do
            mapM_ (tellEvent . SuccessfulPlutusScriptsEvent) (nonEmpty ps)
            tellEvent (FailedPlutusScriptsEvent (scriptFailuresToPlutusDebug fs))
    Left info -> failBecause (CollectErrors info)

  () <- pure $! traceEvent invalidEnd ()

  {- utxoKeep = getField @"collateral" txb ⋪ utxo -}
  {- utxoDel  = getField @"collateral" txb ◁ utxo -}
  let !(utxoKeep, utxoDel) = extractKeys (unUTxO utxo) (txBody ^. collateralInputsTxBodyL)
      UTxO collouts = collOuts txBody
      collateralFees = Val.coin (collBalance txBody utxo) -- NEW to Babbage
  pure
    $! us {- (collInputs txb ⋪ utxo) ∪ collouts tx -}
      { _utxo = UTxO (Map.union utxoKeep collouts), -- NEW to Babbage
      {- fees + collateralFees -}
        _fees = fees <> collateralFees, -- NEW to Babbage
        _stakeDistro = updateStakeDistribution (_stakeDistro us) (UTxO utxoDel) (UTxO collouts)
      }
