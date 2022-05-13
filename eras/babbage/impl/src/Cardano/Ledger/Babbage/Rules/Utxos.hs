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

module Cardano.Ledger.Babbage.Rules.Utxos where

import Cardano.Binary (ToCBOR (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi
  ( collectTwoPhaseScriptInputs,
    evalScripts,
  )
import Cardano.Ledger.Alonzo.Rules.Utxos
  ( TagMismatchDescription (..),
    UtxosEvent (..),
    UtxosPredicateFailure (..),
    invalidBegin,
    invalidEnd,
    scriptFailuresToPlutusDebug,
    scriptFailuresToPredicateFailure,
    validBegin,
    validEnd,
    when2Phase,
  )
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO, ScriptResult (Fails, Passes))
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (..))
import qualified Cardano.Ledger.Babbage.Collateral as Babbage
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import Cardano.Ledger.Babbage.Tx (ValidatedTx (..))
import qualified Cardano.Ledger.Babbage.Tx as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import Cardano.Ledger.BaseTypes (ShelleyBase, epochInfo, systemStart)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..), ValidateScript)
import qualified Cardano.Ledger.Mary.Value as Mary
import Cardano.Ledger.Shelley.LedgerState (PPUPState (..), UTxOState (..), keyRefunds, updateStakeDistribution)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules.Ppup (PPUP, PPUPEnv (..), PpupPredicateFailure)
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..), updateUTxOState)
import Cardano.Ledger.Shelley.UTxO (UTxO (..), totalDeposits)
import Cardano.Ledger.TxIn (TxIn)
import qualified Cardano.Ledger.Val as Val
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import qualified Data.Compact.SplitMap as SplitMap
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe.Strict
import Data.Set (Set)
import Debug.Trace (traceEvent)
import GHC.Records (HasField (..))

-- =====================================================

type ConcreteBabbage era =
  ( Core.Script era ~ Alonzo.Script era,
    Core.Value era ~ Mary.Value (Crypto era),
    Core.TxBody era ~ Babbage.TxBody era,
    Core.PParams era ~ Babbage.PParams era,
    Core.PParamsDelta era ~ Babbage.PParamsUpdate era,
    Core.TxOut era ~ Babbage.TxOut era,
    Core.Tx era ~ ValidatedTx era,
    Core.Witnesses era ~ TxWitness era
  )

data BabbageUTXOS era

instance
  forall era.
  ( Era era,
    ConcreteBabbage era,
    ExtendedUTxO era,
    Embed (Core.EraRule "PPUP" era) (BabbageUTXOS era),
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era),
    ValidateScript era,
    ToCBOR (PredicateFailure (Core.EraRule "PPUP" era)) -- Serializing the PredicateFailure
  ) =>
  STS (BabbageUTXOS era)
  where
  type BaseM (BabbageUTXOS era) = ShelleyBase
  type Environment (BabbageUTXOS era) = UtxoEnv era
  type State (BabbageUTXOS era) = UTxOState era
  type Signal (BabbageUTXOS era) = ValidatedTx era
  type PredicateFailure (BabbageUTXOS era) = UtxosPredicateFailure era
  type Event (BabbageUTXOS era) = UtxosEvent era
  transitionRules = [utxosTransition]

instance
  ( Era era,
    STS (PPUP era),
    PredicateFailure (Core.EraRule "PPUP" era) ~ PpupPredicateFailure era,
    Event (Core.EraRule "PPUP" era) ~ Event (PPUP era)
  ) =>
  Embed (PPUP era) (BabbageUTXOS era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = AlonzoPpupToUtxosEvent

utxosTransition ::
  forall era.
  ( ConcreteBabbage era,
    ExtendedUTxO era,
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era),
    Embed (Core.EraRule "PPUP" era) (BabbageUTXOS era),
    ValidateScript era,
    ToCBOR (PredicateFailure (Core.EraRule "PPUP" era)),
    HasField "collateral" (Babbage.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  TransitionRule (BabbageUTXOS era)
utxosTransition =
  judgmentContext >>= \(TRC (_, _, tx)) -> do
    case getField @"isValid" tx of
      IsValid True -> scriptsYes
      IsValid False -> scriptsNo

-- ===================================================================

scriptsYes ::
  forall era.
  ( ValidateScript era,
    ConcreteBabbage era,
    ExtendedUTxO era,
    STS (BabbageUTXOS era),
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era),
    Embed (Core.EraRule "PPUP" era) (BabbageUTXOS era)
  ) =>
  TransitionRule (BabbageUTXOS era)
scriptsYes = do
  TRC (UtxoEnv _ slot pp poolParams genDelegs, u@(UTxOState utxo _ _ pup _), tx) <-
    judgmentContext
  let {- txb := txbody tx -}
      txb = body tx
      {- refunded := keyRefunds pp txb -}
      refunded = keyRefunds pp txb
      txcerts = toList $ getField @"certs" txb
      {- depositChange := (totalDeposits pp poolParams txcerts txb) − refunded -}
      depositChange =
        totalDeposits pp (`Map.notMember` poolParams) txcerts Val.<-> refunded
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  -- We intentionally run the PPUP rule before evaluating any Plutus scripts.
  -- We do not want to waste computation running plutus scripts if the
  -- transaction will fail due to `PPUP`
  ppup' <-
    trans @(Core.EraRule "PPUP" era) $
      TRC
        (PPUPEnv slot pp genDelegs, pup, strictMaybeToMaybe $ getField @"update" txb)

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
                  (getField @"isValid" tx)
                  (FailedUnexpectedly (scriptFailuresToPredicateFailure fs))
            Passes _ -> pure ()
    Left info -> failBecause (CollectErrors info)

  let !_ = traceEvent validEnd ()

  pure $! updateUTxOState u txb depositChange ppup'

scriptsNo ::
  forall era.
  ( ValidateScript era,
    ConcreteBabbage era,
    ExtendedUTxO era,
    STS (BabbageUTXOS era),
    HasField "collateral" (Babbage.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  TransitionRule (BabbageUTXOS era)
scriptsNo = do
  TRC (UtxoEnv _ _ pp _ _, us@(UTxOState utxo _ fees _ _), tx) <- judgmentContext
  {- txb := txbody tx -}
  let txb = body tx
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  () <- pure $! traceEvent invalidBegin ()

  case collectTwoPhaseScriptInputs ei sysSt pp tx utxo of
    Right sLst ->
      {- sLst := collectTwoPhaseScriptInputs pp tx utxo -}
      {- isValid tx = evalScripts tx sLst = False -}
      when2Phase $ case evalScripts @era (getField @"_protocolVersion" pp) tx sLst of
        Passes _ -> failBecause $ ValidationTagMismatch (getField @"isValid" tx) PassedUnexpectedly
        Fails ps fs -> do
          tellEvent (SuccessfulPlutusScriptsEvent ps)
          tellEvent (FailedPlutusScriptsEvent (scriptFailuresToPlutusDebug fs))
    Left info -> failBecause (CollectErrors info)

  () <- pure $! traceEvent invalidEnd ()

  {- utxoKeep = getField @"collateral" txb ⋪ utxo -}
  {- utxoDel  = getField @"collateral" txb ◁ utxo -}
  let !(!utxoKeep, !utxoDel) =
        SplitMap.extractKeysSet (unUTxO utxo) (getField @"collateral" txb)
      UTxO collouts = Babbage.collOuts txb
      collateralFees = Val.coin (Babbage.collBalance txb utxo) -- NEW to Babbage
  pure
    $! us {- (collInputs txb ⋪ utxo) ∪ collouts tx -}
      { _utxo = UTxO (SplitMap.union utxoKeep collouts), -- NEW to Babbage
      {- fees + collateralFees -}
        _fees = fees <> collateralFees, -- NEW to Babbage
        _stakeDistro = updateStakeDistribution (_stakeDistro us) (UTxO utxoDel) (UTxO collouts)
      }
