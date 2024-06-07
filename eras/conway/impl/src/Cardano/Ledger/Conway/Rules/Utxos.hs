{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Utxos (
  ConwayUTXOS,
  ConwayUtxosPredFailure (..),
  ConwayUtxosEvent (..),
) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError, EraPlutusContext)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  CollectError (..),
 )
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent (..),
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosEvent,
  AlonzoUtxosPredFailure,
  TagMismatchDescription,
  validBegin,
  validEnd,
 )
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo (
  AlonzoUtxosEvent (..),
  AlonzoUtxosPredFailure (..),
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
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.CertState (certsTotalDepositsTxBody, certsTotalRefundsTxBody)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayUTXOS)
import Cardano.Ledger.Conway.FRxO (txfrxo)
import Cardano.Ledger.Conway.Governance (ConwayGovState (..))
import Cardano.Ledger.Conway.TxInfo ()
import Cardano.Ledger.FRxO (FRxO (FRxO, unFRxO))
import Cardano.Ledger.Plutus (
  PlutusWithContext,
 )
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Shelley.LedgerState (
  CertState,
  UTxOState (..),
  updateStakeDistribution,
  utxosDonationL,
 )
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (UTxO, unUTxO))
import Cardano.Ledger.Val ((<->))
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import Data.MapExtras (extractKeys)
import Debug.Trace (traceEvent)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

data ConwayUtxosPredFailure era
  = -- | The 'isValid' tag on the transaction is incorrect. The tag given
    --   here is that provided on the transaction (whereas evaluation of the
    --   scripts gives the opposite.). The Text tries to explain why it failed.
    ValidationTagMismatch IsValid TagMismatchDescription
  | -- | We could not find all the necessary inputs for a Plutus Script.
    -- Previous PredicateFailure tests should make this impossible, but the
    -- consequences of not detecting this means scripts get dropped, so things
    -- might validate that shouldn't. So we double check in the function
    -- collectTwoPhaseScriptInputs, it should find data for every Script.
    CollectErrors [CollectError era]
  deriving
    (Generic)

data ConwayUtxosEvent era
  = TotalDeposits (SafeHash (EraCrypto era) EraIndependentTxBody) Coin
  | SuccessfulPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
  | FailedPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
  | -- | The UTxOs consumed and created by a signal tx
    TxUTxODiff
      -- | UTxO consumed
      (UTxO era)
      -- | UTxO created
      (UTxO era)
  deriving (Generic)

deriving instance (Era era, Eq (TxOut era)) => Eq (ConwayUtxosEvent era)

instance (Era era, NFData (TxOut era)) => NFData (ConwayUtxosEvent era)

type instance EraRuleFailure "UTXOS" (ConwayEra c) = ConwayUtxosPredFailure (ConwayEra c)

type instance EraRuleEvent "UTXOS" (ConwayEra c) = ConwayUtxosEvent (ConwayEra c)

instance InjectRuleFailure "UTXOS" ConwayUtxosPredFailure (ConwayEra c)

instance InjectRuleEvent "UTXOS" ConwayUtxosEvent (ConwayEra c)

instance InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure (ConwayEra c) where
  injectFailure = alonzoToConwayUtxosPredFailure

instance InjectRuleEvent "UTXOS" AlonzoUtxosEvent (ConwayEra c) where
  injectEvent = alonzoToConwayUtxosEvent

alonzoToConwayUtxosPredFailure ::
  forall era.
  EraRuleFailure "PPUP" era ~ VoidEraRule "PPUP" era =>
  Alonzo.AlonzoUtxosPredFailure era ->
  ConwayUtxosPredFailure era
alonzoToConwayUtxosPredFailure = \case
  Alonzo.ValidationTagMismatch t x -> ValidationTagMismatch t x
  Alonzo.CollectErrors x -> CollectErrors x
  Alonzo.UpdateFailure x -> absurdEraRule @"PPUP" @era x

alonzoToConwayUtxosEvent ::
  forall era.
  EraRuleEvent "PPUP" era ~ VoidEraRule "PPUP" era =>
  Alonzo.AlonzoUtxosEvent era ->
  ConwayUtxosEvent era
alonzoToConwayUtxosEvent = \case
  Alonzo.AlonzoPpupToUtxosEvent x -> absurdEraRule @"PPUP" @era x
  Alonzo.TotalDeposits h c -> TotalDeposits h c
  Alonzo.SuccessfulPlutusScriptsEvent l -> SuccessfulPlutusScriptsEvent l
  Alonzo.FailedPlutusScriptsEvent l -> FailedPlutusScriptsEvent l
  Alonzo.TxUTxODiff x y -> TxUTxODiff x y

instance
  ( EraTxCert era
  , ConwayEraScript era
  , EncCBOR (ContextError era)
  ) =>
  EncCBOR (ConwayUtxosPredFailure era)
  where
  encCBOR =
    encode . \case
      ValidationTagMismatch v descr -> Sum ValidationTagMismatch 0 !> To v !> To descr
      CollectErrors cs -> Sum (CollectErrors @era) 1 !> To cs

instance
  ( EraTxCert era
  , ConwayEraScript era
  , DecCBOR (ContextError era)
  ) =>
  DecCBOR (ConwayUtxosPredFailure era)
  where
  decCBOR = decode (Summands "ConwayUtxosPredicateFailure" dec)
    where
      dec 0 = SumD ValidationTagMismatch <! From <! From
      dec 1 = SumD (CollectErrors @era) <! From
      dec n = Invalid n

deriving stock instance
  ( ConwayEraScript era
  , Show (TxCert era)
  , Show (ContextError era)
  , Show (UTxOState era)
  ) =>
  Show (ConwayUtxosPredFailure era)

deriving stock instance
  ( ConwayEraScript era
  , Eq (TxCert era)
  , Eq (ContextError era)
  , Eq (UTxOState era)
  ) =>
  Eq (ConwayUtxosPredFailure era)

instance
  ( ConwayEraScript era
  , NoThunks (TxCert era)
  , NoThunks (ContextError era)
  , NoThunks (UTxOState era)
  ) =>
  NoThunks (ConwayUtxosPredFailure era)

instance
  ( ConwayEraScript era
  , NFData (TxCert era)
  , NFData (ContextError era)
  , NFData (UTxOState era)
  ) =>
  NFData (ConwayUtxosPredFailure era)

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
  , EraRule "UTXOS" era ~ ConwayUTXOS era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" ConwayUtxosEvent era
  ) =>
  STS (ConwayUTXOS era)
  where
  type BaseM (ConwayUTXOS era) = Cardano.Ledger.BaseTypes.ShelleyBase
  type Environment (ConwayUTXOS era) = UtxoEnv era
  type State (ConwayUTXOS era) = UTxOState era
  type Signal (ConwayUTXOS era) = AlonzoTx era
  type PredicateFailure (ConwayUTXOS era) = ConwayUtxosPredFailure era
  type Event (ConwayUTXOS era) = ConwayUtxosEvent era

  transitionRules = [utxosTransition]

instance
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ConwayEraTxBody era
  , ConwayEraPParams era
  , EraGov era
  , EraPlutusContext era
  , GovState era ~ ConwayGovState era
  , PredicateFailure (EraRule "UTXOS" era) ~ ConwayUtxosPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (ConwayUTXOS era) ~ Tx era
  , EraRule "UTXOS" era ~ ConwayUTXOS era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" ConwayUtxosEvent era
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
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , STS (EraRule "UTXOS" era)
  , Environment (EraRule "UTXOS" era) ~ UtxoEnv era
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" ConwayUtxosEvent era
  ) =>
  TransitionRule (EraRule "UTXOS" era)
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
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , STS (EraRule "UTXOS" era)
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , Environment (EraRule "UTXOS" era) ~ UtxoEnv era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" ConwayUtxosEvent era
  ) =>
  TransitionRule (EraRule "UTXOS" era)
conwayEvalScriptsTxValid = do
  TRC (UtxoEnv _ pp certState, utxos@(UTxOState utxo _frxo _ _ govState _ _), tx) <-
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
      (tellEvent . injectEvent . TotalDeposits (hashAnnotated txBody))
      (\a b -> tellEvent . injectEvent $ TxUTxODiff a b)
  pure $! utxos' & utxosDonationL <>~ txBody ^. treasuryDonationTxBodyL

-- | This monadic action captures the final stages of the UTXO(S) rule. In particular it
-- applies all of the UTxO related aditions and removals, gathers all of the fees into the
-- fee pot `utxosFees` and updates the `utxosDeposited` field. Continuation supplied will
-- be called on the @deposit - refund@ change, which is normally used to emit the
-- `TotalDeposits` event.

-- TODO WG: This shouldn't be here. Need to figure out how to alter original without changing tons of callsites
updateUTxOState ::
  (ConwayEraTxBody era, Monad m) =>
  PParams era ->
  UTxOState era ->
  TxBody era ->
  CertState era ->
  GovState era ->
  (Coin -> m ()) ->
  (UTxO era -> UTxO era -> m ()) ->
  m (UTxOState era)
updateUTxOState pp utxos txBody certState govState depositChangeEvent txUtxODiffEvent = do
  let UTxOState
        { utxosUtxo
        , utxosFrxo
        , utxosDeposited
        , utxosFees
        , utxosStakeDistr
        , utxosDonation
        } = utxos
      UTxO utxo = utxosUtxo
      !utxoAdd = txouts txBody -- These will be inserted into the UTxO
      {- utxoDel  = txins txb ◁ utxo -}
      !(utxoWithout, utxoDel) = extractKeys utxo (txBody ^. inputsTxBodyL)
      {- newUTxO = (txins txb ⋪ utxo) ∪ outs txb -}
      newUTxO = utxoWithout `Map.union` unUTxO utxoAdd
      FRxO frxo = utxosFrxo
      !frxoAdd = txfrxo txBody -- These will be inserted into the FRxO
      {- utxoDel  = txins txb ◁ utxo -}
      !(frxoWithout, _frxoDel) = extractKeys frxo (txBody ^. fulfillsTxBodyL)
      {- newUTxO = (txins txb ⋪ utxo) ∪ outs txb -}
      newFRxO = frxoWithout `Map.union` unFRxO frxoAdd
      deletedUTxO = UTxO utxoDel
      newIncStakeDistro = updateStakeDistribution pp utxosStakeDistr deletedUTxO utxoAdd
      totalRefunds = certsTotalRefundsTxBody pp certState txBody
      totalDeposits = certsTotalDepositsTxBody pp certState txBody
      depositChange = totalDeposits <-> totalRefunds
  depositChangeEvent depositChange
  txUtxODiffEvent deletedUTxO utxoAdd
  pure $!
    UTxOState
      { utxosUtxo = UTxO newUTxO
      , utxosFrxo = FRxO newFRxO
      , utxosDeposited = utxosDeposited <> depositChange
      , utxosFees = utxosFees <> txBody ^. feeTxBodyL
      , utxosGovState = govState
      , utxosStakeDistr = newIncStakeDistro
      , utxosDonation = utxosDonation
      }
