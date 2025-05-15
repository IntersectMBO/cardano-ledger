{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Utxos (
  DijkstraUTXOS,
  DijkstraUtxosPredFailure (..),
  DijkstraUtxosEvent (..),
) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError, EraPlutusContext)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
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
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, DijkstraUTXOS)
import Cardano.Ledger.Dijkstra.Governance (DijkstraGovState)
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Dijkstra.TxInfo ()
import Cardano.Ledger.Plutus (PlutusWithContext)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), utxosDonationL)
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..), updateUTxOState)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import qualified Debug.Trace as Debug
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

data DijkstraUtxosPredFailure era
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

data DijkstraUtxosEvent era
  = TotalDeposits (SafeHash EraIndependentTxBody) Coin
  | SuccessfulPlutusScriptsEvent (NonEmpty PlutusWithContext)
  | FailedPlutusScriptsEvent (NonEmpty PlutusWithContext)
  | -- | The UTxOs consumed and created by a signal tx
    TxUTxODiff
      -- | UTxO consumed
      (UTxO era)
      -- | UTxO created
      (UTxO era)
  deriving (Generic)

deriving instance (Era era, Eq (TxOut era)) => Eq (DijkstraUtxosEvent era)

instance (Era era, NFData (TxOut era)) => NFData (DijkstraUtxosEvent era)

type instance EraRuleFailure "UTXOS" DijkstraEra = DijkstraUtxosPredFailure DijkstraEra

type instance EraRuleEvent "UTXOS" DijkstraEra = DijkstraUtxosEvent DijkstraEra

instance InjectRuleFailure "UTXOS" DijkstraUtxosPredFailure DijkstraEra

instance InjectRuleEvent "UTXOS" DijkstraUtxosEvent DijkstraEra

instance InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = alonzoToDijkstraUtxosPredFailure

instance InjectRuleEvent "UTXOS" AlonzoUtxosEvent DijkstraEra where
  injectEvent = alonzoToDijkstraUtxosEvent

alonzoToDijkstraUtxosPredFailure ::
  forall era.
  EraRuleFailure "PPUP" era ~ VoidEraRule "PPUP" era =>
  Alonzo.AlonzoUtxosPredFailure era ->
  DijkstraUtxosPredFailure era
alonzoToDijkstraUtxosPredFailure = \case
  Alonzo.ValidationTagMismatch t x -> ValidationTagMismatch t x
  Alonzo.CollectErrors x -> CollectErrors x
  Alonzo.UpdateFailure x -> absurdEraRule @"PPUP" @era x

alonzoToDijkstraUtxosEvent ::
  forall era.
  EraRuleEvent "PPUP" era ~ VoidEraRule "PPUP" era =>
  Alonzo.AlonzoUtxosEvent era ->
  DijkstraUtxosEvent era
alonzoToDijkstraUtxosEvent = \case
  Alonzo.AlonzoPpupToUtxosEvent x -> absurdEraRule @"PPUP" @era x
  Alonzo.TotalDeposits h c -> TotalDeposits h c
  Alonzo.SuccessfulPlutusScriptsEvent l -> SuccessfulPlutusScriptsEvent l
  Alonzo.FailedPlutusScriptsEvent l -> FailedPlutusScriptsEvent l
  Alonzo.TxUTxODiff x y -> TxUTxODiff x y

instance
  ( EraTxCert era
  , DijkstraEraScript era
  , EncCBOR (ContextError era)
  ) =>
  EncCBOR (DijkstraUtxosPredFailure era)
  where
  encCBOR =
    encode . \case
      ValidationTagMismatch v descr -> Sum ValidationTagMismatch 0 !> To v !> To descr
      CollectErrors cs -> Sum (CollectErrors @era) 1 !> To cs

instance
  ( EraTxCert era
  , DijkstraEraScript era
  , DecCBOR (ContextError era)
  ) =>
  DecCBOR (DijkstraUtxosPredFailure era)
  where
  decCBOR = decode (Summands "DijkstraUtxosPredicateFailure" dec)
    where
      dec 0 = SumD ValidationTagMismatch <! From <! From
      dec 1 = SumD (CollectErrors @era) <! From
      dec n = Invalid n

deriving stock instance
  ( DijkstraEraScript era
  , Show (TxCert era)
  , Show (ContextError era)
  , Show (UTxOState era)
  ) =>
  Show (DijkstraUtxosPredFailure era)

deriving stock instance
  ( DijkstraEraScript era
  , Eq (TxCert era)
  , Eq (ContextError era)
  , Eq (UTxOState era)
  ) =>
  Eq (DijkstraUtxosPredFailure era)

instance
  ( DijkstraEraScript era
  , NoThunks (TxCert era)
  , NoThunks (ContextError era)
  , NoThunks (UTxOState era)
  ) =>
  NoThunks (DijkstraUtxosPredFailure era)

instance
  ( DijkstraEraScript era
  , NFData (TxCert era)
  , NFData (ContextError era)
  , NFData (UTxOState era)
  ) =>
  NFData (DijkstraUtxosPredFailure era)

instance
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , DijkstraEraTxBody era
  , DijkstraEraPParams era
  , EraGov era
  , EraStake era
  , EraCertState era
  , EraPlutusContext era
  , GovState era ~ DijkstraGovState era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (DijkstraUTXOS era) ~ Tx era
  , EraRule "UTXOS" era ~ DijkstraUTXOS era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" DijkstraUtxosEvent era
  ) =>
  STS (DijkstraUTXOS era)
  where
  type BaseM (DijkstraUTXOS era) = Cardano.Ledger.BaseTypes.ShelleyBase
  type Environment (DijkstraUTXOS era) = UtxoEnv era
  type State (DijkstraUTXOS era) = UTxOState era
  type Signal (DijkstraUTXOS era) = AlonzoTx era
  type PredicateFailure (DijkstraUTXOS era) = DijkstraUtxosPredFailure era
  type Event (DijkstraUTXOS era) = DijkstraUtxosEvent era

  transitionRules = [utxosTransition]

instance
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , DijkstraEraTxBody era
  , DijkstraEraPParams era
  , EraGov era
  , EraStake era
  , EraCertState era
  , EraPlutusContext era
  , GovState era ~ DijkstraGovState era
  , PredicateFailure (EraRule "UTXOS" era) ~ DijkstraUtxosPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (DijkstraUTXOS era) ~ Tx era
  , EraRule "UTXOS" era ~ DijkstraUTXOS era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" DijkstraUtxosEvent era
  ) =>
  Embed (DijkstraUTXOS era) (BabbageUTXO era)
  where
  wrapFailed = AlonzoInBabbageUtxoPredFailure . UtxosFailure
  wrapEvent = UtxosEvent

utxosTransition ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , DijkstraEraTxBody era
  , EraPlutusContext era
  , EraStake era
  , EraCertState era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , STS (EraRule "UTXOS" era)
  , Environment (EraRule "UTXOS" era) ~ UtxoEnv era
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" DijkstraUtxosEvent era
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
  , DijkstraEraTxBody era
  , EraPlutusContext era
  , EraStake era
  , EraCertState era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXOS" era) ~ Tx era
  , STS (EraRule "UTXOS" era)
  , State (EraRule "UTXOS" era) ~ UTxOState era
  , Environment (EraRule "UTXOS" era) ~ UtxoEnv era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" DijkstraUtxosEvent era
  ) =>
  TransitionRule (EraRule "UTXOS" era)
conwayEvalScriptsTxValid = do
  TRC (UtxoEnv _ pp certState, utxos@(UTxOState utxo _ _ govState _ _), tx) <-
    judgmentContext
  let txBody = tx ^. bodyTxL

  () <- pure $! Debug.traceEvent validBegin ()
  expectScriptsToPass pp tx utxo
  () <- pure $! Debug.traceEvent validEnd ()

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
