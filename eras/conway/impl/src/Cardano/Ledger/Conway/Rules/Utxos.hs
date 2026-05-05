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

module Cardano.Ledger.Conway.Rules.Utxos (
  ConwayUTXOS,
  ConwayUtxosEnv (..),
  ConwayUtxosPredFailure (..),
  ConwayUtxosEvent (..),
  alonzoToConwayUtxosPredFailure,
  alonzoToConwayUtxosEvent,
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
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayUTXOS)
import Cardano.Ledger.Conway.Governance (ConwayGovState)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Plutus (PlutusWithContext)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import qualified Debug.Trace as Debug
import GHC.Generics (Generic)
import Lens.Micro ((^.))

data ConwayUtxosEnv era = ConwayUtxosEnv
  { cuePParams :: !(PParams era)
  , cueUTxO :: !(UTxO era)
  }
  deriving (Generic)

deriving instance (Show (PParams era), Show (UTxO era)) => Show (ConwayUtxosEnv era)

deriving instance (Eq (PParams era), Eq (UTxO era)) => Eq (ConwayUtxosEnv era)

instance (Era era, NFData (PParams era), NFData (UTxO era)) => NFData (ConwayUtxosEnv era)

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
    CollectErrors (NonEmpty (CollectError era))
  deriving
    (Generic)

data ConwayUtxosEvent era
  = SuccessfulPlutusScriptsEvent (NonEmpty PlutusWithContext)
  | FailedPlutusScriptsEvent (NonEmpty PlutusWithContext)
  deriving (Generic)

deriving instance Eq (ConwayUtxosEvent era)

instance NFData (ConwayUtxosEvent era)

type instance EraRuleFailure "UTXOS" ConwayEra = ConwayUtxosPredFailure ConwayEra

type instance EraRuleEvent "UTXOS" ConwayEra = ConwayUtxosEvent ConwayEra

instance InjectRuleFailure "UTXOS" ConwayUtxosPredFailure ConwayEra

instance InjectRuleEvent "UTXOS" ConwayUtxosEvent ConwayEra

instance InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure ConwayEra where
  injectFailure = alonzoToConwayUtxosPredFailure

instance InjectRuleEvent "UTXOS" AlonzoUtxosEvent ConwayEra where
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
  Alonzo.SuccessfulPlutusScriptsEvent l -> SuccessfulPlutusScriptsEvent l
  Alonzo.FailedPlutusScriptsEvent l -> FailedPlutusScriptsEvent l

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
  ) =>
  Show (ConwayUtxosPredFailure era)

deriving stock instance
  ( ConwayEraScript era
  , Eq (TxCert era)
  , Eq (ContextError era)
  ) =>
  Eq (ConwayUtxosPredFailure era)

instance
  ( ConwayEraScript era
  , NFData (TxCert era)
  , NFData (ContextError era)
  ) =>
  NFData (ConwayUtxosPredFailure era)

instance
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ConwayEraScript era
  , ConwayEraPParams era
  , EraGov era
  , EraStake era
  , EraCertState era
  , EraPlutusContext era
  , GovState era ~ ConwayGovState era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (ConwayUTXOS era) ~ StAnnTx TopTx era
  , EraRule "UTXOS" era ~ ConwayUTXOS era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  , InjectRuleEvent "UTXOS" ConwayUtxosEvent era
  ) =>
  STS (ConwayUTXOS era)
  where
  type BaseM (ConwayUTXOS era) = Cardano.Ledger.BaseTypes.ShelleyBase
  type Environment (ConwayUTXOS era) = ConwayUtxosEnv era
  type State (ConwayUTXOS era) = ()
  type Signal (ConwayUTXOS era) = StAnnTx TopTx era
  type PredicateFailure (ConwayUTXOS era) = ConwayUtxosPredFailure era
  type Event (ConwayUTXOS era) = ConwayUtxosEvent era

  transitionRules = [utxosTransition]

instance
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ConwayEraScript era
  , ConwayEraPParams era
  , EraGov era
  , EraStake era
  , EraCertState era
  , EraPlutusContext era
  , GovState era ~ ConwayGovState era
  , PredicateFailure (EraRule "UTXOS" era) ~ ConwayUtxosPredFailure era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (ConwayUTXOS era) ~ StAnnTx TopTx era
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
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXOS" era) ~ StAnnTx TopTx era
  , STS (EraRule "UTXOS" era)
  , Environment (EraRule "UTXOS" era) ~ ConwayUtxosEnv era
  , State (EraRule "UTXOS" era) ~ ()
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  ) =>
  TransitionRule (EraRule "UTXOS" era)
utxosTransition =
  judgmentContext >>= \(TRC (ConwayUtxosEnv pp utxo, (), stAnnTx)) -> do
    let tx = stAnnTx ^. txStAnnTxG
    case tx ^. isValidTxL of
      IsValid True -> conwayEvalScriptsTxValid
      IsValid False -> do
        babbageEvalScriptsTxInvalid @era pp tx utxo

conwayEvalScriptsTxValid ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXOS" era) ~ StAnnTx TopTx era
  , STS (EraRule "UTXOS" era)
  , State (EraRule "UTXOS" era) ~ ()
  , Environment (EraRule "UTXOS" era) ~ ConwayUtxosEnv era
  , InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure era
  , BaseM (EraRule "UTXOS" era) ~ ShelleyBase
  , InjectRuleEvent "UTXOS" AlonzoUtxosEvent era
  ) =>
  TransitionRule (EraRule "UTXOS" era)
conwayEvalScriptsTxValid = do
  TRC (ConwayUtxosEnv pp utxo, (), stAnnTx) <- judgmentContext
  let tx = stAnnTx ^. txStAnnTxG

  () <- pure $! Debug.traceEvent validBegin ()
  expectScriptsToPass pp tx utxo
  pure $! Debug.traceEvent validEnd ()
