{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Ledger.Babbage.Rules.Utxow where

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Alonzo.Rules.Utxo (FeeNeeds)
import Cardano.Ledger.Alonzo.Rules.Utxow
  ( AlonzoEvent (..),
    AlonzoPredFail (..),
    AlonzoUtxowNeeds,
    UtxowDeltaA (UtxowDeltaA),
    genericAlonzoUtxow,
    witsVKeyNeeded,
  )
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo (Script)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness, nullRedeemers, unTxDats)
import Cardano.Ledger.Babbage.Collateral (collBalance, isTwoPhaseScriptAddress, minCollateral)
import Cardano.Ledger.Babbage.PParams
  ( PParams,
    PParamsUpdate,
    _coinsPerUTxOWord,
    _maxCollateralInputs,
    _maxTxExUnits,
    _maxTxSize,
    _maxValSize,
    _minfeeA,
    _minfeeB,
    _prices,
  )
import Cardano.Ledger.Babbage.Rules.Utxo
  ( BabbageUTXO,
    BabbageUtxoPred (FromAlonzoUtxoFail, FromAlonzoUtxowFail),
  )
import Cardano.Ledger.Babbage.Rules.Utxos (BabbageUTXOS)
import Cardano.Ledger.Babbage.Scripts (languages, txscripts)
import Cardano.Ledger.Babbage.Tx (DataHash, ValidatedTx (..), wits)
import Cardano.Ledger.Babbage.TxBody
  ( TxBody (..),
    TxOut,
    collateralInputs',
    collateralReturn',
    mint',
    referenceInputs',
    spendingInputs',
    totalCollateral',
    txfee',
    txnetworkid',
    vldt',
  )
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (..))
import Cardano.Ledger.Keys (DSignable, GenDelegPair (..), GenDelegs (..), Hash, KeyHash, KeyRole (..), asWitness, coerceKeyRole)
import Cardano.Ledger.Mary.Value (Value)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..), UtxoPredicateFailure (..))
import Cardano.Ledger.Shelley.Rules.Utxow
  ( PredicateFailure,
    ShelleyUtxowNeeds,
    UtxowDeltaS (UtxowDeltaS),
    UtxowEvent (UtxoEvent),
    UtxowPredicateFailure (..),
    genericShelleyUtxow,
    shelleyUtxowDelta,
  )
import Cardano.Ledger.Shelley.TxBody (EraIndependentTxBody, unWdrl)
import Control.State.Transition.Extended
  ( Embed (..),
    Rule,
    RuleType (Transition),
    STS (..),
    TRC (..),
    TransitionRule,
    failBecause,
    judgmentContext,
    liftSTS,
    trans,
    (?!),
    (?!:),
  )
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import GHC.Records (HasField (..))

-- ==========================================================

-- | The uninhabited type that marks the Babbage UTxO rule
data BabbageUTXOW era

instance
  forall era.
  ( Era era,
    Core.TxBody era ~ TxBody era,
    Core.Tx era ~ ValidatedTx era,
    Core.PParams era ~ PParams era,
    Core.Witnesses era ~ TxWitness era,
    Core.Script era ~ Alonzo.Script era,
    Core.TxOut era ~ TxOut era,
    Core.Value era ~ Value (Crypto era),
    Core.PParamsDelta era ~ PParamsUpdate era,
    --
    Signal (Core.EraRule "UTXO" era) ~ ValidatedTx era,
    Environment (Core.EraRule "UTXO" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXO" era) ~ UTxOState era,
    Embed (Core.EraRule "UTXO" era) (BabbageUTXOW era),
    --
    Show (PredicateFailure (Core.EraRule "UTXO" era)),
    Eq (PredicateFailure (Core.EraRule "UTXO" era)),
    Show (PredicateFailure (Core.EraRule "UTXOS" era)),
    Eq (PredicateFailure (Core.EraRule "UTXOS" era)),
    --
    AlonzoUtxowNeeds era,
    ShelleyUtxowNeeds era
  ) =>
  STS (BabbageUTXOW era)
  where
  type State (BabbageUTXOW era) = UTxOState era
  type Signal (BabbageUTXOW era) = ValidatedTx era
  type Environment (BabbageUTXOW era) = UtxoEnv era
  type BaseM (BabbageUTXOW era) = ShelleyBase
  type PredicateFailure (BabbageUTXOW era) = BabbageUtxoPred era
  type Event (BabbageUTXOW era) = AlonzoEvent era
  transitionRules = [genericAlonzoUtxow babbageUtxowDeltaA]
  initialRules = []

instance
  ( ValidateScript era,
    Core.Tx era ~ ValidatedTx era,
    Core.Witnesses era ~ TxWitness era,
    Core.TxBody era ~ TxBody era,
    Core.TxOut era ~ TxOut era,
    Core.PParams era ~ PParams era,
    Core.Value era ~ Value (Crypto era),
    --
    Signal (Core.EraRule "UTXOS" era) ~ ValidatedTx era,
    Environment (Core.EraRule "UTXOS" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOS" era) ~ UTxOState era,
    Embed (Core.EraRule "UTXOS" era) (BabbageUTXO era),
    Core.EraRule "UTXO" era ~ BabbageUTXO era,
    --
    Show (Core.PParamsDelta era),
    Show (Core.Script era),
    Eq (Core.Script era),
    --
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    FeeNeeds era,
    AlonzoUtxowNeeds era
  ) =>
  Embed (BabbageUTXO era) (BabbageUTXOW era)
  where
  wrapFailed = id
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent

-- =======================================================
-- Specializing utxow to the babbage era

babbageUtxowDeltaS ::
  forall era.
  ( Era era,
    Core.TxBody era ~ TxBody era,
    Core.Witnesses era ~ TxWitness era,
    Ord (Core.Script era)
  ) =>
  UtxowDeltaS BabbageUTXOW era
babbageUtxowDeltaS =
  UtxowDeltaS
    witsVKeyNeeded -- This is the Babbage version
    (FromAlonzoUtxowFail . WrappedShelleyEraFailure)
    txscripts -- This is the Babbage version
    (referenceInputs' @era . getField @"body")

babbageUtxowDeltaA ::
  ( ValidateScript era,
    Core.Witnesses era ~ TxWitness era,
    Core.TxBody era ~ TxBody era,
    Core.Script era ~ Alonzo.Script era,
    Ord (Core.Script era)
  ) =>
  UtxowDeltaA BabbageUTXOW era
babbageUtxowDeltaA = UtxowDeltaA languages isTwoPhaseScriptAddress babbageUtxowDeltaS FromAlonzoUtxowFail
