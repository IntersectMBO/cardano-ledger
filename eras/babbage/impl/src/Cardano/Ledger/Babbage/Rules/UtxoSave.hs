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

module Cardano.Ledger.Babbage.Rules.Utxo where

-- ==========================================================================
-- Utxo Depends on Utxos, which means we need to import types and operations
-- from the Utxo and Utxos files from Babbage and earlier Eras that can be reused

import Cardano.Ledger.Alonzo.Rules.Utxo
  ( AlonzoUtxoNeeds,
    FeeNeeds,
    UtxoDelta (UtxoDelta),
    UtxoEvent (..),
    UtxoPredicateFailure (..),
    genericAlonzoUtxo,
    vKeyLocked,
  )
import Cardano.Ledger.Alonzo.Rules.Utxos (UtxosPredicateFailure (..))
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoPredFail)
import Cardano.Ledger.Alonzo.Tx (minfee)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (..), nullRedeemers)
import Cardano.Ledger.Babbage.Collateral (collBalance, minCollateral)
import Cardano.Ledger.Babbage.Rules.Utxos (BabbageUTXOS)
import Cardano.Ledger.Babbage.Tx (ValidatedTx (..), wits)
import Cardano.Ledger.Babbage.TxBody
  ( TxBody (..),
    TxOut,
    collateralInputs',
    collateralReturn',
    totalCollateral',
    txfee',
  )
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (..))
import Cardano.Ledger.Mary.Value (Value)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Cardano.Ledger.Shelley.Rules.Utxo as Shelley (UtxoEnv (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (coin), adaOnly)
import Control.Monad (unless)
import Control.State.Transition.Extended
  ( Embed (..),
    Rule,
    RuleType (Transition),
    STS (..),
    (?!),
  )
import qualified Data.Compact.SplitMap as SplitMap
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import GHC.Records (HasField (..))

-- ========================================================

-- | The uninhabited type that marks the Babbage UTxO rule
data BabbageUTXO era

-- =============================================================

-- | Predicate failure for the Babbage Era
data BabbageUtxoPred era
  = FromAlonzoUtxoFail (UtxoPredicateFailure era)
  | FromAlonzoUtxowFail (AlonzoPredFail era)
  | UnequalCollateralReturn Coin Coin

deriving instance
  ( Era era,
    Show (UtxoPredicateFailure era),
    Show (PredicateFailure (Core.EraRule "UTXO" era)),
    Show (Core.Script era)
  ) =>
  Show (BabbageUtxoPred era)

deriving instance
  ( Era era,
    Eq (UtxoPredicateFailure era),
    Eq (PredicateFailure (Core.EraRule "UTXO" era)),
    Eq (Core.Script era)
  ) =>
  Eq (BabbageUtxoPred era)

-- TODO FIXME  add CBOR instances

-- ================================================================================

-- | feesOK can differ from Era to Era, as new notions of fees arise. This is the Babbage version
--   See: Figure 2: Functions related to fees and collateral, in the Babbage specification
--   In the spec feesOK is a boolean function. Because wee need to handle predicate failures
--   in the implementaion, it is coded as a TransitionRule. It will return () if it succeeds,
--   and raise an error (rather than return) if any of the required parts are False.
--   This version is generic in that it can be lifted to any PredicateFailure type that
--   embeds BabbageUtxoPred era. This makes it possibly useful in future Eras.
feesOK ::
  forall era utxo.
  ( Era era,
    ValidateScript era, -- isTwoPhaseScriptAddress
    Core.Tx era ~ ValidatedTx era,
    Core.Witnesses era ~ TxWitness era,
    Core.TxBody era ~ TxBody era,
    FeeNeeds era
  ) =>
  (BabbageUtxoPred era -> PredicateFailure (utxo era)) ->
  Core.PParams era ->
  Core.Tx era ->
  UTxO era ->
  Rule (utxo era) 'Transition ()
feesOK lift pp tx (UTxO utxo) = do
  let txb = getField @"body" tx
      theFee = txfee' txb -- Coin supplied to pay fees
      minimumFee = minfee @era pp tx
      lift2 = lift . FromAlonzoUtxoFail
  -- Part 1  (minfee pp tx ≤ txfee tx )
  (minimumFee <= theFee) ?! lift2 (FeeTooSmallUTxO minimumFee theFee)

  -- Part 2 (txrdmrs tx /= ◇ ⇒ ... )
  unless (nullRedeemers . txrdmrs' . wits $ tx) $ do
    -- Part 3 ((∀(a, , ) ∈ range (collInputs txb ◁ utxo), paymentHK a ∈ Addr^{vkey})
    let collateral = collateralInputs' txb SplitMap.◁ utxo
    -- UTxO restricted to the inputs allocated to pay the Fee
    all vKeyLocked collateral
      ?! lift2 (ScriptsNotPaidUTxO (UTxO (SplitMap.filter (not . vKeyLocked) collateral)))

    -- Part 4 (balance ≥ minCollateral tx pp)
    let valbalance = collBalance txb (UTxO utxo)
        balance = coin valbalance
    balance >= minCollateral txb pp ?! lift2 (InsufficientCollateral balance (minCollateral txb pp))

    -- Part 5 (adaOnly balance)
    adaOnly valbalance ?! lift2 (CollateralContainsNonADA valbalance)

    -- Part 6 ((txcoll tx 6 = 3) ⇒ balance = txcoll tx)
    case collateralReturn' txb of
      SNothing -> pure ()
      SJust _txout -> balance == total ?! lift (UnequalCollateralReturn balance total)
        where
          total = totalCollateral' txb

    -- Part 7 (collInputs tx 6 = ∅)
    not (Set.null (collateralInputs' txb)) ?! lift2 NoCollateralInputs

    pure ()

-- =============================================================
-- The STS BabbageUTXO instance

-- | The uninhabited type that marks the Babbage UTxO rule
data BabbageUTXO era

instance
  forall era.
  ( Era era,
    ValidateScript era,
    -- Concrete types assumptions for the Babbage Era
    Core.Tx era ~ ValidatedTx era,
    Core.TxBody era ~ TxBody era,
    Core.TxOut era ~ TxOut era,
    Core.Witnesses era ~ TxWitness era,
    Core.Value era ~ Value (Crypto era),
    Core.EraRule "UTXO" era ~ BabbageUTXO era,
    -- We can call the UTXOS rule
    Embed (Core.EraRule "UTXOS" era) (BabbageUTXO era),
    Signal (Core.EraRule "UTXOS" era) ~ ValidatedTx era,
    Environment (Core.EraRule "UTXOS" era) ~ Shelley.UtxoEnv era,
    State (Core.EraRule "UTXOS" era) ~ Shelley.UTxOState era,
    -- Properties needed to Show and compare the predicate failures
    Show (Core.Value era),
    Show (Core.PParamsDelta era),
    Show (Core.Script era),
    Eq (Core.Value era),
    Eq (Core.Script era),
    -- Substructural properties
    FeeNeeds era,
    AlonzoUtxoNeeds era
  ) =>
  STS (BabbageUTXO era)
  where
  type State (BabbageUTXO era) = Shelley.UTxOState era
  type Signal (BabbageUTXO era) = ValidatedTx era
  type Environment (BabbageUTXO era) = Shelley.UtxoEnv era
  type BaseM (BabbageUTXO era) = ShelleyBase
  type PredicateFailure (BabbageUTXO era) = BabbageUtxoPred era
  type Event (BabbageUTXO era) = UtxoEvent era

  initialRules = []
  transitionRules = [genericAlonzoUtxo babbageUtxoDelta]

instance
  ( Era era,
    STS (BabbageUTXOS era),
    PredicateFailure (Core.EraRule "UTXOS" era) ~ UtxosPredicateFailure era,
    Event (Core.EraRule "UTXOS" era) ~ Event (BabbageUTXOS era),
    BaseM (BabbageUTXOS era) ~ ShelleyBase,
    PredicateFailure (Core.EraRule "UTXOS" era) ~ PredicateFailure (BabbageUTXOS era),
    PredicateFailure (Core.EraRule "UTXOS" era) ~ PredicateFailure (BabbageUTXOS era)
  ) =>
  Embed (BabbageUTXOS era) (BabbageUTXO era)
  where
  wrapFailed = FromAlonzoUtxoFail . UtxosFailure
  wrapEvent = UtxosEvent

-- ====================================================
-- Specializing Utxo to Babbage Era

babbageUtxoDelta ::
  ( Core.Witnesses era ~ TxWitness era,
    Core.Tx era ~ ValidatedTx era,
    Core.TxBody era ~ TxBody era,
    ValidateScript era,
    FeeNeeds era
  ) =>
  UtxoDelta BabbageUTXO era
babbageUtxoDelta = UtxoDelta (feesOK id) (const (Set.empty)) FromAlonzoUtxoFail
