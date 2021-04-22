{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Rules.Utxos where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Alonzo.Language (Language)
import Cardano.Ledger.Alonzo.PlutusScriptApi
  ( CollectError,
    collectTwoPhaseScriptInputs,
    evalScripts,
  )
import Cardano.Ledger.Alonzo.Scripts (Script)
import Cardano.Ledger.Alonzo.Tx
  ( CostModel,
    DataHash,
    IsValidating (..),
    ValidatedTx (..),
    txbody,
    txins,
    txouts,
  )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Mary.Value (Value)
import Cardano.Ledger.Shelley.Constraints (PParamsDelta)
import qualified Cardano.Ledger.Val as Val
import Control.Iterate.SetAlgebra (eval, (∪), (⋪), (◁))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
import Data.Coders
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.BaseTypes
  ( Globals,
    ShelleyBase,
    StrictMaybe (..),
    strictMaybeToMaybe,
  )
import Shelley.Spec.Ledger.LedgerState (PPUPState (..), UTxOState (..), keyRefunds)
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import Shelley.Spec.Ledger.PParams (Update)
import Shelley.Spec.Ledger.STS.Ppup (PPUP, PPUPEnv (..), PpupPredicateFailure)
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv (..))
import Shelley.Spec.Ledger.TxBody (DCert, TxIn (..), Wdrl)
import Shelley.Spec.Ledger.UTxO (balance, totalDeposits)

--------------------------------------------------------------------------------
-- The UTXOS transition system
--------------------------------------------------------------------------------

data UTXOS era

instance
  forall era.
  ( Era era,
    Eq (Core.PParams era),
    Show (Core.PParams era),
    Show (PParamsDelta era),
    Eq (PParamsDelta era),
    Embed (Core.EraRule "PPUP" era) (UTXOS era),
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era),
    Core.Script era ~ Script era,
    Core.Value era ~ Value (Crypto era),
    Core.TxOut era ~ Alonzo.TxOut era,
    Core.TxBody era ~ Alonzo.TxBody era,
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "_costmdls" (Core.PParams era) (Map.Map Language CostModel),
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era)))
  ) =>
  STS (UTXOS era)
  where
  type BaseM (UTXOS era) = ShelleyBase
  type Environment (UTXOS era) = UtxoEnv era
  type State (UTXOS era) = UTxOState era
  type Signal (UTXOS era) = ValidatedTx era
  type PredicateFailure (UTXOS era) = UtxosPredicateFailure era

  transitionRules = [utxosTransition]

utxosTransition ::
  forall era.
  ( Era era,
    Core.Script era ~ Script era,
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era),
    Embed (Core.EraRule "PPUP" era) (UTXOS era),
    Core.TxOut era ~ Alonzo.TxOut era,
    Core.Value era ~ Value (Crypto era),
    Core.TxBody era ~ Alonzo.TxBody era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "_costmdls" (Core.PParams era) (Map.Map Language CostModel),
    HasField "txinputs_fee" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  TransitionRule (UTXOS era)
utxosTransition =
  judgmentContext >>= \(TRC (UtxoEnv _ pp _ _, st@(UTxOState utxo _ _ _), tx)) -> do
    case collectTwoPhaseScriptInputs pp tx utxo of
      Right sLst ->
        let scriptEvalResult = evalScripts @era tx sLst
         in if scriptEvalResult
              then scriptsValidateTransition
              else scriptsNotValidateTransition
      Left info -> (failBecause $ ShouldNeverHappenScriptInputsNotFound info) >> pure st

scriptsValidateTransition ::
  forall era.
  ( Show (Core.Value era), -- Arises because of the use of (∪) from SetAlgebra, needs Show to report problems.
    Era era,
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era),
    Embed (Core.EraRule "PPUP" era) (UTXOS era),
    Core.TxOut era ~ Alonzo.TxOut era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "txinputs_fee" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin
  ) =>
  TransitionRule (UTXOS era)
scriptsValidateTransition = do
  TRC
    ( UtxoEnv slot pp poolParams genDelegs,
      UTxOState utxo deposited fees pup,
      tx
      ) <-
    judgmentContext
  let txb = txbody tx
      refunded = keyRefunds pp txb
      depositChange =
        ( totalDeposits
            pp
            poolParams
            (toList $ getField @"certs" txb)
        )
          Val.<-> refunded
  getField @"isValidating" tx == IsValidating True
    ?! ValidationTagMismatch (getField @"isValidating" tx)
  pup' <-
    trans @(Core.EraRule "PPUP" era) $
      TRC
        (PPUPEnv slot pp genDelegs, pup, strictMaybeToMaybe $ getField @"update" txb)
  pure $
    UTxOState
      { _utxo = eval ((txins @era txb ⋪ utxo) ∪ txouts @era txb),
        _deposited = deposited <> depositChange,
        _fees = fees <> getField @"txfee" txb,
        _ppups = pup'
      }

scriptsNotValidateTransition ::
  forall era.
  ( Era era,
    HasField "txinputs_fee" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  TransitionRule (UTXOS era)
scriptsNotValidateTransition = do
  TRC (_, us@(UTxOState utxo _ fees _), tx) <- judgmentContext
  let txb = txbody tx
  getField @"isValidating" tx == IsValidating False
    ?! ValidationTagMismatch (getField @"isValidating" tx)
  pure $
    us
      { _utxo = eval (getField @"txinputs_fee" txb ⋪ utxo),
        _fees = fees <> Val.coin (balance @era (eval (getField @"txinputs_fee" txb ◁ utxo)))
      }

data UtxosPredicateFailure era
  = -- | The 'isValidating' tag on the transaction is incorrect. The tag given
    --   here is that provided on the transaction (whereas evaluation of the
    --   scripts gives the opposite.)
    ValidationTagMismatch IsValidating
  | -- | We could not find all the necessary inputs for a Plutus Script.
    --         Previous PredicateFailure tests should make this impossible, but the
    --         consequences of not detecting this means scripts get dropped, so things
    --         might validate that shouldn't. So we double check in the function
    --         collectTwoPhaseScriptInputs, it should find data for every Script.
    ShouldNeverHappenScriptInputsNotFound [CollectError (Crypto era)]
  | UpdateFailure (PredicateFailure (Core.EraRule "PPUP" era))
  deriving
    (Generic)

instance
  ( Era era,
    ToCBOR (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  ToCBOR (UtxosPredicateFailure era)
  where
  toCBOR (ValidationTagMismatch v) = encode (Sum ValidationTagMismatch 0 !> To v)
  toCBOR (ShouldNeverHappenScriptInputsNotFound cs) =
    encode (Sum (ShouldNeverHappenScriptInputsNotFound @era) 1 !> To cs)
  toCBOR (UpdateFailure pf) = encode (Sum (UpdateFailure @era) 2 !> To pf)

instance
  ( Era era,
    FromCBOR (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  FromCBOR (UtxosPredicateFailure era)
  where
  fromCBOR = decode (Summands "UtxosPredicateFailure" dec)
    where
      dec 0 = SumD ValidationTagMismatch <! From
      dec 1 = SumD (ShouldNeverHappenScriptInputsNotFound @era) <! From
      dec 2 = SumD UpdateFailure <! From
      dec n = Invalid n

deriving stock instance
  ( Shelley.TransUTxOState Show era,
    Show (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  Show (UtxosPredicateFailure era)

deriving stock instance
  ( Shelley.TransUTxOState Eq era,
    Eq (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  Eq (UtxosPredicateFailure era)

instance
  ( Shelley.TransUTxOState NoThunks era,
    NoThunks (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  NoThunks (UtxosPredicateFailure era)

instance
  ( Era era,
    STS (PPUP era),
    PredicateFailure (Core.EraRule "PPUP" era) ~ PpupPredicateFailure era
  ) =>
  Embed (PPUP era) (UTXOS era)
  where
  wrapFailed = UpdateFailure

-- =================================================================

constructValidated ::
  forall era m.
  ( MonadError [UtxosPredicateFailure era] m,
    Era era,
    Eq (Core.PParams era),
    Show (Core.PParams era),
    Show (PParamsDelta era),
    Eq (PParamsDelta era),
    ToCBOR (Core.AuxiliaryData era),
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era),
    Embed (Core.EraRule "PPUP" era) (UTXOS era),
    Core.Script era ~ Script era,
    Core.TxOut era ~ Alonzo.TxOut era,
    Core.Value era ~ Value (Crypto era),
    Core.TxBody era ~ Alonzo.TxBody era,
    Core.Witnesses era ~ Alonzo.TxWitness era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "_costmdls" (Core.PParams era) (Map.Map Language CostModel),
    HasField "txinputs_fee" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era))
  ) =>
  Globals ->
  UtxoEnv era ->
  UTxOState era ->
  Core.Tx era ->
  m (UTxOState era, ValidatedTx era)
constructValidated globals env@(UtxoEnv _ pp _ _) st tx =
  case collectTwoPhaseScriptInputs pp tx utxo of
    Left errs -> throwError [ShouldNeverHappenScriptInputsNotFound errs]
    Right sLst ->
      let scriptEvalResult = evalScripts @era tx sLst
          vTx =
            ValidatedTx
              (getField @"body" tx)
              (getField @"wits" tx)
              (IsValidating scriptEvalResult)
              (getField @"auxiliaryData" tx)
          (newState, errs) =
            flip runReader globals . runTransitionRule (TRC (env, st, vTx)) $
              if scriptEvalResult
                then scriptsValidateTransition
                else scriptsNotValidateTransition
       in case errs of
            [] -> pure (newState, vTx)
            _ -> throwError errs
  where
    runTransitionRule :: RuleInterpreter
    runTransitionRule = applyRuleInternal ValidateAll runSTS
    runSTS :: STSInterpreter
    runSTS = applySTSInternal AssertionsOff runTransitionRule
    utxo = _utxo st
