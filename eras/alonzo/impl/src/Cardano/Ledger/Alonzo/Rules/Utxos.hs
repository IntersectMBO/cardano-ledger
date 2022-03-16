{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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

module Cardano.Ledger.Alonzo.Rules.Utxos
  ( UTXOS,
    UtxosPredicateFailure (..),
    constructValidated,
    lbl2Phase,
    TagMismatchDescription (..),
    validBegin,
    validEnd,
    invalidBegin,
    invalidEnd,
    UtxosEvent (..),
    (?!##),
    ConcreteAlonzo,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import Cardano.Ledger.Alonzo.PlutusScriptApi
  ( CollectError,
    collectTwoPhaseScriptInputs,
    evalScripts,
  )
import Cardano.Ledger.Alonzo.Scripts (CostModels, Script)
import Cardano.Ledger.Alonzo.Tx
  ( DataHash,
    IsValid (..),
    ValidatedTx (..),
  )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..), FailureDescription (..), ScriptResult (..))
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import Cardano.Ledger.BaseTypes
  ( Globals,
    ProtVer,
    ShelleyBase,
    StrictMaybe (..),
    epochInfo,
    strictMaybeToMaybe,
    systemStart,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript)
import Cardano.Ledger.Mary.Value (Value)
import Cardano.Ledger.Rules.ValidationMode (Inject (..), lblStatic)
import Cardano.Ledger.Shelley.LedgerState (PPUPState (..), UTxOState (..), keyRefunds, updateStakeDistribution)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules.Ppup (PPUP, PPUPEnv (..), PpupPredicateFailure)
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..), updateUTxOState)
import Cardano.Ledger.Shelley.TxBody (DCert, Wdrl)
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance, totalDeposits)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val as Val
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.Coders
import qualified Data.Compact.SplitMap as SplitMap
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Debug.Trace (traceEvent)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks)

--------------------------------------------------------------------------------
-- The UTXOS transition system
--------------------------------------------------------------------------------

type ConcreteAlonzo era =
  ( Core.Script era ~ Script era,
    Core.Value era ~ Value (Crypto era),
    Core.TxBody era ~ Alonzo.TxBody era,
    Core.PParams era ~ Alonzo.PParams era,
    Core.PParamsDelta era ~ Alonzo.PParamsUpdate era,
    Core.TxOut era ~ Alonzo.TxOut era,
    Core.Witnesses era ~ Alonzo.TxWitness era,
    Core.Tx era ~ ValidatedTx era
  )

data UTXOS era

instance
  forall era.
  ( Era era,
    ConcreteAlonzo era,
    ExtendedUTxO era,
    Embed (Core.EraRule "PPUP" era) (UTXOS era),
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era),
    ValidateScript era,
    ToCBOR (PredicateFailure (Core.EraRule "PPUP" era)) -- Serializing the PredicateFailure
  ) =>
  STS (UTXOS era)
  where
  type BaseM (UTXOS era) = ShelleyBase
  type Environment (UTXOS era) = UtxoEnv era
  type State (UTXOS era) = UTxOState era
  type Signal (UTXOS era) = ValidatedTx era
  type PredicateFailure (UTXOS era) = UtxosPredicateFailure era
  type Event (UTXOS era) = UtxosEvent era
  transitionRules = [utxosTransition]

newtype UtxosEvent era
  = UpdateEvent (Event (Core.EraRule "PPUP" era))

instance
  ( Era era,
    STS (PPUP era),
    PredicateFailure (Core.EraRule "PPUP" era) ~ PpupPredicateFailure era,
    Event (Core.EraRule "PPUP" era) ~ Event (PPUP era)
  ) =>
  Embed (PPUP era) (UTXOS era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = UpdateEvent

utxosTransition ::
  forall era.
  ( ConcreteAlonzo era,
    ExtendedUTxO era,
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era),
    Embed (Core.EraRule "PPUP" era) (UTXOS era),
    ValidateScript era,
    ToCBOR (PredicateFailure (Core.EraRule "PPUP" era)) -- Serializing the PredicateFailure
  ) =>
  TransitionRule (UTXOS era)
utxosTransition =
  judgmentContext >>= \(TRC (_, _, tx)) -> do
    case getField @"isValid" tx of
      IsValid True -> scriptsValidateTransition
      IsValid False -> scriptsNotValidateTransition

-- ===================================================================

scriptsValidateTransition ::
  forall era.
  ( ValidateScript era,
    ConcreteAlonzo era,
    ExtendedUTxO era,
    STS (UTXOS era),
    Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Signal (Core.EraRule "PPUP" era) ~ Maybe (Update era),
    Embed (Core.EraRule "PPUP" era) (UTXOS era)
  ) =>
  TransitionRule (UTXOS era)
scriptsValidateTransition = do
  TRC (UtxoEnv slot pp poolParams genDelegs, u@(UTxOState utxo _ _ pup _), tx) <-
    judgmentContext
  let txb = body tx
      refunded = keyRefunds pp txb
      txcerts = toList $ getField @"certs" txb
      depositChange =
        totalDeposits pp (`Map.notMember` poolParams) txcerts <-> refunded
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  () <- pure $! traceEvent validBegin ()

  case collectTwoPhaseScriptInputs ei sysSt pp tx utxo of
    Right sLst ->
      case evalScripts @era (getField @"_protocolVersion" pp) tx sLst of
        Fails sss ->
          False
            ?!## ValidationTagMismatch
              (getField @"isValid" tx)
              (FailedUnexpectedly sss)
        Passes -> pure ()
    Left info -> failBecause (CollectErrors info)

  () <- pure $! traceEvent validEnd ()

  ppup' <-
    trans @(Core.EraRule "PPUP" era) $
      TRC
        (PPUPEnv slot pp genDelegs, pup, strictMaybeToMaybe $ getField @"update" txb)

  pure $! updateUTxOState u txb depositChange ppup'

scriptsNotValidateTransition ::
  forall era.
  ( ValidateScript era,
    ConcreteAlonzo era,
    ExtendedUTxO era,
    STS (UTXOS era)
  ) =>
  TransitionRule (UTXOS era)
scriptsNotValidateTransition = do
  TRC (UtxoEnv _ pp _ _, us@(UTxOState utxo _ fees _ _), tx) <- judgmentContext
  let txb = body tx
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  let !_ = traceEvent invalidBegin ()

  case collectTwoPhaseScriptInputs ei sysSt pp tx utxo of
    Right sLst ->
      whenFailureFree $
        case evalScripts @era (getField @"_protocolVersion" pp) tx sLst of
          Passes -> False ?!## ValidationTagMismatch (getField @"isValid" tx) PassedUnexpectedly
          Fails _sss -> pure ()
    Left info -> failBecause (CollectErrors info)

  let !_ = traceEvent invalidEnd ()

      {- utxoKeep = getField @"collateral" txb ⋪ utxo -}
      {- utxoDel  = getField @"collateral" txb ◁ utxo -}
      !(!utxoKeep, !utxoDel) =
        SplitMap.extractKeysSet (unUTxO utxo) (getField @"collateral" txb)
  pure
    $! us
      { _utxo = UTxO utxoKeep,
        _fees = fees <> Val.coin (balance (UTxO utxoDel)),
        _stakeDistro = updateStakeDistribution (_stakeDistro us) (UTxO utxoDel) mempty
      }

-- =======================================
-- Names for the events we will tell

validBegin, validEnd, invalidBegin, invalidEnd :: String
validBegin = intercalate "," ["[LEDGER][SCRIPTS_VALIDATION]", "BEGIN"]
validEnd = intercalate "," ["[LEDGER][SCRIPTS_VALIDATION]", "END"]
invalidBegin = intercalate "," ["[LEDGER][SCRIPTS_NOT_VALIDATE_TRANSITION]", "BEGIN"]
invalidEnd = intercalate "," ["[LEDGER][SCRIPTS_NOT_VALIDATE_TRANSITION]", "END"]

-- =============================================
-- PredicateFailure data type for UTXOS

data TagMismatchDescription
  = PassedUnexpectedly
  | FailedUnexpectedly [FailureDescription]
  deriving (Show, Eq, Generic, NoThunks)

instance ToCBOR TagMismatchDescription where
  toCBOR PassedUnexpectedly = encode (Sum PassedUnexpectedly 0)
  toCBOR (FailedUnexpectedly fs) = encode (Sum FailedUnexpectedly 1 !> To fs)

instance FromCBOR TagMismatchDescription where
  fromCBOR = decode (Summands "TagMismatchDescription" dec)
    where
      dec 0 = SumD PassedUnexpectedly
      dec 1 = SumD FailedUnexpectedly <! From
      dec n = Invalid n

data UtxosPredicateFailure era
  = -- | The 'isValid' tag on the transaction is incorrect. The tag given
    --   here is that provided on the transaction (whereas evaluation of the
    --   scripts gives the opposite.). The Text tries to explain why it failed.
    ValidationTagMismatch IsValid TagMismatchDescription
  | -- | We could not find all the necessary inputs for a Plutus Script.
    --         Previous PredicateFailure tests should make this impossible, but the
    --         consequences of not detecting this means scripts get dropped, so things
    --         might validate that shouldn't. So we double check in the function
    --         collectTwoPhaseScriptInputs, it should find data for every Script.
    CollectErrors [CollectError (Crypto era)]
  | UpdateFailure (PredicateFailure (Core.EraRule "PPUP" era))
  deriving
    (Generic)

instance
  ( Era era,
    ToCBOR (PredicateFailure (Core.EraRule "PPUP" era)),
    Show (Core.TxOut era)
  ) =>
  ToCBOR (UtxosPredicateFailure era)
  where
  toCBOR (ValidationTagMismatch v descr) = encode (Sum ValidationTagMismatch 0 !> To v !> To descr)
  toCBOR (CollectErrors cs) =
    encode (Sum (CollectErrors @era) 1 !> To cs)
  toCBOR (UpdateFailure pf) = encode (Sum (UpdateFailure @era) 2 !> To pf)

instance
  ( Era era,
    FromCBOR (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  FromCBOR (UtxosPredicateFailure era)
  where
  fromCBOR = decode (Summands "UtxosPredicateFailure" dec)
    where
      dec 0 = SumD ValidationTagMismatch <! From <! From
      dec 1 = SumD (CollectErrors @era) <! From
      dec 2 = SumD UpdateFailure <! From
      dec n = Invalid n

deriving stock instance
  ( Shelley.TransUTxOState Show era,
    Show (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  Show (UtxosPredicateFailure era)

instance
  ( Shelley.TransUTxOState Eq era,
    Eq (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  Eq (UtxosPredicateFailure era)
  where
  (ValidationTagMismatch a x) == (ValidationTagMismatch b y) = a == b && x == y
  (CollectErrors x) == (CollectErrors y) = x == y
  (UpdateFailure x) == (UpdateFailure y) = x == y
  _ == _ = False

instance
  ( Shelley.TransUTxOState NoThunks era,
    NoThunks (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  NoThunks (UtxosPredicateFailure era)

-- =================================================================

-- | Construct a 'ValidatedTx' from a 'Core.Tx' by setting the `IsValid`
-- flag.
--
-- Note that this simply constructs the transaction; it does not validate
-- anything other than the scripts. Thus the resulting transaction may be
-- completely invalid.
constructValidated ::
  forall era m.
  ( MonadError [UtxosPredicateFailure era] m,
    Core.Script era ~ Script era,
    Core.TxOut era ~ Alonzo.TxOut era,
    Core.Witnesses era ~ Alonzo.TxWitness era,
    ValidateScript era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "_costmdls" (Core.PParams era) CostModels,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "vldt" (Core.TxBody era) ValidityInterval,
    ExtendedUTxO era
  ) =>
  Globals ->
  UtxoEnv era ->
  UTxOState era ->
  Core.Tx era ->
  m (ValidatedTx era)
constructValidated globals (UtxoEnv _ pp _ _) st tx =
  case collectTwoPhaseScriptInputs ei sysS pp tx utxo of
    Left errs -> throwError [CollectErrors errs]
    Right sLst ->
      let scriptEvalResult = evalScripts @era (getField @"_protocolVersion" pp) tx sLst
          vTx =
            ValidatedTx
              (getField @"body" tx)
              (getField @"wits" tx)
              (IsValid (lift scriptEvalResult))
              (getField @"auxiliaryData" tx)
       in pure vTx
  where
    utxo = _utxo st
    sysS = systemStart globals
    ei = epochInfo globals
    lift Passes = True -- Convert a ScriptResult into a Bool
    lift (Fails _) = False

--------------------------------------------------------------------------------
-- 2-phase checks
--------------------------------------------------------------------------------

-- $2-phase
--
-- Above and beyond 'static' checks (see 'Cardano.Ledger.Rules.ValidateMode') we
-- additionally label 2-phase checks. This is to support a workflow where we
-- validate a 'ValidatedTx' directly after constructing it with
-- 'constructValidated' - we would like to trust the flag we have ourselves just
-- computed rather than re-calculating it. However, all other checks should be
-- computed as normal.

-- | Indicates that this check depends only upon the signal to the transition,
-- not the state or environment.
lbl2Phase :: Label
lbl2Phase = "2phase"

-- | Construct a 2-phase predicate check.
--
--   Note that 2-phase predicate checks are by definition static.
(?!##) :: Bool -> PredicateFailure sts -> Rule sts ctx ()
(?!##) = labeledPred [lblStatic, lbl2Phase]

infix 1 ?!##

-- =========================================================
-- Inject instances

instance
  PredicateFailure (Core.EraRule "PPUP" era) ~ PpupPredicateFailure era =>
  Inject (PpupPredicateFailure era) (UtxosPredicateFailure era)
  where
  inject = UpdateFailure

instance Inject (UtxosPredicateFailure era) (UtxosPredicateFailure era) where
  inject = id
