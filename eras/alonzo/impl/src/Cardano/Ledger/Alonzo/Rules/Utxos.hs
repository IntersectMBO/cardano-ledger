{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
    lbl2Phase,
    TagMismatchDescription (..),
    validBegin,
    validEnd,
    invalidBegin,
    invalidEnd,
    UtxosEvent (..),
    when2Phase,
    ConcreteAlonzo,
    FailureDescription (..),
    scriptFailuresToPredicateFailure,
    scriptFailuresToPlutusDebug,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), serialize')
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import Cardano.Ledger.Alonzo.PlutusScriptApi
  ( CollectError (..),
    collectTwoPhaseScriptInputs,
    evalScripts,
  )
import Cardano.Ledger.Alonzo.Scripts (CostModels, Script)
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ValidatedTx (..))
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.Alonzo.TxInfo
  ( ExtendedUTxO (..),
    PlutusDebug,
    ScriptFailure (..),
    ScriptResult (..),
  )
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import Cardano.Ledger.BaseTypes
  ( Globals,
    ProtVer,
    ShelleyBase,
    epochInfo,
    strictMaybeToMaybe,
    systemStart,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript)
import Cardano.Ledger.Mary.Value (Value)
import Cardano.Ledger.Rules.ValidationMode (Inject (..), lblStatic)
import Cardano.Ledger.Shelley.LedgerState
  ( PPUPState (..),
    UTxOState (..),
    keyRefunds,
    updateStakeDistribution,
  )
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules.Ppup (PPUP, PPUPEnv (..), PpupPredicateFailure)
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..), updateUTxOState)
import Cardano.Ledger.Shelley.TxBody (DCert, Wdrl)
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance, totalDeposits)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val as Val (Val (coin, (<->)))
import Cardano.Slotting.EpochInfo.Extend (unsafeLinearExtendEpochInfo)
import Cardano.Slotting.Slot (SlotNo)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Control.State.Transition.Extended
import Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Coders
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.MapExtras (extractKeys)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Text (Text)
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

data UtxosEvent era
  = AlonzoPpupToUtxosEvent (Event (Core.EraRule "PPUP" era))
  | SuccessfulPlutusScriptsEvent (NonEmpty PlutusDebug)
  | FailedPlutusScriptsEvent (NonEmpty PlutusDebug)

instance
  ( Era era,
    STS (PPUP era),
    PredicateFailure (Core.EraRule "PPUP" era) ~ PpupPredicateFailure era,
    Event (Core.EraRule "PPUP" era) ~ Event (PPUP era)
  ) =>
  Embed (PPUP era) (UTXOS era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = AlonzoPpupToUtxosEvent

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

scriptsTransition ::
  ( STS sts,
    Monad m,
    ExtendedUTxO era,
    ValidateScript era,
    HasField "_costmdls" (Core.PParams era) CostModels,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "vldt" (Core.TxBody era) ValidityInterval,
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "wits" (Core.Tx era) (Alonzo.TxWitness era),
    BaseM sts ~ ReaderT Globals m,
    PredicateFailure sts ~ UtxosPredicateFailure era,
    Core.Script era ~ Script era
  ) =>
  SlotNo ->
  Core.PParams era ->
  Core.Tx era ->
  UTxO era ->
  (ScriptResult -> Rule sts ctx ()) ->
  Rule sts ctx ()
scriptsTransition slot pp tx utxo action = do
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo
  case collectTwoPhaseScriptInputs (unsafeLinearExtendEpochInfo slot ei) sysSt pp tx utxo of
    Right sLst ->
      when2Phase $ action $ evalScripts (getField @"_protocolVersion" pp) tx sLst
    Left info
      | alonzoFailures <- filter isNotBadTranslation info,
        not (null alonzoFailures) ->
          failBecause (CollectErrors alonzoFailures)
      | otherwise -> pure ()
  where
    -- BadTranslation was introduced in Babbage, thus we need to filter those failures out.
    isNotBadTranslation = \case
      BadTranslation {} -> False
      _ -> True

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

  () <- pure $! traceEvent validBegin ()

  scriptsTransition slot pp tx utxo $ \case
    Fails _ps fs ->
      failBecause $
        ValidationTagMismatch
          (getField @"isValid" tx)
          (FailedUnexpectedly (scriptFailuresToPredicateFailure fs))
    Passes ps -> mapM_ (tellEvent . SuccessfulPlutusScriptsEvent) (nonEmpty ps)

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
  TRC (UtxoEnv slot pp _ _, us@(UTxOState utxo _ fees _ _), tx) <- judgmentContext
  let txb = body tx

  let !_ = traceEvent invalidBegin ()

  scriptsTransition slot pp tx utxo $ \case
    Passes _ps ->
      failBecause $
        ValidationTagMismatch (getField @"isValid" tx) PassedUnexpectedly
    Fails ps fs -> do
      mapM_ (tellEvent . SuccessfulPlutusScriptsEvent) (nonEmpty ps)
      tellEvent (FailedPlutusScriptsEvent (scriptFailuresToPlutusDebug fs))

  let !_ = traceEvent invalidEnd ()

      {- utxoKeep = getField @"collateral" txb ⋪ utxo -}
      {- utxoDel  = getField @"collateral" txb ◁ utxo -}
      !(utxoKeep, utxoDel) = extractKeys (unUTxO utxo) (getField @"collateral" txb)
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

data FailureDescription
  = PlutusFailure Text BS.ByteString
  deriving (Show, Eq, Generic, NoThunks)

instance ToCBOR FailureDescription where
  -- This strange encoding results from the fact that 'FailureDescription'
  -- used to have another constructor, which used key 0.
  -- We must maintain the original serialization in order to not disrupt
  -- the node-to-client protocol of the cardano node.
  toCBOR (PlutusFailure s b) = encode $ Sum PlutusFailure 1 !> To s !> To b

instance FromCBOR FailureDescription where
  fromCBOR = decode (Summands "FailureDescription" dec)
    where
      -- Note the lack of key 0. See the ToCBOR instance above for an explanation.
      dec 1 = SumD PlutusFailure <! From <! From
      dec n = Invalid n

scriptFailureToFailureDescription :: ScriptFailure -> FailureDescription
scriptFailureToFailureDescription (PlutusSF t pd) =
  PlutusFailure t (B64.encode $ serialize' pd)

scriptFailuresToPredicateFailure :: NonEmpty ScriptFailure -> NonEmpty FailureDescription
scriptFailuresToPredicateFailure = fmap scriptFailureToFailureDescription

scriptFailuresToPlutusDebug :: NonEmpty ScriptFailure -> NonEmpty PlutusDebug
scriptFailuresToPlutusDebug = fmap (\(PlutusSF _ pdb) -> pdb)

data TagMismatchDescription
  = PassedUnexpectedly
  | FailedUnexpectedly (NonEmpty FailureDescription)
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
  ( Show (Shelley.UTxOState era),
    Show (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  Show (UtxosPredicateFailure era)

instance
  ( Eq (Shelley.UTxOState era),
    Eq (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  Eq (UtxosPredicateFailure era)
  where
  (ValidationTagMismatch a x) == (ValidationTagMismatch b y) = a == b && x == y
  (CollectErrors x) == (CollectErrors y) = x == y
  (UpdateFailure x) == (UpdateFailure y) = x == y
  _ == _ = False

instance
  ( NoThunks (Shelley.UTxOState era),
    NoThunks (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  NoThunks (UtxosPredicateFailure era)

--------------------------------------------------------------------------------
-- 2-phase checks
--------------------------------------------------------------------------------

-- $2-phase
--
-- Above and beyond 'static' checks (see 'Cardano.Ledger.Rules.ValidateMode') we
-- additionally label 2-phase checks. This is to support a workflow where we
-- validate a 'ValidatedTx'. We would like to trust the flag we have ourselves just
-- computed rather than re-calculating it. However, all other checks should be
-- computed as normal.

-- | Indicates that this check depends only upon the signal to the transition,
-- not the state or environment.
lbl2Phase :: Label
lbl2Phase = "2phase"

-- | Construct a 2-phase predicate check.
--
--   Note that 2-phase predicate checks are by definition static.
when2Phase :: Rule sts ctx () -> Rule sts ctx ()
when2Phase = labeled $ lblStatic NE.:| [lbl2Phase]

-- =========================================================
-- Inject instances

instance
  PredicateFailure (Core.EraRule "PPUP" era) ~ PpupPredicateFailure era =>
  Inject (PpupPredicateFailure era) (UtxosPredicateFailure era)
  where
  inject = UpdateFailure

instance Inject (UtxosPredicateFailure era) (UtxosPredicateFailure era) where
  inject = id
