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
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Rules.Utxos
  ( AlonzoUTXOS,
    AlonzoUtxosPredFailure (..),
    lbl2Phase,
    TagMismatchDescription (..),
    validBegin,
    validEnd,
    invalidBegin,
    invalidEnd,
    AlonzoUtxosEvent (..),
    when2Phase,
    FailureDescription (..),
    scriptFailuresToPredicateFailure,
    scriptFailuresToPlutusDebug,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), serialize')
import Cardano.Ledger.Alonzo.Era (AlonzoUTXOS)
import Cardano.Ledger.Alonzo.PlutusScriptApi
  ( CollectError (..),
    collectTwoPhaseScriptInputs,
    evalScripts,
  )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript, CostModels)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (..), ShelleyEraTxBody (..))
import Cardano.Ledger.Alonzo.TxInfo
  ( ExtendedUTxO (..),
    PlutusDebug,
    ScriptFailure (..),
    ScriptResult (..),
  )
import Cardano.Ledger.Alonzo.TxWitness (TxWitness)
import Cardano.Ledger.BaseTypes
  ( Globals,
    ProtVer,
    ShelleyBase,
    epochInfo,
    strictMaybeToMaybe,
    systemStart,
  )
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Rules.ValidationMode (Inject (..), lblStatic)
import Cardano.Ledger.Shelley.LedgerState
  ( PPUPState (..),
    UTxOState (..),
    keyRefunds,
    updateStakeDistribution,
  )
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules.Ppup (ShelleyPPUP, ShelleyPPUPEnv (..), ShelleyPpupPredFailure)
import Cardano.Ledger.Shelley.Rules.Utxo (ShelleyUtxoEnv (..), updateUTxOState)
import Cardano.Ledger.Shelley.UTxO (UTxO (..), balance, totalDeposits)
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
import Data.Text (Text)
import Debug.Trace (traceEvent)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import Lens.Micro
import NoThunks.Class (NoThunks)

--------------------------------------------------------------------------------
-- The AlonzoUTXOS transition system
--------------------------------------------------------------------------------

instance
  forall era.
  ( AlonzoEraTx era,
    ExtendedUTxO era,
    TxWits era ~ TxWitness era,
    Script era ~ AlonzoScript era,
    Tx era ~ AlonzoTx era,
    Embed (EraRule "PPUP" era) (AlonzoUTXOS era),
    Environment (EraRule "PPUP" era) ~ ShelleyPPUPEnv era,
    State (EraRule "PPUP" era) ~ PPUPState era,
    Signal (EraRule "PPUP" era) ~ Maybe (Update era),
    HasField "_costmdls" (PParams era) CostModels,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin,
    HasField "_protocolVersion" (PParams era) ProtVer,
    ToCBOR (PredicateFailure (EraRule "PPUP" era)) -- Serializing the PredicateFailure
  ) =>
  STS (AlonzoUTXOS era)
  where
  type BaseM (AlonzoUTXOS era) = ShelleyBase
  type Environment (AlonzoUTXOS era) = ShelleyUtxoEnv era
  type State (AlonzoUTXOS era) = UTxOState era
  type Signal (AlonzoUTXOS era) = AlonzoTx era
  type PredicateFailure (AlonzoUTXOS era) = AlonzoUtxosPredFailure era
  type Event (AlonzoUTXOS era) = AlonzoUtxosEvent era
  transitionRules = [utxosTransition]

data AlonzoUtxosEvent era
  = AlonzoPpupToUtxosEvent (Event (EraRule "PPUP" era))
  | SuccessfulPlutusScriptsEvent (NonEmpty PlutusDebug)
  | FailedPlutusScriptsEvent (NonEmpty PlutusDebug)

instance
  ( Era era,
    STS (ShelleyPPUP era),
    PredicateFailure (EraRule "PPUP" era) ~ ShelleyPpupPredFailure era,
    Event (EraRule "PPUP" era) ~ Event (ShelleyPPUP era)
  ) =>
  Embed (ShelleyPPUP era) (AlonzoUTXOS era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = AlonzoPpupToUtxosEvent

utxosTransition ::
  forall era.
  ( ExtendedUTxO era,
    AlonzoEraTx era,
    Tx era ~ AlonzoTx era,
    Script era ~ AlonzoScript era,
    TxWits era ~ TxWitness era,
    Environment (EraRule "PPUP" era) ~ ShelleyPPUPEnv era,
    State (EraRule "PPUP" era) ~ PPUPState era,
    Signal (EraRule "PPUP" era) ~ Maybe (Update era),
    Embed (EraRule "PPUP" era) (AlonzoUTXOS era),
    HasField "_costmdls" (PParams era) CostModels,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin,
    HasField "_protocolVersion" (PParams era) ProtVer,
    ToCBOR (PredicateFailure (EraRule "PPUP" era)) -- Serializing the PredicateFailure
  ) =>
  TransitionRule (AlonzoUTXOS era)
utxosTransition =
  judgmentContext >>= \(TRC (_, _, tx)) -> do
    case tx ^. isValidTxL of
      IsValid True -> scriptsValidateTransition
      IsValid False -> scriptsNotValidateTransition

-- ===================================================================

scriptsTransition ::
  ( STS sts,
    Monad m,
    EraTx era,
    ShelleyEraTxBody era,
    ExtendedUTxO era,
    TxWits era ~ TxWitness era,
    HasField "_costmdls" (PParams era) CostModels,
    HasField "_protocolVersion" (PParams era) ProtVer,
    BaseM sts ~ ReaderT Globals m,
    PredicateFailure sts ~ AlonzoUtxosPredFailure era,
    Script era ~ AlonzoScript era
  ) =>
  SlotNo ->
  PParams era ->
  Tx era ->
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
  ( AlonzoEraTx era,
    ExtendedUTxO era,
    STS (AlonzoUTXOS era),
    Tx era ~ AlonzoTx era,
    TxWits era ~ TxWitness era,
    Script era ~ AlonzoScript era,
    Environment (EraRule "PPUP" era) ~ ShelleyPPUPEnv era,
    State (EraRule "PPUP" era) ~ PPUPState era,
    Signal (EraRule "PPUP" era) ~ Maybe (Update era),
    Embed (EraRule "PPUP" era) (AlonzoUTXOS era),
    HasField "_costmdls" (PParams era) CostModels,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin,
    HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  TransitionRule (AlonzoUTXOS era)
scriptsValidateTransition = do
  TRC (UtxoEnv slot pp poolParams genDelegs, u@(UTxOState utxo _ _ pup _), tx) <-
    judgmentContext
  let txBody = body tx
      refunded = keyRefunds pp txBody
      txcerts = toList $ txBody ^. certsTxBodyL
      depositChange =
        totalDeposits pp (`Map.notMember` poolParams) txcerts <-> refunded

  () <- pure $! traceEvent validBegin ()

  scriptsTransition slot pp tx utxo $ \case
    Fails _ps fs ->
      failBecause $
        ValidationTagMismatch
          (tx ^. isValidTxL)
          (FailedUnexpectedly (scriptFailuresToPredicateFailure fs))
    Passes ps -> mapM_ (tellEvent . SuccessfulPlutusScriptsEvent) (nonEmpty ps)

  () <- pure $! traceEvent validEnd ()

  ppup' <-
    trans @(EraRule "PPUP" era) $
      TRC
        (PPUPEnv slot pp genDelegs, pup, strictMaybeToMaybe $ txBody ^. updateTxBodyL)

  pure $! updateUTxOState u txBody depositChange ppup'

scriptsNotValidateTransition ::
  forall era.
  ( AlonzoEraTx era,
    ExtendedUTxO era,
    STS (AlonzoUTXOS era),
    TxWits era ~ TxWitness era,
    Script era ~ AlonzoScript era,
    Tx era ~ AlonzoTx era,
    HasField "_costmdls" (PParams era) CostModels,
    HasField "_protocolVersion" (PParams era) ProtVer
  ) =>
  TransitionRule (AlonzoUTXOS era)
scriptsNotValidateTransition = do
  TRC (UtxoEnv slot pp _ _, us@(UTxOState utxo _ fees _ _), tx) <- judgmentContext
  let txBody = body tx

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
      !(utxoKeep, utxoDel) = extractKeys (unUTxO utxo) (txBody ^. collateralInputsTxBodyL)
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
-- PredicateFailure data type for AlonzoUTXOS

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

data AlonzoUtxosPredFailure era
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
  | UpdateFailure (PredicateFailure (EraRule "PPUP" era))
  deriving
    (Generic)

instance
  ( Era era,
    ToCBOR (PredicateFailure (EraRule "PPUP" era)),
    Show (TxOut era)
  ) =>
  ToCBOR (AlonzoUtxosPredFailure era)
  where
  toCBOR (ValidationTagMismatch v descr) = encode (Sum ValidationTagMismatch 0 !> To v !> To descr)
  toCBOR (CollectErrors cs) =
    encode (Sum (CollectErrors @era) 1 !> To cs)
  toCBOR (UpdateFailure pf) = encode (Sum (UpdateFailure @era) 2 !> To pf)

instance
  ( Era era,
    FromCBOR (PredicateFailure (EraRule "PPUP" era))
  ) =>
  FromCBOR (AlonzoUtxosPredFailure era)
  where
  fromCBOR = decode (Summands "UtxosPredicateFailure" dec)
    where
      dec 0 = SumD ValidationTagMismatch <! From <! From
      dec 1 = SumD (CollectErrors @era) <! From
      dec 2 = SumD UpdateFailure <! From
      dec n = Invalid n

deriving stock instance
  ( Show (Shelley.UTxOState era),
    Show (PredicateFailure (EraRule "PPUP" era))
  ) =>
  Show (AlonzoUtxosPredFailure era)

instance
  ( Eq (Shelley.UTxOState era),
    Eq (PredicateFailure (EraRule "PPUP" era))
  ) =>
  Eq (AlonzoUtxosPredFailure era)
  where
  (ValidationTagMismatch a x) == (ValidationTagMismatch b y) = a == b && x == y
  (CollectErrors x) == (CollectErrors y) = x == y
  (UpdateFailure x) == (UpdateFailure y) = x == y
  _ == _ = False

instance
  ( NoThunks (Shelley.UTxOState era),
    NoThunks (PredicateFailure (EraRule "PPUP" era))
  ) =>
  NoThunks (AlonzoUtxosPredFailure era)

--------------------------------------------------------------------------------
-- 2-phase checks
--------------------------------------------------------------------------------

-- $2-phase
--
-- Above and beyond 'static' checks (see 'Cardano.Ledger.Rules.ValidateMode') we
-- additionally label 2-phase checks. This is to support a workflow where we
-- validate a 'AlonzoTx'. We would like to trust the flag we have ourselves just
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
  PredicateFailure (EraRule "PPUP" era) ~ ShelleyPpupPredFailure era =>
  Inject (ShelleyPpupPredFailure era) (AlonzoUtxosPredFailure era)
  where
  inject = UpdateFailure

instance Inject (AlonzoUtxosPredFailure era) (AlonzoUtxosPredFailure era) where
  inject = id
