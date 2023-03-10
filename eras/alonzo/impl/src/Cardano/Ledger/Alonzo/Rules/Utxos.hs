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

module Cardano.Ledger.Alonzo.Rules.Utxos (
  AlonzoUTXOS,
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

import Cardano.Ledger.Alonzo.Era (AlonzoUTXOS)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.PlutusScriptApi (
  CollectError (..),
  collectTwoPhaseScriptInputs,
  evalScripts,
 )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxBody (
  AlonzoEraTxBody (..),
  MaryEraTxBody (..),
  ShelleyEraTxBody (..),
 )
import Cardano.Ledger.Alonzo.TxInfo (
  EraPlutusContext,
  ExtendedUTxO (..),
  PlutusDebug (..),
  ScriptFailure (..),
  ScriptResult (..),
 )
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded)
import Cardano.Ledger.BaseTypes (
  Globals,
  ProtVer (..),
  ShelleyBase,
  epochInfo,
  strictMaybeToMaybe,
  systemStart,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), serialize')
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Rules.ValidationMode (Inject (..), lblStatic)
import Cardano.Ledger.Shelley.Delegation (ShelleyDCert)
import Cardano.Ledger.Shelley.Governance (EraGovernance (GovernanceState), ShelleyPPUPState)
import Cardano.Ledger.Shelley.LedgerState (
  PPUPPredFailure,
  UTxOState (..),
  keyTxRefunds,
  totalTxDeposits,
  updateStakeDistribution,
 )
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules (
  PpupEnv (..),
  ShelleyPPUP,
  ShelleyPpupPredFailure,
  UtxoEnv (..),
  updateUTxOState,
 )
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..), coinBalance)
import Cardano.Ledger.Val ((<->))
import Cardano.Slotting.EpochInfo.Extend (unsafeLinearExtendEpochInfo)
import Cardano.Slotting.Slot (SlotNo)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Control.State.Transition.Extended
import Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.MapExtras (extractKeys)
import Data.Text (Text)
import Debug.Trace (traceEvent)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

--------------------------------------------------------------------------------
-- The AlonzoUTXOS transition system
--------------------------------------------------------------------------------

instance
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraPParams era
  , ShelleyEraTxBody era
  , ExtendedUTxO era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Script era ~ AlonzoScript era
  , DCert era ~ ShelleyDCert era
  , EraGovernance era
  , GovernanceState era ~ ShelleyPPUPState era
  , State (EraRule "PPUP" era) ~ ShelleyPPUPState era
  , Embed (EraRule "PPUP" era) (AlonzoUTXOS era)
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ Maybe (Update era)
  , EncCBOR (PredicateFailure (EraRule "PPUP" era)) -- Serializing the PredicateFailure,
  , ProtVerAtMost era 8
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  , EraPlutusContext 'PlutusV1 era
  ) =>
  STS (AlonzoUTXOS era)
  where
  type BaseM (AlonzoUTXOS era) = ShelleyBase
  type Environment (AlonzoUTXOS era) = UtxoEnv era
  type State (AlonzoUTXOS era) = UTxOState era
  type Signal (AlonzoUTXOS era) = Tx era
  type PredicateFailure (AlonzoUTXOS era) = AlonzoUtxosPredFailure era
  type Event (AlonzoUTXOS era) = AlonzoUtxosEvent era
  transitionRules = [utxosTransition]

data AlonzoUtxosEvent era
  = AlonzoPpupToUtxosEvent (Event (EraRule "PPUP" era))
  | SuccessfulPlutusScriptsEvent (NonEmpty PlutusDebug)
  | FailedPlutusScriptsEvent (NonEmpty PlutusDebug)

instance
  ( Era era
  , STS (ShelleyPPUP era)
  , PPUPPredFailure era ~ ShelleyPpupPredFailure era
  , Event (EraRule "PPUP" era) ~ Event (ShelleyPPUP era)
  ) =>
  Embed (ShelleyPPUP era) (AlonzoUTXOS era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = AlonzoPpupToUtxosEvent

utxosTransition ::
  forall era.
  ( ExtendedUTxO era
  , AlonzoEraTx era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Script era ~ AlonzoScript era
  , DCert era ~ ShelleyDCert era
  , EraGovernance era
  , GovernanceState era ~ ShelleyPPUPState era
  , State (EraRule "PPUP" era) ~ ShelleyPPUPState era
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ Maybe (Update era)
  , Embed (EraRule "PPUP" era) (AlonzoUTXOS era)
  , EncCBOR (PredicateFailure (EraRule "PPUP" era)) -- Serializing the PredicateFailure
  , ProtVerAtMost era 8
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  , EraPlutusContext 'PlutusV1 era
  ) =>
  TransitionRule (AlonzoUTXOS era)
utxosTransition =
  judgmentContext >>= \(TRC (_, _, tx)) -> do
    case tx ^. isValidTxL of
      IsValid True -> scriptsValidateTransition
      IsValid False -> scriptsNotValidateTransition

-- ===================================================================

scriptsTransition ::
  ( STS sts
  , Monad m
  , EraTx era
  , Script era ~ AlonzoScript era
  , MaryEraTxBody era
  , AlonzoEraTxWits era
  , ExtendedUTxO era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , BaseM sts ~ ReaderT Globals m
  , PredicateFailure sts ~ AlonzoUtxosPredFailure era
  , AlonzoEraPParams era
  , EraPlutusContext 'PlutusV1 era
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
      when2Phase $ action $ evalScripts (pp ^. ppProtocolVersionL) tx sLst
    Left info
      | alonzoFailures <- filter isNotBadTranslation info
      , not (null alonzoFailures) ->
          failBecause (CollectErrors alonzoFailures)
      | otherwise -> pure ()
  where
    -- BadTranslation was introduced in Babbage, thus we need to filter those failures out.
    isNotBadTranslation = \case
      BadTranslation {} -> False
      _ -> True

scriptsValidateTransition ::
  forall era.
  ( AlonzoEraTx era
  , ExtendedUTxO era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , STS (AlonzoUTXOS era)
  , Script era ~ AlonzoScript era
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ Maybe (Update era)
  , Embed (EraRule "PPUP" era) (AlonzoUTXOS era)
  , ProtVerAtMost era 8
  , GovernanceState era ~ ShelleyPPUPState era
  , State (EraRule "PPUP" era) ~ ShelleyPPUPState era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  TransitionRule (AlonzoUTXOS era)
scriptsValidateTransition = do
  TRC (UtxoEnv slot pp dpstate genDelegs, u@(UTxOState utxo _ _ pup _), tx) <-
    judgmentContext
  let txBody = tx ^. bodyTxL
      protVer = pp ^. ppProtocolVersionL
      refunded = keyTxRefunds pp dpstate txBody
      depositChange = totalTxDeposits pp dpstate txBody <-> refunded

  () <- pure $! traceEvent validBegin ()

  scriptsTransition slot pp tx utxo $ \case
    Fails _ps fs ->
      failBecause $
        ValidationTagMismatch
          (tx ^. isValidTxL)
          (FailedUnexpectedly (scriptFailuresToPredicateFailure protVer fs))
    Passes ps -> mapM_ (tellEvent . SuccessfulPlutusScriptsEvent) (nonEmpty ps)

  () <- pure $! traceEvent validEnd ()

  ppup' <-
    trans @(EraRule "PPUP" era) $
      TRC
        (PPUPEnv slot pp genDelegs, pup, strictMaybeToMaybe $ txBody ^. updateTxBodyL)

  pure $! updateUTxOState pp u txBody depositChange ppup'

scriptsNotValidateTransition ::
  forall era.
  ( AlonzoEraTx era
  , ExtendedUTxO era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , STS (AlonzoUTXOS era)
  , Script era ~ AlonzoScript era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  TransitionRule (AlonzoUTXOS era)
scriptsNotValidateTransition = do
  TRC (UtxoEnv slot pp _ _, us@(UTxOState utxo _ fees _ _), tx) <- judgmentContext
  let txBody = tx ^. bodyTxL

  let !_ = traceEvent invalidBegin ()

  scriptsTransition slot pp tx utxo $ \case
    Passes _ps ->
      failBecause $
        ValidationTagMismatch (tx ^. isValidTxL) PassedUnexpectedly
    Fails ps fs -> do
      mapM_ (tellEvent . SuccessfulPlutusScriptsEvent) (nonEmpty ps)
      tellEvent (FailedPlutusScriptsEvent (scriptFailuresToPlutusDebug fs))

  let !_ = traceEvent invalidEnd ()

      {- utxoKeep = txBody ^. collateralInputsTxBodyL ⋪ utxo -}
      {- utxoDel  = txBody ^. collateralInputsTxBodyL ◁ utxo -}
      !(utxoKeep, utxoDel) = extractKeys (unUTxO utxo) (txBody ^. collateralInputsTxBodyL)
  pure $!
    us
      { utxosUtxo = UTxO utxoKeep
      , utxosFees = fees <> coinBalance (UTxO utxoDel)
      , utxosStakeDistr = updateStakeDistribution pp (utxosStakeDistr us) (UTxO utxoDel) mempty
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

instance EncCBOR FailureDescription where
  -- This strange encoding results from the fact that 'FailureDescription'
  -- used to have another constructor, which used key 0.
  -- We must maintain the original serialization in order to not disrupt
  -- the node-to-client protocol of the cardano node.
  encCBOR (PlutusFailure s b) = encode $ Sum PlutusFailure 1 !> To s !> To b

instance DecCBOR FailureDescription where
  decCBOR = decode (Summands "FailureDescription" dec)
    where
      -- Note the lack of key 0. See the EncCBOR instance above for an explanation.
      dec 1 = SumD PlutusFailure <! From <! From
      dec n = Invalid n

scriptFailureToFailureDescription :: ProtVer -> ScriptFailure -> FailureDescription
scriptFailureToFailureDescription protVer (PlutusSF t pd) =
  PlutusFailure t (B64.encode $ serialize' (pvMajor protVer) pd)

scriptFailuresToPredicateFailure :: ProtVer -> NonEmpty ScriptFailure -> NonEmpty FailureDescription
scriptFailuresToPredicateFailure protVer = fmap (scriptFailureToFailureDescription protVer)

scriptFailuresToPlutusDebug :: NonEmpty ScriptFailure -> NonEmpty PlutusDebug
scriptFailuresToPlutusDebug = fmap (\(PlutusSF _ pdb) -> pdb)

data TagMismatchDescription
  = PassedUnexpectedly
  | FailedUnexpectedly (NonEmpty FailureDescription)
  deriving (Show, Eq, Generic, NoThunks)

instance EncCBOR TagMismatchDescription where
  encCBOR PassedUnexpectedly = encode (Sum PassedUnexpectedly 0)
  encCBOR (FailedUnexpectedly fs) = encode (Sum FailedUnexpectedly 1 !> To fs)

instance DecCBOR TagMismatchDescription where
  decCBOR = decode (Summands "TagMismatchDescription" dec)
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
    CollectErrors [CollectError era]
  | UpdateFailure (PPUPPredFailure era)
  deriving
    (Generic)

instance PPUPPredFailure era ~ () => Inject () (AlonzoUtxosPredFailure era) where
  inject () = UpdateFailure ()

instance
  ( EraDCert era
  , EncCBOR (PPUPPredFailure era)
  ) =>
  EncCBOR (AlonzoUtxosPredFailure era)
  where
  encCBOR (ValidationTagMismatch v descr) = encode (Sum ValidationTagMismatch 0 !> To v !> To descr)
  encCBOR (CollectErrors cs) =
    encode (Sum (CollectErrors @era) 1 !> To cs)
  encCBOR (UpdateFailure pf) = encode (Sum (UpdateFailure @era) 2 !> To pf)

instance
  ( Era era
  , DecCBOR (DCert era)
  , DecCBOR (PPUPPredFailure era)
  ) =>
  DecCBOR (AlonzoUtxosPredFailure era)
  where
  decCBOR = decode (Summands "UtxosPredicateFailure" dec)
    where
      dec 0 = SumD ValidationTagMismatch <! From <! From
      dec 1 = SumD (CollectErrors @era) <! From
      dec 2 = SumD UpdateFailure <! From
      dec n = Invalid n

deriving stock instance
  ( Era era
  , Show (DCert era)
  , Show (Shelley.UTxOState era)
  , Show (PPUPPredFailure era)
  ) =>
  Show (AlonzoUtxosPredFailure era)

instance
  ( Era era
  , Eq (DCert era)
  , Eq (Shelley.UTxOState era)
  , Eq (PPUPPredFailure era)
  ) =>
  Eq (AlonzoUtxosPredFailure era)
  where
  (ValidationTagMismatch a x) == (ValidationTagMismatch b y) = a == b && x == y
  (CollectErrors x) == (CollectErrors y) = x == y
  (UpdateFailure x) == (UpdateFailure y) = x == y
  _ == _ = False

instance
  ( Era era
  , NoThunks (DCert era)
  , NoThunks (Shelley.UTxOState era)
  , NoThunks (PPUPPredFailure era)
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
  PPUPPredFailure era ~ ShelleyPpupPredFailure era =>
  Inject (ShelleyPpupPredFailure era) (AlonzoUtxosPredFailure era)
  where
  inject = UpdateFailure

instance Inject (AlonzoUtxosPredFailure era) (AlonzoUtxosPredFailure era) where
  inject = id
