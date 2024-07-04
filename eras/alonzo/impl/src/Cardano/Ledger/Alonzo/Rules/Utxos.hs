{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
  scriptFailureToFailureDescription,
)
where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Era (AlonzoEra, AlonzoUTXOS)
import Cardano.Ledger.Alonzo.Plutus.Context (ContextError, EraPlutusContext)
import Cardano.Ledger.Alonzo.Plutus.Evaluate (
  CollectError (..),
  collectPlutusScriptsWithContext,
  evalPlutusScripts,
 )
import Cardano.Ledger.Alonzo.Rules.Ppup ()
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO (..), AlonzoScriptsNeeded)
import Cardano.Ledger.BaseTypes (
  Globals,
  ShelleyBase,
  StrictMaybe,
  epochInfo,
  kindObject,
  systemStart,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.CertState (certDState, dsGenDelegs)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Plutus.Evaluate (
  PlutusWithContext,
  ScriptFailure (..),
  ScriptResult (..),
 )
import Cardano.Ledger.Rules.ValidationMode (lblStatic)
import Cardano.Ledger.SafeHash (SafeHash, hashAnnotated)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..), updateStakeDistribution)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Rules (
  PpupEnv (..),
  PpupEvent,
  ShelleyPPUP,
  ShelleyPpupPredFailure,
  UtxoEnv (..),
  updateUTxOState,
 )
import Cardano.Ledger.Shelley.TxCert (ShelleyTxCert)
import Cardano.Ledger.UTxO (
  EraUTxO (..),
  UTxO (..),
  coinBalance,
 )
import Cardano.Slotting.EpochInfo.Extend (unsafeLinearExtendEpochInfo)
import Cardano.Slotting.Slot (SlotNo)
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Control.State.Transition.Extended
import Data.Aeson (ToJSON (..), (.=))
import qualified Data.Aeson as Aeson
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
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , AlonzoEraScript era
  , TxCert era ~ ShelleyTxCert era
  , EraGov era
  , GovState era ~ ShelleyGovState era
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , Embed (EraRule "PPUP" era) (AlonzoUTXOS era)
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ StrictMaybe (Update era)
  , EncCBOR (PredicateFailure (EraRule "PPUP" era)) -- Serializing the PredicateFailure,
  , Eq (EraRuleFailure "PPUP" era)
  , Show (EraRuleFailure "PPUP" era)
  , EraPlutusContext era
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
  = AlonzoPpupToUtxosEvent (EraRuleEvent "PPUP" era)
  | TotalDeposits (SafeHash (EraCrypto era) EraIndependentTxBody) Coin
  | SuccessfulPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
  | FailedPlutusScriptsEvent (NonEmpty (PlutusWithContext (EraCrypto era)))
  | -- | The UTxOs consumed and created by a signal tx
    TxUTxODiff
      -- | UTxO consumed
      (UTxO era)
      -- | UTxO created
      (UTxO era)
  deriving (Generic)

deriving instance
  ( Era era
  , Eq (TxOut era)
  , Eq (EraRuleEvent "PPUP" era)
  ) =>
  Eq (AlonzoUtxosEvent era)

instance
  ( Era era
  , NFData (TxOut era)
  , NFData (EraRuleEvent "PPUP" era)
  ) =>
  NFData (AlonzoUtxosEvent era)

instance
  ( Era era
  , STS (ShelleyPPUP era)
  , EraRuleFailure "PPUP" era ~ ShelleyPpupPredFailure era
  , Event (EraRule "PPUP" era) ~ Event (ShelleyPPUP era)
  , EraRuleEvent "PPUP" era ~ PpupEvent era
  ) =>
  Embed (ShelleyPPUP era) (AlonzoUTXOS era)
  where
  wrapFailed = UpdateFailure
  wrapEvent = AlonzoPpupToUtxosEvent

utxosTransition ::
  forall era.
  ( AlonzoEraTx era
  , ShelleyEraTxBody era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , TxCert era ~ ShelleyTxCert era
  , EraGov era
  , GovState era ~ ShelleyGovState era
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ StrictMaybe (Update era)
  , Embed (EraRule "PPUP" era) (AlonzoUTXOS era)
  , EncCBOR (PredicateFailure (EraRule "PPUP" era)) -- Serializing the PredicateFailure
  , Eq (EraRuleFailure "PPUP" era)
  , Show (EraRuleFailure "PPUP" era)
  , EraPlutusContext era
  ) =>
  TransitionRule (AlonzoUTXOS era)
utxosTransition =
  judgmentContext >>= \(TRC (_, _, tx)) -> do
    case tx ^. isValidTxL of
      IsValid True -> alonzoEvalScriptsTxValid
      IsValid False -> alonzoEvalScriptsTxInvalid

-- ===================================================================

scriptsTransition ::
  ( STS sts
  , Monad m
  , AlonzoEraTxBody era
  , AlonzoEraTxWits era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , BaseM sts ~ ReaderT Globals m
  , PredicateFailure sts ~ AlonzoUtxosPredFailure era
  , EraPlutusContext era
  ) =>
  SlotNo ->
  PParams era ->
  Tx era ->
  UTxO era ->
  (ScriptResult (EraCrypto era) -> Rule sts ctx ()) ->
  Rule sts ctx ()
scriptsTransition slot pp tx utxo action = do
  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo
  case collectPlutusScriptsWithContext (unsafeLinearExtendEpochInfo slot ei) sysSt pp tx utxo of
    Right sLst ->
      when2Phase $ action $ evalPlutusScripts tx sLst
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

alonzoEvalScriptsTxValid ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ShelleyEraTxBody era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , STS (AlonzoUTXOS era)
  , Environment (EraRule "PPUP" era) ~ PpupEnv era
  , Signal (EraRule "PPUP" era) ~ StrictMaybe (Update era)
  , Embed (EraRule "PPUP" era) (AlonzoUTXOS era)
  , GovState era ~ ShelleyGovState era
  , State (EraRule "PPUP" era) ~ ShelleyGovState era
  , EraPlutusContext era
  ) =>
  TransitionRule (AlonzoUTXOS era)
alonzoEvalScriptsTxValid = do
  TRC (UtxoEnv slot pp certState, utxos@(UTxOState utxo _ _ pup _ _), tx) <-
    judgmentContext
  let txBody = tx ^. bodyTxL
      genDelegs = dsGenDelegs (certDState certState)

  () <- pure $! traceEvent validBegin ()

  scriptsTransition slot pp tx utxo $ \case
    Fails _ps fs ->
      failBecause $
        ValidationTagMismatch
          (tx ^. isValidTxL)
          (FailedUnexpectedly (scriptFailureToFailureDescription <$> fs))
    Passes ps -> mapM_ (tellEvent . SuccessfulPlutusScriptsEvent) (nonEmpty ps)

  () <- pure $! traceEvent validEnd ()

  ppup' <-
    trans @(EraRule "PPUP" era) $
      TRC (PPUPEnv slot pp genDelegs, pup, txBody ^. updateTxBodyL)

  updateUTxOState
    pp
    utxos
    txBody
    certState
    ppup'
    (tellEvent . TotalDeposits (hashAnnotated txBody))
    (\a b -> tellEvent $ TxUTxODiff a b)

alonzoEvalScriptsTxInvalid ::
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , STS (AlonzoUTXOS era)
  , EraPlutusContext era
  ) =>
  TransitionRule (AlonzoUTXOS era)
alonzoEvalScriptsTxInvalid = do
  TRC (UtxoEnv slot pp _, us@(UTxOState utxo _ fees _ _ _), tx) <- judgmentContext
  let txBody = tx ^. bodyTxL

  () <- pure $! traceEvent invalidBegin ()

  scriptsTransition slot pp tx utxo $ \case
    Passes _ps ->
      failBecause $
        ValidationTagMismatch (tx ^. isValidTxL) PassedUnexpectedly
    Fails ps fs -> do
      mapM_ (tellEvent . SuccessfulPlutusScriptsEvent) (nonEmpty ps)
      tellEvent (FailedPlutusScriptsEvent (scriptFailurePlutus <$> fs))

  () <- pure $! traceEvent invalidEnd ()

  {- utxoKeep = txBody ^. collateralInputsTxBodyL ⋪ utxo -}
  {- utxoDel  = txBody ^. collateralInputsTxBodyL ◁ utxo -}
  let !(utxoKeep, utxoDel) = extractKeys (unUTxO utxo) (txBody ^. collateralInputsTxBodyL)
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

instance NFData FailureDescription

instance EncCBOR FailureDescription where
  -- This strange encoding results from the fact that 'FailureDescription'
  -- used to have another constructor, which used key 0.
  -- We must maintain the original serialization in order to not disrupt
  -- the node-to-client protocol of the cardano node.
  encCBOR (PlutusFailure s b) = encode $ Sum PlutusFailure 1 !> To s !> To b

instance DecCBOR FailureDescription where
  decCBOR = decode $ Summands "FailureDescription" $ \case
    -- Note the lack of key 0. See the EncCBOR instance above for an explanation.
    1 -> SumD PlutusFailure <! From <! From
    n -> Invalid n

instance ToJSON FailureDescription where
  toJSON (PlutusFailure t _bs) =
    kindObject
      "FailureDescription"
      [ "error" .= Aeson.String "PlutusFailure"
      , "description" .= t
      -- Plutus context can be pretty big, therefore it's omitted in JSON
      -- "reconstructionDetail" .= bs
      ]

scriptFailureToFailureDescription :: Crypto c => ScriptFailure c -> FailureDescription
scriptFailureToFailureDescription (ScriptFailure msg pwc) =
  PlutusFailure msg (B64.encode $ Plain.serialize' pwc)

data TagMismatchDescription
  = PassedUnexpectedly
  | FailedUnexpectedly (NonEmpty FailureDescription)
  deriving (Show, Eq, Generic, NoThunks)

instance NFData TagMismatchDescription

instance EncCBOR TagMismatchDescription where
  encCBOR PassedUnexpectedly = encode (Sum PassedUnexpectedly 0)
  encCBOR (FailedUnexpectedly fs) = encode (Sum FailedUnexpectedly 1 !> To fs)

instance DecCBOR TagMismatchDescription where
  decCBOR = decode (Summands "TagMismatchDescription" dec)
    where
      dec 0 = SumD PassedUnexpectedly
      dec 1 = SumD FailedUnexpectedly <! From
      dec n = Invalid n

instance ToJSON TagMismatchDescription where
  toJSON = \case
    PassedUnexpectedly ->
      kindObject
        "TagMismatchDescription"
        ["error" .= Aeson.String "PassedUnexpectedly"]
    FailedUnexpectedly forReasons ->
      kindObject
        "TagMismatchDescription"
        [ "error" .= Aeson.String "FailedUnexpectedly"
        , "reconstruction" .= forReasons
        ]

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
  | UpdateFailure (EraRuleFailure "PPUP" era)
  deriving
    (Generic)

type instance EraRuleFailure "UTXOS" (AlonzoEra c) = AlonzoUtxosPredFailure (AlonzoEra c)

instance InjectRuleFailure "UTXOS" AlonzoUtxosPredFailure (AlonzoEra c)

instance InjectRuleFailure "UTXOS" ShelleyPpupPredFailure (AlonzoEra c) where
  injectFailure = UpdateFailure

instance
  ( EraTxCert era
  , AlonzoEraScript era
  , EncCBOR (ContextError era)
  , EncCBOR (EraRuleFailure "PPUP" era)
  ) =>
  EncCBOR (AlonzoUtxosPredFailure era)
  where
  encCBOR (ValidationTagMismatch v descr) = encode (Sum ValidationTagMismatch 0 !> To v !> To descr)
  encCBOR (CollectErrors cs) =
    encode (Sum (CollectErrors @era) 1 !> To cs)
  encCBOR (UpdateFailure pf) = encode (Sum (UpdateFailure @era) 2 !> To pf)

instance
  ( EraTxCert era
  , AlonzoEraScript era
  , DecCBOR (ContextError era)
  , DecCBOR (EraRuleFailure "PPUP" era)
  ) =>
  DecCBOR (AlonzoUtxosPredFailure era)
  where
  decCBOR = decode (Summands "AlonzoUtxosPredicateFailure" dec)
    where
      dec 0 = SumD ValidationTagMismatch <! From <! From
      dec 1 = SumD (CollectErrors @era) <! From
      dec 2 = SumD UpdateFailure <! From
      dec n = Invalid n

deriving stock instance
  ( AlonzoEraScript era
  , Show (TxCert era)
  , Show (ContextError era)
  , Show (Shelley.UTxOState era)
  , Show (EraRuleFailure "PPUP" era)
  ) =>
  Show (AlonzoUtxosPredFailure era)

deriving stock instance
  ( AlonzoEraScript era
  , Eq (TxCert era)
  , Eq (ContextError era)
  , Eq (Shelley.UTxOState era)
  , Eq (EraRuleFailure "PPUP" era)
  ) =>
  Eq (AlonzoUtxosPredFailure era)

instance
  ( AlonzoEraScript era
  , NoThunks (TxCert era)
  , NoThunks (ContextError era)
  , NoThunks (Shelley.UTxOState era)
  , NoThunks (EraRuleFailure "PPUP" era)
  ) =>
  NoThunks (AlonzoUtxosPredFailure era)

instance
  ( AlonzoEraScript era
  , NFData (TxCert era)
  , NFData (ContextError era)
  , NFData (Shelley.UTxOState era)
  , NFData (EraRuleFailure "PPUP" era)
  ) =>
  NFData (AlonzoUtxosPredFailure era)

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
