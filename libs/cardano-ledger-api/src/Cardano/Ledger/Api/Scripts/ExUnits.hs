{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Api.Scripts.ExUnits (
  TransactionScriptFailure (..),
  ValidationFailed (..),
  evalTxExUnits,
  evaluateTransactionExecutionUnits,
  RedeemerReport,
  evalTxExUnitsWithLogs,
  evaluateTransactionExecutionUnitsWithLogs,
  RedeemerReportWithLogs,
)
where

import Cardano.Ledger.Alonzo.Language (Language (..), SLanguage (..))
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.PlutusScriptApi (knownToNotBe1Phase)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoScript (..),
  CostModel,
  CostModels (..),
  ExUnits (..),
  getEvaluationContext,
 )
import Cardano.Ledger.Alonzo.Scripts.Data (Data, Datum (..), binaryDataToData, getPlutusData)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx, ScriptPurpose (..), rdptr)
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxOut (..))
import Cardano.Ledger.Alonzo.TxInfo (
  EraPlutusContext,
  ExtendedUTxO (txscripts),
  PlutusData (..),
  PlutusDebugLang (..),
  TranslationError,
  VersionedTxInfo (..),
  exBudgetToExUnits,
  transExUnits,
  transProtocolVersion,
  txInfo,
  valContext,
 )
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoEraTxWits (..),
  RdmrPtr (..),
  unRedeemers,
  unTxDats,
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Core hiding (TranslationError)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO (..))
import Cardano.Slotting.EpochInfo.API (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.Array (Array, bounds, (!))
import Data.ByteString.Short as SBS (ShortByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Lens.Micro
import qualified PlutusLedgerApi.Common as Plutus
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3

-- | Script failures that can be returned by 'evaluateTransactionExecutionUnits'.
data TransactionScriptFailure era
  = -- | A redeemer was supplied that does not point to a
    --  valid plutus evaluation site in the given transaction.
    RedeemerNotNeeded !RdmrPtr !(ScriptHash (EraCrypto era))
  | -- | A redeemer was supplied which points to a script hash which
    -- we cannot connect to a Plutus script.
    RedeemerPointsToUnknownScriptHash !RdmrPtr
  | -- | Missing redeemer. The first parameter is the redeemer pointer which cannot be resolved,
    -- and the second parameter is the map of pointers which can be resolved.
    MissingScript
      !RdmrPtr
      !( Map
          RdmrPtr
          (ScriptPurpose era, Maybe (ShortByteString, Language), ScriptHash (EraCrypto era))
       )
  | -- | Missing datum.
    MissingDatum !(DataHash (EraCrypto era))
  | -- | Plutus evaluation error, for any version
    ValidationFailure ValidationFailed
  | -- | A redeemer points to a transaction input which is not
    --  present in the current UTxO.
    UnknownTxIn !(TxIn (EraCrypto era))
  | -- | A redeemer points to a transaction input which is not
    --  plutus locked.
    InvalidTxIn !(TxIn (EraCrypto era))
  | -- | The execution budget that was calculated by the Plutus
    --  evaluator is out of bounds.
    IncompatibleBudget !PV1.ExBudget
  | -- | There was no cost model for a given version of Plutus in the ledger state
    NoCostModelInLedgerState !Language

deriving instance (Era era, Eq (DCert era)) => Eq (TransactionScriptFailure era)

deriving instance (Era era, Show (DCert era)) => Show (TransactionScriptFailure era)

data ValidationFailed where
  ValidationFailedV1 :: !Plutus.EvaluationError -> ![Text] -> PlutusDebugLang 'PlutusV1 -> ValidationFailed
  ValidationFailedV2 :: !Plutus.EvaluationError -> ![Text] -> PlutusDebugLang 'PlutusV2 -> ValidationFailed
  ValidationFailedV3 :: !Plutus.EvaluationError -> ![Text] -> PlutusDebugLang 'PlutusV3 -> ValidationFailed

deriving instance Eq ValidationFailed

deriving instance Show ValidationFailed

note :: e -> Maybe a -> Either e a
note _ (Just x) = Right x
note e Nothing = Left e

type RedeemerReport era = Map RdmrPtr (Either (TransactionScriptFailure era) ExUnits)

type RedeemerReportWithLogs era = Map RdmrPtr (Either (TransactionScriptFailure era) ([Text], ExUnits))

-- | Evaluate the execution budgets needed for all the redeemers in
--  a given transaction. If a redeemer is invalid, a failure is returned instead.
--
--  The execution budgets in the supplied transaction are completely ignored.
--  The results of 'evalTxExUnitsWithLogs' are intended to replace them.
evalTxExUnits ::
  forall era.
  ( AlonzoEraTx era
  , ExtendedUTxO era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Script era ~ AlonzoScript era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  PParams era ->
  -- | The transaction.
  Tx era ->
  -- | The current UTxO set (or the relevant portion for the transaction).
  UTxO era ->
  -- | The epoch info, used to translate slots to POSIX time for plutus.
  EpochInfo (Either Text) ->
  -- | The start time of the given block chain.
  SystemStart ->
  -- | We return a map from redeemer pointers to either a failure or a
  --  sufficient execution budget.
  --  Otherwise, we return a 'TranslationError' manifesting from failed attempts
  --  to construct a valid execution context for the given transaction.
  Either (TranslationError (EraCrypto era)) (RedeemerReport era)
evalTxExUnits pp tx utxo ei sysS =
  Map.map (fmap snd) <$> evalTxExUnitsWithLogs pp tx utxo ei sysS

-- | Evaluate the execution budgets needed for all the redeemers in
--  a given transaction. If a redeemer is invalid, a failure is returned instead.
--
--  The execution budgets in the supplied transaction are completely ignored.
--  The results of 'evaluateTransactionExecutionUnitsWithLogs' are intended to replace them.
evalTxExUnitsWithLogs ::
  forall era.
  ( AlonzoEraTx era
  , ExtendedUTxO era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Script era ~ AlonzoScript era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  PParams era ->
  -- | The transaction.
  Tx era ->
  -- | The current UTxO set (or the relevant portion for the transaction).
  UTxO era ->
  -- | The epoch info, used to translate slots to POSIX time for plutus.
  EpochInfo (Either Text) ->
  -- | The start time of the given block chain.
  SystemStart ->
  -- | We return a map from redeemer pointers to either a failure or a sufficient
  --  execution budget with logs of the script.  Otherwise, we return a 'TranslationError'
  --  manifesting from failed attempts to construct a valid execution context for the
  --  given transaction.
  --
  --  Unlike `evalTxExUnits`, this function also returns evaluation logs, useful for
  --  debugging.
  Either (TranslationError (EraCrypto era)) (RedeemerReportWithLogs era)
evalTxExUnitsWithLogs pp tx utxo ei sysS =
  let lookupCostModel lang = Map.lookup lang $ costModelsValid (pp ^. ppCostModelsL)
   in evalTxExUnitsWithLogsInternal pp tx utxo ei sysS lookupCostModel

-- | Evaluate the execution budgets needed for all the redeemers in
--  a given transaction. If a redeemer is invalid, a failure is returned instead.
--
--  The execution budgets in the supplied transaction are completely ignored.
--  The results of 'evaluateTransactionExecutionUnits' are intended to replace them.
evaluateTransactionExecutionUnits ::
  forall era.
  ( AlonzoEraTx era
  , ExtendedUTxO era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Script era ~ AlonzoScript era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  PParams era ->
  -- | The transaction.
  Tx era ->
  -- | The current UTxO set (or the relevant portion for the transaction).
  UTxO era ->
  -- | The epoch info, used to translate slots to POSIX time for plutus.
  EpochInfo (Either Text) ->
  -- | The start time of the given block chain.
  SystemStart ->
  -- | The array of cost models, indexed by the supported languages.
  Array Language CostModel ->
  -- | We return a map from redeemer pointers to either a failure or a
  --  sufficient execution budget.
  --  Otherwise, we return a 'TranslationError' manifesting from failed attempts
  --  to construct a valid execution context for the given transaction.
  Either (TranslationError (EraCrypto era)) (RedeemerReport era)
evaluateTransactionExecutionUnits pp tx utxo ei sysS costModels =
  Map.map (fmap snd) <$> evaluateTransactionExecutionUnitsWithLogs pp tx utxo ei sysS costModels
{-# DEPRECATED evaluateTransactionExecutionUnits "In favor of `evalTxExUnits`" #-}

-- | Evaluate the execution budgets needed for all the redeemers in
--  a given transaction. If a redeemer is invalid, a failure is returned instead.
--
--  The execution budgets in the supplied transaction are completely ignored.
--  The results of 'evaluateTransactionExecutionUnitsWithLogs' are intended to replace them.
evaluateTransactionExecutionUnitsWithLogs ::
  forall era.
  ( AlonzoEraTx era
  , ExtendedUTxO era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Script era ~ AlonzoScript era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  PParams era ->
  -- | The transaction.
  Tx era ->
  -- | The current UTxO set (or the relevant portion for the transaction).
  UTxO era ->
  -- | The epoch info, used to translate slots to POSIX time for plutus.
  EpochInfo (Either Text) ->
  -- | The start time of the given block chain.
  SystemStart ->
  -- | The array of cost models, indexed by the supported languages.
  Array Language CostModel ->
  -- | We return a map from redeemer pointers to either a failure or a
  --  sufficient execution budget with logs of the script.
  --  Otherwise, we return a 'TranslationError' manifesting from failed attempts
  --  to construct a valid execution context for the given transaction.
  Either (TranslationError (EraCrypto era)) (RedeemerReportWithLogs era)
evaluateTransactionExecutionUnitsWithLogs pp tx utxo ei sysS costModels =
  evalTxExUnitsWithLogsInternal pp tx utxo ei sysS $ \lang ->
    if l1 <= lang && lang <= l2
      then Just (costModels ! lang)
      else Nothing
  where
    (l1, l2) = bounds costModels
{-# DEPRECATED evaluateTransactionExecutionUnitsWithLogs "In favor of `evalTxExUnitsWithLogs`" #-}

-- | Evaluate the execution budgets needed for all the redeemers in
--  a given transaction. If a redeemer is invalid, a failure is returned instead.
--
--  The execution budgets in the supplied transaction are completely ignored.
--  The results of 'evaluateTransactionExecutionUnitsWithLogs' are intended to replace them.
evalTxExUnitsWithLogsInternal ::
  forall era.
  ( AlonzoEraTx era
  , ExtendedUTxO era
  , EraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Script era ~ AlonzoScript era
  , EraPlutusContext 'PlutusV1 era
  ) =>
  PParams era ->
  -- | The transaction.
  Tx era ->
  -- | The current UTxO set (or the relevant portion for the transaction).
  UTxO era ->
  -- | The epoch info, used to translate slots to POSIX time for plutus.
  EpochInfo (Either Text) ->
  -- | The start time of the given block chain.
  SystemStart ->
  -- | The array of cost models, indexed by the supported languages.
  (Language -> Maybe CostModel) ->
  -- | We return a map from redeemer pointers to either a failure or a
  --  sufficient execution budget with logs of the script.
  --  Otherwise, we return a 'TranslationError' manifesting from failed attempts
  --  to construct a valid execution context for the given transaction.
  Either (TranslationError (EraCrypto era)) (RedeemerReportWithLogs era)
evalTxExUnitsWithLogsInternal pp tx utxo ei sysS lookupCostModel = do
  let getInfo :: Language -> Either (TranslationError (EraCrypto era)) VersionedTxInfo
      getInfo lang = txInfo pp lang ei sysS utxo tx
  ctx <- sequence $ Map.fromSet getInfo languagesUsed
  pure $
    Map.mapWithKey
      (findAndCount ctx)
      (unRedeemers $ wits ^. rdmrsTxWitsL)
  where
    txBody = tx ^. bodyTxL
    wits = tx ^. witsTxL
    dats = unTxDats $ wits ^. datsTxWitsL
    scriptsAvailable = txscripts utxo tx
    AlonzoScriptsNeeded needed = getScriptsNeeded utxo txBody
    neededAndConfirmedToBePlutus = mapMaybe (knownToNotBe1Phase scriptsAvailable) needed
    languagesUsed = Set.fromList [lang | (_, lang, _) <- neededAndConfirmedToBePlutus]

    ptrToPlutusScript = Map.fromList $ do
      (sp, sh) <- needed
      msb <- case Map.lookup sh scriptsAvailable of
        Nothing -> pure Nothing
        Just (TimelockScript _) -> []
        Just (PlutusScript lang bytes) -> pure $ Just (bytes, lang)
      pointer <- case rdptr txBody sp of
        SNothing -> []
        -- Since scriptsNeeded used the transaction to create script purposes,
        -- it would be a logic error if rdptr was not able to find sp.
        SJust p -> pure p
      pure (pointer, (sp, msb, sh))

    findAndCount ::
      Map Language VersionedTxInfo ->
      RdmrPtr ->
      (Data era, ExUnits) ->
      Either (TransactionScriptFailure era) ([Text], ExUnits)
    findAndCount info pointer (rdmr, exunits) = do
      (sp, mscript, sh) <-
        note (RedeemerPointsToUnknownScriptHash pointer) $
          Map.lookup pointer ptrToPlutusScript
      (script, lang) <- note (MissingScript pointer ptrToPlutusScript) mscript
      inf <- note (RedeemerNotNeeded pointer sh) $ Map.lookup lang info
      cm <- note (NoCostModelInLedgerState lang) (lookupCostModel lang)
      args <- case sp of
        Spending txin -> do
          txOut <- note (UnknownTxIn txin) $ Map.lookup txin (unUTxO utxo)
          datum <- case txOut ^. datumTxOutF of
            Datum binaryData -> pure $ binaryDataToData binaryData
            DatumHash dh -> note (MissingDatum dh) $ Map.lookup dh dats
            NoDatum -> Left (InvalidTxIn txin)
          pure [datum, rdmr, valContext inf sp]
        _ -> pure [rdmr, valContext inf sp]
      let pArgs = map getPlutusData args

      case interpreter lang (getEvaluationContext cm) maxBudget script pArgs of
        (logs, Left e) -> case lang of
          PlutusV1 ->
            let debug = PlutusDebugLang SPlutusV1 cm exunits script (PlutusData pArgs) protVer
             in Left $ ValidationFailure $ ValidationFailedV1 e logs debug
          PlutusV2 ->
            let debug = PlutusDebugLang SPlutusV2 cm exunits script (PlutusData pArgs) protVer
             in Left $ ValidationFailure $ ValidationFailedV2 e logs debug
          PlutusV3 ->
            let debug = PlutusDebugLang SPlutusV3 cm exunits script (PlutusData pArgs) protVer
             in Left $ ValidationFailure $ ValidationFailedV3 e logs debug
        (logs, Right exBudget) ->
          note (IncompatibleBudget exBudget) $ (,) logs <$> exBudgetToExUnits exBudget
      where
        maxBudget = transExUnits $ pp ^. ppMaxTxExUnitsL
        protVer = pp ^. ppProtocolVersionL
        plutusProtVer = transProtocolVersion protVer
        interpreter = \case
          PlutusV1 -> PV1.evaluateScriptRestricting plutusProtVer PV1.Verbose
          PlutusV2 -> PV2.evaluateScriptRestricting plutusProtVer PV2.Verbose
          PlutusV3 -> PV3.evaluateScriptRestricting plutusProtVer PV3.Verbose
