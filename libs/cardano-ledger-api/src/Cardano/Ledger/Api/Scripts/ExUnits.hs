{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Api.Scripts.ExUnits (
  TransactionScriptFailure (..),
  evalTxExUnits,
  RedeemerReport,
  evalTxExUnitsWithLogs,
  RedeemerReportWithLogs,
)
where

import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Plutus.Context (
  ContextError,
  EraPlutusContext (mkPlutusScriptContext),
 )
import Cardano.Ledger.Alonzo.Plutus.Evaluate (lookupPlutusScript)
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoEraScript (..),
  AsIndex (..),
  AsItem (..),
  PlutusPurpose (..),
  plutusScriptLanguage,
 )
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx)
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (redeemerPointer))
import Cardano.Ledger.Alonzo.TxOut (AlonzoEraTxOut (..))
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoEraTxWits (..),
  unRedeemers,
  unTxDats,
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..), pvMajor)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.CostModels (costModelsValid)
import Cardano.Ledger.Plutus.Data (Datum (..), binaryDataToData, getPlutusData)
import Cardano.Ledger.Plutus.Evaluate (
  PlutusDatums (..),
  PlutusWithContext (..),
  evaluatePlutusWithContext,
 )
import Cardano.Ledger.Plutus.ExUnits
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Plutus.TxInfo (exBudgetToExUnits)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (EraUTxO (..), ScriptsProvided (..), UTxO (..))
import Cardano.Slotting.EpochInfo.API (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.Monad (forM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Lens.Micro
import qualified PlutusLedgerApi.Common as P

-- | Script failures that can be returned by 'evalTxExUnitsWithLogs'.
data TransactionScriptFailure era
  = -- | A redeemer was supplied which points to a script hash which
    -- we cannot connect to a Plutus script.
    RedeemerPointsToUnknownScriptHash !(PlutusPurpose AsIndex era)
  | -- | Missing redeemer.
    MissingScript
      -- | Redeemer pointer which cannot be resolved
      !(PlutusPurpose AsIndex era)
      -- | Map of pointers which can be resolved together with PlutusScripts and their
      -- respective contexts
      !( Map
          (PlutusPurpose AsIndex era)
          (PlutusPurpose AsItem era, Maybe (PlutusScript era), ScriptHash (EraCrypto era))
       )
  | -- | Missing datum.
    MissingDatum !(DataHash (EraCrypto era))
  | -- | Plutus evaluation error, for any version
    ValidationFailure
      -- | Supplied execution units in the transaction, which were ignored for calculating
      -- the actual execution units.
      !ExUnits
      !P.EvaluationError
      ![Text]
      !PlutusWithContext
  | -- | A redeemer points to a transaction input which is not
    --  present in the current UTxO.
    UnknownTxIn !(TxIn (EraCrypto era))
  | -- | A redeemer points to a transaction input which is not
    --  plutus locked.
    InvalidTxIn !(TxIn (EraCrypto era))
  | -- | The execution budget that was calculated by the Plutus
    --  evaluator is out of bounds.
    IncompatibleBudget !P.ExBudget
  | -- | There was no cost model for a given version of Plutus in the ledger state
    NoCostModelInLedgerState !Language

deriving instance
  ( Era era
  , Eq (TxCert era)
  , Eq (PlutusScript era)
  , Eq (PlutusPurpose AsIndex era)
  , Eq (PlutusPurpose AsItem era)
  ) =>
  Eq (TransactionScriptFailure era)

deriving instance
  ( Era era
  , Show (TxCert era)
  , Show (PlutusScript era)
  , Show (PlutusPurpose AsIndex era)
  , Show (PlutusPurpose AsItem era)
  ) =>
  Show (TransactionScriptFailure era)

note :: e -> Maybe a -> Either e a
note _ (Just x) = Right x
note e Nothing = Left e

type RedeemerReport era =
  Map (PlutusPurpose AsIndex era) (Either (TransactionScriptFailure era) ExUnits)

type RedeemerReportWithLogs era =
  Map (PlutusPurpose AsIndex era) (Either (TransactionScriptFailure era) ([Text], ExUnits))

-- | Evaluate the execution budgets needed for all the redeemers in
--  a given transaction. If a redeemer is invalid, a failure is returned instead.
--
--  The execution budgets in the supplied transaction are completely ignored.
--  The results of 'evalTxExUnitsWithLogs' are intended to replace them.
evalTxExUnits ::
  forall era.
  ( AlonzoEraTx era
  , EraUTxO era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
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
  Either (ContextError era) (RedeemerReport era)
evalTxExUnits pp tx utxo ei sysS =
  Map.map (fmap snd) <$> evalTxExUnitsWithLogs pp tx utxo ei sysS

-- | Evaluate the execution budgets needed for all the redeemers in
--  a given transaction. If a redeemer is invalid, a failure is returned instead.
--
--  The execution budgets in the supplied transaction are completely ignored.
--  The results of 'evalTxExUnitsWithLogs' are intended to replace them.
evalTxExUnitsWithLogs ::
  forall era.
  ( AlonzoEraTx era
  , EraUTxO era
  , EraPlutusContext era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
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
  Either (ContextError era) (RedeemerReportWithLogs era)
evalTxExUnitsWithLogs pp tx utxo epochInfo sysStart = do
  ptrToPlutusScript <-
    forM neededPlutusScripts $ \(plutusPurpose, mPlutusScript, scriptHash) -> do
      mPlutusScriptAndContext <-
        forM mPlutusScript $ \plutusScript -> do
          scriptContext <-
            mkPlutusScriptContext
              plutusScript
              plutusPurpose
              pp
              epochInfo
              sysStart
              utxo
              tx
          pure (plutusScript, scriptContext)
      -- Since `getScriptsNeeded` used the `txBody` to create script purposes, it would be
      -- a logic error if `redeemerPointer` was not able to find `plutusPurpose`.
      let !pointer =
            case redeemerPointer txBody plutusPurpose of
              SNothing ->
                error "Impossible: Redeemer pointer was not found in the TxBody"
              SJust p -> p
      pure (pointer, (plutusPurpose, mPlutusScriptAndContext, scriptHash))
  pure $ Map.mapWithKey (findAndCount $ Map.fromList ptrToPlutusScript) rdmrs
  where
    maxBudget = pp ^. ppMaxTxExUnitsL
    txBody = tx ^. bodyTxL
    wits = tx ^. witsTxL
    dats = unTxDats $ wits ^. datsTxWitsL
    rdmrs = unRedeemers $ wits ^. rdmrsTxWitsL
    protVerMajor = pvMajor (pp ^. ppProtocolVersionL)
    costModels = costModelsValid $ pp ^. ppCostModelsL
    ScriptsProvided scriptsProvided = getScriptsProvided utxo tx
    AlonzoScriptsNeeded scriptsNeeded = getScriptsNeeded utxo txBody
    neededPlutusScripts =
      map
        (\(sp, sh) -> (sp, lookupPlutusScript scriptsProvided sh, sh))
        scriptsNeeded
    findAndCount ptrToPlutusScript pointer (rdmr, exUnits) = do
      (plutusPurpose, mPlutusScript, _) <-
        note (RedeemerPointsToUnknownScriptHash pointer) $
          Map.lookup pointer ptrToPlutusScript
      let ptrToPlutusScriptNoContext =
            Map.map (\(sp, mps, sh) -> (sp, fst <$> mps, sh)) ptrToPlutusScript
      (plutusScript, scriptContext) <-
        note (MissingScript pointer ptrToPlutusScriptNoContext) mPlutusScript
      let lang = plutusScriptLanguage plutusScript
      costModel <-
        note (NoCostModelInLedgerState lang) $ Map.lookup lang costModels
      -- Similar to getSpendingDatum, but with more informative errors. It is OK to use
      -- inline datums, when they are present, since for PlutusV1 presence of inline
      -- datums would short circuit earlier on PlutusContext translation.
      datums <-
        case toSpendingPurpose plutusPurpose of
          Just (AsItem txIn) -> do
            txOut <- note (UnknownTxIn txIn) $ Map.lookup txIn (unUTxO utxo)
            datum <-
              case txOut ^. datumTxOutF of
                Datum binaryData -> pure $ binaryDataToData binaryData
                DatumHash dh -> note (MissingDatum dh) $ Map.lookup dh dats
                NoDatum -> Left (InvalidTxIn txIn)
            pure [datum, rdmr, scriptContext]
          Nothing -> pure [rdmr, scriptContext]
      let pwc =
            withPlutusScript plutusScript $ \plutus ->
              PlutusWithContext
                { pwcProtocolVersion = protVerMajor
                , pwcScript = Left plutus
                , pwcDatums = PlutusDatums (getPlutusData <$> datums)
                , pwcExUnits = maxBudget
                , pwcCostModel = costModel
                }
      case evaluatePlutusWithContext P.Verbose pwc of
        (logs, Left err) -> Left $ ValidationFailure exUnits err logs pwc
        (logs, Right exBudget) ->
          note (IncompatibleBudget exBudget) $
            (,) logs <$> exBudgetToExUnits exBudget
