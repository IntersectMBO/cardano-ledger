{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Plutus.Evaluate (
  evalPlutusScripts,
  CollectError (..),
  collectPlutusScriptsWithContext,

  -- * Execution units estimation

  -- | Functions in this section are provided for testing and downstream users like cardano-api
  evalPlutusScriptsWithLogs,
  TransactionScriptFailure (..),
  evalTxExUnits,
  RedeemerReport,
  evalTxExUnitsWithLogs,
  RedeemerReportWithLogs,
) where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context (ContextError, EraPlutusContext (..), LedgerTxInfo (..))
import Cardano.Ledger.Alonzo.Scripts (lookupPlutusScript, plutusScriptLanguage, toAsItem, toAsIx)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded (..))
import Cardano.Ledger.BaseTypes (kindObject)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Plutus.CostModels (costModelsValid)
import Cardano.Ledger.Plutus.Evaluate (
  PlutusWithContext (..),
  ScriptResult (..),
  evaluatePlutusWithContext,
  runPlutusScriptWithLogs,
 )
import Cardano.Ledger.Plutus.ExUnits
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Plutus.TxInfo (exBudgetToExUnits)
import Cardano.Ledger.State (EraUTxO (..), ScriptsProvided (..), UTxO (..))
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), (.=), pattern String)
import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MapExtras (fromElems)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Debug.Trace as Debug
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks)
import qualified PlutusLedgerApi.Common as P

-- ===============================================================
-- From the specification, Figure 7 "Scripts and their Arguments"
-- ===============================================================

-- | When collecting inputs for two phase scripts, 3 things can go wrong.
data CollectError era
  = NoRedeemer !(PlutusPurpose AsItem era)
  | NoWitness !ScriptHash
  | NoCostModel !Language
  | BadTranslation !(ContextError era)
  deriving (Generic)

deriving instance
  (AlonzoEraScript era, Eq (ContextError era)) =>
  Eq (CollectError era)

deriving instance
  (AlonzoEraScript era, Show (ContextError era)) =>
  Show (CollectError era)

deriving instance
  (AlonzoEraScript era, NoThunks (ContextError era)) =>
  NoThunks (CollectError era)

deriving instance
  (AlonzoEraScript era, NFData (ContextError era)) =>
  NFData (CollectError era)

instance (AlonzoEraScript era, EncCBOR (ContextError era)) => EncCBOR (CollectError era) where
  encCBOR (NoRedeemer x) = encode $ Sum NoRedeemer 0 !> To x
  encCBOR (NoWitness x) = encode $ Sum (NoWitness @era) 1 !> To x
  encCBOR (NoCostModel x) = encode $ Sum NoCostModel 2 !> To x
  encCBOR (BadTranslation x) = encode $ Sum (BadTranslation @era) 3 !> To x

instance (AlonzoEraScript era, DecCBOR (ContextError era)) => DecCBOR (CollectError era) where
  decCBOR = decode (Summands "CollectError" dec)
    where
      dec 0 = SumD NoRedeemer <! From
      dec 1 = SumD NoWitness <! From
      dec 2 = SumD NoCostModel <! From
      dec 3 = SumD BadTranslation <! From
      dec n = Invalid n

instance
  ( Era era
  , ToJSON (PlutusPurpose AsItem era)
  , ToJSON (ContextError era)
  ) =>
  ToJSON (CollectError era)
  where
  toJSON = \case
    NoRedeemer sPurpose ->
      kindObject "CollectError" $
        [ "error" .= String "NoRedeemer"
        , "plutusPurpose" .= toJSON sPurpose
        ]
    NoWitness sHash ->
      kindObject "CollectError" $
        [ "error" .= String "NoWitness"
        , "scriptHash" .= toJSON sHash
        ]
    NoCostModel lang ->
      kindObject "CollectError" $
        [ "error" .= String "NoCostModel"
        , "language" .= toJSON lang
        ]
    BadTranslation err ->
      kindObject "BadTranslation" ["error" .= toJSON err]

collectPlutusScriptsWithContext ::
  forall era.
  ( AlonzoEraTxBody era
  , AlonzoEraTxWits era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , EraPlutusContext era
  ) =>
  EpochInfo (Either Text) ->
  SystemStart ->
  PParams era ->
  Tx era ->
  UTxO era ->
  Either [CollectError era] [PlutusWithContext]
collectPlutusScriptsWithContext epochInfo systemStart pp tx utxo =
  merge
    apply
    (map getScriptWithRedeemer neededPlutusScripts)
    (Right [])
  where
    -- We need to pass major protocol version to the script for script evaluation
    protVer = pp ^. ppProtocolVersionL
    costModels = costModelsValid $ pp ^. ppCostModelsL
    ledgerTxInfo =
      LedgerTxInfo
        { ltiProtVer = protVer
        , ltiEpochInfo = epochInfo
        , ltiSystemStart = systemStart
        , ltiUTxO = utxo
        , ltiTx = tx
        }
    txInfoResult = mkTxInfoResult ledgerTxInfo

    ScriptsProvided scriptsProvided = getScriptsProvided utxo tx
    AlonzoScriptsNeeded scriptsNeeded = getScriptsNeeded utxo (tx ^. bodyTxL)
    neededPlutusScripts =
      mapMaybe (\(sp, sh) -> (,) (sh, sp) <$> lookupPlutusScript sh scriptsProvided) scriptsNeeded

    getScriptWithRedeemer ((plutusScriptHash, plutusPurpose), plutusScript) =
      let redeemerIndex = hoistPlutusPurpose toAsIx plutusPurpose
       in case Map.lookup redeemerIndex $ tx ^. witsTxL . rdmrsTxWitsL . unRedeemersL of
            Just (d, exUnits) -> Right (plutusScript, plutusPurpose, d, exUnits, plutusScriptHash)
            Nothing -> Left (NoRedeemer (hoistPlutusPurpose toAsItem plutusPurpose))
    apply (plutusScript, plutusPurpose, redeemerData, exUnits, plutusScriptHash) = do
      let lang = plutusScriptLanguage plutusScript
      costModel <- maybe (Left (NoCostModel lang)) Right $ Map.lookup lang costModels
      first BadTranslation $
        mkPlutusWithContext
          plutusScript
          plutusScriptHash
          plutusPurpose
          ledgerTxInfo
          txInfoResult
          (redeemerData, exUnits)
          costModel

-- | Merge two lists (the first of which may have failures, i.e. (Left _)), collect all the failures
--   but if there are none, use 'f' to construct a success.
merge :: forall t b a. (t -> Either a b) -> [Either a t] -> Either [a] [b] -> Either [a] [b]
merge _f [] answer = answer
merge f (x : xs) zs = merge f xs (gg x zs)
  where
    gg :: Either a t -> Either [a] [b] -> Either [a] [b]
    gg (Right t) (Right cs) =
      case f t of
        Right c -> Right $ c : cs
        Left e -> Left [e]
    gg (Left a) (Right _) = Left [a]
    gg (Right _) (Left cs) = Left cs
    gg (Left a) (Left cs) = Left (a : cs)

-- | Evaluate a list of Plutus scripts. All scripts in the list must evaluate to `True`.
evalPlutusScripts :: [PlutusWithContext] -> ScriptResult
evalPlutusScripts pwcs = snd $ evalPlutusScriptsWithLogs pwcs

evalPlutusScriptsWithLogs :: [PlutusWithContext] -> ([Text], ScriptResult)
evalPlutusScriptsWithLogs [] = mempty
evalPlutusScriptsWithLogs (plutusWithContext : rest) =
  let beginMsg =
        intercalate
          ","
          [ "[LEDGER][PLUTUS_SCRIPT]"
          , "BEGIN"
          ]
      !res = Debug.traceEvent beginMsg $ runPlutusScriptWithLogs plutusWithContext
      endMsg =
        intercalate
          ","
          [ "[LEDGER][PLUTUS_SCRIPT]"
          , "END"
          ]
   in Debug.traceEvent endMsg res <> evalPlutusScriptsWithLogs rest

-- | Script failures that can be returned by 'evalTxExUnitsWithLogs'.
data TransactionScriptFailure era
  = -- | A redeemer was supplied which points to a script hash which
    -- we cannot connect to a Plutus script.
    RedeemerPointsToUnknownScriptHash !(PlutusPurpose AsIx era)
  | -- | Missing redeemer.
    MissingScript
      -- | Redeemer pointer which cannot be resolved
      !(PlutusPurpose AsIx era)
      -- | Map of pointers which can be resolved together with PlutusScripts and their
      -- respective contexts
      !( Map
           (PlutusPurpose AsIx era)
           (PlutusPurpose AsItem era, Maybe (PlutusScript era), ScriptHash)
       )
  | -- | Missing datum.
    MissingDatum !DataHash
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
    UnknownTxIn !TxIn
  | -- | A redeemer points to a transaction input which is not
    --  plutus locked.
    InvalidTxIn !TxIn
  | -- | The execution budget that was calculated by the Plutus
    --  evaluator is out of bounds.
    IncompatibleBudget !P.ExBudget
  | -- | There was no cost model for a given version of Plutus in the ledger state
    NoCostModelInLedgerState !Language
  | -- | Error that can happen during plutus context translation
    ContextError !(ContextError era)
  deriving (Generic)

deriving instance
  ( Era era
  , Eq (TxCert era)
  , Eq (PlutusScript era)
  , Eq (ContextError era)
  , Eq (PlutusPurpose AsIx era)
  , Eq (PlutusPurpose AsItem era)
  ) =>
  Eq (TransactionScriptFailure era)

deriving instance
  ( Era era
  , Show (TxCert era)
  , Show (ContextError era)
  , Show (PlutusScript era)
  , Show (PlutusPurpose AsIx era)
  , Show (PlutusPurpose AsItem era)
  ) =>
  Show (TransactionScriptFailure era)

note :: e -> Maybe a -> Either e a
note _ (Just x) = Right x
note e Nothing = Left e

type RedeemerReport era =
  Map (PlutusPurpose AsIx era) (Either (TransactionScriptFailure era) ExUnits)

type RedeemerReportWithLogs era =
  Map (PlutusPurpose AsIx era) (Either (TransactionScriptFailure era) ([Text], ExUnits))

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
  RedeemerReport era
evalTxExUnits pp tx utxo epochInfo systemStart =
  Map.map (fmap snd) $ evalTxExUnitsWithLogs pp tx utxo epochInfo systemStart

-- | Evaluate the execution budgets needed for all the redeemers in
--  a given transaction.
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
  RedeemerReportWithLogs era
evalTxExUnitsWithLogs pp tx utxo epochInfo systemStart = Map.mapWithKey findAndCount rdmrs
  where
    keyedByPurpose (plutusPurpose, _) = hoistPlutusPurpose toAsIx plutusPurpose
    purposeToScriptHash = fromElems keyedByPurpose scriptsNeeded
    ledgerTxInfo =
      LedgerTxInfo
        { ltiProtVer = protVer
        , ltiEpochInfo = epochInfo
        , ltiSystemStart = systemStart
        , ltiUTxO = utxo
        , ltiTx = tx
        }
    txInfoResult = mkTxInfoResult ledgerTxInfo
    maxBudget = pp ^. ppMaxTxExUnitsL
    txBody = tx ^. bodyTxL
    wits = tx ^. witsTxL
    rdmrs = wits ^. rdmrsTxWitsL . unRedeemersL
    protVer = pp ^. ppProtocolVersionL
    costModels = costModelsValid $ pp ^. ppCostModelsL
    ScriptsProvided scriptsProvided = getScriptsProvided utxo tx
    AlonzoScriptsNeeded scriptsNeeded = getScriptsNeeded utxo txBody
    findAndCount pointer (redeemerData, exUnits) = do
      (plutusPurpose, plutusScriptHash) <-
        note (RedeemerPointsToUnknownScriptHash pointer) $
          Map.lookup pointer purposeToScriptHash
      let ptrToPlutusScriptNoContext =
            Map.map
              ( \(sp, sh) ->
                  ( hoistPlutusPurpose toAsItem sp
                  , lookupPlutusScript sh scriptsProvided
                  , sh
                  )
              )
              purposeToScriptHash
      plutusScript <-
        note (MissingScript pointer ptrToPlutusScriptNoContext) $
          lookupPlutusScript plutusScriptHash scriptsProvided
      let lang = plutusScriptLanguage plutusScript
      costModel <-
        note (NoCostModelInLedgerState lang) $ Map.lookup lang costModels
      pwc <-
        first ContextError $
          mkPlutusWithContext
            plutusScript
            plutusScriptHash
            plutusPurpose
            ledgerTxInfo
            txInfoResult
            (redeemerData, maxBudget)
            costModel
      case evaluatePlutusWithContext P.Verbose pwc of
        (logs, Left err) -> Left $ ValidationFailure exUnits err logs pwc
        (logs, Right exBudget) ->
          note (IncompatibleBudget exBudget) $
            (,) logs <$> exBudgetToExUnits exBudget
