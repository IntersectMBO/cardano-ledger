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
  evalPlutusScriptsWithLogs,
  CollectError (..),
  collectPlutusScriptsWithContext,
  lookupPlutusScript,

  -- * Execution units estimation
  TransactionScriptFailure (..),
  evalTxExUnits,
  RedeemerReport,
  evalTxExUnitsWithLogs,
  RedeemerReportWithLogs,
)
where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context (ContextError, EraPlutusContext (..))
import Cardano.Ledger.Alonzo.Scripts (plutusScriptLanguage, toAsItem, toAsIx)
import Cardano.Ledger.Alonzo.TxWits (lookupRedeemer, unRedeemers, unTxDats)
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO (getSpendingDatum), AlonzoScriptsNeeded (..))
import Cardano.Ledger.BaseTypes (ProtVer (pvMajor), kindObject, natVersion, pvMajor)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Plutus.CostModels (costModelsValid)
import Cardano.Ledger.Plutus.Data (Datum (..), binaryDataToData, getPlutusData)
import Cardano.Ledger.Plutus.Evaluate (
  PlutusDatums (..),
  PlutusWithContext (..),
  ScriptResult (..),
  evaluatePlutusWithContext,
  runPlutusScriptWithLogs,
 )
import Cardano.Ledger.Plutus.ExUnits
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Plutus.TxInfo (exBudgetToExUnits)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UTxO (EraUTxO (..), ScriptsProvided (..), UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData)
import Control.Monad (forM, guard)
import Data.Aeson (ToJSON (..), (.=), pattern String)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.Trace (traceEvent)
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
  | NoWitness !(ScriptHash (EraCrypto era))
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

-- | Given a script hash and a Map of available scripts, find the PlutusScript. Returns
-- Nothing when script is missing or it is not a PlutusScript
lookupPlutusScript ::
  AlonzoEraScript era =>
  Map.Map (ScriptHash (EraCrypto era)) (Script era) ->
  ScriptHash (EraCrypto era) ->
  Maybe (PlutusScript era)
lookupPlutusScript scriptsAvailable scriptHash = do
  script <- scriptHash `Map.lookup` scriptsAvailable
  toPlutusScript script

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
  Either [CollectError era] [PlutusWithContext (EraCrypto era)]
collectPlutusScriptsWithContext epochInfo sysStart pp tx utxo =
  -- TODO: remove this whole complicated check when we get into Conway. It is much simpler
  -- to fail on a CostModel lookup in the `apply` function (already implemented).
  let missingCostModels = Set.filter (`Map.notMember` costModels) usedLanguages
   in case guard (protVerMajor < natVersion @9) >> Set.lookupMin missingCostModels of
        Just l -> Left [NoCostModel l]
        Nothing ->
          merge
            apply
            (map getScriptWithRedeemer neededPlutusScripts)
            (Right [])
  where
    -- Check on a protocol version to preserve failure mode (a single NoCostModel failure
    -- for languages with missing cost models) until we are in Conway era. After we hard
    -- fork into Conway it will be safe to remove this check together with the
    -- `missingCostModels` lookup
    --
    -- We also need to pass major protocol version to the script for script evaluation
    protVerMajor = pvMajor (pp ^. ppProtocolVersionL)
    costModels = costModelsValid $ pp ^. ppCostModelsL

    ScriptsProvided scriptsProvided = getScriptsProvided utxo tx
    AlonzoScriptsNeeded scriptsNeeded = getScriptsNeeded utxo (tx ^. bodyTxL)
    neededPlutusScripts =
      mapMaybe (\(sp, sh) -> (,) (sh, sp) <$> lookupPlutusScript scriptsProvided sh) scriptsNeeded
    usedLanguages = Set.fromList $ map (plutusScriptLanguage . snd) neededPlutusScripts

    getScriptWithRedeemer ((plutusScriptHash, plutusPurpose), plutusScript) =
      let redeemerIndex = hoistPlutusPurpose toAsIx plutusPurpose
       in case lookupRedeemer redeemerIndex $ tx ^. witsTxL . rdmrsTxWitsL of
            Just (d, exUnits) -> Right (plutusScript, plutusPurpose, d, exUnits, plutusScriptHash)
            Nothing -> Left (NoRedeemer (hoistPlutusPurpose toAsItem plutusPurpose))
    apply (plutusScript, plutusPurpose, d, exUnits, plutusScriptHash) = do
      let lang = plutusScriptLanguage plutusScript
      costModel <- maybe (Left (NoCostModel lang)) Right $ Map.lookup lang costModels
      case mkPlutusScriptContext plutusScript plutusPurpose pp epochInfo sysStart utxo tx of
        Right scriptContext ->
          let spendingDatum = getSpendingDatum utxo tx $ hoistPlutusPurpose toAsItem plutusPurpose
              datums = maybe id (:) spendingDatum [d, scriptContext]
           in Right $
                withPlutusScript plutusScript $ \plutus ->
                  PlutusWithContext
                    { pwcProtocolVersion = protVerMajor
                    , pwcScript = Left plutus
                    , pwcScriptHash = plutusScriptHash
                    , pwcDatums = PlutusDatums (getPlutusData <$> datums)
                    , pwcExUnits = exUnits
                    , pwcCostModel = costModel
                    }
        Left te -> Left $ BadTranslation te

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
evalPlutusScripts ::
  EraTx era =>
  Tx era ->
  [PlutusWithContext (EraCrypto era)] ->
  ScriptResult (EraCrypto era)
evalPlutusScripts tx pwcs = snd $ evalPlutusScriptsWithLogs tx pwcs

evalPlutusScriptsWithLogs ::
  EraTx era =>
  Tx era ->
  [PlutusWithContext (EraCrypto era)] ->
  ([Text], ScriptResult (EraCrypto era))
evalPlutusScriptsWithLogs _tx [] = mempty
evalPlutusScriptsWithLogs tx (plutusWithContext : rest) =
  let beginMsg =
        intercalate
          ","
          [ "[LEDGER][PLUTUS_SCRIPT]"
          , "BEGIN"
          ]
      !res = traceEvent beginMsg $ runPlutusScriptWithLogs plutusWithContext
      endMsg =
        intercalate
          ","
          [ "[LEDGER][PLUTUS_SCRIPT]"
          , "END"
          ]
   in traceEvent endMsg res <> evalPlutusScriptsWithLogs tx rest

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
      !(PlutusWithContext (EraCrypto era))
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
  , Eq (PlutusPurpose AsIx era)
  , Eq (PlutusPurpose AsItem era)
  ) =>
  Eq (TransactionScriptFailure era)

deriving instance
  ( Era era
  , Show (TxCert era)
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
      let pointer = hoistPlutusPurpose toAsIx plutusPurpose
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
      (plutusPurpose, mPlutusScript, scriptHash) <-
        note (RedeemerPointsToUnknownScriptHash pointer) $
          Map.lookup pointer ptrToPlutusScript
      let ptrToPlutusScriptNoContext =
            Map.map
              (\(sp, mps, sh) -> (hoistPlutusPurpose toAsItem sp, fst <$> mps, sh))
              ptrToPlutusScript
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
          Just (AsIxItem _ txIn) -> do
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
                , pwcScriptHash = scriptHash
                , pwcDatums = PlutusDatums (getPlutusData <$> datums)
                , pwcExUnits = maxBudget
                , pwcCostModel = costModel
                }
      case evaluatePlutusWithContext P.Verbose pwc of
        (logs, Left err) -> Left $ ValidationFailure exUnits err logs pwc
        (logs, Right exBudget) ->
          note (IncompatibleBudget exBudget) $
            (,) logs <$> exBudgetToExUnits exBudget
