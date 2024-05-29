{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
  EraPlutusContext (mkPlutusWithContext),
  LedgerTxInfo (..),
 )
import Cardano.Ledger.Alonzo.Plutus.Evaluate (lookupPlutusScript)
import Cardano.Ledger.Alonzo.Scripts (plutusScriptLanguage, toAsItem, toAsIx)
import Cardano.Ledger.Alonzo.TxWits (unRedeemers)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Plutus.CostModels (costModelsValid)
import Cardano.Ledger.Plutus.Evaluate (
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
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MapExtras (fromElems)
import Data.Text (Text)
import Lens.Micro
import qualified PlutusLedgerApi.Common as P

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
  | -- | Error that can happen during plutus context translation
    ContextError !(ContextError era)

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
  --  Otherwise, we return a 'TranslationError' manifesting from failed attempts
  --  to construct a valid execution context for the given transaction.
  RedeemerReport era
evalTxExUnits pp tx utxo epochInfo systemStart =
  Map.map (fmap snd) $ evalTxExUnitsWithLogs pp tx utxo epochInfo systemStart

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
  RedeemerReportWithLogs era
evalTxExUnitsWithLogs pp tx utxo epochInfo systemStart = Map.mapWithKey findAndCount rdmrs
  where
    usePointerAsKey (plutusPurpose, _) = hoistPlutusPurpose toAsIx plutusPurpose
    ptrToPlutusScript = fromElems usePointerAsKey scriptsNeeded
    ledgerTxInfo =
      LedgerTxInfo
        { ltiProtVer = protVer
        , ltiEpochInfo = epochInfo
        , ltiSystemStart = systemStart
        , ltiUTxO = utxo
        , ltiTx = tx
        }
    maxBudget = pp ^. ppMaxTxExUnitsL
    txBody = tx ^. bodyTxL
    wits = tx ^. witsTxL
    rdmrs = unRedeemers $ wits ^. rdmrsTxWitsL
    protVer = pp ^. ppProtocolVersionL
    costModels = costModelsValid $ pp ^. ppCostModelsL
    ScriptsProvided scriptsProvided = getScriptsProvided utxo tx
    AlonzoScriptsNeeded scriptsNeeded = getScriptsNeeded utxo txBody
    findAndCount pointer (redeemerData, exUnits) = do
      (plutusPurpose, plutusScriptHash) <-
        note (RedeemerPointsToUnknownScriptHash pointer) $
          Map.lookup pointer ptrToPlutusScript
      let ptrToPlutusScriptNoContext =
            Map.map
              (\(sp, sh) -> (hoistPlutusPurpose toAsItem sp, lookupPlutusScript scriptsProvided sh, sh))
              ptrToPlutusScript
      plutusScript <-
        note (MissingScript pointer ptrToPlutusScriptNoContext) $
          lookupPlutusScript scriptsProvided plutusScriptHash
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
            (redeemerData, maxBudget)
            costModel
      case evaluatePlutusWithContext P.Verbose pwc of
        (logs, Left err) -> Left $ ValidationFailure exUnits err logs pwc
        (logs, Right exBudget) ->
          note (IncompatibleBudget exBudget) $
            (,) logs <$> exBudgetToExUnits exBudget
