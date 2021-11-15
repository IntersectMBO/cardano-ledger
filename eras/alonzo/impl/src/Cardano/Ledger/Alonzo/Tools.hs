{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.Tools
  ( evaluateTransactionExecutionUnits,
    BasicFailure (..),
    ScriptFailure (..),
  )
where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data, getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (..), nonNativeLanguages)
import Cardano.Ledger.Alonzo.PParams (_maxTxExUnits, _protocolVersion)
import Cardano.Ledger.Alonzo.PlutusScriptApi (scriptsNeeded)
import Cardano.Ledger.Alonzo.Scripts
  ( CostModel (..),
    ExUnits (..),
    Script (..),
  )
import Cardano.Ledger.Alonzo.Tx (DataHash, ScriptPurpose (Spending), ValidatedTx (..), rdptr)
import Cardano.Ledger.Alonzo.TxBody (TxOut (..))
import Cardano.Ledger.Alonzo.TxInfo
  ( VersionedTxInfo (..),
    exBudgetToExUnits,
    transExUnits,
    txInfo,
    valContext,
  )
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (..), unRedeemers, unTxDats)
import Cardano.Ledger.BaseTypes (StrictMaybe (..), strictMaybeToMaybe)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Shelley.Tx (TxIn)
import Cardano.Ledger.Shelley.UTxO (UTxO (..), unUTxO)
import Cardano.Slotting.EpochInfo.API (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.Array (Array, array, bounds, (!))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Records (HasField (..))
import qualified Plutus.V1.Ledger.Api as PV1
import qualified Plutus.V2.Ledger.Api as PV2

-- | Basic validtion failures that can be returned by 'evaluateTransactionExecutionUnits'.
data BasicFailure c
  = -- | The transaction contains inputs that are not present in the UTxO.
    UnknownTxIns (Set (TxIn c))
  deriving (Show)

-- | Script failures that can be returned by 'evaluateTransactionExecutionUnits'.
data ScriptFailure c
  = -- | A redeemer was supplied that does not point to a
    --  valid plutus evaluation site in the given transaction.
    RedeemerNotNeeded RdmrPtr
  | -- | Missing redeemer.
    MissingScript RdmrPtr
  | -- | Missing datum.
    MissingDatum (DataHash c)
  | -- | Plutus V1 evaluation error.
    ValidationFailedV1 PV1.EvaluationError [Text]
  | -- | Plutus V2 evaluation error.
    ValidationFailedV2 PV2.EvaluationError [Text]
  | -- | A redeemer points to a transaction input which is not
    --  present in the current UTxO.
    UnknownTxIn (TxIn c)
  | -- | A redeemer points to a transaction input which is not
    --  plutus locked.
    InvalidTxIn (TxIn c)
  | -- | The execution budget that was calculated by the Plutus
    --  evaluator is out of bounds.
    IncompatibleBudget PV1.ExBudget
  | -- | There was no cost model for given version of Plutus
    NoCostModel Language
  deriving (Show)

note :: e -> Maybe a -> Either e a
note _ (Just x) = Right x
note e Nothing = Left e

basicValidation ::
  -- | The transaction.
  Core.Tx (AlonzoEra c) ->
  -- | The current UTxO set (or the relevant portion for the transaction).
  UTxO (AlonzoEra c) ->
  -- | Basic failures.
  Maybe (BasicFailure c)
basicValidation tx utxo =
  if Set.null badIns
    then Nothing
    else Just (UnknownTxIns badIns)
  where
    txb = getField @"body" tx
    ins = getField @"inputs" txb
    badIns = Set.filter (`Map.notMember` (unUTxO utxo)) ins

type RedeemerReport c = Map RdmrPtr (Either (ScriptFailure c) ExUnits)

-- | Evaluate the execution budgets needed for all the redeemers in
--  a given transaction. If a redeemer is invalid, a failure is returned instead.
--
--  The execution budgets in the supplied transaction are completely ignored.
--  The results of 'evaluateTransactionExecutionUnits' are intended to replace them.
evaluateTransactionExecutionUnits ::
  forall c m.
  ( CC.Crypto c,
    Monad m
  ) =>
  Core.PParams (AlonzoEra c) ->
  -- | The transaction.
  Core.Tx (AlonzoEra c) ->
  -- | The current UTxO set (or the relevant portion for the transaction).
  UTxO (AlonzoEra c) ->
  -- | The epoch info, used to translate slots to POSIX time for plutus.
  EpochInfo m ->
  -- | The start time of the given block chain.
  SystemStart ->
  -- | The array of cost models, indexed by the supported languages.
  Array Language CostModel ->
  -- | If the transaction meets basic validation, we return a map from
  --  redeemer pointers to either a failure or a sufficient execution budget.
  --  Otherwise we return a basic validation error.
  --  The value is monadic, depending on the epoch info.
  m (Either (BasicFailure c) (RedeemerReport c))
evaluateTransactionExecutionUnits pp tx utxo ei sysS costModels = do
  case basicValidation tx utxo of
    Nothing -> do
      let getInfo lang = (,) lang <$> txInfo pp lang ei sysS utxo tx
      txInfos <- array (PlutusV1, PlutusV2) <$> mapM getInfo (Set.toList nonNativeLanguages)
      pure . Right $ Map.mapWithKey (findAndCount pp txInfos) (unRedeemers $ getField @"txrdmrs" ws)
    Just e -> pure . Left $ e
  where
    txb = getField @"body" tx
    ws = getField @"wits" tx
    dats = unTxDats $ getField @"txdats" ws
    scripts = getField @"txscripts" ws

    ptrToPlutusScript = Map.fromList $ do
      (sp, sh) <- scriptsNeeded utxo tx
      msb <- case Map.lookup sh scripts of
        Nothing -> pure Nothing
        Just (TimelockScript _) -> []
        Just (PlutusScript v bytes) -> pure $ Just (bytes, v)
      pointer <- case rdptr @(AlonzoEra c) txb sp of
        SNothing -> []
        -- Since scriptsNeeded used the transaction to create script purposes,
        -- it would be a logic error if rdptr was not able to find sp.
        SJust p -> pure p
      pure (pointer, (sp, msb))

    findAndCount ::
      Core.PParams (AlonzoEra c) ->
      Array Language VersionedTxInfo ->
      RdmrPtr ->
      (Data (AlonzoEra c), ExUnits) ->
      Either (ScriptFailure c) ExUnits
    findAndCount pparams info pointer (rdmr, _) = do
      (sp, mscript) <- note (RedeemerNotNeeded pointer) $ Map.lookup pointer ptrToPlutusScript
      (script, lang) <- note (MissingScript pointer) mscript
      let inf = info ! lang
      let (l1, l2) = bounds costModels
      (CostModel costModel) <-
        if l1 <= lang && lang <= l2 then Right (costModels ! lang) else Left (NoCostModel lang)
      args <- case sp of
        (Spending txin) -> do
          txOut <- note (UnknownTxIn txin) $ Map.lookup txin (unUTxO utxo)
          let TxOut _ _ mdh = txOut
          dh <- note (InvalidTxIn txin) $ strictMaybeToMaybe mdh
          dat <- note (MissingDatum dh) $ Map.lookup dh dats
          pure [dat, rdmr, valContext inf sp]
        _ -> pure [rdmr, valContext inf sp]
      let pArgs = map getPlutusData args

      case interpreter lang costModel maxBudget script pArgs of
        (logs, Left e) -> case lang of
          PlutusV1 -> Left $ ValidationFailedV1 e logs
          PlutusV2 -> Left $ ValidationFailedV2 e logs
        (_, Right exBudget) -> note (IncompatibleBudget exBudget) $ exBudgetToExUnits exBudget
      where
        maxBudget = transExUnits . _maxTxExUnits $ pparams
        interpreter lang = case lang of
          PlutusV1 -> PV1.evaluateScriptRestricting PV1.Verbose
          PlutusV2 -> PV2.evaluateScriptRestricting PV2.Verbose
