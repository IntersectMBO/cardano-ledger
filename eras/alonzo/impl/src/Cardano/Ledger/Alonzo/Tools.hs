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

import Cardano.Ledger.Alonzo.Data (Data, getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (scriptsNeeded)
import Cardano.Ledger.Alonzo.Scripts
  ( CostModel (..),
    ExUnits (..),
    Script (..),
    getEvaluationContext,
  )
import Cardano.Ledger.Alonzo.Tx (DataHash, ScriptPurpose (..), rdptr)
import Cardano.Ledger.Alonzo.TxInfo
  ( ExtendedUTxO (txscripts),
    TranslationError,
    VersionedTxInfo (..),
    exBudgetToExUnits,
    transExUnits,
    transProtocolVersion,
    txInfo,
    valContext,
  )
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (..), Redeemers, TxDats, unRedeemers, unTxDats)
import Cardano.Ledger.BaseTypes (ProtVer, StrictMaybe (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley.Tx (TxIn)
import Cardano.Ledger.Shelley.TxBody (DCert, Wdrl)
import Cardano.Ledger.Shelley.UTxO (UTxO (..), unUTxO)
import Cardano.Slotting.EpochInfo.API (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.Array (Array, array, bounds, (!))
import qualified Data.Compact.SplitMap as SplitMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Sequence.Strict (StrictSeq)
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
  | -- | The transaction context translation failed
    BadTranslation TranslationError
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
  | -- | There was no cost model for a given version of Plutus
    NoCostModel Language
  | -- | There was a corruptp cost model for a given version of Plutus
    CorruptCostModel Language
  deriving (Show)

note :: e -> Maybe a -> Either e a
note _ (Just x) = Right x
note e Nothing = Left e

basicValidation ::
  ( HasField "body" (Core.Tx era) (Core.TxBody era),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  -- | The transaction.
  Core.Tx era ->
  -- | The current UTxO set (or the relevant portion for the transaction).
  UTxO era ->
  -- | Basic failures.
  Maybe (BasicFailure (Crypto era))
basicValidation tx utxo =
  if Set.null badIns
    then Nothing
    else Just (UnknownTxIns badIns)
  where
    txb = getField @"body" tx
    ins = getField @"inputs" txb
    badIns = Set.filter (`SplitMap.notMember` unUTxO utxo) ins

type RedeemerReport c = Map RdmrPtr (Either (ScriptFailure c) ExUnits)

languagesUsed :: [Script era] -> Set Language
languagesUsed scripts = Set.fromList $ mapMaybe getLanguage scripts
  where
    getLanguage (TimelockScript _) = Nothing
    getLanguage (PlutusScript lang _) = Just lang

-- | Evaluate the execution budgets needed for all the redeemers in
--  a given transaction. If a redeemer is invalid, a failure is returned instead.
--
--  The execution budgets in the supplied transaction are completely ignored.
--  The results of 'evaluateTransactionExecutionUnits' are intended to replace them.
evaluateTransactionExecutionUnits ::
  forall era m.
  ( Era era,
    ExtendedUTxO era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "txdats" (Core.Witnesses era) (TxDats era),
    HasField "txrdmrs" (Core.Witnesses era) (Redeemers era),
    HasField "_maxTxExUnits" (Core.PParams era) ExUnits,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "datum" (Core.TxOut era) (StrictMaybe (Data era)),
    Core.Script era ~ Script era,
    Monad m
  ) =>
  Core.PParams era ->
  -- | The transaction.
  Core.Tx era ->
  -- | The current UTxO set (or the relevant portion for the transaction).
  UTxO era ->
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
  m (Either (BasicFailure (Crypto era)) (RedeemerReport (Crypto era)))
evaluateTransactionExecutionUnits pp tx utxo ei sysS costModels = do
  case basicValidation tx utxo of
    Nothing -> do
      let getInfo :: Language -> m (Either TranslationError (Language, VersionedTxInfo))
          getInfo lang = ((,) lang <$>) <$> txInfo pp lang ei sysS utxo tx
      txInfos <- mapM getInfo (Set.toList $ languagesUsed (Map.elems scripts))
      case sequence txInfos of
        Left transEr -> pure . Left $ BadTranslation transEr
        Right ctx ->
          pure . Right $
            Map.mapWithKey
              (findAndCount pp (array (PlutusV1, PlutusV2) ctx))
              (unRedeemers $ getField @"txrdmrs" ws)
    Just e -> pure . Left $ e
  where
    txb = getField @"body" tx
    ws = getField @"wits" tx
    dats = unTxDats $ getField @"txdats" ws
    scripts = txscripts utxo tx
    needed = scriptsNeeded utxo tx

    ptrToPlutusScript = Map.fromList $ do
      (sp, sh) <- needed
      msb <- case Map.lookup sh scripts of
        Nothing -> pure Nothing
        Just (TimelockScript _) -> []
        Just (PlutusScript v bytes) -> pure $ Just (bytes, v)
      pointer <- case rdptr txb sp of
        SNothing -> []
        -- Since scriptsNeeded used the transaction to create script purposes,
        -- it would be a logic error if rdptr was not able to find sp.
        SJust p -> pure p
      pure (pointer, (sp, msb))

    findAndCount ::
      Core.PParams era ->
      Array Language VersionedTxInfo ->
      RdmrPtr ->
      (Data era, ExUnits) ->
      Either (ScriptFailure (Crypto era)) ExUnits
    findAndCount pparams info pointer (rdmr, _) = do
      (sp, mscript) <- note (RedeemerNotNeeded pointer) $ Map.lookup pointer ptrToPlutusScript
      (script, lang) <- note (MissingScript pointer) mscript
      let inf = info ! lang
      let (l1, l2) = bounds costModels
      cm <- if l1 <= lang && lang <= l2 then Right (costModels ! lang) else Left (NoCostModel lang)
      args <- case sp of
        (Spending txin) -> do
          txOut <- note (UnknownTxIn txin) $ SplitMap.lookup txin (unUTxO utxo)
          let mdh = getField @"datahash" txOut
              md = getField @"datum" txOut
          dat <- case (md, mdh) of
            (SJust d, _) -> pure d
            (_, SJust dh) -> note (MissingDatum dh) $ Map.lookup dh dats
            (SNothing, SNothing) -> Left (InvalidTxIn txin)
          pure [dat, rdmr, valContext inf sp]
        _ -> pure [rdmr, valContext inf sp]
      let pArgs = map getPlutusData args

      case interpreter lang (getEvaluationContext cm) maxBudget script pArgs of
        (logs, Left e) -> case lang of
          PlutusV1 -> Left $ ValidationFailedV1 e logs
          PlutusV2 -> Left $ ValidationFailedV2 e logs
        (_, Right exBudget) -> note (IncompatibleBudget exBudget) $ exBudgetToExUnits exBudget
      where
        maxBudget = transExUnits . getField @"_maxTxExUnits" $ pparams
        pv = transProtocolVersion . getField @"_protocolVersion" $ pparams
        interpreter lang = case lang of
          PlutusV1 -> PV1.evaluateScriptRestricting pv PV1.Verbose
          PlutusV2 -> PV2.evaluateScriptRestricting pv PV2.Verbose
