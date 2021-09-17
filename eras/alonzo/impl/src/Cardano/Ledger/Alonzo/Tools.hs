{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.Tools
  ( evaluateTransactionExecutionUnits,
    ScriptFailure (..),
  )
where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data, getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams (_protocolVersion)
import Cardano.Ledger.Alonzo.PlutusScriptApi (scriptsNeeded)
import Cardano.Ledger.Alonzo.Scripts
  ( CostModel (..),
    ExUnits (..),
    Script (..),
  )
import Cardano.Ledger.Alonzo.Tx (DataHash, ScriptPurpose (Spending), ValidatedTx (..), rdptr)
import Cardano.Ledger.Alonzo.TxBody (TxOut (..))
import Cardano.Ledger.Alonzo.TxInfo (exBudgetToExUnits, txInfo, valContext)
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (..), unRedeemers, unTxDats)
import Cardano.Ledger.BaseTypes (StrictMaybe (..), strictMaybeToMaybe)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Shelley.Tx (TxIn)
import Cardano.Ledger.Shelley.UTxO (UTxO (..), unUTxO)
import Cardano.Slotting.EpochInfo.API (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.Array (Array, (!))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Records (HasField (..))
import qualified Plutus.V1.Ledger.Api as P

-- | Failures that can be returned by 'evaluateTransactionExecutionUnits'.
data ScriptFailure c
  = -- | A redeemer was supplied that does not point to a
    --  valid plutus evaluation site in the given transaction.
    RedeemerNotNeeded RdmrPtr
  | -- | Missing redeemer.
    MissingScript RdmrPtr
  | -- | Missing datum.
    MissingDatum (DataHash c)
  | -- | Plutus evaluation error.
    ValidationFailed P.EvaluationError
  | -- | A redeemer points to a transaction input which is not
    --  present in the current UTxO.
    UnknownTxIn (TxIn c)
  | -- | A redeemer points to a transaction input which is not
    --  plutus locked.
    InvalidTxIn (TxIn c)
  | -- | The execution budget that was calculated by the Plutus
    --  evaluator is out of bounds.
    IncompatibleBudget P.ExBudget
  deriving (Show)

note :: e -> Maybe a -> Either e a
note _ (Just x) = Right x
note e Nothing = Left e

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
  -- | A map from redeemer pointers to either a failure or a sufficient execution budget.
  --  The value is monadic, depending on the epoch info.
  m (Map RdmrPtr (Either (ScriptFailure c) ExUnits))
evaluateTransactionExecutionUnits pp tx utxo ei sysS costModels = do
  txinfo <- txInfo pp ei sysS utxo tx
  pure $ Map.mapWithKey (findAndCount txinfo) (unRedeemers $ getField @"txrdmrs" ws)
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
        Just (PlutusScript bytes) -> pure $ Just bytes
      pointer <- case rdptr @(AlonzoEra c) txb sp of
        SNothing -> []
        -- Since scriptsNeeded used the transaction to create script purposes,
        -- it would be a logic error if rdptr was not able to find sp.
        SJust p -> pure p
      pure (pointer, (sp, msb))

    (CostModel costModel) = costModels ! PlutusV1

    findAndCount ::
      P.TxInfo ->
      RdmrPtr ->
      (Data (AlonzoEra c), ExUnits) ->
      Either (ScriptFailure c) ExUnits
    findAndCount inf pointer (rdmr, _) = do
      (sp, mscript) <- note (RedeemerNotNeeded pointer) $ Map.lookup pointer ptrToPlutusScript
      script <- note (MissingScript pointer) mscript
      args <- case sp of
        (Spending txin) -> do
          txOut <- note (UnknownTxIn txin) $ Map.lookup txin (unUTxO utxo)
          let TxOut _ _ mdh = txOut
          dh <- note (InvalidTxIn txin) $ strictMaybeToMaybe mdh
          dat <- note (MissingDatum dh) $ Map.lookup dh dats
          pure [dat, rdmr, valContext inf sp]
        _ -> pure [rdmr, valContext inf sp]
      let pArgs = map getPlutusData args

      case snd $ P.evaluateScriptCounting P.Quiet costModel script pArgs of
        Left e -> Left $ ValidationFailed e
        Right exBudget -> note (IncompatibleBudget exBudget) $ exBudgetToExUnits exBudget
