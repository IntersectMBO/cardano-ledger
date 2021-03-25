{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.RunPlutus where

import Cardano.Ledger.Alonzo.Data (getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (CostModel, ExUnits (..))
import qualified Cardano.Ledger.Alonzo.Scripts as AlonzoScript (Script (..))
import Cardano.Ledger.Alonzo.Tx
  ( Data,
    DataHash,
    IsValidating (..),
    ScriptPurpose (..),
    Tx (..),
    indexedRdmrs,
    scriptsNeeded,
    txdats',
  )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (TxBody (..), TxOut (..))
import Cardano.Ledger.Alonzo.TxInfo (evalPlutusScript, valContext)
import Cardano.Ledger.Core as Core hiding (Tx)
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (..))
import qualified Cardano.Ledger.Mary.Value as Mary (Value (..))
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Records (HasField (..))
import qualified Plutus.V1.Ledger.Scripts as Plutus (Script)
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.Delegation.Certificates (DCert (..))
import Shelley.Spec.Ledger.Scripts (ScriptHash (..))
import Shelley.Spec.Ledger.TxBody (TxIn (..), Wdrl (..))
import Shelley.Spec.Ledger.UTxO (UTxO (..))

-- ===============================================================
-- From the specification, Figure 7 "Script Validation, cont."
-- ===============================================================

-- TODO  Specification says CostMod, not CostModel
runPLCScript ::
  CostModel ->
  AlonzoScript.Script era ->
  [Data era] ->
  ExUnits ->
  (IsValidating, ExUnits)
runPLCScript _cost _script _data _exunits = (IsValidating True, ExUnits 0 0) -- TODO FIX THIS

-- ===============================================================
-- From the specification, Figure 8 "Scripts and their Arguments"
-- ===============================================================

getData ::
  forall era.
  ( HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era)))
  ) =>
  Tx era ->
  UTxO era ->
  ScriptPurpose (Crypto era) ->
  [Data era]
getData tx (UTxO m) sp = case sp of
  Minting _policyid -> []
  Rewarding _rewaccnt -> []
  Certifying _dcert -> []
  Spending txin ->
    -- Only the Spending ScriptPurpose contains Data
    case Map.lookup txin m of
      Nothing -> []
      Just txout ->
        case getField @"datahash" txout of
          SNothing -> []
          SJust hash ->
            case Map.lookup hash (txdats' (getField @"wits" tx)) of
              Nothing -> []
              Just d -> [d]

collectNNScriptInputs ::
  ( Era era,
    Core.Script era ~ AlonzoScript.Script era,
    Core.TxOut era ~ Alonzo.TxOut era,
    Core.TxBody era ~ Alonzo.TxBody era,
    Core.Value era ~ Mary.Value (Crypto era),
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "_costmdls" (Core.PParams era) (Map.Map Language CostModel),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
  ) =>
  Core.PParams era ->
  Tx era ->
  UTxO era ->
  [(Plutus.Script, [Data era], ExUnits, CostModel)]
collectNNScriptInputs pp tx utxo =
  [ (script, d : (valContext utxo tx sp ++ getData tx utxo sp), eu, cost)
    | (sp, scripthash) <- scriptsNeeded utxo tx, -- TODO, IN specification ORDER IS WRONG
      (d, eu) <- maybeToList (indexedRdmrs tx sp),
      script <- onlytwoPhaseScripts tx scripthash, -- maybeToList (Map.lookup scripthash (txscripts' (getField @"wits" tx))),
      cost <- maybeToList (Map.lookup PlutusV1 (getField @"_costmdls" pp))
  ]

-- | return only the scripts that use two-phase validation (Here that means Plutus scripts)
onlytwoPhaseScripts ::
  ( Era era,
    Script era ~ AlonzoScript.Script era
  ) =>
  Tx era ->
  ScriptHash (Crypto era) ->
  [Plutus.Script]
onlytwoPhaseScripts tx scripthash =
  case Map.lookup scripthash (getField @"scriptWits" tx) of
    Just (AlonzoScript.PlutusScript pscript) -> [pscript]
    Just (AlonzoScript.NativeScript _) -> []
    Nothing -> []

evalScripts ::
  Typeable (Crypto era) =>
  [(AlonzoScript.Script era, [Data era], ExUnits, CostModel)] ->
  Bool
evalScripts [] = True
-- We may safely skip over the Timelock scripts
evalScripts ((AlonzoScript.NativeScript _, _, _, _) : rest) = evalScripts rest
evalScripts ((AlonzoScript.PlutusScript pscript, ds, units, cost) : rest) =
  evalPlutusScript cost units pscript (map getPlutusData ds) && evalScripts rest
