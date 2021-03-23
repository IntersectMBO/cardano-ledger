{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RunPlutus where

import Cardano.Ledger.Alonzo.Language (Language (..), nonNativeLanguages)
import Cardano.Ledger.Alonzo.Scripts (CostModel, ExUnits (..), Prices, scriptfee)
import qualified Cardano.Ledger.Alonzo.Scripts as AlonzoScript (Script (..), Tag (..))
import Cardano.Ledger.Alonzo.Tx
  ( CostModel (..),
    Data (..),
    DataHash (..),
    IsValidating (..),
    ScriptPurpose (..),
    Tx (..),
    indexedRdmrs,
    scriptsNeeded,
    txdats',
    txscripts',
  )
import Cardano.Ledger.Alonzo.TxBody (TxBody (..))
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (TxBody (..), TxOut (..))
import Cardano.Ledger.Alonzo.TxInfo (valContext)
import Cardano.Ledger.Core as Core hiding (Tx)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto, Era)
import qualified Cardano.Ledger.Mary.Value as Mary (Value (..))
import Data.ByteString as BS (ByteString)
import Data.ByteString.Short as SBS (fromShort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, maybeToList)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Records (HasField (..))
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..), maybeToStrictMaybe, strictMaybeToMaybe)
import Shelley.Spec.Ledger.Delegation.Certificates (DCert (..))
import Shelley.Spec.Ledger.TxBody (TxId (..), TxIn (..), Wdrl (..))
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
  [(AlonzoScript.Script era, [Data era], ExUnits, CostModel)]
collectNNScriptInputs pp tx utxo =
  [ (script, d : (valContext utxo tx sp ++ getData tx utxo sp), eu, cost)
    | (sp, scripthash) <- scriptsNeeded utxo tx, -- TODO, IN specification ORDER IS WRONG
      (d, eu) <- maybeToList (indexedRdmrs tx sp),
      script <- maybeToList (Map.lookup scripthash (txscripts' (getField @"wits" tx))),
      cost <- case language script of
        Nothing -> []
        Just lang -> maybeToList (Map.lookup lang (getField @"_costmdls" pp))
  ]

language :: Typeable (Crypto era) => AlonzoScript.Script era -> Maybe Language
language (AlonzoScript.NativeScript _) = Nothing
language (AlonzoScript.PlutusScript _) = Just PlutusV1

evalScripts ::
  Typeable (Crypto era) =>
  [(AlonzoScript.Script era, [Data era], ExUnits, CostModel)] ->
  Bool
evalScripts [] = True
evalScripts ((AlonzoScript.NativeScript _timelock, _, _, _) : rest) =
  evalScripts rest
evalScripts ((AlonzoScript.PlutusScript s, ds, units, cost) : rest) =
  b && evalScripts rest
  where
    (IsValidating b, _exunits) = runPLCScript cost (AlonzoScript.PlutusScript s) ds units
