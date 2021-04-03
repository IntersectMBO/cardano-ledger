{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.PlutusScriptApi
  ( -- Figure 8
    getData,
    collectNNScriptInputs,
    evalScripts,
    -- Figure 12
    scriptsNeeded,
    checkScriptData,
    language,
  )
where

import Cardano.Ledger.Alonzo.Data (getPlutusData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (CostModel, ExUnits (..))
import qualified Cardano.Ledger.Alonzo.Scripts as AlonzoScript (Script (..))
import Cardano.Ledger.Alonzo.Tx
  ( Data,
    DataHash,
    ScriptPurpose (..),
    Tx (..),
    body',
    getValidatorHash,
    indexedRdmrs,
    txdats',
    wits',
  )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo (TxBody (..), TxOut (..), vldt')
import Cardano.Ledger.Alonzo.TxInfo (runPLCScript, transTx, valContext)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (txwitsVKey'), txscripts')
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (..))
import Cardano.Ledger.Mary.Value (PolicyID (..))
import qualified Cardano.Ledger.Mary.Value as Mary (Value (..))
import Cardano.Ledger.ShelleyMA.Timelocks (evalTimelock)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (isJust, maybeToList)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Records (HasField (..))
import Shelley.Spec.Ledger.Address (Addr)
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.Credential (Credential (ScriptHashObj))
import Shelley.Spec.Ledger.Delegation.Certificates (DCert (..))
import Shelley.Spec.Ledger.Scripts (ScriptHash (..))
import Shelley.Spec.Ledger.TxBody
  ( DelegCert (..),
    Delegation (..),
    TxIn (..),
    Wdrl (..),
    getRwdCred,
    witKeyHash,
  )
import Shelley.Spec.Ledger.UTxO (UTxO (..))

-- ===============================================================
-- From the specification, Figure 8 "Scripts and their Arguments"
-- ===============================================================

-- | Get the Data associated with a ScriptPurpose. Only the Spending
--   ScriptPurpose contains Data. The null list is returned for the other kinds.
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

-- | Collect the inputs (Data, execution budget, costModel) for all twoPhase scripts.
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
  let txinfo = transTx utxo tx
   in [ (script, d : (valContext txinfo sp : getData tx utxo sp), eu, cost)
        | (sp, scripthash) <- scriptsNeeded utxo tx, -- TODO, IN specification ORDER IS WRONG
          (d, eu) <- maybeToList (indexedRdmrs tx sp),
          script <- maybeToList (Map.lookup scripthash (txscripts' (getField @"wits" tx))),
          cost <- maybeToList (Map.lookup PlutusV1 (getField @"_costmdls" pp))
      ]

language :: Era era => AlonzoScript.Script era -> Maybe Language
language (AlonzoScript.PlutusScript _) = Just PlutusV1
language (AlonzoScript.TimelockScript _) = Nothing

-- | evaluate a list of scripts, All scripts in the list must be True.
--   There are two kinds of scripts, evaluate each kind using the
--   appropriate mechanism.
evalScripts ::
  forall era.
  ( Era era,
    Alonzo.TxBody era ~ Core.TxBody era
  ) =>
  Tx era ->
  [(AlonzoScript.Script era, [Data era], ExUnits, CostModel)] ->
  Bool
evalScripts _tx [] = True
evalScripts tx ((AlonzoScript.TimelockScript timelock, _, _, _) : rest) =
  evalTimelock vhks (Alonzo.vldt' (body' tx)) timelock && evalScripts tx rest
  where
    vhks = Set.map witKeyHash (txwitsVKey' (wits' tx))
evalScripts tx ((AlonzoScript.PlutusScript pscript, ds, units, cost) : rest) =
  runPLCScript cost pscript units (map getPlutusData ds) && evalScripts tx rest

-- ===================================================================
-- From Specification, Figure 12 "UTXOW helper functions"

-- This is called checkRedeemers in the Specification

-- | Check that a script has whatever associated Data that it requires.
--     There are several things need to test this:
--     1) The hash appears in the script map in the Witnesses
--     2) The script is Not a twoPhase script, so it doesn't need any data
--     3) The script is a twoPhase script, and the _txrdmrs Map of the TxWitness,
--        contains Data for the script.
checkScriptData ::
  forall era.
  ( ValidateScript era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
  ) =>
  Tx era ->
  -- UTxO era ->   -- TODO check that we really don't use the UTxO
  (ScriptPurpose (Crypto era), ScriptHash (Crypto era)) ->
  Bool
checkScriptData tx {- utxo -} (sp, h) =
  case Map.lookup h (txscripts' (getField @"wits" tx)) of
    Nothing -> False
    Just s -> if isNativeScript @era s then True else isJust (indexedRdmrs tx sp)

-- THE SPEC CALLS FOR A SET, BUT THAT NEEDS A BUNCH OF ORD INSTANCES (DCert)

-- Collect information (purpose and hash) about all the scripts in a Tx.
scriptsNeeded ::
  forall era.
  ( Era era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "address" (Core.TxOut era) (Addr (Crypto era))
  ) =>
  UTxO era ->
  Tx era ->
  [(ScriptPurpose (Crypto era), ScriptHash (Crypto era))]
scriptsNeeded (UTxO utxomap) tx = spend ++ reward ++ cert ++ minted
  where
    txb = body' tx
    !spend = foldl' accum [] (getField @"inputs" txb)
      where
        accum !ans !i =
          case Map.lookup i utxomap of
            Nothing -> ans
            Just txout ->
              case getValidatorHash (getField @"address" txout) of
                Nothing -> ans
                Just hash -> (Spending i, hash) : ans

    !reward = foldl' accum [] (Map.keys m2)
      where
        (Wdrl m2) = getField @"wdrls" txb
        accum !ans !accnt = case getRwdCred accnt of
          (ScriptHashObj hash) -> (Rewarding accnt, hash) : ans
          _ -> ans

    !cert = foldl addOnlyCwitness [] (getField @"certs" txb)

    !minted = foldr (\hash ans -> (Minting (PolicyID hash), hash) : ans) [] valuePolicyHashes
      where
        valuePolicyHashes = getField @"minted" txb

-- We only find certificate witnesses in Delegating and Deregistration DCerts
-- that have ScriptHashObj credentials.
addOnlyCwitness ::
  [(ScriptPurpose crypto, ScriptHash crypto)] ->
  DCert crypto ->
  [(ScriptPurpose crypto, ScriptHash crypto)]
addOnlyCwitness !ans (DCertDeleg c@(DeRegKey (ScriptHashObj hk))) =
  (Certifying $ DCertDeleg c, hk) : ans
addOnlyCwitness !ans (DCertDeleg c@(Delegate (Delegation (ScriptHashObj hk) _dpool))) =
  (Certifying $ DCertDeleg c, hk) : ans
addOnlyCwitness !ans _ = ans
