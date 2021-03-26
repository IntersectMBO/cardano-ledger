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
import Cardano.Ledger.Alonzo.TxInfo (evalPlutusScript, transTx, valContext)
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
   in [ (script, d : (valContext txinfo sp ++ getData tx utxo sp), eu, cost)
        | (sp, scripthash) <- scriptsNeeded utxo tx, -- TODO, IN specification ORDER IS WRONG
          (d, eu) <- maybeToList (indexedRdmrs tx sp),
          script <- -- onlytwoPhaseScripts tx scripthash
            maybeToList (Map.lookup scripthash (txscripts' (getField @"wits" tx))),
          cost <- maybeToList (Map.lookup PlutusV1 (getField @"_costmdls" pp))
      ]

language :: Era era => AlonzoScript.Script era -> Maybe Language
language (AlonzoScript.PlutusScript _) = Just PlutusV1
language (AlonzoScript.TimelockScript _) = Nothing

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
  evalPlutusScript cost units pscript (map getPlutusData ds) && evalScripts tx rest

-- ===================================================================
-- From Specification, Figure 12 "UTXOW helper functions"

-- This is called checkRedeemers in the Specification

-- | Check that a script has whatever associated Data that it requires.
--     There are several ways this can happen
--     1) The script is Not a twoPhase script, so it doesn't need any data
--     2) The _txrdmrs Map of the TxWitness, contains Data for the script AND, either of the following
--        a) It is not a Spending script  OR
--        b) If it is a Spending Script, we can getData for it.
checkScriptData ::
  forall era.
  ( ValidateScript era,
    HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Crypto era))),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
  ) =>
  Tx era ->
  UTxO era ->
  (ScriptPurpose (Crypto era), ScriptHash (Crypto era)) ->
  Bool
checkScriptData tx utxo (sp, _h) = any ok scripts
  where
    scripts = txscripts' (getField @"wits" tx)
    isSpending (Spending _) = True
    isSpending _ = False
    ok s =
      isNativeScript @era s
        || ( isJust (indexedRdmrs tx sp)
               && (not (isSpending sp) || not (null (getData tx utxo sp)))
           )

-- THE SPEC CALLS FOR A SET, BUT THAT NEEDS A BUNCH OF ORD INSTANCES (DCert)
scriptsNeeded ::
  forall era.
  ( Era era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era)))
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
        accum !ans !accnt = case getRwdCred accnt of -- TODO  IS THIS RIGHT?
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
