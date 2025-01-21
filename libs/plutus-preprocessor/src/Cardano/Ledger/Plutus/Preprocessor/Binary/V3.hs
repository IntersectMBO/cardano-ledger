{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Cardano.Ledger.Plutus.Preprocessor.Binary.V3 where

import Cardano.Ledger.Plutus.Language (PlutusBinary (..))
import Cardano.Ledger.Plutus.Preprocessor.Source.V3
import Language.Haskell.TH
import qualified PlutusLedgerApi.V3 as PV3

import qualified PlutusTx as P
import qualified PlutusTx.Builtins as P
import qualified PlutusTx.Prelude as P
import qualified PlutusTx.AssocMap as PAM

-- ==========================================================================
-- Turn the Template Haskell Declarations into real haskell functions

{-

$alwaysSucceedsNoDatumQ
$alwaysSucceedsWithDatumQ
$alwaysFailsNoDatumQ
$alwaysFailsWithDatumQ
$redeemerSameAsDatumQ
$evenDatumQ
$evenRedeemerNoDatumQ
$evenRedeemerWithDatumQ
$purposeIsWellformedNoDatumQ
$purposeIsWellformedWithDatumQ
$datumIsWellformedQ
$inputsOutputsAreNotEmptyNoDatumQ
$inputsOutputsAreNotEmptyWithDatumQ

-}

-- Manually spliced TH quotes:

alwaysSucceedsNoDatum :: P.BuiltinData -> P.BuiltinUnit
alwaysSucceedsNoDatum arg =
  P.check $
    case P.unsafeFromBuiltinData arg of
      PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) scriptInfo ->
        case scriptInfo of
          -- We fail if this is a spending script with a Datum
          PV3.SpendingScript _ (Just _) -> False
          _ -> True

alwaysSucceedsWithDatum :: P.BuiltinData -> P.BuiltinUnit
alwaysSucceedsWithDatum arg =
  P.check $
    case P.unsafeFromBuiltinData arg of
      -- Expecting a spending script with a Datum, thus failing when it is not
      PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) (PV3.SpendingScript _ (Just _)) -> True
      _ -> False

alwaysFailsNoDatum :: P.BuiltinData -> P.BuiltinUnit
alwaysFailsNoDatum arg =
  P.check $
    case P.fromBuiltinData arg of
      Just (PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) scriptInfo) ->
        case scriptInfo of
          -- We fail only if this is not a spending script with a Datum
          PV3.SpendingScript _ (Just _) -> True
          _ -> False
      Nothing -> True

alwaysFailsWithDatum :: P.BuiltinData -> P.BuiltinUnit
alwaysFailsWithDatum arg =
  P.check $
    case P.fromBuiltinData arg of
      Just (PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) scriptInfo) ->
        case scriptInfo of
          -- We fail only if this is a spending script with a Datum
          PV3.SpendingScript _ (Just _) -> False
          _ -> True
      Nothing -> True

redeemerSameAsDatum :: P.BuiltinData -> P.BuiltinUnit
redeemerSameAsDatum arg =
  P.check $
    case P.unsafeFromBuiltinData arg of
      PV3.ScriptContext _txInfo (PV3.Redeemer redeemer) (PV3.SpendingScript _ (Just (PV3.Datum datum))) ->
        -- Expecting a spending script with a Datum, thus failing when it is not
        datum P.== redeemer
      _ -> False

evenDatum :: P.BuiltinData -> P.BuiltinUnit
evenDatum arg =
  P.check $
    case P.unsafeFromBuiltinData arg of
      PV3.ScriptContext _txInfo _redeemer (PV3.SpendingScript _ (Just (PV3.Datum datum))) ->
        -- Expecting a spending script with a Datum, thus failing when it is not
        P.modulo (P.unsafeDataAsI datum) 2 P.== 0
      _ -> False

evenRedeemerNoDatum :: P.BuiltinData -> P.BuiltinUnit
evenRedeemerNoDatum arg =
  P.check $
    case P.unsafeFromBuiltinData arg of
      PV3.ScriptContext _txInfo (PV3.Redeemer redeemer) scriptInfo ->
        case scriptInfo of
          -- Expecting No Datum, therefore should fail when it is supplied
          PV3.SpendingScript _ (Just _) -> False
          _ -> P.modulo (P.unsafeDataAsI redeemer) 2 P.== 0

evenRedeemerWithDatum :: P.BuiltinData -> P.BuiltinUnit
evenRedeemerWithDatum arg =
  P.check $
    case P.unsafeFromBuiltinData arg of
      PV3.ScriptContext _txInfo (PV3.Redeemer redeemer) (PV3.SpendingScript _ (Just _)) ->
        -- Expecting a spending script with a Datum, thus failing when it is not
        P.modulo (P.unsafeDataAsI redeemer) 2 P.== 0
      _ -> False

purposeIsWellformedNoDatum :: P.BuiltinData -> P.BuiltinUnit
purposeIsWellformedNoDatum arg =
  P.check $
    case P.unsafeFromBuiltinData arg of
      PV3.ScriptContext txInfo _redeemer scriptInfo ->
        case scriptInfo of
          PV3.MintingScript cs ->
            PAM.member cs $ PV3.getValue . PV3.mintValueMinted $ PV3.txInfoMint txInfo
          -- Expecting No Datum, therefore should fail when it is supplied
          PV3.SpendingScript txOutRef mDatum ->
            case mDatum of
              Just _ -> False
              Nothing ->
                null $ P.filter ((txOutRef P.==) . PV3.txInInfoOutRef) $ PV3.txInfoInputs txInfo
          PV3.RewardingScript stakingCredential ->
            PAM.member stakingCredential $ PV3.txInfoWdrl txInfo
          PV3.CertifyingScript _idx txCert ->
            null $ P.filter (txCert P.==) $ PV3.txInfoTxCerts txInfo
          PV3.VotingScript voter ->
            PAM.member voter $ PV3.txInfoVotes txInfo
          PV3.ProposingScript _idx _propProc -> True
          -- Eq instance for proposals have been removed. I am not sure if this is a
          -- good idea, but it only affects ledger test script, so we'll deal with
          -- this later:
          --
          -- null $ P.filter (propProc P.==) $ PV3.txInfoProposalProcedures txInfo

purposeIsWellformedWithDatum :: P.BuiltinData -> P.BuiltinUnit
purposeIsWellformedWithDatum arg =
  P.check $
    case P.unsafeFromBuiltinData arg of
      PV3.ScriptContext txInfo _redeemer (PV3.SpendingScript txOutRef (Just _)) ->
        not $ null $ P.filter ((txOutRef P.==) . PV3.txInInfoOutRef) $ PV3.txInfoInputs txInfo
      _ -> False

datumIsWellformed :: P.BuiltinData -> P.BuiltinUnit
datumIsWellformed arg =
  P.check $
    case P.unsafeFromBuiltinData arg of
      PV3.ScriptContext txInfo _redeemer (PV3.SpendingScript _txOutRef (Just datum)) ->
        not $ null $ P.filter (datum P.==) $ PAM.elems $ PV3.txInfoData txInfo
      _ -> False

inputsOutputsAreNotEmptyNoDatum :: P.BuiltinData -> P.BuiltinUnit
inputsOutputsAreNotEmptyNoDatum arg =
  P.check $
    case P.unsafeFromBuiltinData arg of
      -- When there is a datum supplied, we need to fail.
      PV3.ScriptContext _txInfo _redeemer (PV3.SpendingScript _txOutRef (Just _)) -> False
      PV3.ScriptContext txInfo _redeemer _scriptPurpose ->
        not $ null (PV3.txInfoInputs txInfo) || null (PV3.txInfoOutputs txInfo)

inputsOutputsAreNotEmptyWithDatum :: P.BuiltinData -> P.BuiltinUnit
inputsOutputsAreNotEmptyWithDatum arg =
  P.check $
    case P.unsafeFromBuiltinData arg of
      PV3.ScriptContext _txInfo _redeemer (PV3.SpendingScript _txOutRef Nothing) -> False
      PV3.ScriptContext txInfo _redeemer _scriptPurpose ->
        not $ null (PV3.txInfoInputs txInfo) || null (PV3.txInfoOutputs txInfo)

-- ================================================================
-- Compile and serialize the real functions as Plutus scripts.
-- Here is where we depend on plutus-plugin.

alwaysSucceedsNoDatumBytes :: (Q [Dec], PlutusBinary)
alwaysSucceedsNoDatumBytes =
  ( alwaysSucceedsNoDatumQ
  , PlutusBinary $ PV3.serialiseCompiledCode $$(P.compile [||alwaysSucceedsNoDatum||])
  )

alwaysSucceedsWithDatumBytes :: (Q [Dec], PlutusBinary)
alwaysSucceedsWithDatumBytes =
  ( alwaysSucceedsWithDatumQ
  , PlutusBinary $ PV3.serialiseCompiledCode $$(P.compile [||alwaysSucceedsWithDatum||])
  )

alwaysFailsNoDatumBytes :: (Q [Dec], PlutusBinary)
alwaysFailsNoDatumBytes =
  ( alwaysFailsNoDatumQ
  , PlutusBinary $ PV3.serialiseCompiledCode $$(P.compile [||alwaysFailsNoDatum||])
  )

alwaysFailsWithDatumBytes :: (Q [Dec], PlutusBinary)
alwaysFailsWithDatumBytes =
  ( alwaysFailsWithDatumQ
  , PlutusBinary $ PV3.serialiseCompiledCode $$(P.compile [||alwaysFailsWithDatum||])
  )

redeemerSameAsDatumBytes :: (Q [Dec], PlutusBinary)
redeemerSameAsDatumBytes =
  ( redeemerSameAsDatumQ
  , PlutusBinary $ PV3.serialiseCompiledCode $$(P.compile [||redeemerSameAsDatum||])
  )

evenDatumBytes :: (Q [Dec], PlutusBinary)
evenDatumBytes =
  ( evenDatumQ
  , PlutusBinary $ PV3.serialiseCompiledCode $$(P.compile [||evenDatum||])
  )

evenRedeemerNoDatumBytes :: (Q [Dec], PlutusBinary)
evenRedeemerNoDatumBytes =
  ( evenRedeemerNoDatumQ
  , PlutusBinary $ PV3.serialiseCompiledCode $$(P.compile [||evenRedeemerNoDatum||])
  )

evenRedeemerWithDatumBytes :: (Q [Dec], PlutusBinary)
evenRedeemerWithDatumBytes =
  ( evenRedeemerWithDatumQ
  , PlutusBinary $ PV3.serialiseCompiledCode $$(P.compile [||evenRedeemerWithDatum||])
  )

purposeIsWellformedNoDatumBytes :: (Q [Dec], PlutusBinary)
purposeIsWellformedNoDatumBytes =
  ( purposeIsWellformedNoDatumQ
  , PlutusBinary $ PV3.serialiseCompiledCode $$(P.compile [||purposeIsWellformedNoDatum||])
  )

purposeIsWellformedWithDatumBytes :: (Q [Dec], PlutusBinary)
purposeIsWellformedWithDatumBytes =
  ( purposeIsWellformedWithDatumQ
  , PlutusBinary $ PV3.serialiseCompiledCode $$(P.compile [||purposeIsWellformedWithDatum||])
  )

datumIsWellformedBytes :: (Q [Dec], PlutusBinary)
datumIsWellformedBytes =
  ( datumIsWellformedQ
  , PlutusBinary $ PV3.serialiseCompiledCode $$(P.compile [||datumIsWellformed||])
  )

inputsOutputsAreNotEmptyNoDatumBytes :: (Q [Dec], PlutusBinary)
inputsOutputsAreNotEmptyNoDatumBytes =
  ( inputsOutputsAreNotEmptyNoDatumQ
  , PlutusBinary $ PV3.serialiseCompiledCode $$(P.compile [||inputsOutputsAreNotEmptyNoDatum||])
  )

inputsOutputsAreNotEmptyWithDatumBytes :: (Q [Dec], PlutusBinary)
inputsOutputsAreNotEmptyWithDatumBytes =
  ( inputsOutputsAreNotEmptyWithDatumQ
  , PlutusBinary $ PV3.serialiseCompiledCode $$(P.compile [||inputsOutputsAreNotEmptyWithDatum||])
  )
