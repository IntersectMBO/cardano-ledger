{-# LANGUAGE TemplateHaskell #-}

module Cardano.Ledger.Plutus.Preprocessor.Source.V3 where

import Language.Haskell.TH
import qualified PlutusLedgerApi.V3 as PV3
import PlutusTx (fromBuiltinData, unsafeFromBuiltinData)
import qualified PlutusTx.AssocMap as PAM
import qualified PlutusTx.Builtins as P
import qualified PlutusTx.Prelude as P

alwaysSucceedsNoDatumQ :: Q [Dec]
alwaysSucceedsNoDatumQ =
  [d|
    alwaysSucceedsNoDatum :: P.BuiltinData -> P.BuiltinUnit
    alwaysSucceedsNoDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) scriptInfo ->
            case scriptInfo of
              -- We fail if this is a spending script with a Datum
              PV3.SpendingScript _ (Just _) -> False
              _ -> True
    |]

alwaysSucceedsWithDatumQ :: Q [Dec]
alwaysSucceedsWithDatumQ =
  [d|
    alwaysSucceedsWithDatum :: P.BuiltinData -> P.BuiltinUnit
    alwaysSucceedsWithDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          -- Expecting a spending script with a Datum, thus failing when it is not
          PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) (PV3.SpendingScript _ (Just _)) -> True
          _ -> False
    |]

alwaysFailsNoDatumQ :: Q [Dec]
alwaysFailsNoDatumQ =
  [d|
    alwaysFailsNoDatum :: P.BuiltinData -> P.BuiltinUnit
    alwaysFailsNoDatum arg =
      P.check $
        case fromBuiltinData arg of
          Just (PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) scriptInfo) ->
            case scriptInfo of
              -- We fail only if this is not a spending script with a Datum
              PV3.SpendingScript _ (Just _) -> True
              _ -> False
          Nothing -> True
    |]

alwaysFailsWithDatumQ :: Q [Dec]
alwaysFailsWithDatumQ =
  [d|
    alwaysFailsWithDatum :: P.BuiltinData -> P.BuiltinUnit
    alwaysFailsWithDatum arg =
      P.check $
        case fromBuiltinData arg of
          Just (PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) scriptInfo) ->
            case scriptInfo of
              -- We fail only if this is a spending script with a Datum
              PV3.SpendingScript _ (Just _) -> False
              _ -> True
          Nothing -> True
    |]

redeemerSameAsDatumQ :: Q [Dec]
redeemerSameAsDatumQ =
  [d|
    redeemerSameAsDatum :: P.BuiltinData -> P.BuiltinUnit
    redeemerSameAsDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV3.ScriptContext _txInfo (PV3.Redeemer redeemer) (PV3.SpendingScript _ (Just (PV3.Datum datum))) ->
            -- Expecting a spending script with a Datum, thus failing when it is not
            datum P.== redeemer
          _ -> False
    |]

evenDatumQ :: Q [Dec]
evenDatumQ =
  [d|
    evenDatum :: P.BuiltinData -> P.BuiltinUnit
    evenDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV3.ScriptContext _txInfo _redeemer (PV3.SpendingScript _ (Just (PV3.Datum datum))) ->
            -- Expecting a spending script with a Datum, thus failing when it is not
            P.modulo (P.unsafeDataAsI datum) 2 P.== 0
    |]

evenRedeemerNoDatumQ :: Q [Dec]
evenRedeemerNoDatumQ =
  [d|
    evenRedeemerNoDatum :: P.BuiltinData -> P.BuiltinUnit
    evenRedeemerNoDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV3.ScriptContext _txInfo (PV3.Redeemer redeemer) scriptInfo ->
            case scriptInfo of
              -- Expecting No Datum, therefore should fail when it is supplied
              PV3.SpendingScript _ (Just _) -> False
              _ -> P.modulo (P.unsafeDataAsI redeemer) 2 P.== 0
    |]

evenRedeemerWithDatumQ :: Q [Dec]
evenRedeemerWithDatumQ =
  [d|
    evenRedeemerWithDatum :: P.BuiltinData -> P.BuiltinUnit
    evenRedeemerWithDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV3.ScriptContext _txInfo (PV3.Redeemer redeemer) (PV3.SpendingScript _ (Just _)) ->
            -- Expecting a spending script with a Datum, thus failing when it is not
            P.modulo (P.unsafeDataAsI redeemer) 2 P.== 0
          _ -> False
    |]

purposeIsWellformedNoDatumQ :: Q [Dec]
purposeIsWellformedNoDatumQ =
  [d|
    purposeIsWellformedNoDatum :: P.BuiltinData -> P.BuiltinUnit
    purposeIsWellformedNoDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV3.ScriptContext txInfo _redeemer scriptInfo ->
            case scriptInfo of
              PV3.MintingScript cs ->
                PAM.member cs $ PV3.getValue $ PV3.txInfoMint txInfo
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
              PV3.ProposingScript _idx propProc ->
                null $ P.filter (propProc P.==) $ PV3.txInfoProposalProcedures txInfo
    |]

purposeIsWellformedWithDatumQ :: Q [Dec]
purposeIsWellformedWithDatumQ =
  [d|
    purposeIsWellformedWithDatum :: P.BuiltinData -> P.BuiltinUnit
    purposeIsWellformedWithDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV3.ScriptContext txInfo _redeemer (PV3.SpendingScript txOutRef (Just _)) ->
            null $ P.filter ((txOutRef P.==) . PV3.txInInfoOutRef) $ PV3.txInfoInputs txInfo
          _ -> False
    |]

datumIsWellformedQ :: Q [Dec]
datumIsWellformedQ =
  [d|
    datumIsWellformed :: P.BuiltinData -> P.BuiltinUnit
    datumIsWellformed arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV3.ScriptContext _txInfo _redeemer (PV3.SpendingScript _txOutRef Nothing) -> True
          PV3.ScriptContext txInfo _redeemer (PV3.SpendingScript _txOutRef (Just datum)) ->
            null $ P.filter (datum P.==) $ PAM.elems $ PV3.txInfoData txInfo
          _ -> False
    |]

inputsOutputsAreNotEmptyNoDatumQ :: Q [Dec]
inputsOutputsAreNotEmptyNoDatumQ =
  [d|
    inputsOutputsAreNotEmptyNoDatum :: P.BuiltinData -> P.BuiltinUnit
    inputsOutputsAreNotEmptyNoDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          -- When there is a datum supplied, we need to fail.
          PV3.ScriptContext _txInfo _redeemer (PV3.SpendingScript _txOutRef (Just _)) -> False
          PV3.ScriptContext txInfo _redeemer _scriptPurpose ->
            null (PV3.txInfoInputs txInfo) || null (PV3.txInfoOutputs txInfo)
    |]

inputsOutputsAreNotEmptyWithDatumQ :: Q [Dec]
inputsOutputsAreNotEmptyWithDatumQ =
  [d|
    inputsOutputsAreNotEmptyWithDatum :: P.BuiltinData -> P.BuiltinUnit
    inputsOutputsAreNotEmptyWithDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV3.ScriptContext _txInfo _redeemer (PV3.SpendingScript _txOutRef Nothing) -> False
          PV3.ScriptContext txInfo _redeemer _scriptPurpose ->
            null (PV3.txInfoInputs txInfo) || null (PV3.txInfoOutputs txInfo)
    |]
