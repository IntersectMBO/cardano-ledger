{-# LANGUAGE TemplateHaskell #-}

module Cardano.Ledger.Plutus.Preprocessor.Source.V3 where

import Language.Haskell.TH
import qualified PlutusLedgerApi.Data.V3 as PV3D
import PlutusTx (fromBuiltinData, unsafeFromBuiltinData)
import qualified PlutusTx.Builtins as P
import qualified PlutusTx.Data.AssocMap as PAMD
import qualified PlutusTx.Data.List as PLD
import qualified PlutusTx.Prelude as P

alwaysSucceedsNoDatumQ :: Q [Dec]
alwaysSucceedsNoDatumQ =
  [d|
    alwaysSucceedsNoDatum :: P.BuiltinData -> P.BuiltinUnit
    alwaysSucceedsNoDatum arg =
      let PV3D.ScriptContext _txInfo (PV3D.Redeemer _redeemer) scriptInfo =
            P.unsafeFromBuiltinData arg
       in P.check $
            case scriptInfo of
              -- We fail if this is a spending script with a Datum
              PV3D.SpendingScript _ (Just _) -> False
              _ -> True
    |]

alwaysSucceedsWithDatumQ :: Q [Dec]
alwaysSucceedsWithDatumQ =
  [d|
    alwaysSucceedsWithDatum :: P.BuiltinData -> P.BuiltinUnit
    alwaysSucceedsWithDatum arg =
      let PV3D.ScriptContext _txInfo (PV3D.Redeemer _redeemer) scriptPurpose =
            P.unsafeFromBuiltinData arg
       in P.check $
            case scriptPurpose of
              PV3D.SpendingScript _ (Just _) -> True
              -- Expecting a spending script with a Datum, thus failing when it is not
              _ -> False
    |]

alwaysFailsNoDatumQ :: Q [Dec]
alwaysFailsNoDatumQ =
  [d|
    alwaysFailsNoDatum :: P.BuiltinData -> P.BuiltinUnit
    alwaysFailsNoDatum arg =
      P.check $
        case fromBuiltinData arg of
          Just (PV3D.ScriptContext _txInfo (PV3D.Redeemer _redeemer) scriptInfo) ->
            case scriptInfo of
              -- We fail only if this is not a spending script with a Datum
              PV3D.SpendingScript _ (Just _) -> True
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
          Just (PV3D.ScriptContext _txInfo (PV3D.Redeemer _redeemer) scriptInfo) ->
            case scriptInfo of
              -- We fail only if this is a spending script with a Datum
              PV3D.SpendingScript _ (Just _) -> False
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
          PV3D.ScriptContext
            _txInfo
            (PV3D.Redeemer redeemer)
            (PV3D.SpendingScript _ (Just (PV3D.Datum datum))) ->
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
          PV3D.ScriptContext _txInfo _redeemer (PV3D.SpendingScript _ (Just (PV3D.Datum datum))) ->
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
          PV3D.ScriptContext _txInfo (PV3D.Redeemer redeemer) scriptInfo ->
            case scriptInfo of
              -- Expecting No Datum, therefore should fail when it is supplied
              PV3D.SpendingScript _ (Just _) -> False
              _ -> P.modulo (P.unsafeDataAsI redeemer) 2 P.== 0
    |]

evenRedeemerWithDatumQ :: Q [Dec]
evenRedeemerWithDatumQ =
  [d|
    evenRedeemerWithDatum :: P.BuiltinData -> P.BuiltinUnit
    evenRedeemerWithDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV3D.ScriptContext _txInfo (PV3D.Redeemer redeemer) (PV3D.SpendingScript _ (Just _)) ->
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
          PV3D.ScriptContext
            PV3D.TxInfo
              { PV3D.txInfoMint = infoMint
              , PV3D.txInfoInputs = infoInputs
              , PV3D.txInfoWdrl = infoWdrl
              , PV3D.txInfoTxCerts = infoTxCerts
              , PV3D.txInfoVotes = infoVotes
              }
            _redeemer
            scriptInfo -> case scriptInfo of
              PV3D.MintingScript cs ->
                PAMD.member cs $ PV3D.getValue $ PV3D.mintValueMinted infoMint
              -- Expecting No Datum, therefore should fail when it is supplied
              PV3D.SpendingScript txOutRef mDatum ->
                case mDatum of
                  Just _ -> False
                  Nothing ->
                    PLD.null $ PLD.filter ((txOutRef P.==) . PV3D.txInInfoOutRef) infoInputs
              PV3D.RewardingScript cred ->
                PAMD.member cred infoWdrl
              PV3D.CertifyingScript _idx txCert ->
                PLD.null $ PLD.filter (txCert P.==) infoTxCerts
              PV3D.VotingScript voter ->
                PAMD.member voter infoVotes
              PV3D.ProposingScript _idx _propProc -> True
              -- Eq instance for proposals have been removed. I am not sure if this is a
              -- good idea, but it only affects ledger test script, so we'll deal with
              -- this later:
              --
              -- null $ P.filter (propProc P.==) $ PV3.txInfoProposalProcedures txInfo
    |]

purposeIsWellformedWithDatumQ :: Q [Dec]
purposeIsWellformedWithDatumQ =
  [d|
    purposeIsWellformedWithDatum :: P.BuiltinData -> P.BuiltinUnit
    purposeIsWellformedWithDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV3D.ScriptContext txInfo _redeemer (PV3D.SpendingScript txOutRef (Just _)) ->
            not $ PLD.null $ PLD.filter ((txOutRef P.==) . PV3D.txInInfoOutRef) $ PV3D.txInfoInputs txInfo
          _ -> False
    |]

datumIsWellformedQ :: Q [Dec]
datumIsWellformedQ =
  [d|
    datumIsWellformed :: P.BuiltinData -> P.BuiltinUnit
    datumIsWellformed arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV3D.ScriptContext txInfo _redeemer (PV3D.SpendingScript _txOutRef (Just datum)) ->
            not $ PLD.null $ PLD.filter (datum P.==) $ PAMD.elems $ PV3D.txInfoData txInfo
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
          PV3D.ScriptContext _txInfo _redeemer (PV3D.SpendingScript _txOutRef (Just _)) -> False
          PV3D.ScriptContext txInfo _redeemer _scriptPurpose ->
            not $ PLD.null (PV3D.txInfoInputs txInfo) || PLD.null (PV3D.txInfoOutputs txInfo)
    |]

inputsOutputsAreNotEmptyWithDatumQ :: Q [Dec]
inputsOutputsAreNotEmptyWithDatumQ =
  [d|
    inputsOutputsAreNotEmptyWithDatum :: P.BuiltinData -> P.BuiltinUnit
    inputsOutputsAreNotEmptyWithDatum arg =
      P.check $
        case unsafeFromBuiltinData arg of
          PV3D.ScriptContext _txInfo _redeemer (PV3D.SpendingScript _txOutRef Nothing) -> False
          PV3D.ScriptContext txInfo _redeemer _scriptPurpose ->
            not $ PLD.null (PV3D.txInfoInputs txInfo) || PLD.null (PV3D.txInfoOutputs txInfo)
    |]

inputsIsSubsetOfRefInputsQ :: Q [Dec]
inputsIsSubsetOfRefInputsQ =
  [d|
    inputsIsSubsetOfRefInputs :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    inputsIsSubsetOfRefInputs _datum _redeemer context =
      case unsafeFromBuiltinData context of
        PV3D.ScriptContext txInfo _redeemer _scriptPurpose ->
          if PLD.all (\x -> P.isJust . PLD.find (P.== x) $ PV3D.txInfoReferenceInputs txInfo) $
            PV3D.txInfoInputs txInfo
            then ()
            else P.error ()
    |]
