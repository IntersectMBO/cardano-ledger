{-# LANGUAGE TemplateHaskell #-}

module Cardano.Ledger.Plutus.Preprocessor.Source.V1 where

import Language.Haskell.TH
import qualified PlutusLedgerApi.V1 as PV1
import PlutusTx (fromBuiltinData, unsafeFromBuiltinData)
import qualified PlutusTx.AssocMap as PAM
import qualified PlutusTx.Builtins as P
import qualified PlutusTx.Prelude as P

alwaysSucceedsNoDatumQ :: Q [Dec]
alwaysSucceedsNoDatumQ =
  [d|
    alwaysSucceedsNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    alwaysSucceedsNoDatum redeemer context =
      case unsafeFromBuiltinData redeemer of
        PV1.Redeemer _ ->
          case unsafeFromBuiltinData context of
            PV1.ScriptContext _ scriptPurpose ->
              case scriptPurpose of
                -- Spending PlutusV1 scripts must have a Datum.
                PV1.Spending _ -> P.error ()
                _ -> ()
    |]

alwaysSucceedsWithDatumQ :: Q [Dec]
alwaysSucceedsWithDatumQ =
  [d|
    alwaysSucceedsWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    alwaysSucceedsWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV1.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV1.Redeemer _ ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum.
                PV1.ScriptContext _ (PV1.Spending _) -> ()
    |]

alwaysFailsNoDatumQ :: Q [Dec]
alwaysFailsNoDatumQ =
  [d|
    alwaysFailsNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    alwaysFailsNoDatum redeemer context =
      case fromBuiltinData redeemer of
        Nothing -> ()
        Just (PV1.Redeemer _) ->
          case fromBuiltinData context of
            Nothing -> ()
            -- Spending PlutusV1 scripts must have a Datum, thas passing a script that is
            -- expected to fail
            Just (PV1.ScriptContext _ (PV1.Spending _)) -> ()
            Just (PV1.ScriptContext _ _) -> P.error ()
    |]

alwaysFailsWithDatumQ :: Q [Dec]
alwaysFailsWithDatumQ =
  [d|
    alwaysFailsWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    alwaysFailsWithDatum datum redeemer context =
      case fromBuiltinData datum of
        Nothing -> ()
        Just (PV1.Datum _) ->
          case fromBuiltinData redeemer of
            Nothing -> ()
            Just (PV1.Redeemer _) ->
              case fromBuiltinData context of
                Nothing -> ()
                -- Purposefully failing only on Spending with Datum because only spending
                -- scripts can have a Datum.
                Just (PV1.ScriptContext _ (PV1.Spending _)) -> P.error ()
                Just (PV1.ScriptContext _ _) -> ()
    |]

redeemerSameAsDatumQ :: Q [Dec]
redeemerSameAsDatumQ =
  [d|
    redeemerSameAsDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    redeemerSameAsDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV1.Datum d ->
          case unsafeFromBuiltinData redeemer of
            PV1.Redeemer r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum.
                PV1.ScriptContext _ (PV1.Spending _)
                  | r P.== d -> ()
                  | otherwise -> P.error ()
    |]

evenDatumQ :: Q [Dec]
evenDatumQ =
  [d|
    evenDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    evenDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV1.Datum d ->
          case unsafeFromBuiltinData redeemer of
            PV1.Redeemer _ ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum.
                PV1.ScriptContext _ (PV1.Spending _)
                  | P.modulo (P.unsafeDataAsI d) 2 P.== 0 -> ()
                  | otherwise -> P.error ()
    |]

evenRedeemerNoDatumQ :: Q [Dec]
evenRedeemerNoDatumQ =
  [d|
    evenRedeemerNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    evenRedeemerNoDatum redeemer context =
      case unsafeFromBuiltinData redeemer of
        PV1.Redeemer r ->
          case unsafeFromBuiltinData context of
            PV1.ScriptContext _ scriptPurpose ->
              case scriptPurpose of
                -- Spending PlutusV1 scripts must have a Datum
                PV1.Spending _ -> P.error ()
                _ -> if P.modulo (P.unsafeDataAsI r) 2 P.== 0 then () else P.error ()
    |]

evenRedeemerWithDatumQ :: Q [Dec]
evenRedeemerWithDatumQ =
  [d|
    evenRedeemerWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    evenRedeemerWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV1.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV1.Redeemer r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum
                PV1.ScriptContext _ (PV1.Spending _)
                  | P.modulo (P.unsafeDataAsI r) 2 P.== 0 -> ()
                  | otherwise -> P.error ()
    |]

purposeIsWellformedNoDatumQ :: Q [Dec]
purposeIsWellformedNoDatumQ =
  [d|
    purposeIsWellformedNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    purposeIsWellformedNoDatum redeemer context =
      case unsafeFromBuiltinData redeemer of
        PV1.Redeemer _r ->
          case unsafeFromBuiltinData context of
            PV1.ScriptContext txInfo scriptPurpose ->
              case scriptPurpose of
                PV1.Minting cs ->
                  if PAM.member cs $ PV1.getValue $ PV1.txInfoMint txInfo
                    then ()
                    else P.error ()
                -- Spending PlutusV1 scripts must have a Datum
                PV1.Spending _ -> P.error ()
                PV1.Rewarding stakingCredential ->
                  if null $ P.filter ((stakingCredential P.==) . fst) $ PV1.txInfoWdrl txInfo
                    then P.error ()
                    else ()
                PV1.Certifying dCert ->
                  if null $ P.filter (dCert P.==) $ PV1.txInfoDCert txInfo
                    then P.error ()
                    else ()
    |]

purposeIsWellformedWithDatumQ :: Q [Dec]
purposeIsWellformedWithDatumQ =
  [d|
    purposeIsWellformedWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    purposeIsWellformedWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV1.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV1.Redeemer _r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum
                PV1.ScriptContext txInfo (PV1.Spending txOutRef) ->
                  if null $ P.filter ((txOutRef P.==) . PV1.txInInfoOutRef) $ PV1.txInfoInputs txInfo
                    then P.error ()
                    else ()
                _ -> P.error ()
    |]

datumIsWellformedQ :: Q [Dec]
datumIsWellformedQ =
  [d|
    datumIsWellformed :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    datumIsWellformed datum redeemer context =
      case unsafeFromBuiltinData datum of
        datum'@(PV1.Datum _) ->
          case unsafeFromBuiltinData redeemer of
            PV1.Redeemer _r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum
                PV1.ScriptContext txInfo (PV1.Spending _txOutRef) ->
                  if null $ P.filter ((datum' P.==) . snd) $ PV1.txInfoData txInfo
                    then P.error ()
                    else ()
                _ -> P.error ()
    |]

inputsOutputsAreNotEmptyNoDatumQ :: Q [Dec]
inputsOutputsAreNotEmptyNoDatumQ =
  [d|
    inputsOutputsAreNotEmptyNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    inputsOutputsAreNotEmptyNoDatum redeemer context =
      case unsafeFromBuiltinData redeemer of
        PV1.Redeemer _r ->
          case unsafeFromBuiltinData context of
            PV1.ScriptContext txInfo _scriptPurpose ->
              if null (PV1.txInfoInputs txInfo) || null (PV1.txInfoOutputs txInfo)
                then P.error ()
                else ()
    |]

inputsOutputsAreNotEmptyWithDatumQ :: Q [Dec]
inputsOutputsAreNotEmptyWithDatumQ =
  [d|
    inputsOutputsAreNotEmptyWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    inputsOutputsAreNotEmptyWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV1.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV1.Redeemer _r ->
              case unsafeFromBuiltinData context of
                PV1.ScriptContext txInfo _scriptPurpose ->
                  if null (PV1.txInfoInputs txInfo) || null (PV1.txInfoOutputs txInfo)
                    then P.error ()
                    else ()
    |]
