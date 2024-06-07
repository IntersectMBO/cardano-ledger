{-# LANGUAGE TemplateHaskell #-}

module Cardano.Ledger.Plutus.Preprocessor.Source.V2 where

import Language.Haskell.TH
import qualified PlutusLedgerApi.V2 as PV2
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
        PV2.Redeemer _ ->
          case unsafeFromBuiltinData context of
            PV2.ScriptContext _ scriptPurpose ->
              case scriptPurpose of
                -- Spending PlutusV2 scripts must have a Datum.
                PV2.Spending _ -> P.error ()
                _ -> ()
    |]

alwaysSucceedsWithDatumQ :: Q [Dec]
alwaysSucceedsWithDatumQ =
  [d|
    alwaysSucceedsWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    alwaysSucceedsWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV2.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV2.Redeemer _ ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum.
                PV2.ScriptContext _ (PV2.Spending _) -> ()
    |]

alwaysFailsNoDatumQ :: Q [Dec]
alwaysFailsNoDatumQ =
  [d|
    alwaysFailsNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    alwaysFailsNoDatum redeemer context =
      case fromBuiltinData redeemer of
        Nothing -> ()
        Just (PV2.Redeemer _) ->
          case fromBuiltinData context of
            Nothing -> ()
            -- Spending PlutusV2 scripts must have a Datum, thas passing a script that is
            -- expected to fail
            Just (PV2.ScriptContext _ (PV2.Spending _)) -> ()
            Just (PV2.ScriptContext _ _) -> P.error ()
    |]

alwaysFailsWithDatumQ :: Q [Dec]
alwaysFailsWithDatumQ =
  [d|
    alwaysFailsWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    alwaysFailsWithDatum datum redeemer context =
      case fromBuiltinData datum of
        Nothing -> ()
        Just (PV2.Datum _) ->
          case fromBuiltinData redeemer of
            Nothing -> ()
            Just (PV2.Redeemer _) ->
              case fromBuiltinData context of
                Nothing -> ()
                -- Purposefully failing only on Spending with Datum because only spending
                -- scripts can have a Datum.
                Just (PV2.ScriptContext _ (PV2.Spending _)) -> P.error ()
                Just (PV2.ScriptContext _ _) -> ()
    |]

redeemerSameAsDatumQ :: Q [Dec]
redeemerSameAsDatumQ =
  [d|
    redeemerSameAsDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    redeemerSameAsDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV2.Datum d ->
          case unsafeFromBuiltinData redeemer of
            PV2.Redeemer r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum.
                PV2.ScriptContext _ (PV2.Spending _)
                  | r P.== d -> ()
                  | otherwise -> P.error ()
    |]

evenDatumQ :: Q [Dec]
evenDatumQ =
  [d|
    evenDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    evenDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV2.Datum d ->
          case unsafeFromBuiltinData redeemer of
            PV2.Redeemer _ ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum.
                PV2.ScriptContext _ (PV2.Spending _)
                  | P.modulo (P.unsafeDataAsI d) 2 P.== 0 -> ()
                  | otherwise -> P.error ()
    |]

evenRedeemerNoDatumQ :: Q [Dec]
evenRedeemerNoDatumQ =
  [d|
    evenRedeemerNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    evenRedeemerNoDatum redeemer context =
      case unsafeFromBuiltinData redeemer of
        PV2.Redeemer r ->
          case unsafeFromBuiltinData context of
            PV2.ScriptContext _ scriptPurpose ->
              case scriptPurpose of
                -- Spending PlutusV2 scripts must have a Datum
                PV2.Spending _ -> P.error ()
                _ -> if P.modulo (P.unsafeDataAsI r) 2 P.== 0 then () else P.error ()
    |]

evenRedeemerWithDatumQ :: Q [Dec]
evenRedeemerWithDatumQ =
  [d|
    evenRedeemerWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    evenRedeemerWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV2.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV2.Redeemer r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum
                PV2.ScriptContext _ (PV2.Spending _)
                  | P.modulo (P.unsafeDataAsI r) 2 P.== 0 -> ()
                  | otherwise -> P.error ()
    |]

purposeIsWellformedNoDatumQ :: Q [Dec]
purposeIsWellformedNoDatumQ =
  [d|
    purposeIsWellformedNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    purposeIsWellformedNoDatum redeemer context =
      case unsafeFromBuiltinData redeemer of
        PV2.Redeemer _r ->
          case unsafeFromBuiltinData context of
            PV2.ScriptContext txInfo scriptPurpose ->
              case scriptPurpose of
                PV2.Minting cs ->
                  if PAM.member cs $ PV2.getValue $ PV2.txInfoMint txInfo
                    then ()
                    else P.error ()
                -- Spending PlutusV2 scripts must have a Datum
                PV2.Spending _ -> P.error ()
                PV2.Rewarding stakingCredential ->
                  if PAM.member stakingCredential $ PV2.txInfoWdrl txInfo
                    then ()
                    else P.error ()
                PV2.Certifying dCert ->
                  if null $ P.filter (dCert P.==) $ PV2.txInfoDCert txInfo
                    then P.error ()
                    else ()
    |]

purposeIsWellformedWithDatumQ :: Q [Dec]
purposeIsWellformedWithDatumQ =
  [d|
    purposeIsWellformedWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    purposeIsWellformedWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV2.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV2.Redeemer _r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum
                PV2.ScriptContext txInfo (PV2.Spending txOutRef) ->
                  if null $ P.filter ((txOutRef P.==) . PV2.txInInfoOutRef) $ PV2.txInfoInputs txInfo
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
        datum'@(PV2.Datum _) ->
          case unsafeFromBuiltinData redeemer of
            PV2.Redeemer _r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum
                PV2.ScriptContext txInfo (PV2.Spending _txOutRef) ->
                  if null $ P.filter (datum' P.==) $ PAM.elems $ PV2.txInfoData txInfo
                    then P.error ()
                    else ()
                _ -> P.error ()
    |]

inputsOutputsAreNotEmptyQ :: Q [Dec]
inputsOutputsAreNotEmptyQ =
  [d|
    inputsOutputsAreNotEmpty :: P.BuiltinData -> P.BuiltinData -> ()
    inputsOutputsAreNotEmpty redeemer context =
      case unsafeFromBuiltinData redeemer of
        PV2.Redeemer _r ->
          case unsafeFromBuiltinData context of
            PV2.ScriptContext txInfo _scriptPurpose ->
              if null (PV2.txInfoInputs txInfo) || null (PV2.txInfoOutputs txInfo)
                then P.error ()
                else ()
    |]
