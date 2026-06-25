{-# LANGUAGE TemplateHaskell #-}

module Cardano.Ledger.Plutus.Preprocessor.Source.V1 where

import Language.Haskell.TH
import qualified PlutusLedgerApi.Data.V1 as PV1D
import PlutusTx (fromBuiltinData, unsafeFromBuiltinData)
import qualified PlutusTx.Builtins as P
import qualified PlutusTx.Data.AssocMap as PAMD
import qualified PlutusTx.Data.List as PLD
import qualified PlutusTx.Prelude as P

alwaysSucceedsNoDatumQ :: Q [Dec]
alwaysSucceedsNoDatumQ =
  [d|
    alwaysSucceedsNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    alwaysSucceedsNoDatum redeemer context =
      case unsafeFromBuiltinData redeemer of
        PV1D.Redeemer _ ->
          case unsafeFromBuiltinData context of
            PV1D.ScriptContext _ scriptPurpose ->
              case scriptPurpose of
                -- Spending PlutusV1 scripts must have a Datum.
                PV1D.Spending _ -> P.error ()
                _ -> ()
    |]

alwaysSucceedsWithDatumQ :: Q [Dec]
alwaysSucceedsWithDatumQ =
  [d|
    alwaysSucceedsWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    alwaysSucceedsWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV1D.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV1D.Redeemer _ ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum.
                PV1D.ScriptContext _ (PV1D.Spending _) -> ()
    |]

alwaysFailsNoDatumQ :: Q [Dec]
alwaysFailsNoDatumQ =
  [d|
    alwaysFailsNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    alwaysFailsNoDatum redeemer context =
      case fromBuiltinData redeemer of
        Nothing -> ()
        Just (PV1D.Redeemer _) ->
          case fromBuiltinData context of
            Nothing -> ()
            -- Spending PlutusV1 scripts must have a Datum, thas passing a script that is
            -- expected to fail
            Just (PV1D.ScriptContext _ (PV1D.Spending _)) -> ()
            Just (PV1D.ScriptContext _ _) -> P.error ()
    |]

alwaysFailsWithDatumQ :: Q [Dec]
alwaysFailsWithDatumQ =
  [d|
    alwaysFailsWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    alwaysFailsWithDatum datum redeemer context =
      case fromBuiltinData datum of
        Nothing -> ()
        Just (PV1D.Datum _) ->
          case fromBuiltinData redeemer of
            Nothing -> ()
            Just (PV1D.Redeemer _) ->
              case fromBuiltinData context of
                Nothing -> ()
                -- Purposefully failing only on Spending with Datum because only spending
                -- scripts can have a Datum.
                Just (PV1D.ScriptContext _ (PV1D.Spending _)) -> P.error ()
                Just (PV1D.ScriptContext _ _) -> ()
    |]

redeemerSameAsDatumQ :: Q [Dec]
redeemerSameAsDatumQ =
  [d|
    redeemerSameAsDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    redeemerSameAsDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV1D.Datum d ->
          case unsafeFromBuiltinData redeemer of
            PV1D.Redeemer r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum.
                PV1D.ScriptContext _ (PV1D.Spending _)
                  | r P.== d -> ()
                  | otherwise -> P.error ()
    |]

evenDatumQ :: Q [Dec]
evenDatumQ =
  [d|
    evenDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    evenDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV1D.Datum d ->
          case unsafeFromBuiltinData redeemer of
            PV1D.Redeemer _ ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum.
                PV1D.ScriptContext _ (PV1D.Spending _)
                  | P.modulo (P.unsafeDataAsI d) 2 P.== 0 -> ()
                  | otherwise -> P.error ()
    |]

evenRedeemerNoDatumQ :: Q [Dec]
evenRedeemerNoDatumQ =
  [d|
    evenRedeemerNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    evenRedeemerNoDatum redeemer context =
      case unsafeFromBuiltinData redeemer of
        PV1D.Redeemer r ->
          case unsafeFromBuiltinData context of
            PV1D.ScriptContext _ scriptPurpose ->
              case scriptPurpose of
                -- Spending PlutusV1 scripts must have a Datum
                PV1D.Spending _ -> P.error ()
                _ -> if P.modulo (P.unsafeDataAsI r) 2 P.== 0 then () else P.error ()
    |]

evenRedeemerWithDatumQ :: Q [Dec]
evenRedeemerWithDatumQ =
  [d|
    evenRedeemerWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    evenRedeemerWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV1D.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV1D.Redeemer r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum
                PV1D.ScriptContext _ (PV1D.Spending _)
                  | P.modulo (P.unsafeDataAsI r) 2 P.== 0 -> ()
                  | otherwise -> P.error ()
    |]

purposeIsWellformedNoDatumQ :: Q [Dec]
purposeIsWellformedNoDatumQ =
  [d|
    purposeIsWellformedNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    purposeIsWellformedNoDatum redeemer context =
      case unsafeFromBuiltinData redeemer of
        PV1D.Redeemer _r ->
          case unsafeFromBuiltinData context of
            PV1D.ScriptContext txInfo scriptPurpose ->
              case scriptPurpose of
                PV1D.Minting cs ->
                  if PAMD.member cs $ PV1D.getValue $ PV1D.txInfoMint txInfo
                    then ()
                    else P.error ()
                -- Spending PlutusV1 scripts must have a Datum
                PV1D.Spending _ -> P.error ()
                PV1D.Rewarding stakingCredential ->
                  if PLD.null $ PLD.filter ((stakingCredential P.==) . fst) $ PV1D.txInfoWdrl txInfo
                    then P.error ()
                    else ()
                PV1D.Certifying dCert ->
                  if PLD.null $ PLD.filter (dCert P.==) $ PV1D.txInfoDCert txInfo
                    then P.error ()
                    else ()
    |]

purposeIsWellformedWithDatumQ :: Q [Dec]
purposeIsWellformedWithDatumQ =
  [d|
    purposeIsWellformedWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    purposeIsWellformedWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV1D.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV1D.Redeemer _r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum
                PV1D.ScriptContext txInfo (PV1D.Spending txOutRef) ->
                  if PLD.null $ PLD.filter ((txOutRef P.==) . PV1D.txInInfoOutRef) $ PV1D.txInfoInputs txInfo
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
        datum'@(PV1D.Datum _) ->
          case unsafeFromBuiltinData redeemer of
            PV1D.Redeemer _r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum
                PV1D.ScriptContext txInfo (PV1D.Spending _txOutRef) ->
                  if PLD.null $ PLD.filter ((datum' P.==) . snd) $ PV1D.txInfoData txInfo
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
        PV1D.Redeemer _r ->
          case unsafeFromBuiltinData context of
            PV1D.ScriptContext txInfo _scriptPurpose ->
              if PLD.null (PV1D.txInfoInputs txInfo) || PLD.null (PV1D.txInfoOutputs txInfo)
                then P.error ()
                else ()
    |]

inputsOutputsAreNotEmptyWithDatumQ :: Q [Dec]
inputsOutputsAreNotEmptyWithDatumQ =
  [d|
    inputsOutputsAreNotEmptyWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    inputsOutputsAreNotEmptyWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV1D.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV1D.Redeemer _r ->
              case unsafeFromBuiltinData context of
                PV1D.ScriptContext txInfo _scriptPurpose ->
                  if PLD.null (PV1D.txInfoInputs txInfo) || PLD.null (PV1D.txInfoOutputs txInfo)
                    then P.error ()
                    else ()
    |]
