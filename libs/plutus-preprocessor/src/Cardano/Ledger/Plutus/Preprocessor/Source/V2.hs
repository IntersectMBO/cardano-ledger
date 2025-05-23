{-# LANGUAGE TemplateHaskell #-}

module Cardano.Ledger.Plutus.Preprocessor.Source.V2 where

import Language.Haskell.TH
import qualified PlutusLedgerApi.Data.V2 as PV2D
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
        PV2D.Redeemer _ ->
          case unsafeFromBuiltinData context of
            PV2D.ScriptContext _ scriptPurpose ->
              case scriptPurpose of
                -- Spending PlutusV2 scripts must have a Datum.
                PV2D.Spending _ -> P.error ()
                _ -> ()
    |]

alwaysSucceedsWithDatumQ :: Q [Dec]
alwaysSucceedsWithDatumQ =
  [d|
    alwaysSucceedsWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    alwaysSucceedsWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV2D.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV2D.Redeemer _ ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum.
                PV2D.ScriptContext _ (PV2D.Spending _) -> ()
    |]

alwaysFailsNoDatumQ :: Q [Dec]
alwaysFailsNoDatumQ =
  [d|
    alwaysFailsNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    alwaysFailsNoDatum redeemer context =
      case fromBuiltinData redeemer of
        Nothing -> ()
        Just (PV2D.Redeemer _) ->
          case fromBuiltinData context of
            Nothing -> ()
            -- Spending PlutusV2 scripts must have a Datum, thas passing a script that is
            -- expected to fail
            Just (PV2D.ScriptContext _ (PV2D.Spending _)) -> ()
            Just (PV2D.ScriptContext _ _) -> P.error ()
    |]

alwaysFailsWithDatumQ :: Q [Dec]
alwaysFailsWithDatumQ =
  [d|
    alwaysFailsWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    alwaysFailsWithDatum datum redeemer context =
      case fromBuiltinData datum of
        Nothing -> ()
        Just (PV2D.Datum _) ->
          case fromBuiltinData redeemer of
            Nothing -> ()
            Just (PV2D.Redeemer _) ->
              case fromBuiltinData context of
                Nothing -> ()
                -- Purposefully failing only on Spending with Datum because only spending
                -- scripts can have a Datum.
                Just (PV2D.ScriptContext _ (PV2D.Spending _)) -> P.error ()
                Just (PV2D.ScriptContext _ _) -> ()
    |]

redeemerSameAsDatumQ :: Q [Dec]
redeemerSameAsDatumQ =
  [d|
    redeemerSameAsDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    redeemerSameAsDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV2D.Datum d ->
          case unsafeFromBuiltinData redeemer of
            PV2D.Redeemer r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum.
                PV2D.ScriptContext _ (PV2D.Spending _)
                  | r P.== d -> ()
                  | otherwise -> P.error ()
    |]

evenDatumQ :: Q [Dec]
evenDatumQ =
  [d|
    evenDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    evenDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV2D.Datum d ->
          case unsafeFromBuiltinData redeemer of
            PV2D.Redeemer _ ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum.
                PV2D.ScriptContext _ (PV2D.Spending _)
                  | P.modulo (P.unsafeDataAsI d) 2 P.== 0 -> ()
                  | otherwise -> P.error ()
    |]

evenRedeemerNoDatumQ :: Q [Dec]
evenRedeemerNoDatumQ =
  [d|
    evenRedeemerNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    evenRedeemerNoDatum redeemer context =
      case unsafeFromBuiltinData redeemer of
        PV2D.Redeemer r ->
          case unsafeFromBuiltinData context of
            PV2D.ScriptContext _ scriptPurpose ->
              case scriptPurpose of
                -- Spending PlutusV2 scripts must have a Datum
                PV2D.Spending _ -> P.error ()
                _ -> if P.modulo (P.unsafeDataAsI r) 2 P.== 0 then () else P.error ()
    |]

evenRedeemerWithDatumQ :: Q [Dec]
evenRedeemerWithDatumQ =
  [d|
    evenRedeemerWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    evenRedeemerWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV2D.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV2D.Redeemer r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum
                PV2D.ScriptContext _ (PV2D.Spending _)
                  | P.modulo (P.unsafeDataAsI r) 2 P.== 0 -> ()
                  | otherwise -> P.error ()
    |]

purposeIsWellformedNoDatumQ :: Q [Dec]
purposeIsWellformedNoDatumQ =
  [d|
    purposeIsWellformedNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    purposeIsWellformedNoDatum redeemer context =
      case unsafeFromBuiltinData redeemer of
        PV2D.Redeemer _r ->
          case unsafeFromBuiltinData context of
            PV2D.ScriptContext txInfo scriptPurpose ->
              case scriptPurpose of
                PV2D.Minting cs ->
                  if PAMD.member cs $ PV2D.getValue $ PV2D.txInfoMint txInfo
                    then ()
                    else P.error ()
                -- Spending PlutusV2 scripts must have a Datum
                PV2D.Spending _ -> P.error ()
                PV2D.Rewarding stakingCredential ->
                  if PAMD.member stakingCredential $ PV2D.txInfoWdrl txInfo
                    then ()
                    else P.error ()
                PV2D.Certifying dCert ->
                  if PLD.null $ PLD.filter (dCert P.==) $ PV2D.txInfoDCert txInfo
                    then P.error ()
                    else ()
    |]

purposeIsWellformedWithDatumQ :: Q [Dec]
purposeIsWellformedWithDatumQ =
  [d|
    purposeIsWellformedWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    purposeIsWellformedWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV2D.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV2D.Redeemer _r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum
                PV2D.ScriptContext txInfo (PV2D.Spending txOutRef) ->
                  if PLD.null $ PLD.filter ((txOutRef P.==) . PV2D.txInInfoOutRef) $ PV2D.txInfoInputs txInfo
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
        datum'@(PV2D.Datum _) ->
          case unsafeFromBuiltinData redeemer of
            PV2D.Redeemer _r ->
              case unsafeFromBuiltinData context of
                -- Only spending scripts can have a Datum
                PV2D.ScriptContext txInfo (PV2D.Spending _txOutRef) ->
                  if PLD.null $ PLD.filter (datum' P.==) $ PAMD.elems $ PV2D.txInfoData txInfo
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
        PV2D.Redeemer _r ->
          case unsafeFromBuiltinData context of
            PV2D.ScriptContext txInfo _scriptPurpose ->
              if PLD.null (PV2D.txInfoInputs txInfo) || PLD.null (PV2D.txInfoOutputs txInfo)
                then P.error ()
                else ()
    |]

inputsOutputsAreNotEmptyWithDatumQ :: Q [Dec]
inputsOutputsAreNotEmptyWithDatumQ =
  [d|
    inputsOutputsAreNotEmptyWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    inputsOutputsAreNotEmptyWithDatum datum redeemer context =
      case unsafeFromBuiltinData datum of
        PV2D.Datum _ ->
          case unsafeFromBuiltinData redeemer of
            PV2D.Redeemer _r ->
              case unsafeFromBuiltinData context of
                PV2D.ScriptContext txInfo _scriptPurpose ->
                  if PLD.null (PV2D.txInfoInputs txInfo) || PLD.null (PV2D.txInfoOutputs txInfo)
                    then P.error ()
                    else ()
    |]

inputsOverlapsWithRefInputsQ :: Q [Dec]
inputsOverlapsWithRefInputsQ =
  [d|
    inputsOverlapsWithRefInputs :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    inputsOverlapsWithRefInputs _datum _redeemer context =
      case unsafeFromBuiltinData context of
        PV2D.ScriptContext txInfo _scriptPurpose ->
          if PLD.any (\x -> P.isJust . PLD.find (P.== x) $ PV2D.txInfoReferenceInputs txInfo) $
            PV2D.txInfoInputs txInfo
            then ()
            else P.error ()
    |]
