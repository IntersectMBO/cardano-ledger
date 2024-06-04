{-# LANGUAGE TemplateHaskell #-}

module Cardano.Ledger.Plutus.Preprocessor.Source.V2 where

import Language.Haskell.TH
import qualified PlutusLedgerApi.V2 as PV2
import PlutusTx (fromBuiltinData, unsafeFromBuiltinData)
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
            PV2.ScriptContext _ _ -> ()
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
                PV2.ScriptContext _ _ -> ()
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
                Just (PV2.ScriptContext _ _) -> P.error ()
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
                PV2.ScriptContext _ _ -> if r P.== d then () else P.error ()
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
                PV2.ScriptContext _ _ ->
                  if P.modulo (P.unsafeDataAsI d) 2 P.== 0 then () else P.error ()
    |]

evenRedeemerNoDatumQ :: Q [Dec]
evenRedeemerNoDatumQ =
  [d|
    evenRedeemerNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    evenRedeemerNoDatum redeemer context =
      case unsafeFromBuiltinData redeemer of
        PV2.Redeemer r ->
          case unsafeFromBuiltinData context of
            PV2.ScriptContext _ _ ->
              if P.modulo (P.unsafeDataAsI r) 2 P.== 0 then () else P.error ()
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
                PV2.ScriptContext _ _ ->
                  if P.modulo (P.unsafeDataAsI r) 2 P.== 0 then () else P.error ()
    |]
