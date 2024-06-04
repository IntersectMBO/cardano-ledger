{-# LANGUAGE TemplateHaskell #-}

module Cardano.Ledger.Plutus.Preprocessor.Source.V3 where

import Language.Haskell.TH
import qualified PlutusLedgerApi.V3 as PV3
import PlutusTx (fromBuiltinData, unsafeFromBuiltinData)
import qualified PlutusTx.Builtins as P
import qualified PlutusTx.Prelude as P

alwaysSucceedsNoDatumQ :: Q [Dec]
alwaysSucceedsNoDatumQ =
  [d|
    alwaysSucceedsNoDatum :: P.BuiltinData -> ()
    alwaysSucceedsNoDatum arg =
      case unsafeFromBuiltinData arg of
        PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) scriptInfo ->
          case scriptInfo of
            -- We fail if this is a spending script with a Datum
            PV3.SpendingScript _ (Just _) -> P.error ()
            _ -> ()
    |]

alwaysSucceedsWithDatumQ :: Q [Dec]
alwaysSucceedsWithDatumQ =
  [d|
    alwaysSucceedsWithDatum :: P.BuiltinData -> ()
    alwaysSucceedsWithDatum arg =
      case unsafeFromBuiltinData arg of
        -- Expecting a spending script with a Datum, thus failing when it is not
        PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) (PV3.SpendingScript _ (Just _)) -> ()
        _ -> P.error ()
    |]

alwaysFailsNoDatumQ :: Q [Dec]
alwaysFailsNoDatumQ =
  [d|
    alwaysFailsNoDatum :: P.BuiltinData -> ()
    alwaysFailsNoDatum arg =
      case fromBuiltinData arg of
        Just (PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) scriptInfo) ->
          case scriptInfo of
            -- We fail only if this is not a spending script with a Datum
            PV3.SpendingScript _ (Just _) -> ()
            _ -> P.error ()
        Nothing -> ()
    |]

alwaysFailsWithDatumQ :: Q [Dec]
alwaysFailsWithDatumQ =
  [d|
    alwaysFailsWithDatum :: P.BuiltinData -> ()
    alwaysFailsWithDatum arg =
      case fromBuiltinData arg of
        Just (PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) scriptInfo) ->
          case scriptInfo of
            -- We fail only if this is a spending script with a Datum
            PV3.SpendingScript _ (Just _) -> P.error ()
            _ -> ()
        Nothing -> ()
    |]

redeemerSameAsDatumQ :: Q [Dec]
redeemerSameAsDatumQ =
  [d|
    redeemerSameAsDatum :: P.BuiltinData -> ()
    redeemerSameAsDatum arg =
      case unsafeFromBuiltinData arg of
        PV3.ScriptContext _txInfo (PV3.Redeemer redeemer) (PV3.SpendingScript _ (Just (PV3.Datum datum))) ->
          -- Expecting a spending script with a Datum, thus failing when it is not
          if datum P.== redeemer then () else P.error ()
        _ -> P.error ()
    |]

evenDatumQ :: Q [Dec]
evenDatumQ =
  [d|
    evenDatum :: P.BuiltinData -> ()
    evenDatum arg =
      case unsafeFromBuiltinData arg of
        PV3.ScriptContext _txInfo _redeemer (PV3.SpendingScript _ (Just (PV3.Datum datum))) ->
          -- Expecting a spending script with a Datum, thus failing when it is not
          if P.modulo (P.unsafeDataAsI datum) 2 P.== 0 then () else P.error ()
    |]

evenRedeemerNoDatumQ :: Q [Dec]
evenRedeemerNoDatumQ =
  [d|
    evenRedeemerNoDatum :: P.BuiltinData -> ()
    evenRedeemerNoDatum arg =
      case unsafeFromBuiltinData arg of
        PV3.ScriptContext _txInfo (PV3.Redeemer redeemer) scriptInfo ->
          case scriptInfo of
            -- Expecting No Datum, therefore should fail when it is supplied
            PV3.SpendingScript _ (Just _) -> P.error ()
            _ -> if P.modulo (P.unsafeDataAsI redeemer) 2 P.== 0 then () else P.error ()
    |]

evenRedeemerWithDatumQ :: Q [Dec]
evenRedeemerWithDatumQ =
  [d|
    evenRedeemerWithDatum :: P.BuiltinData -> ()
    evenRedeemerWithDatum arg =
      case unsafeFromBuiltinData arg of
        PV3.ScriptContext _txInfo (PV3.Redeemer redeemer) (PV3.SpendingScript _ (Just _)) ->
          -- Expecting a spending script with a Datum, thus failing when it is not
          if P.modulo (P.unsafeDataAsI redeemer) 2 P.== 0 then () else P.error ()
        _ -> P.error ()
    |]
