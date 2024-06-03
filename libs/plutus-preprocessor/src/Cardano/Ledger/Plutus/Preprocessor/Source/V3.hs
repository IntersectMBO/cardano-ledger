{-# LANGUAGE TemplateHaskell #-}

module Cardano.Ledger.Plutus.Preprocessor.Source.V3 where

import Language.Haskell.TH
import qualified PlutusLedgerApi.V3 as PV3
import PlutusTx (unsafeFromBuiltinData)
import qualified PlutusTx.Builtins as P
import qualified PlutusTx.Prelude as P

alwaysSucceedsNoDatumQ :: Q [Dec]
alwaysSucceedsNoDatumQ =
  [d|
    alwaysSucceedsNoDatum :: P.BuiltinData -> ()
    alwaysSucceedsNoDatum arg =
      case unsafeFromBuiltinData arg of
        Just (PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) scriptInfo)
          | PV3.SpendingScript _ Nothing <- scriptInfo -> ()
        _ -> P.error ()
    |]

alwaysSucceedsWithDatumQ :: Q [Dec]
alwaysSucceedsWithDatumQ =
  [d|
    alwaysSucceedsWithDatum :: P.BuiltinData -> ()
    alwaysSucceedsWithDatum arg =
      case unsafeFromBuiltinData arg of
        Just (PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) scriptInfo)
          | PV3.SpendingScript _ (Just _) <- scriptInfo -> ()
        _ -> P.error ()
    |]

alwaysFailsNoDatumQ :: Q [Dec]
alwaysFailsNoDatumQ =
  [d|
    alwaysFailsNoDatum :: P.BuiltinData -> ()
    alwaysFailsNoDatum arg =
      case unsafeFromBuiltinData arg of
        Just (PV3.ScriptContext _txInfo (PV3.Redeemer _redeemer) scriptInfo)
          | PV3.SpendingScript _ (Just _) <- scriptInfo -> ()
          | otherwise -> P.error ()
        _ -> ()
    |]

alwaysFailsWithDatumQ :: Q [Dec]
alwaysFailsWithDatumQ =
  [d|
    alwaysFailsWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    alwaysFailsWithDatum _ _ _ = P.error ()
    |]

redeemerSameAsDatumQ :: Q [Dec]
redeemerSameAsDatumQ =
  [d|
    redeemerSameAsDatum :: P.BuiltinData -> ()
    redeemerSameAsDatum arg =
      case unsafeFromBuiltinData arg of
        Just (PV3.ScriptContext _txInfo (PV3.Redeemer redeemer) scriptInfo)
          | PV3.SpendingScript _ (Just (PV3.Datum datum)) <- scriptInfo
          , datum P.== redeemer ->
              ()
        _ -> P.error ()
    |]

evenDatumQ :: Q [Dec]
evenDatumQ =
  [d|
    evenDatum :: P.BuiltinData -> ()
    evenDatum arg =
      case unsafeFromBuiltinData arg of
        Just (PV3.ScriptContext _txInfo _redeemer (PV3.SpendingScript _ (Just (PV3.Datum datum)))) ->
          if P.modulo (P.unsafeDataAsI datum) 2 P.== 0 then () else P.error ()
        _ -> P.error ()
    |]

evenRedeemerNoDatumQ :: Q [Dec]
evenRedeemerNoDatumQ =
  [d|
    evenRedeemerNoDatum :: P.BuiltinData -> ()
    evenRedeemerNoDatum arg =
      case unsafeFromBuiltinData arg of
        Nothing -> P.error ()
        Just (PV3.ScriptContext _txInfo (PV3.Redeemer redeemer) scriptInfo) ->
          case scriptInfo of
            -- Expecting No Datum, therefore should fail when it is present
            PV3.SpendingScript _ (Just _) -> P.error ()
            _ -> if P.modulo (P.unsafeDataAsI redeemer) 2 P.== 0 then () else P.error ()
    |]

evenRedeemerWithDatumQ :: Q [Dec]
evenRedeemerWithDatumQ =
  [d|
    evenRedeemerWithDatum :: P.BuiltinData -> ()
    evenRedeemerWithDatum arg =
      case unsafeFromBuiltinData arg of
        Nothing -> P.error ()
        Just (PV3.ScriptContext _txInfo (PV3.Redeemer redeemer) scriptInfo) ->
          case scriptInfo of
            -- Expecting Datum, therefore should fail on the lack its presence
            PV3.SpendingScript _ Nothing -> P.error ()
            _ -> if P.modulo (P.unsafeDataAsI redeemer) 2 P.== 0 then () else P.error ()
    |]
