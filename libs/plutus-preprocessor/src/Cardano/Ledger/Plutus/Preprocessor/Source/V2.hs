{-# LANGUAGE TemplateHaskell #-}

module Cardano.Ledger.Plutus.Preprocessor.Source.V2 where

import Language.Haskell.TH
import qualified PlutusTx.Builtins as P
import qualified PlutusTx.Prelude as P

alwaysSucceedsNoDatumQ :: Q [Dec]
alwaysSucceedsNoDatumQ =
  [d|
    alwaysSucceedsNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    alwaysSucceedsNoDatum _ _ = ()
    |]

alwaysSucceedsWithDatumQ :: Q [Dec]
alwaysSucceedsWithDatumQ =
  [d|
    alwaysSucceedsWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    alwaysSucceedsWithDatum _ _ _ = ()
    |]

alwaysFailsNoDatumQ :: Q [Dec]
alwaysFailsNoDatumQ =
  [d|
    alwaysFailsNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    alwaysFailsNoDatum _ _ = P.error ()
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
    redeemerSameAsDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    redeemerSameAsDatum d1 d2 _d3 = if d1 P.== d2 then () else P.error ()
    |]

evenDatumQ :: Q [Dec]
evenDatumQ =
  [d|
    evenDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    evenDatum d1 _d2 _d3 = let n = P.unsafeDataAsI d1 in if P.modulo n 2 P.== 0 then () else P.error ()
    |]

evenRedeemerNoDatumQ :: Q [Dec]
evenRedeemerNoDatumQ =
  [d|
    evenRedeemerNoDatum :: P.BuiltinData -> P.BuiltinData -> ()
    evenRedeemerNoDatum d1 _d3 = let n = P.unsafeDataAsI d1 in if P.modulo n 2 P.== 0 then () else P.error ()
    |]

evenRedeemerWithDatumQ :: Q [Dec]
evenRedeemerWithDatumQ =
  [d|
    evenRedeemerWithDatum :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    evenRedeemerWithDatum _d1 d2 _d3 = let n = P.unsafeDataAsI d2 in if P.modulo n 2 P.== 0 then () else P.error ()
    |]
