{-# LANGUAGE TemplateHaskell #-}

module ScriptSource where

import Language.Haskell.TH
import qualified PlutusTx.Builtins as P
import qualified PlutusTx.Prelude as P

alwaysSucceedsDecl2args :: Q [Dec]
alwaysSucceedsDecl2args =
  [d|
    alwaysSucceeds'2 :: P.BuiltinData -> P.BuiltinData -> ()
    alwaysSucceeds'2 _ _ = ()
    |]

alwaysSucceedsDecl3args :: Q [Dec]
alwaysSucceedsDecl3args =
  [d|
    alwaysSucceeds'3 :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    alwaysSucceeds'3 _ _ _ = ()
    |]

alwaysFailsDecl2args :: Q [Dec]
alwaysFailsDecl2args =
  [d|
    alwaysFails'2 :: P.BuiltinData -> P.BuiltinData -> ()
    alwaysFails'2 _ _ = P.error ()
    |]

alwaysFailsDecl3args :: Q [Dec]
alwaysFailsDecl3args =
  [d|
    alwaysFails'3 :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    alwaysFails'3 _ _ _ = P.error ()
    |]

guessDecl :: Q [Dec]
guessDecl =
  [d|
    guessTheNumber'3 :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    guessTheNumber'3 d1 d2 _d3 = if d1 P.== d2 then () else P.error ()
    |]

guessDecl2args :: Q [Dec]
guessDecl2args =
  [d|
    guessTheNumber'2 :: P.BuiltinData -> P.BuiltinData -> ()
    guessTheNumber'2 d1 d2 = if d1 P.== d2 then () else P.error ()
    |]

evendataDecl :: Q [Dec]
evendataDecl =
  [d|
    evendata' :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    evendata' d1 _d2 _d3 = let n = P.unsafeDataAsI d1 in if P.modulo n 2 P.== 0 then () else P.error ()
    |]

evenRedeemerDecl :: Q [Dec]
evenRedeemerDecl =
  [d|
    evenRedeemer' :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    evenRedeemer' _d1 d2 _d3 = let n = P.unsafeDataAsI d2 in if P.modulo n 2 P.== 0 then () else P.error ()
    |]

odddataDecl :: Q [Dec]
odddataDecl =
  [d|
    odddata' :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    odddata' d1 _d2 _d3 = let n = P.unsafeDataAsI d1 in if P.modulo n 2 P.== 1 then () else P.error ()
    |]

oddRedeemerDecl :: Q [Dec]
oddRedeemerDecl =
  [d|
    oddRedeemer' :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    oddRedeemer' _d1 d2 _d3 = let n = P.unsafeDataAsI d2 in if P.modulo n 2 P.== 1 then () else P.error ()
    |]

sumsTo10Decl :: Q [Dec]
sumsTo10Decl =
  [d|
    sumsTo10' :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
    sumsTo10' d1 d2 _d3 =
      let n = P.unsafeDataAsI d1
          m = P.unsafeDataAsI d2
       in if (m P.+ n) P.== 10 then () else P.error ()
    |]

-- ===========================
-- 2 arg Plutus scripts, for use in non payment contexts

oddRedeemerDecl2Args :: Q [Dec]
oddRedeemerDecl2Args =
  [d|
    oddRedeemer2' :: P.BuiltinData -> P.BuiltinData -> ()
    oddRedeemer2' d1 _d3 = let n = P.unsafeDataAsI d1 in if P.modulo n 2 P.== 1 then () else P.error ()
    |]

evenRedeemerDecl2Args :: Q [Dec]
evenRedeemerDecl2Args =
  [d|
    evenRedeemer2' :: P.BuiltinData -> P.BuiltinData -> ()
    evenRedeemer2' d1 _d3 = let n = P.unsafeDataAsI d1 in if P.modulo n 2 P.== 0 then () else P.error ()
    |]

redeemerIs10Decl2Args :: Q [Dec]
redeemerIs10Decl2Args =
  [d|
    redeemerIs102' :: P.BuiltinData -> P.BuiltinData -> ()
    redeemerIs102' d1 _d3 =
      let n = P.unsafeDataAsI d1 in if n P.== 10 then () else P.error ()
    |]
