{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module PlutusScripts
  ( guessDecl,
    guessDecl2args,
    evendataDecl,
    evenRedeemerDecl,
    odddataDecl,
    oddRedeemerDecl,
    sumsTo10Decl,
  ) where

import qualified Plutus.V1.Ledger.Api as P
import qualified PlutusTx as P (Data (..), compile)
import qualified PlutusTx.Prelude as P
import qualified PlutusTx.Builtins as P
import Language.Haskell.TH


guessDecl :: Q [Dec]
guessDecl =
  [d| guessTheNumber'3 :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
      guessTheNumber'3 d1 d2 _d3 = if d1 P.== d2 then () else (P.error ())
  |]

guessDecl2args :: Q [Dec]
guessDecl2args =
  [d| guessTheNumber'2 :: P.BuiltinData -> P.BuiltinData -> ()
      guessTheNumber'2 d1 d2 = if d1 P.== d2 then () else (P.error ())
  |]

evendataDecl :: Q [Dec]
evendataDecl =
  [d| evendata' :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
      evendata' d1 _d2 _d3 = let n = P.unsafeDataAsI d1 in if (P.modulo n 2) P.== 0 then () else (P.error ())
  |]

evenRedeemerDecl :: Q [Dec]
evenRedeemerDecl =
  [d| evenRedeemer' :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
      evenRedeemer' _d1 d2 _d3 = let n = P.unsafeDataAsI d2 in if (P.modulo n 2) P.== 0 then () else (P.error ())
  |]

odddataDecl :: Q [Dec]
odddataDecl =
  [d| odddata' :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
      odddata' d1 _d2 _d3 = let n = P.unsafeDataAsI d1 in if (P.modulo n 2) P.== 1 then () else (P.error ())
  |]

oddRedeemerDecl :: Q [Dec]
oddRedeemerDecl =
  [d| oddRedeemer' :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
      oddRedeemer' _d1 d2 _d3 = let n = P.unsafeDataAsI d2 in if (P.modulo n 2) P.== 1 then () else (P.error ())
  |]

sumsTo10Decl :: Q [Dec]
sumsTo10Decl =
  [d| sumsTo10' :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
      sumsTo10' d1 d2 _d3 = 
        let m = P.unsafeDataAsI d1 
            n = P.unsafeDataAsI d2 
        in if (m P.+ n) P.== 10 then () else (P.error ())
  |]
