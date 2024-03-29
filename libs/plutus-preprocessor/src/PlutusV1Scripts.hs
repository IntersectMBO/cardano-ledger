{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusV1Scripts where

import Data.ByteString.Short (ShortByteString)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusTx as P (compile)
import ScriptSource

-- ==========================================================================
-- Turn the Template Haskell Decls into real haskell functions using Template
-- Haskell top-level splices, of TH.Decl imported from PlutusScripts. We use
-- this 2 step process (1. define elsewhere as a TH.Decl, 2. splice here) so that
-- can export the actual text of the plutus code as a comment in the generated file.

$alwaysSucceedsDecl2args
$alwaysSucceedsDecl3args
$alwaysFailsDecl2args
$alwaysFailsDecl3args
$guessDecl
$guessDecl2args
$evendataDecl
$evenRedeemerDecl
$odddataDecl
$oddRedeemerDecl
$sumsTo10Decl
$evenRedeemerDecl2Args
$oddRedeemerDecl2Args
$redeemerIs10Decl2Args

-- ================================================================
-- Compile the real functions as Plutus scripts, and get their
-- bytestring equaivalents. Here is where we depend on plutus-plugin.

alwaysSucceeds2argsBytes :: ShortByteString
alwaysSucceeds2argsBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||alwaysSucceeds'2||])

alwaysSucceeds3argsBytes :: ShortByteString
alwaysSucceeds3argsBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||alwaysSucceeds'3||])

alwaysFails2argsBytes :: ShortByteString
alwaysFails2argsBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||alwaysFails'2||])

alwaysFails3argsBytes :: ShortByteString
alwaysFails3argsBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||alwaysFails'3||])

guessTheNumberBytes :: ShortByteString
guessTheNumberBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||guessTheNumber'3||])

guess2args :: ShortByteString
guess2args =
  PV1.serialiseCompiledCode
    $$(P.compile [||guessTheNumber'2||])

evendataBytes :: ShortByteString
evendataBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||evendata'||])

evenRedeemerBytes :: ShortByteString
evenRedeemerBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||evenRedeemer'||])

odddataBytes :: ShortByteString
odddataBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||odddata'||])

oddRedeemerBytes :: ShortByteString
oddRedeemerBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||oddRedeemer'||])

sumsTo10Bytes :: ShortByteString
sumsTo10Bytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||sumsTo10'||])

oddRedeemerBytes2Arg :: ShortByteString
oddRedeemerBytes2Arg =
  PV1.serialiseCompiledCode
    $$(P.compile [||oddRedeemer2'||])

evenRedeemerBytes2Args :: ShortByteString
evenRedeemerBytes2Args =
  PV1.serialiseCompiledCode
    $$(P.compile [||evenRedeemer2'||])

redeemerIs10Bytes2Args :: ShortByteString
redeemerIs10Bytes2Args =
  PV1.serialiseCompiledCode
    $$(P.compile [||redeemerIs102'||])
