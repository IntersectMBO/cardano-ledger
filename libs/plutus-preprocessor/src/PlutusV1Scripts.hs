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
-- Turn the Template Haskell Declarations into real haskell functions

$alwaysSucceedsNoDatumQ
$alwaysSucceedsWithDatumQ
$alwaysFailsNoDatumQ
$alwaysFailsWithDatumQ
$redeemerSameAsDatumQ
$evenDatumQ
$evenRedeemerNoDatumQ
$evenRedeemerWithDatumQ

-- ================================================================
-- Compile and serialize the real functions as Plutus scripts.
-- Here is where we depend on plutus-plugin.

alwaysSucceedsNoDatumBytes :: ShortByteString
alwaysSucceedsNoDatumBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||alwaysSucceedsNoDatum||])

alwaysSucceedsWithDatumBytes :: ShortByteString
alwaysSucceedsWithDatumBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||alwaysSucceedsWithDatum||])

alwaysFailsNoDatumBytes :: ShortByteString
alwaysFailsNoDatumBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||alwaysFailsNoDatum||])

alwaysFailsWithDatumBytes :: ShortByteString
alwaysFailsWithDatumBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||alwaysFailsWithDatum||])

redeemerSameAsDatumBytes :: ShortByteString
redeemerSameAsDatumBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||redeemerSameAsDatum||])

evenDatumBytes :: ShortByteString
evenDatumBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||evenDatum||])

evenRedeemerNoDatumBytes :: ShortByteString
evenRedeemerNoDatumBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||evenRedeemerNoDatum||])

evenRedeemerWithDatumBytes :: ShortByteString
evenRedeemerWithDatumBytes =
  PV1.serialiseCompiledCode
    $$(P.compile [||evenRedeemerWithDatum||])
