{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusV3Scripts where

import Data.ByteString.Short (ShortByteString)
import Language.Haskell.TH
import qualified PlutusLedgerApi.V3 as PV3
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

alwaysSucceedsNoDatumBytes :: (Q [Dec], ShortByteString)
alwaysSucceedsNoDatumBytes =
  ( alwaysSucceedsNoDatumQ
  , PV3.serialiseCompiledCode $$(P.compile [||alwaysSucceedsNoDatum||])
  )

alwaysSucceedsWithDatumBytes :: (Q [Dec], ShortByteString)
alwaysSucceedsWithDatumBytes =
  ( alwaysSucceedsWithDatumQ
  , PV3.serialiseCompiledCode $$(P.compile [||alwaysSucceedsWithDatum||])
  )

alwaysFailsNoDatumBytes :: (Q [Dec], ShortByteString)
alwaysFailsNoDatumBytes =
  ( alwaysFailsNoDatumQ
  , PV3.serialiseCompiledCode $$(P.compile [||alwaysFailsNoDatum||])
  )

alwaysFailsWithDatumBytes :: (Q [Dec], ShortByteString)
alwaysFailsWithDatumBytes =
  ( alwaysFailsWithDatumQ
  , PV3.serialiseCompiledCode $$(P.compile [||alwaysFailsWithDatum||])
  )

redeemerSameAsDatumBytes :: (Q [Dec], ShortByteString)
redeemerSameAsDatumBytes =
  ( redeemerSameAsDatumQ
  , PV3.serialiseCompiledCode $$(P.compile [||redeemerSameAsDatum||])
  )

evenDatumBytes :: (Q [Dec], ShortByteString)
evenDatumBytes =
  ( evenDatumQ
  , PV3.serialiseCompiledCode $$(P.compile [||evenDatum||])
  )

evenRedeemerNoDatumBytes :: (Q [Dec], ShortByteString)
evenRedeemerNoDatumBytes =
  ( evenRedeemerNoDatumQ
  , PV3.serialiseCompiledCode $$(P.compile [||evenRedeemerNoDatum||])
  )

evenRedeemerWithDatumBytes :: (Q [Dec], ShortByteString)
evenRedeemerWithDatumBytes =
  ( evenRedeemerWithDatumQ
  , PV3.serialiseCompiledCode $$(P.compile [||evenRedeemerWithDatum||])
  )
