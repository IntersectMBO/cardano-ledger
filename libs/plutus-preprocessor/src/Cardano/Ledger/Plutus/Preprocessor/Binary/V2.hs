{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Cardano.Ledger.Plutus.Preprocessor.Binary.V2 where

import Cardano.Ledger.Plutus.Language (PlutusBinary (..))
import Cardano.Ledger.Plutus.Preprocessor.Source.V2
import Language.Haskell.TH
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusTx as P (compile)

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
$purposeIsWellformedNoDatumQ
$purposeIsWellformedWithDatumQ
$datumIsWellformedQ
$inputsOutputsAreNotEmptyNoDatumQ
$inputsOutputsAreNotEmptyWithDatumQ

-- ================================================================
-- Compile and serialize the real functions as Plutus scripts.
-- Here is where we depend on plutus-plugin.

alwaysSucceedsNoDatumBytes :: (Q [Dec], PlutusBinary)
alwaysSucceedsNoDatumBytes =
  ( alwaysSucceedsNoDatumQ
  , PlutusBinary $ PV2.serialiseCompiledCode $$(P.compile [||alwaysSucceedsNoDatum||])
  )

alwaysSucceedsWithDatumBytes :: (Q [Dec], PlutusBinary)
alwaysSucceedsWithDatumBytes =
  ( alwaysSucceedsWithDatumQ
  , PlutusBinary $ PV2.serialiseCompiledCode $$(P.compile [||alwaysSucceedsWithDatum||])
  )

alwaysFailsNoDatumBytes :: (Q [Dec], PlutusBinary)
alwaysFailsNoDatumBytes =
  ( alwaysFailsNoDatumQ
  , PlutusBinary $ PV2.serialiseCompiledCode $$(P.compile [||alwaysFailsNoDatum||])
  )

alwaysFailsWithDatumBytes :: (Q [Dec], PlutusBinary)
alwaysFailsWithDatumBytes =
  ( alwaysFailsWithDatumQ
  , PlutusBinary $ PV2.serialiseCompiledCode $$(P.compile [||alwaysFailsWithDatum||])
  )

redeemerSameAsDatumBytes :: (Q [Dec], PlutusBinary)
redeemerSameAsDatumBytes =
  ( redeemerSameAsDatumQ
  , PlutusBinary $ PV2.serialiseCompiledCode $$(P.compile [||redeemerSameAsDatum||])
  )

evenDatumBytes :: (Q [Dec], PlutusBinary)
evenDatumBytes =
  ( evenDatumQ
  , PlutusBinary $ PV2.serialiseCompiledCode $$(P.compile [||evenDatum||])
  )

evenRedeemerNoDatumBytes :: (Q [Dec], PlutusBinary)
evenRedeemerNoDatumBytes =
  ( evenRedeemerNoDatumQ
  , PlutusBinary $ PV2.serialiseCompiledCode $$(P.compile [||evenRedeemerNoDatum||])
  )

evenRedeemerWithDatumBytes :: (Q [Dec], PlutusBinary)
evenRedeemerWithDatumBytes =
  ( evenRedeemerWithDatumQ
  , PlutusBinary $ PV2.serialiseCompiledCode $$(P.compile [||evenRedeemerWithDatum||])
  )

purposeIsWellformedNoDatumBytes :: (Q [Dec], PlutusBinary)
purposeIsWellformedNoDatumBytes =
  ( purposeIsWellformedNoDatumQ
  , PlutusBinary $ PV2.serialiseCompiledCode $$(P.compile [||purposeIsWellformedNoDatum||])
  )

purposeIsWellformedWithDatumBytes :: (Q [Dec], PlutusBinary)
purposeIsWellformedWithDatumBytes =
  ( purposeIsWellformedWithDatumQ
  , PlutusBinary $ PV2.serialiseCompiledCode $$(P.compile [||purposeIsWellformedWithDatum||])
  )

datumIsWellformedBytes :: (Q [Dec], PlutusBinary)
datumIsWellformedBytes =
  ( datumIsWellformedQ
  , PlutusBinary $ PV2.serialiseCompiledCode $$(P.compile [||datumIsWellformed||])
  )

inputsOutputsAreNotEmptyNoDatumBytes :: (Q [Dec], PlutusBinary)
inputsOutputsAreNotEmptyNoDatumBytes =
  ( inputsOutputsAreNotEmptyNoDatumQ
  , PlutusBinary $ PV2.serialiseCompiledCode $$(P.compile [||inputsOutputsAreNotEmptyNoDatum||])
  )

inputsOutputsAreNotEmptyWithDatumBytes :: (Q [Dec], PlutusBinary)
inputsOutputsAreNotEmptyWithDatumBytes =
  ( inputsOutputsAreNotEmptyWithDatumQ
  , PlutusBinary $ PV2.serialiseCompiledCode $$(P.compile [||inputsOutputsAreNotEmptyWithDatum||])
  )
