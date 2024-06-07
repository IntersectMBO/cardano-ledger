{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Cardano.Ledger.Plutus.Preprocessor.Binary.V1 where

import Cardano.Ledger.Plutus.Language (PlutusBinary (..))
import Cardano.Ledger.Plutus.Preprocessor.Source.V1
import Language.Haskell.TH
import qualified PlutusLedgerApi.V1 as PV1
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
$inputsOutputsAreNotEmptyQ

-- ================================================================
-- Compile and serialize the real functions as Plutus scripts.
-- Here is where we depend on plutus-plugin.

alwaysSucceedsNoDatumBytes :: (Q [Dec], PlutusBinary)
alwaysSucceedsNoDatumBytes =
  ( alwaysSucceedsNoDatumQ
  , PlutusBinary $ PV1.serialiseCompiledCode $$(P.compile [||alwaysSucceedsNoDatum||])
  )

alwaysSucceedsWithDatumBytes :: (Q [Dec], PlutusBinary)
alwaysSucceedsWithDatumBytes =
  ( alwaysSucceedsWithDatumQ
  , PlutusBinary $ PV1.serialiseCompiledCode $$(P.compile [||alwaysSucceedsWithDatum||])
  )

alwaysFailsNoDatumBytes :: (Q [Dec], PlutusBinary)
alwaysFailsNoDatumBytes =
  ( alwaysFailsNoDatumQ
  , PlutusBinary $ PV1.serialiseCompiledCode $$(P.compile [||alwaysFailsNoDatum||])
  )

alwaysFailsWithDatumBytes :: (Q [Dec], PlutusBinary)
alwaysFailsWithDatumBytes =
  ( alwaysFailsWithDatumQ
  , PlutusBinary $ PV1.serialiseCompiledCode $$(P.compile [||alwaysFailsWithDatum||])
  )

redeemerSameAsDatumBytes :: (Q [Dec], PlutusBinary)
redeemerSameAsDatumBytes =
  ( redeemerSameAsDatumQ
  , PlutusBinary $ PV1.serialiseCompiledCode $$(P.compile [||redeemerSameAsDatum||])
  )

evenDatumBytes :: (Q [Dec], PlutusBinary)
evenDatumBytes =
  ( evenDatumQ
  , PlutusBinary $ PV1.serialiseCompiledCode $$(P.compile [||evenDatum||])
  )

evenRedeemerNoDatumBytes :: (Q [Dec], PlutusBinary)
evenRedeemerNoDatumBytes =
  ( evenRedeemerNoDatumQ
  , PlutusBinary $ PV1.serialiseCompiledCode $$(P.compile [||evenRedeemerNoDatum||])
  )

evenRedeemerWithDatumBytes :: (Q [Dec], PlutusBinary)
evenRedeemerWithDatumBytes =
  ( evenRedeemerWithDatumQ
  , PlutusBinary $ PV1.serialiseCompiledCode $$(P.compile [||evenRedeemerWithDatum||])
  )

purposeIsWellformedNoDatumBytes :: (Q [Dec], PlutusBinary)
purposeIsWellformedNoDatumBytes =
  ( purposeIsWellformedNoDatumQ
  , PlutusBinary $ PV1.serialiseCompiledCode $$(P.compile [||purposeIsWellformedNoDatum||])
  )

purposeIsWellformedWithDatumBytes :: (Q [Dec], PlutusBinary)
purposeIsWellformedWithDatumBytes =
  ( purposeIsWellformedWithDatumQ
  , PlutusBinary $ PV1.serialiseCompiledCode $$(P.compile [||purposeIsWellformedWithDatum||])
  )

datumIsWellformedBytes :: (Q [Dec], PlutusBinary)
datumIsWellformedBytes =
  ( datumIsWellformedQ
  , PlutusBinary $ PV1.serialiseCompiledCode $$(P.compile [||datumIsWellformed||])
  )

inputsOutputsAreNotEmptyBytes :: (Q [Dec], PlutusBinary)
inputsOutputsAreNotEmptyBytes =
  ( inputsOutputsAreNotEmptyQ
  , PlutusBinary $ PV1.serialiseCompiledCode $$(P.compile [||inputsOutputsAreNotEmpty||])
  )
