{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:target-version=1.1.0 #-}

module Cardano.Ledger.Plutus.Preprocessor.Binary.V4 where

import Cardano.Ledger.Plutus.Language (PlutusBinary (..))
import Cardano.Ledger.Plutus.Preprocessor.Source.V4
import Language.Haskell.TH
import qualified PlutusLedgerApi.Common as Common
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
$inputsOverlapsWithRefInputsQ
$ensureTreasuryReserveQ

-- ================================================================
-- Compile and serialize the real functions as Plutus scripts.
-- Here is where we depend on plutus-plugin.

alwaysSucceedsNoDatumBytes :: (Q [Dec], PlutusBinary)
alwaysSucceedsNoDatumBytes =
  ( alwaysSucceedsNoDatumQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||alwaysSucceedsNoDatum||])
  )

alwaysSucceedsWithDatumBytes :: (Q [Dec], PlutusBinary)
alwaysSucceedsWithDatumBytes =
  ( alwaysSucceedsWithDatumQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||alwaysSucceedsWithDatum||])
  )

alwaysFailsNoDatumBytes :: (Q [Dec], PlutusBinary)
alwaysFailsNoDatumBytes =
  ( alwaysFailsNoDatumQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||alwaysFailsNoDatum||])
  )

alwaysFailsWithDatumBytes :: (Q [Dec], PlutusBinary)
alwaysFailsWithDatumBytes =
  ( alwaysFailsWithDatumQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||alwaysFailsWithDatum||])
  )

redeemerSameAsDatumBytes :: (Q [Dec], PlutusBinary)
redeemerSameAsDatumBytes =
  ( redeemerSameAsDatumQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||redeemerSameAsDatum||])
  )

evenDatumBytes :: (Q [Dec], PlutusBinary)
evenDatumBytes =
  ( evenDatumQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||evenDatum||])
  )

evenRedeemerNoDatumBytes :: (Q [Dec], PlutusBinary)
evenRedeemerNoDatumBytes =
  ( evenRedeemerNoDatumQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||evenRedeemerNoDatum||])
  )

evenRedeemerWithDatumBytes :: (Q [Dec], PlutusBinary)
evenRedeemerWithDatumBytes =
  ( evenRedeemerWithDatumQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||evenRedeemerWithDatum||])
  )

purposeIsWellformedNoDatumBytes :: (Q [Dec], PlutusBinary)
purposeIsWellformedNoDatumBytes =
  ( purposeIsWellformedNoDatumQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||purposeIsWellformedNoDatum||])
  )

purposeIsWellformedWithDatumBytes :: (Q [Dec], PlutusBinary)
purposeIsWellformedWithDatumBytes =
  ( purposeIsWellformedWithDatumQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||purposeIsWellformedWithDatum||])
  )

datumIsWellformedBytes :: (Q [Dec], PlutusBinary)
datumIsWellformedBytes =
  ( datumIsWellformedQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||datumIsWellformed||])
  )

inputsOutputsAreNotEmptyNoDatumBytes :: (Q [Dec], PlutusBinary)
inputsOutputsAreNotEmptyNoDatumBytes =
  ( inputsOutputsAreNotEmptyNoDatumQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||inputsOutputsAreNotEmptyNoDatum||])
  )

inputsOutputsAreNotEmptyWithDatumBytes :: (Q [Dec], PlutusBinary)
inputsOutputsAreNotEmptyWithDatumBytes =
  ( inputsOutputsAreNotEmptyWithDatumQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||inputsOutputsAreNotEmptyWithDatum||])
  )

inputsOverlapsWithRefInputsBytes :: (Q [Dec], PlutusBinary)
inputsOverlapsWithRefInputsBytes =
  ( inputsOverlapsWithRefInputsQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||inputsOverlapsWithRefInputs||])
  )

ensureTreasuryReserveBytes :: (Q [Dec], PlutusBinary)
ensureTreasuryReserveBytes =
  ( ensureTreasuryReserveQ
  , PlutusBinary $ Common.serialiseCompiledCode $$(P.compile [||ensureTreasuryReserve||])
  )
