{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Cardano.Ledger.Alonzo.Examples (plutusScriptExamples)
import Test.Cardano.Ledger.Alonzo.Golden as Golden
import Test.Cardano.Ledger.Alonzo.PropertyTests (alonzoPropertyTests, fastPropertyTests)
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.CDDL as CDDL
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.Canonical as Canonical
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.Tripping as Tripping
import qualified Test.Cardano.Ledger.Alonzo.Translation as Translation
import Test.Cardano.Ledger.Alonzo.TxInfo (txInfoTests)
import Test.Tasty
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

-- ====================================================================================

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> fastTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "Alonzo tests"
    [ fastPropertyTests, -- These are still pretty slow (it is just that a few are omitted)
      Tripping.tests,
      Translation.tests,
      Canonical.tests,
      CDDL.tests 5,
      Golden.goldenUTxOEntryMinAda,
      Golden.goldenSerialization,
      Golden.goldenGenesisSerialization,
      Golden.goldenMinFee,
      Golden.goldenScriptIntegrity,
      plutusScriptExamples,
      txInfoTests
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Alonzo tests"
    [ Tripping.tests,
      Translation.tests,
      CDDL.tests 1,
      Golden.goldenUTxOEntryMinAda,
      Golden.goldenSerialization,
      Golden.goldenScriptIntegrity,
      plutusScriptExamples,
      txInfoTests
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Alonzo tests"
    [ alonzoPropertyTests, -- These are the full property tests
      CDDL.tests 50
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
