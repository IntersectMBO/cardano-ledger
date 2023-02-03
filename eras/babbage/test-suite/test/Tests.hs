{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Test.Cardano.Ledger.Babbage.Serialisation.CDDL as CDDL
import qualified Test.Cardano.Ledger.Babbage.Serialisation.Tripping as Tripping
import Test.Cardano.Ledger.Babbage.TxInfo (txInfoTests)
import Test.Tasty
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

import Debug.Trace (trace)

-- ====================================================================================

tests :: TestTree
tests = askOption $ \case
  Nightly -> fakeNightlyTests
  Fast -> fastTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "Babbage tests"
    [ Tripping.tests
    , txInfoTests
    , CDDL.tests 5
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Babbage tests"
    [ Tripping.tests
    , txInfoTests
    , CDDL.tests 1
    ]

fakeNightlyTests :: TestTree
fakeNightlyTests = testGroup "FAKE NIGHTLY BABBAGE" []

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Babbage tests"
    [ (trace "!!!RUNNING NIGHTLY BABBAGE!!!" CDDL.tests 50)
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
