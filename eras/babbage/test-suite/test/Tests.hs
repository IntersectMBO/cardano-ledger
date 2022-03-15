{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Test.Cardano.Ledger.Babbage.Serialisation.CDDL as CDDL
import qualified Test.Cardano.Ledger.Babbage.Serialisation.Tripping as Tripping
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
    "Babbage tests"
    [ Tripping.tests,
      CDDL.tests 5
    ]

fastTests :: TestTree
fastTests =
  testGroup
    "Babbage tests"
    [ Tripping.tests,
      CDDL.tests 1
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Babbage tests"
    [ CDDL.tests 50
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
