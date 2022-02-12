{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Test.Cardano.Ledger.Alonzo.Tools as Tools
import Test.Cardano.Ledger.BaseTypes (baseTypesTests)
import Test.Cardano.Ledger.Examples.TwoPhaseValidation
  ( alonzoAPITests,
    alonzoBBODYexamples,
    alonzoUTXOWexamples,
    collectOrderingAlonzo,
  )
import Test.Cardano.Ledger.Generic.Properties (genericProperties)
import Test.Cardano.Ledger.Model.Properties (modelUnitTests_)
import Test.Cardano.Ledger.Properties (alonzoProperties)
import Test.Tasty
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

-- ====================================================================================

tests :: TestTree
tests = askOption $ \case
  Nightly -> mainTests
  Fast -> mainTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "cardano-core"
    [ baseTypesTests,
      Tools.tests,
      testGroup
        "STS Tests"
        [ alonzoUTXOWexamples,
          alonzoBBODYexamples,
          alonzoAPITests,
          collectOrderingAlonzo,
          alonzoProperties,
          modelUnitTests_
        ],
      genericProperties
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
