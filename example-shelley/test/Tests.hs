{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Example (ExampleEra)
import Shelley.Spec.Ledger.PParams (PParams' (..))
import Shelley.Spec.Ledger.RewardUpdate ()
import Shelley.Spec.Ledger.STS.Ledger (LEDGER)
import Test.Cardano.Ledger.Example ()
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Shelley.Spec.Ledger.PropertyTests (minimalPropertyTests, propertyTests)
import Test.Tasty
import Test.Tasty.HUnit ()
import Test.TestScenario (TestScenario (..), mainWithTestScenario)

type E = ExampleEra C_Crypto

type L = LEDGER E

tests :: TestTree
tests = askOption $ \case
  Nightly -> nightlyTests
  Fast -> mainTests
  _ -> mainTests

mainTests :: TestTree
mainTests =
  testGroup
    "Example Consensus Tests"
    [ minimalPropertyTests @E @L
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Example Consensus - nightly"
    [ testGroup
        "Example Era - nightly"
        [ propertyTests @E @L
        ]
    ]

-- main entry point
main :: IO ()
main = mainWithTestScenario tests
