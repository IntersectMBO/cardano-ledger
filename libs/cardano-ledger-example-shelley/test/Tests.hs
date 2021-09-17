{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Example (ExampleEra)
import Cardano.Ledger.Shelley.PParams (PParams' (..))
import Cardano.Ledger.Shelley.RewardUpdate ()
import Cardano.Ledger.Shelley.Rules.Ledger (LEDGER)
import Test.Cardano.Ledger.Example ()
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
import Test.Cardano.Ledger.Shelley.PropertyTests (minimalPropertyTests, propertyTests)
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
