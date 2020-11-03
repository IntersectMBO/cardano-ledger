module Main where

import Test.Cardano.Ledger.ShelleyMA.Timelocks (timelockTests)
import Test.Cardano.Ledger.ShelleyMA.TxBody (txBodyTest)
import Test.Cardano.Ledger.ShelleyMA.Coders (codersTest)
import Test.Tasty
import Test.Tasty.HUnit ()

tests :: TestTree
tests =
  testGroup
    "Cardano-Legder-Tests"
    [ codersTest,
      txBodyTest,
      timelockTests
    ]

-- main entry point
main :: IO ()
main = defaultMain tests
