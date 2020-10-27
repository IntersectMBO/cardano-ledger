module Main where

import Test.Cardano.Ledger.ShelleyMA.Timelocks (timelockAndDecodeTests)
import Test.Cardano.Ledger.ShelleyMA.TxBody (txBodyTest)
import Test.Tasty
import Test.Tasty.HUnit ()

tests :: TestTree
tests =
  testGroup
    "Cardano-Legder-Tests"
    [ txBodyTest,
      timelockAndDecodeTests
    ]

-- main entry point
main :: IO ()
main = defaultMain tests
