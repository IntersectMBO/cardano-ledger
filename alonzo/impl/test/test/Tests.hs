module Main where

import Test.Cardano.Ledger.Alonzo.DescribeEra (describeTest)
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.Tripping as Tripping
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Alonzo tests"
    [ Tripping.tests,
      describeTest
    ]

main :: IO ()
main = defaultMain tests
