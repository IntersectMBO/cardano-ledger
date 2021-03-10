module Main where

import qualified Test.Cardano.Ledger.Alonzo.Serialisation.Tripping as Tripping
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Alonzo tests"
    [ Tripping.tests
    ]

main :: IO ()
main = defaultMain tests
