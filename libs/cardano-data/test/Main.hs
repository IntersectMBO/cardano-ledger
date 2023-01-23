module Main where

import Test.Data.MapExtras (mapExtrasTests)
import Test.Tasty

-- ====================================================================================

tests :: TestTree
tests =
  testGroup
    "cardano-data"
    [ mapExtrasTests
    ]

main :: IO ()
main = defaultMain tests
