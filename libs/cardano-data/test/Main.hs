module Main where

import Test.Data.MapExtras (mapExtrasTests)
import Test.Data.UMap (alltests)
import Test.Tasty

-- ====================================================================================

tests :: TestTree
tests =
  testGroup
    "cardano-data"
    [ mapExtrasTests,
      alltests
    ]

main :: IO ()
main = defaultMain tests
