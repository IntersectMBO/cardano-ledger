module Main where

import Test.Data.UMap (alltests)
import Test.Tasty

-- ====================================================================================

tests :: TestTree
tests =
  testGroup
    "cardano-data"
    [ alltests
    ]

main :: IO ()
main = defaultMain tests
