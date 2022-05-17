module Main where

import Test.Tasty
import Test.VMap

-- ====================================================================================

tests :: TestTree
tests =
  testGroup
    "vector-map"
    [ vMapTests
    ]

main :: IO ()
main = defaultMain tests
