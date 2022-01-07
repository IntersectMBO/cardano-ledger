module Main where

import Test.Data.Coders (codersTest)
import Test.Data.UMap (alltests)
import Test.Tasty

-- ====================================================================================

tests :: TestTree
tests =
  testGroup
    "cardano-data"
    [ alltests,
      codersTest
    ]

main :: IO ()
main = defaultMain tests
