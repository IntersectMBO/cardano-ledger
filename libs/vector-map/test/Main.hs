module Main where

import Test.Compact.KeyMap (alltests)
import Test.Compact.SplitMap (splitMapTests)
import Test.Compact.VMap
import Test.Tasty

-- ====================================================================================

tests :: TestTree
tests =
  testGroup
    "compact-map"
    [ alltests,
      vMapTests,
      splitMapTests
    ]

main :: IO ()
main = defaultMain tests
