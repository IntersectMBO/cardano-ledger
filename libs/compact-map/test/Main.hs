module Main where

import Test.Compact.KeyMap (alltests)
import Test.Compact.VMap
import Test.Tasty

-- ====================================================================================

tests :: TestTree
tests =
  testGroup
    "compcat-map"
    [ alltests,
      vMapTests
    ]

main :: IO ()
main = defaultMain tests
