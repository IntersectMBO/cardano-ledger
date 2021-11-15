module Main where

import Test.Compact.KeyMap
import Test.Compact.VMap
import Test.Tasty

-- ====================================================================================

tests :: TestTree
tests =
  testGroup
    "compcat-map"
    [ keyMapTests,
      vMapTests
    ]

main :: IO ()
main = defaultMain tests
