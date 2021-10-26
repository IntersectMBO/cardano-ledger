module Main where

import Test.Compact.KeyMap
import Test.Tasty

-- ====================================================================================

tests :: TestTree
tests =
  testGroup
    "compcat-map"
    [ keyMapTests
    ]

main :: IO ()
main = defaultMain tests
