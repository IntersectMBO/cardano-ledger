module Main where

import Test.Control.Iterate.SetAlgebra (setAlgTest)
import Test.Control.Iterate.SplitMapRules (fastSlow)
import Test.Tasty

-- ====================================================================================

tests :: TestTree
tests =
  testGroup
    "set-algebra"
    [ setAlgTest,
      fastSlow
    ]

main :: IO ()
main = defaultMain tests
