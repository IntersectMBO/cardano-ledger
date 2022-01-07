module Main where

import Test.Control.Iterate.SetAlgebra (setAlgTest)
import Test.Tasty

-- ====================================================================================

tests :: TestTree
tests =
  testGroup
    "set-algebra"
    [ setAlgTest
    ]

main :: IO ()
main = defaultMain tests
