module Main where

import Test.Control.Iterate.RelationReference (relationTests)
import Test.Control.Iterate.SetAlgebra (setAlgTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

-- ====================================================================================

tests :: TestTree
tests =
  testGroup
    "set-algebra"
    [ setAlgTest,
      relationTests
    ]

main :: IO ()
main = defaultMain tests
