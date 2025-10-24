module Main where

import Test.Control.Iterate.RelationReference (relationTests)
import Test.Control.Iterate.SetAlgebra (setAlgTest)
import Test.Hspec (Spec, describe, hspec)

-- ====================================================================================

tests :: Spec
tests =
  describe "set-algebra" $ do
    setAlgTest
    relationTests

main :: IO ()
main = hspec tests
