module Main where

import Test.Hspec
import Test.VMap

-- ====================================================================================

tests :: Spec
tests =
  describe "vector-map" $ do
    vMapTests

main :: IO ()
main = hspec tests
