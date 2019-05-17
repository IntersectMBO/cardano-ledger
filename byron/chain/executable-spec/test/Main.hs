
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup, localOption)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Ingredients.ConsoleReporter (UseColor(Auto))

import Cardano.Spec.Chain.STS.Properties

main :: IO ()
main = defaultMain tests
 where
  tests :: TestTree
  tests =
    testGroup "Chain"
    [ testGroup "Properties"
      [ testProperty "Increasing slots" slotsIncrease
      , testProperty "Block issuers are delegates" blockIssuersAreDelegates
      ]
    ]
