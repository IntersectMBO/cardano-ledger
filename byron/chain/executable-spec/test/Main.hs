
module Main (main) where

import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           Cardano.AbstractSize.Properties
import           Cardano.Spec.Chain.STS.Properties

main :: IO ()
main = defaultMain tests
 where
  tests :: TestTree
  tests =
    testGroup "Chain"
    [ testGroup "Properties"
      [ testProperty "Increasing slots" slotsIncrease
      , testProperty "Block issuers are delegates" blockIssuersAreDelegates

      , testAbstractSize
      ]
    ]
