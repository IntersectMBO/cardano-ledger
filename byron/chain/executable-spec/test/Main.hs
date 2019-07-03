
module Main (main) where

import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           Cardano.AbstractSize.Properties (testAbstractSize)
import           Cardano.Spec.Chain.STS.Properties (blockIssuersAreDelegates,
                     onlyValidSignalsAreGenerated, slotsIncrease)

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
      , testProperty "Only valid signals are generated" onlyValidSignalsAreGenerated
      ]
    ]
