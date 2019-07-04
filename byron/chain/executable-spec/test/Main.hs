
module Main (main) where

import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           Cardano.AbstractSize.Properties (testAbstractSize)
import           Cardano.Spec.Chain.STS.Properties as CHAIN

main :: IO ()
main = defaultMain tests
 where
  tests :: TestTree
  tests =
    testGroup "Chain"
    [ testGroup "Properties"
      [ testProperty "Increasing slots" CHAIN.slotsIncrease
      , testProperty "Block issuers are delegates" CHAIN.blockIssuersAreDelegates
      , testAbstractSize
      , testProperty "Only valid signals are generated" CHAIN.onlyValidSignalsAreGenerated
      , testProperty "Signers list is bounded by k " CHAIN.signersListIsBoundedByK
      ]
    ]
