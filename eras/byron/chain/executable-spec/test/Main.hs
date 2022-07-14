module Main (main) where

import Test.Byron.AbstractSize.Properties (testAbstractSize)
import Test.Byron.Spec.Chain.STS.Properties as CHAIN
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = defaultMain tests
  where
    tests :: TestTree
    tests =
      testGroup
        "Chain"
        [ testGroup
            "Properties"
            [ testProperty "Increasing slots" CHAIN.slotsIncrease,
              testProperty "Block issuers are delegates" CHAIN.blockIssuersAreDelegates,
              testAbstractSize,
              testProperty "Only valid signals are generated" CHAIN.onlyValidSignalsAreGenerated,
              testProperty "Signers list is bounded by k " CHAIN.signersListIsBoundedByK,
              testProperty "We are generating reasonable Chain Traces" CHAIN.relevantCasesAreCovered,
              testProperty "Invalid signals are generated when requested" CHAIN.invalidSignalsAreGenerated
            ]
        ]
