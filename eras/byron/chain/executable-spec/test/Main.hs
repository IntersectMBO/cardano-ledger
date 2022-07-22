{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Byron.AbstractSize.Properties (testAbstractSize)
import Test.Byron.Spec.Chain.STS.Properties as CHAIN
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main = defaultMain tests
  where
    tests :: TestTree
    tests =
      testGroup
        "Chain"
        [ testGroup
            "Properties"
            [ testPropertyNamed
                "Increasing slots"
                "increasing-slots"
                CHAIN.slotsIncrease,
              testPropertyNamed
                "Block issuers are delegates"
                "issuers-delegates"
                CHAIN.blockIssuersAreDelegates,
              testAbstractSize,
              testPropertyNamed
                "Only valid signals are generated"
                "valid-signals"
                CHAIN.onlyValidSignalsAreGenerated,
              testPropertyNamed
                "Signers list is bounded by k "
                "signers-bounded"
                CHAIN.signersListIsBoundedByK,
              testPropertyNamed
                "We are generating reasonable Chain Traces"
                "reasonable-traces"
                CHAIN.relevantCasesAreCovered,
              testPropertyNamed
                "Invalid signals are generated when requested"
                "generate-invalid-signals"
                CHAIN.invalidSignalsAreGenerated
            ]
        ]
