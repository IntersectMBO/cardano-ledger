{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.Hedgehog (testProperty)

import qualified Control.State.Transition.Examples.Sum as Sum

main :: IO ()
main = do
  defaultMain tests
  where
    tests =
      testGroup
        "Sum"
        [ expectFailBecause "it allows to inspect generated trace counterexamples"
          $ testProperty "False" Sum.prop_Bounded
        , testProperty "Classified" Sum.prop_Classified
        ]
