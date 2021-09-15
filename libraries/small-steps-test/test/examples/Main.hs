{-# LANGUAGE OverloadedStrings #-}

import qualified Control.State.Transition.Examples.CommitReveal as CommitReveal
import qualified Control.State.Transition.Examples.GlobalSum as GSum
import qualified Control.State.Transition.Examples.Sum as Sum
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.Hedgehog (testProperty)
import qualified Test.Tasty.QuickCheck as Tasty.QuickCheck

main :: IO ()
main = do
  defaultMain tests
  where
    tests =
      testGroup
        "All"
        [ testGroup
            "Sum"
            [ expectFailBecause
                "it allows to inspect generated trace counterexamples"
                $ testProperty "False (Hedgehog)" Sum.prop_Bounded,
              testProperty
                "Only valid traces are generated"
                Sum.prop_onlyValidTracesAreGenerated,
              testProperty "Classified" Sum.prop_Classified,
              Tasty.QuickCheck.testProperty
                "Classified (QuickCheck)"
                Sum.prop_qc_Classified,
              expectFailBecause
                "it allows to inspect generated trace counterexamples (QuickCheck)"
                $ Tasty.QuickCheck.testProperty
                  "False (QuickCheck)"
                  Sum.prop_qc_Bounded,
              Tasty.QuickCheck.testProperty
                "Only valid traces are generated (QuickCheck)"
                Sum.prop_qc_onlyValidSignalsAreGenerated
            ],
          testGroup
            "CommitReveal"
            [ expectFailBecause
                "we're inspecting generated counterexamples"
                $ Tasty.QuickCheck.testProperty
                  "Unique Data (QuickCheck)"
                  CommitReveal.prop_qc_UniqueData,
              expectFailBecause
                "a counterexample of an invalid signal should be found"
                $ Tasty.QuickCheck.testProperty
                  "Only valid signals are generated (QuickCheck)"
                  CommitReveal.prop_qc_OnlyValidSignals
            ],
          GSum.tests
        ]
