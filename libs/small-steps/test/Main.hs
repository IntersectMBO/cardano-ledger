{-# LANGUAGE OverloadedStrings #-}

import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)
import qualified Test.Control.State.Transition.Examples.CommitReveal as CommitReveal
import qualified Test.Control.State.Transition.Examples.GlobalSum as GSum
import qualified Test.Control.State.Transition.Examples.Sum as Sum
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Runner
import Test.QuickCheck (expectFailure)

conf :: Config
conf =
  defaultConfig
    { configTimes = True
    , configColorMode = ColorAlways
    }

spec :: Spec
spec = describe "All" $ do
  describe "Sum" $ do
    prop "it allows to inspect generated trace counterexamples" $ expectFailure Sum.prop_qc_Bounded
    prop "Only valid traces are generated" Sum.prop_qc_onlyValidSignalsAreGenerated
    prop "Classified" Sum.prop_qc_Classified
  describe "CommitReveal" $ do
    prop "we're inspecting generated counterexamples" $ expectFailure CommitReveal.prop_qc_UniqueData
    prop "a counterexample of an invalid signal should be found" $
      expectFailure CommitReveal.prop_qc_OnlyValidSignals
  GSum.spec

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hspecWith conf spec
