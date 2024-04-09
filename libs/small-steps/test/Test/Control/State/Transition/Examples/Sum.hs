{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simple example of a transition system whose states contain the sum of the
-- integers seen in the signals.
module Test.Control.State.Transition.Examples.Sum where

import Control.State.Transition
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Control.State.Transition.Generator
import Test.Control.State.Transition.Trace
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen
import Test.QuickCheck (Property, arbitrary, shrink, withMaxSuccess)

data SUM

data NoFailure = NoFailure deriving (Eq, Show)

instance STS SUM where
  type Environment SUM = ()

  type State SUM = Int

  type Signal SUM = [Int]

  type PredicateFailure SUM = NoFailure

  initialRules = [pure 0]

  transitionRules =
    [ do
        TRC ((), st, xs) <- judgmentContext
        return $! st + sum xs
    ]

instance HasTrace SUM where
  envGen _ = pure ()

  sigGen _ _ =
    Gen.list (Range.constant 1 100) (Gen.integral (Range.constant (-3) 3))

-- | This property is intended to be used to manually inspect the
-- counterexamples that we get.
prop_qc_Bounded :: Property
prop_qc_Bounded =
  STS.Gen.forAllTrace @SUM @()
    ()
    100
    ()
    ((< 10) . lastState)

-- | Property that simply classifies the trace length distribution.
prop_qc_Classified :: Property
prop_qc_Classified =
  STS.Gen.traceLengthsAreClassified @SUM () 100 10 ()

prop_qc_onlyValidSignalsAreGenerated :: Property
prop_qc_onlyValidSignalsAreGenerated =
  withMaxSuccess 300 $
    STS.Gen.onlyValidSignalsAreGenerated @SUM @() () 100 ()

instance STS.Gen.HasTrace SUM () where
  envGen _ = pure ()

  sigGen _traceEnv _env _st = arbitrary

  shrinkSignal = shrink
