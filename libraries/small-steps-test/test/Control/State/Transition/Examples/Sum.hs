{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simple example of a transition system whose states contain the sum of the
-- integers seen in the signals.
module Control.State.Transition.Examples.Sum where

import Control.State.Transition
import Control.State.Transition.Generator
import Control.State.Transition.Trace
import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen
import Hedgehog (Property, assert, forAll, property, withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.QuickCheck as QC

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
prop_Bounded :: Property
prop_Bounded = property $ do
  tr <- forAll (trace @SUM () 100)
  assert (lastState tr < 10)

prop_onlyValidTracesAreGenerated :: Property
prop_onlyValidTracesAreGenerated =
  withTests 300 $ onlyValidSignalsAreGenerated @SUM () 100

-- | Property that simply classifies the trace length distribution.
prop_Classified :: Property
prop_Classified = withTests 300 $
  property $ do
    let tl = 100
    tr <- forAll (trace @SUM () tl)
    classifyTraceLength tr tl 10
    assert True

-- | See 'prop_Bounded'
prop_qc_Bounded :: QC.Property
prop_qc_Bounded =
  STS.Gen.forAllTrace @SUM @()
    ()
    100
    ()
    ((< 10) . lastState)

-- | See 'prop_Classified'.
prop_qc_Classified :: QC.Property
prop_qc_Classified =
  STS.Gen.traceLengthsAreClassified @SUM () 100 10 ()

prop_qc_onlyValidSignalsAreGenerated :: QC.Property
prop_qc_onlyValidSignalsAreGenerated =
  QC.withMaxSuccess 300 $
    STS.Gen.onlyValidSignalsAreGenerated @SUM @() () 100 ()

instance STS.Gen.HasTrace SUM () where
  envGen _ = pure ()

  sigGen _traceEnv _env _st = QC.arbitrary

  shrinkSignal = QC.shrink
