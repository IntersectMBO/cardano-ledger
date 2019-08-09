{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

 -- | Simple example of a transition system whose states contain the sum of the
-- integers seen in the signals.
--
module Control.State.Transition.Examples.Sum where

import           Hedgehog (Property, assert, forAll, property, withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Control.State.Transition
import           Control.State.Transition.Generator
import           Control.State.Transition.Trace


data SUM

instance STS SUM where

  type Environment SUM = ()

  type State SUM = Int

  type Signal SUM = [Int]

  data PredicateFailure SUM = NoFailure deriving (Eq, Show)

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
  tr <- forAll (trace @SUM 100)
  assert (lastState tr < 10)

-- | Property that simply classifies the trace length distribution.
prop_Classified :: Property
prop_Classified = withTests 300 $ property $ do
  let tl = 200
  tr <- forAll (trace @SUM tl)
  classifyTraceLength tr tl 10
  assert True
