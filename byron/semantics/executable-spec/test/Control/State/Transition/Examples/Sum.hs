{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

 -- | Simple example of a transition system whose states contain the sum of the
-- integers seen in the signals.
--
module Control.State.Transition.Examples.Sum where

import Hedgehog (Property, forAll, property, withTests, assert, classify)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.State.Transition
import Control.State.Transition.Generator
import Control.State.Transition.Trace


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
  initEnvGen = pure ()

  sigGen _ _ =
    Gen.list (Range.constant 1 100) (Gen.integral (Range.constant (-3) 3))

-- | This property is intended to be used to manually inspect the
-- counterexamples that we get.
prop_Bounded :: Property
prop_Bounded = property $ do
  tr <- forAll (trace @SUM 300)
  classify "empty" $ traceLength tr == 0
  classify "[1, 150)" $ 1 < traceLength tr && traceLength tr < 150
  classify "[150, 300]" $ 150 < traceLength tr
  assert (lastState tr < 10)

-- | Property that simply classifies the trace length distribution.
prop_Classified :: Property
prop_Classified = withTests 300 $ property $ do
  tr <- forAll (trace @SUM 1000)
  classify "empty"       $ traceLength tr == 0
  classify "singleton"   $ traceLength tr == 1
  classify "[2, 100)"    $ 2 < traceLength tr && traceLength tr < 100
  classify "[100, 500)"  $ 100 < traceLength tr && traceLength tr < 500
  classify "[500, 1000)" $ 500 < traceLength tr && traceLength tr < 1000
  classify "1000"        $ traceLength tr == 1000
  assert True
