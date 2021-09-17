{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simple example of a transition system whose states contain the sum of the
-- integers seen in the signals, where 'sum' is an abstract monoidal sum given
-- in the enviroment.
module Control.State.Transition.Examples.GlobalSum where

import Control.Arrow (right)
import Control.Monad.Reader
import Control.State.Transition.Extended
import Data.Foldable (foldl')
import Data.Void (Void)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (sum)

data Ops = Ops
  { opSum :: [Int] -> Int
  }

data GSUM

newtype GSUMEvent = ErrorEvent Void deriving (Eq, Show)

data NoFailure = NoFailure deriving (Eq, Show)

instance STS GSUM where
  type Environment GSUM = ()

  type State GSUM = Int

  type Signal GSUM = [Int]

  type BaseM GSUM = Reader Ops

  type PredicateFailure GSUM = NoFailure

  type Event _ = GSUMEvent

  initialRules = [pure 0]

  transitionRules =
    [ do
        TRC ((), st, xs) <- judgmentContext
        sum <- liftSTS $ reader opSum
        tellEvent $ ErrorEvent (error "Event has been evaluated!")
        return $! st + sum xs
    ]

tests :: TestTree
tests =
  testGroup
    "STS.Extended"
    [ testCase "Sum" $ withSum @=? Right 55,
      testCase "Product" $ withProduct @=? Right 3628800,
      testCase "Sum/Lazy Events" $ withLazyEventsSum @=? Right 55
    ]
  where
    inputs = [1 .. 10]
    ctx = TRC ((), 0, inputs)
    withSum = runReader (applySTS @GSUM ctx) (Ops (foldl' (+) 0))
    withProduct = runReader (applySTS @GSUM ctx) (Ops (foldl' (*) 1))
    withLazyEventsSum =
      right fst $
        runReader (applySTSOptsEither @GSUM evtOpts ctx) (Ops (foldl' (+) 0))
      where
        evtOpts =
          ApplySTSOpts
            { asoAssertions = AssertionsOff,
              asoValidation = ValidateAll,
              asoEvents = EPReturn
            }
