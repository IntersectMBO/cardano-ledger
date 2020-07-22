
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

 -- | Simple example of a transition system whose states contain the sum of the
 -- integers seen in the signals, where 'sum' is an abstract monoidal sum given
 -- in the enviroment.
 --
module Control.State.Transition.Examples.GlobalSum where

import           Control.State.Transition.Extended
import Control.Monad.Reader
import Data.Foldable (foldl')

import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding (sum)

data Ops = Ops
  { opSum :: [Int] -> Int
  }

data GSUM


instance STS GSUM where

  type Environment GSUM = ()

  type State GSUM = Int

  type Signal GSUM = [Int]

  type BaseM GSUM = Reader Ops

  data PredicateFailure GSUM = NoFailure deriving (Eq, Show)

  initialRules = [pure 0]

  transitionRules =
    [ do
        TRC ((), st, xs) <- judgmentContext
        sum <- liftSTS $ reader opSum
        return $! st + sum xs
    ]

tests :: TestTree
tests = testGroup "STS.Extended"
    [ testCase "Sum" $ withSum @=? Right 55
    , testCase "Product" $ withProduct @=? Right 3628800
    ]
  where
    inputs = [1..10]
    ctx = TRC ((), 0, inputs)
    withSum = runReader (applySTS @GSUM ctx) (Ops (foldl' (+) 0))
    withProduct = runReader (applySTS @GSUM ctx) (Ops (foldl' (*) 1))
