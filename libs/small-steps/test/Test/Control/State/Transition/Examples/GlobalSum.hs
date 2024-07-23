{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simple example of a transition system whose states contain the sum of the
-- integers seen in the signals, where 'sum' is an abstract monoidal sum given
-- in the enviroment.
module Test.Control.State.Transition.Examples.GlobalSum where

import Control.Arrow (right)
import Control.Monad.Reader
import Control.State.Transition.Extended
import Data.Foldable as F (foldl')
import Data.Void (Void)
import Test.Hspec
import Prelude hiding (sum)

newtype Ops = Ops
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
        labeled ["testLabel"] $ (sum xs /= 56) ?! NoFailure
        sum xs /= 56 ?! NoFailure
        return $! st + sum xs
    ]

spec :: Spec
spec =
  describe "STS.Extended" $ do
    it "Sum" $ withSum `shouldBe` Right 55
    it "Product" $ withProduct `shouldBe` Right 3628800
    it "Sum/Lazy Events" $ withLazyEventsSum `shouldBe` Right 55
    describe "Sum/Validate" $ do
      it "Filtered" $
        withLblSum (ValidateSuchThat ("testLabel" `notElem`)) `shouldBe` Left [NoFailure]
      it "Unfiltered" $
        withLblSum (ValidateSuchThat (const True)) `shouldBe` Left [NoFailure, NoFailure]
      it "None" $
        withLblSum ValidateNone `shouldBe` Right 56
  where
    inputs = [1 .. 10]
    ctx = TRC ((), 0, inputs)
    withSum = runReader (applySTS @GSUM ctx) (Ops (F.foldl' (+) 0))
    withLblSum vp =
      right fst $
        runReader (applySTSOptsEither @GSUM lblOpts ctx) (Ops (F.foldl' (+) 1))
      where
        lblOpts =
          ApplySTSOpts
            { asoAssertions = AssertionsOff
            , asoValidation = vp
            , asoEvents = EPReturn
            }

    withProduct = runReader (applySTS @GSUM ctx) (Ops (F.foldl' (*) 1))
    withLazyEventsSum =
      right fst $
        runReader (applySTSOptsEither @GSUM evtOpts ctx) (Ops (F.foldl' (+) 0))
      where
        evtOpts =
          ApplySTSOpts
            { asoAssertions = AssertionsOff
            , asoValidation = ValidateAll
            , asoEvents = EPReturn
            }
