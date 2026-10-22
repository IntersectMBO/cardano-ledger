{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Control.State.Transition.Examples.RecRule where

import Control.Monad (when)
import Control.State.Transition.Extended
import Data.Functor.Identity (Identity (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Validation

data RecRule

instance STS RecRule where
  type Environment RecRule = ()
  type Signal RecRule = Int
  type State RecRule = ()

  type PredicateFailure RecRule = String

  transitionRules =
    [ do
        TRC (_, _, n) <- judgmentContext
        even n ?! show n <> " is not even"
        n >= 3 ?! show n <> " is less than 3"
        when (n == 4) . validate $ Failure ["foo"] <> Failure ["bar"]
        when (n > 0) $ trans @RecRule $ TRC ((), (), n - 1)
    ]

spec :: Spec
spec = describe "RecRule" $ do
  it "Predicate failures happen in the right order" $ do
    runIdentity (applySTS @RecRule $ TRC ((), (), 5))
      `shouldBe` Left
        [ "5 is not even"
        , "foo"
        , "bar"
        , "3 is not even"
        , "2 is less than 3"
        , "1 is not even"
        , "1 is less than 3"
        , "0 is less than 3"
        ]
