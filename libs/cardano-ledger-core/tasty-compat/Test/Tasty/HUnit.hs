module Test.Tasty.HUnit where

import Control.Monad (unless)
import qualified Test.HUnit as HU
import Test.Hspec

type Assertion = Expectation

(@?=) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(@?=) = shouldBe

testCase :: HasCallStack => String -> Expectation -> Spec
testCase = it

assertBool :: HasCallStack => String -> Bool -> Expectation
assertBool s b = unless b $ expectationFailure s

assertEqual :: (HasCallStack, Show a, Eq a) => String -> a -> a -> Expectation
assertEqual _s = shouldBe

-- We can't use expectationFailure because we need to return IO a
assertFailure :: HasCallStack => String -> IO a
assertFailure = HU.assertFailure
