{-# LANGUAGE ImplicitParams #-}

module Test.ImpSpec.Expectations (
  -- * Expectations

  -- ** Common
  assertBool,
  assertFailure,
  expectationFailure,
  shouldBe,
  shouldSatisfy,
  shouldStartWith,
  shouldEndWith,
  shouldContain,
  shouldMatchList,
  shouldReturn,
  shouldNotBe,
  shouldNotSatisfy,
  shouldNotContain,
  shouldNotReturn,
  shouldThrow,
  Selector,

  -- ** Custom
  assertColorFailure,

  -- *** Either
  shouldBeRight,
  shouldBeLeft,
  expectRight,
  expectRightDeep,
  expectRightDeep_,
  expectLeft,
  expectLeftDeep,
  expectLeftDeep_,

  -- *** Maybe
  shouldBeJust,
  expectJust,
  expectJustDeep,
  expectJustDeep_,
  expectNothing,

  -- * CallStack helpers
  callStackToLocation,
  srcLocToLocation,
) where

import Control.DeepSeq (NFData)
import Control.Monad (void, (>=>))
import GHC.Stack (CallStack, HasCallStack, SrcLoc (..), getCallStack)
import Test.HUnit.Base (assertBool, assertFailure)
import Test.Hspec (
  Expectation,
  Selector,
  expectationFailure,
  shouldBe,
  shouldContain,
  shouldEndWith,
  shouldMatchList,
  shouldNotBe,
  shouldNotContain,
  shouldNotReturn,
  shouldNotSatisfy,
  shouldReturn,
  shouldSatisfy,
  shouldStartWith,
  shouldThrow,
 )
import Test.Hspec.Core.Spec (FailureReason (ColorizedReason), Location (..), ResultStatus (Failure))
import UnliftIO.Exception (evaluateDeep, throwIO)

infix 1 `shouldBeRight`
        , `shouldBeLeft`

-- | Similar to `assertFailure`, except hspec will not interfer with any escape sequences
-- that indicate color output.
assertColorFailure :: HasCallStack => String -> IO a
assertColorFailure msg =
  throwIO $ Failure (callStackToLocation ?callStack) (ColorizedReason msg)

-- | Return value on the `Right` and fail otherwise.
--
-- Difference from @`shouldSatisfy` action `Data.Either.isRight`@ in that `expectRight`
-- will force the content of the `Right` to WHNF and return it. This expectation will also
-- show the content of the `Left` when expectation fails.
expectRight :: (HasCallStack, Show a) => Either a b -> IO b
expectRight (Right r) = pure $! r
expectRight (Left l) = assertFailure $ "Expected Right, got Left:\n" <> show l

-- | Same as `expectRight`, but also evaluate the returned value to NF
expectRightDeep :: (HasCallStack, Show a, NFData b) => Either a b -> IO b
expectRightDeep = expectRight >=> evaluateDeep

-- | Same as `expectRightDeep`, but discards the result
expectRightDeep_ :: (HasCallStack, Show a, NFData b) => Either a b -> IO ()
expectRightDeep_ = void . expectRightDeep

-- | Same as `shouldBe`, except it checks that the value is `Right`
shouldBeRight :: (HasCallStack, Show a, Show b, Eq b) => Either a b -> b -> Expectation
shouldBeRight e x = expectRight e >>= (`shouldBe` x)

-- | Return value on the `Left` an fail otherwise
--
-- Difference from @`shouldSatisfy` action `Data.Either.isLeft`@ in that `expectLeft` will
-- force the content of the `Left` to WHNF and and return it. This expectation will also
-- show the content of the `Right` when expectation fails.
expectLeft :: (HasCallStack, Show b) => Either a b -> IO a
expectLeft (Left l) = pure $! l
expectLeft (Right r) = assertFailure $ "Expected Left, got Right:\n" <> show r

-- | Same as `expectLeft`, but also evaluate the returned value to NF
expectLeftDeep :: (HasCallStack, NFData a, Show b) => Either a b -> IO a
expectLeftDeep = expectLeft >=> evaluateDeep

-- | Same as `expectLeftDeep`, but discards the result
expectLeftDeep_ :: (HasCallStack, NFData a, Show b) => Either a b -> IO ()
expectLeftDeep_ = void . expectLeftDeep

-- | Same as `shouldBe`, except it checks that the value is `Left`
shouldBeLeft :: (HasCallStack, Show a, Eq a, Show b) => Either a b -> a -> Expectation
shouldBeLeft e x = expectLeft e >>= (`shouldBe` x)

-- | Same as `shouldBe`, except it checks that the value is `Just`
shouldBeJust :: (HasCallStack, Show a, Eq a) => Maybe a -> a -> Expectation
shouldBeJust e x = expectJust e >>= (`shouldBe` x)

-- | Return value from the `Just` an fail otherwise
--
-- Difference from @`shouldSatisfy` action `isJust`@ in that `expectJust` will force the
-- content of the `Just` to WHNF and it will also return it.
expectJust :: HasCallStack => Maybe a -> IO a
expectJust (Just x) = pure $! x
expectJust Nothing = assertFailure "Expected Just, got Nothing"

-- | Same as `expectJust`, but will force the value to NF
expectJustDeep :: (HasCallStack, NFData a) => Maybe a -> IO a
expectJustDeep = expectJust >=> evaluateDeep

-- | Same as `expectJustDeep`, but will discard the forced contents of `Just`
expectJustDeep_ :: (HasCallStack, NFData a) => Maybe a -> IO ()
expectJustDeep_ = void . expectJustDeep

-- | Same as @`shouldSatisfy` action `Data.Maybe.isNothing`@
expectNothing :: (HasCallStack, Show a) => Maybe a -> IO ()
expectNothing (Just x) = assertFailure $ "Expected Nothing, got Just: " ++ show x
expectNothing Nothing = pure ()

-- | Convert the top call from the `CallStack` to hspec's `Location`
callStackToLocation :: CallStack -> Maybe Location
callStackToLocation cs =
  case getCallStack cs of
    [] -> Nothing
    (_, loc) : _ -> Just $ srcLocToLocation loc

-- | Convert `SrcLoc` to hspec's `Location`
srcLocToLocation :: SrcLoc -> Location
srcLocToLocation loc =
  Location
    { locationFile = srcLocFile loc
    , locationLine = srcLocStartLine loc
    , locationColumn = srcLocStartCol loc
    }
