module Test.ImpSpec.Expectations.Lifted (
  -- * Lifted Expectations
  io,

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
  IO.Selector,

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
) where

import Control.DeepSeq (NFData)
import GHC.Stack (HasCallStack)
import Test.Hspec.Expectations.Lifted (
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
 )
import qualified Test.ImpSpec.Expectations as IO
import UnliftIO (Exception, MonadIO (liftIO), MonadUnliftIO, withRunInIO)

infix 1 `shouldThrow`
        , `shouldBeRight`
        , `shouldBeLeft`

-- | Enforce the type of expectation
--
-- Useful with polymorphic expectations that are defined below.
--
-- ===__Example__
--
-- Because `shouldBeExpr` is polymorphic in `m`, compiler will choke with a unification
-- error. This is due to the fact that hspec's `it` expects a polymorphic `Example`.
--
-- > it "MyTest" $ do
-- >   "foo" `shouldBeExpr` "bar"
--
-- However, this is easily solved by `io`:
--
-- > it "MyTest" $ io $ do
-- >   "foo" `shouldBeExpr` "bar"
io :: IO a -> IO a
io = id

-- | Just like `expectationFailure`, but does not force the return type to unit. Lifted
-- version of `H.assertFailure`
assertFailure :: (HasCallStack, MonadIO m) => String -> m a
assertFailure = liftIO . IO.assertFailure

assertColorFailure :: (HasCallStack, MonadIO m) => String -> m a
assertColorFailure = liftIO . IO.assertColorFailure

-- | Lifted version of `H.assertBool`
assertBool :: (HasCallStack, MonadIO m) => String -> Bool -> m ()
assertBool msg = liftIO . IO.assertBool msg

-- | Lifted version of `shouldThrow`.
shouldThrow :: (HasCallStack, Exception e, MonadUnliftIO m) => m a -> IO.Selector e -> m ()
shouldThrow f s = withRunInIO $ \run -> IO.shouldThrow (run f) s

-- | Return value on the `Right` and fail otherwise. Lifted version of `H.expectRight`.
expectRight :: (HasCallStack, Show a, MonadIO m) => Either a b -> m b
expectRight = liftIO . IO.expectRight

-- | Same as `expectRight`, but also evaluate the returned value to NF
expectRightDeep :: (HasCallStack, Show a, NFData b, MonadIO m) => Either a b -> m b
expectRightDeep = liftIO . IO.expectRightDeep

-- | Same as `expectRightDeep`, but discards the result
expectRightDeep_ :: (HasCallStack, Show a, NFData b, MonadIO m) => Either a b -> m ()
expectRightDeep_ = liftIO . IO.expectRightDeep_

-- | Same as `shouldBe`, except it checks that the value is `Right`
shouldBeRight :: (HasCallStack, Show a, Show b, Eq b, MonadIO m) => Either a b -> b -> m ()
shouldBeRight e = liftIO . IO.shouldBeRight e

-- | Return value on the `Left` and fail otherwise
expectLeft :: (HasCallStack, Show b, MonadIO m) => Either a b -> m a
expectLeft = liftIO . IO.expectLeft

-- | Same as `expectLeftDeep`, but discards the result
expectLeftDeep_ :: (HasCallStack, NFData a, Show b, MonadIO m) => Either a b -> m ()
expectLeftDeep_ = liftIO . IO.expectLeftDeep_

-- | Same as `expectLeft`, but also evaluate the returned value to NF
expectLeftDeep :: (HasCallStack, NFData a, Show b, MonadIO m) => Either a b -> m a
expectLeftDeep = liftIO . IO.expectLeftDeep

-- | Same as `shouldBe`, except it checks that the value is `Left`
shouldBeLeft :: (HasCallStack, Show a, Eq a, Show b, MonadIO m) => Either a b -> a -> m ()
shouldBeLeft e x = liftIO $ e `IO.shouldBeLeft` x

-- | Same as `shouldBe`, except it checks that the value is `Just`
shouldBeJust :: (HasCallStack, Show a, Eq a, MonadIO m) => Maybe a -> a -> m ()
shouldBeJust e x = liftIO $ e `IO.shouldBeJust` x

expectJust :: (HasCallStack, MonadIO m) => Maybe a -> m a
expectJust = liftIO . IO.expectJust

expectJustDeep :: (HasCallStack, NFData a, MonadIO m) => Maybe a -> m a
expectJustDeep = liftIO . IO.expectJustDeep

expectJustDeep_ :: (HasCallStack, NFData a, MonadIO m) => Maybe a -> m ()
expectJustDeep_ = liftIO . IO.expectJustDeep_

expectNothing :: (HasCallStack, Show a, MonadIO m) => Maybe a -> m ()
expectNothing = liftIO . IO.expectNothing
