{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Imp.Common (
  -- * Ledger
  KeyPair (..),
  mkAddr,
  mkCredential,

  -- * Re-exports
  module X,
  io,

  -- * Expectations

  -- ** Lifted expectations
  assertBool,
  assertFailure,
  assertColorFailure,
  expectationFailure,
  shouldBe,
  shouldSatisfy,
  shouldSatisfyExpr,
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

  -- ** Non-standard expectations
  shouldBeExpr,
  shouldBeRight,
  shouldBeLeft,
  shouldBeRightExpr,
  shouldBeLeftExpr,
  shouldContainExpr,
  expectRight,
  expectRightDeep_,
  expectRightDeep,
  expectRightExpr,
  expectRightDeepExpr,
  expectLeft,
  expectLeftDeep_,
  expectLeftExpr,
  expectLeftDeep,
  expectLeftDeepExpr,
  expectJust,
  expectNothingExpr,

  -- * MonadGen
  module QuickCheckT,
  arbitrary,
  -- TODO: add here any other lifted functions from quickcheck

  -- * Random interface
  HasStatefulGen (..),
  R.StatefulGen,
  uniformM,
  uniformRM,
  uniformListM,
  uniformListRM,
  uniformByteStringM,
  uniformShortByteStringM,

  -- * Re-exports from ImpSpec
  withImpInit,
  modifyImpInit,
)
where

import Control.Monad.IO.Class
import Data.List (isInfixOf)
import qualified System.Random.Stateful as R
import Test.Cardano.Ledger.Binary.TreeDiff (expectExprEqualWithMessage)
import Test.Cardano.Ledger.Common as X hiding (
  arbitrary,
  assertBool,
  assertColorFailure,
  assertFailure,
  choose,
  elements,
  expectLeft,
  expectLeftDeep,
  expectLeftDeepExpr,
  expectLeftDeep_,
  expectLeftExpr,
  expectRight,
  expectRightDeep,
  expectRightDeepExpr,
  expectRightDeep_,
  expectRightExpr,
  expectationFailure,
  frequency,
  growingElements,
  listOf,
  listOf1,
  oneof,
  resize,
  shouldBe,
  shouldBeExpr,
  shouldBeLeft,
  shouldBeLeftExpr,
  shouldBeRight,
  shouldBeRightExpr,
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
  sized,
  suchThat,
  suchThatMaybe,
  variant,
  vectorOf,
 )
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr, mkCredential)
import Test.ImpSpec (modifyImpInit, withImpInit)
import Test.ImpSpec.Expectations.Lifted
import Test.ImpSpec.Random (
  HasStatefulGen (..),
  arbitrary,
  uniformByteStringM,
  uniformListM,
  uniformListRM,
  uniformM,
  uniformRM,
  uniformShortByteStringM,
 )
import Test.QuickCheck.GenT as QuickCheckT
import UnliftIO (MonadUnliftIO (..))
import UnliftIO.Exception (evaluateDeep)

instance MonadUnliftIO m => MonadUnliftIO (GenT m) where
  withRunInIO inner = GenT $ \qc sz ->
    withRunInIO $ \run -> inner $ \(GenT f) -> run (f qc sz)

infix 1 `shouldBeExpr`
        , `shouldBeRightExpr`
        , `shouldBeLeftExpr`

shouldBeExpr :: (HasCallStack, ToExpr a, Eq a, MonadIO m) => a -> a -> m ()
shouldBeExpr expected actual = liftIO $ expectExprEqualWithMessage "" expected actual

shouldSatisfyExpr :: (HasCallStack, MonadIO m, ToExpr a) => a -> (a -> Bool) -> m ()
shouldSatisfyExpr x f
  | f x = pure ()
  | otherwise = assertFailure $ "predicate failed on:\n" <> showExpr x

shouldContainExpr :: (HasCallStack, ToExpr a, Eq a, MonadIO m) => [a] -> [a] -> m ()
shouldContainExpr x y
  | y `isInfixOf` x = pure ()
  | otherwise =
      assertFailure $
        "First list does not contain the second list:\n"
          <> showExpr x
          <> "\ndoes not contain\n"
          <> showExpr y

-- | Same as `expectRight`, but use `ToExpr` instead of `Show`
expectRightExpr :: (HasCallStack, ToExpr a, MonadIO m) => Either a b -> m b
expectRightExpr (Right r) = pure $! r
expectRightExpr (Left l) = assertFailure $ "Expected Right, got Left:\n" <> showExpr l

-- | Same as `expectRightDeep`,  but use `ToExpr` instead of `Show`
expectRightDeepExpr :: (HasCallStack, ToExpr a, NFData b, MonadIO m) => Either a b -> m b
expectRightDeepExpr = expectRightExpr >=> evaluateDeep

-- | Same as `shouldBeExpr`, except it checks that the value is `Right`
shouldBeRightExpr :: (HasCallStack, ToExpr a, Eq b, ToExpr b, MonadIO m) => Either a b -> b -> m ()
shouldBeRightExpr e x = expectRightExpr e >>= (`shouldBeExpr` x)

-- | Same as `expectLeft`, but use `ToExpr` instead of `Show`
expectLeftExpr :: (HasCallStack, ToExpr b, MonadIO m) => Either a b -> m a
expectLeftExpr (Left l) = pure $! l
expectLeftExpr (Right r) = assertFailure $ "Expected Left, got Right:\n" <> showExpr r

-- | Same as `expectLeftDeep`,  but use `ToExpr` instead of `Show`
expectLeftDeepExpr :: (HasCallStack, ToExpr b, NFData a, MonadIO m) => Either a b -> m a
expectLeftDeepExpr = expectLeftExpr >=> evaluateDeep

-- | Same as `shouldBeExpr`, except it checks that the value is `Left`
shouldBeLeftExpr :: (HasCallStack, ToExpr a, ToExpr b, Eq a, MonadIO m) => Either a b -> a -> m ()
shouldBeLeftExpr e x = expectLeftExpr e >>= (`shouldBeExpr` x)

expectNothingExpr :: (HasCallStack, MonadIO m, ToExpr a) => Maybe a -> m ()
expectNothingExpr (Just x) =
  assertFailure $
    "Expected Nothing, got Just:\n" <> showExpr x
expectNothingExpr Nothing = pure ()
