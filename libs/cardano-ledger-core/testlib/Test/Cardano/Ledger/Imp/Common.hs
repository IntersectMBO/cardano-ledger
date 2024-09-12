{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Imp.Common (
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
  HasGenEnv (..),
  HasSubState (..),
  subStateM,
  setSubStateM,
  R.StatefulGen,
  StateGen (..),
  StateGenM (..),
  uniformM,
  uniformRM,
  uniformListM,
  uniformListRM,
  uniformByteStringM,
  uniformShortByteStringM,
)
where

import Control.Monad.IO.Class
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
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
import qualified Test.Cardano.Ledger.Common as H
import Test.QuickCheck.GenT as QuickCheckT
import UnliftIO (MonadUnliftIO (..))
import UnliftIO.Exception (Exception, evaluateDeep)

-- Imports needed for Random interface. Separated from the rest, since they will migrate
-- to `random` at a later point:

import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Kind
import Data.List (isInfixOf)
import Foreign.Storable
import qualified System.Random.Stateful as R

instance MonadUnliftIO m => MonadUnliftIO (GenT m) where
  withRunInIO inner = GenT $ \qc sz ->
    withRunInIO $ \run -> inner $ \(GenT f) -> run (f qc sz)

infix 1 `shouldBe`
        , `shouldBeExpr`
        , `shouldSatisfy`
        , `shouldStartWith`
        , `shouldEndWith`
        , `shouldContain`
        , `shouldMatchList`
        , `shouldReturn`
        , `shouldThrow`
        , `shouldNotBe`
        , `shouldNotSatisfy`
        , `shouldNotContain`
        , `shouldNotReturn`
        , `shouldBeRight`
        , `shouldBeRightExpr`
        , `shouldBeLeft`
        , `shouldBeLeftExpr`

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
assertFailure = liftIO . H.assertFailure

assertColorFailure :: HasCallStack => String -> IO a
assertColorFailure = liftIO . H.assertColorFailure

-- | Just like `expectationBool`, but does not force the return type to unit. Lifted
-- version of `H.assertBool`
assertBool :: (HasCallStack, MonadIO m) => String -> Bool -> m ()
assertBool msg = liftIO . H.assertBool msg

-- | Lifted version of `expectationFailure`.
expectationFailure :: (HasCallStack, MonadIO m) => String -> m ()
expectationFailure = liftIO . H.expectationFailure

-- | Lifted version of `H.shouldBe`.
shouldBe :: (HasCallStack, Show a, Eq a, MonadIO m) => a -> a -> m ()
shouldBe x y = liftIO $ H.shouldBe x y

shouldBeExpr :: (HasCallStack, ToExpr a, Eq a, MonadIO m) => a -> a -> m ()
shouldBeExpr expected actual = liftIO $ expectExprEqualWithMessage "" expected actual

-- | Lifted version of `H.shouldSatisfy`.
shouldSatisfy :: (HasCallStack, Show a, MonadIO m) => a -> (a -> Bool) -> m ()
shouldSatisfy x f = liftIO $ H.shouldSatisfy x f

shouldSatisfyExpr :: (HasCallStack, MonadIO m, ToExpr a) => a -> (a -> Bool) -> m ()
shouldSatisfyExpr x f
  | f x = pure ()
  | otherwise = assertFailure $ "predicate failed on:\n" <> showExpr x

-- | Lifted version of `H.shouldStartWith`.
shouldStartWith :: (HasCallStack, Show a, Eq a, MonadIO m) => [a] -> [a] -> m ()
shouldStartWith x y = liftIO $ H.shouldStartWith x y

-- | Lifted version of `H.shouldEndWith`.
shouldEndWith :: (HasCallStack, Show a, Eq a, MonadIO m) => [a] -> [a] -> m ()
shouldEndWith x y = liftIO $ H.shouldEndWith x y

-- | Lifted version of `H.shouldContain`.
shouldContain :: (HasCallStack, Show a, Eq a, MonadIO m) => [a] -> [a] -> m ()
shouldContain x y = liftIO $ H.shouldContain x y

shouldContainExpr :: (HasCallStack, ToExpr a, Eq a, MonadIO m) => [a] -> [a] -> m ()
shouldContainExpr x y
  | y `isInfixOf` x = pure ()
  | otherwise =
      assertFailure $
        "First list does not contain the second list:\n"
          <> showExpr x
          <> "\ndoes not contain\n"
          <> showExpr y

-- | Lifted version of `H.shouldMatchList`.
shouldMatchList :: (HasCallStack, Show a, Eq a, MonadIO m) => [a] -> [a] -> m ()
shouldMatchList x y = liftIO $ H.shouldMatchList x y

-- | Lifted version of `H.shouldReturn`.
shouldReturn :: (HasCallStack, Show a, Eq a, MonadUnliftIO m) => m a -> a -> m ()
shouldReturn f a = withRunInIO $ \run -> H.shouldReturn (run f) a

-- | Lifted version of `H.shouldNotBe`.
shouldNotBe :: (HasCallStack, Show a, Eq a, MonadIO m) => a -> a -> m ()
shouldNotBe x y = liftIO $ H.shouldNotBe x y

-- | Lifted version of `H.shouldNotSatisfy`.
shouldNotSatisfy :: (HasCallStack, Show a, MonadIO m) => a -> (a -> Bool) -> m ()
shouldNotSatisfy a f = liftIO $ H.shouldNotSatisfy a f

-- | Lifted version of `H.shouldNotContain`.
shouldNotContain :: (HasCallStack, Show a, Eq a, MonadIO m) => [a] -> [a] -> m ()
shouldNotContain x y = liftIO $ H.shouldNotContain x y

-- | Lifted version of `H.shouldNotReturn`.
shouldNotReturn :: (HasCallStack, Show a, Eq a, MonadUnliftIO m) => m a -> a -> m ()
shouldNotReturn f a = withRunInIO $ \run -> H.shouldNotReturn (run f) a

-- | Lifted version of `shouldThrow`.
shouldThrow :: (HasCallStack, Exception e, MonadUnliftIO m) => m a -> Selector e -> m ()
shouldThrow f s = withRunInIO $ \run -> H.shouldThrow (run f) s

-- | Return value on the `Right` and fail otherwise. Lifted version of `H.expectRight`.
expectRight :: (HasCallStack, Show a, MonadIO m) => Either a b -> m b
expectRight (Right r) = pure $! r
expectRight (Left l) = assertFailure $ "Expected Right, got Left:\n" <> show l

-- | Same as `expectRightDeep`, but discards the result
expectRightDeep_ :: (HasCallStack, Show a, NFData b, MonadIO m) => Either a b -> m ()
expectRightDeep_ = void . expectRightDeep

-- | Same as `expectRight`, but also evaluate the returned value to NF
expectRightDeep :: (HasCallStack, Show a, NFData b, MonadIO m) => Either a b -> m b
expectRightDeep = expectRight >=> evaluateDeep

-- | Same as `expectRight`, but use `ToExpr` instead of `Show`
expectRightExpr :: (HasCallStack, ToExpr a, MonadIO m) => Either a b -> m b
expectRightExpr (Right r) = pure $! r
expectRightExpr (Left l) = assertFailure $ "Expected Right, got Left:\n" <> showExpr l

-- | Same as `expectRightDeep`,  but use `ToExpr` instead of `Show`
expectRightDeepExpr :: (HasCallStack, ToExpr a, NFData b, MonadIO m) => Either a b -> m b
expectRightDeepExpr = expectRightExpr >=> evaluateDeep

-- | Same as `shouldBe`, except it checks that the value is `Right`
shouldBeRight :: (HasCallStack, Show a, Show b, Eq b, MonadIO m) => Either a b -> b -> m ()
shouldBeRight e x = expectRight e >>= (`shouldBe` x)

-- | Same as `shouldBeExpr`, except it checks that the value is `Right`
shouldBeRightExpr :: (HasCallStack, ToExpr a, Eq b, ToExpr b, MonadIO m) => Either a b -> b -> m ()
shouldBeRightExpr e x = expectRightExpr e >>= (`shouldBeExpr` x)

-- | Return value on the `Left` and fail otherwise
expectLeft :: (HasCallStack, Show b, MonadIO m) => Either a b -> m a
expectLeft (Left l) = pure $! l
expectLeft (Right r) = assertFailure $ "Expected Left, got Right:\n" <> show r

-- | Same as `expectLeftDeep`, but discards the result
expectLeftDeep_ :: (HasCallStack, MonadIO m, Show b, NFData a) => Either a b -> m ()
expectLeftDeep_ = void . expectLeftDeep

-- | Same as `expectLeft`, but also evaluate the returned value to NF
expectLeftDeep :: (HasCallStack, NFData a, Show b, MonadIO m) => Either a b -> m a
expectLeftDeep = expectLeft >=> evaluateDeep

-- | Same as `expectLeft`, but use `ToExpr` instead of `Show`
expectLeftExpr :: (HasCallStack, ToExpr b, MonadIO m) => Either a b -> m a
expectLeftExpr (Left l) = pure $! l
expectLeftExpr (Right r) = assertFailure $ "Expected Left, got Right:\n" <> showExpr r

-- | Same as `expectLeftDeep`,  but use `ToExpr` instead of `Show`
expectLeftDeepExpr :: (HasCallStack, ToExpr b, NFData a, MonadIO m) => Either a b -> m a
expectLeftDeepExpr = expectLeftExpr >=> evaluateDeep

-- | Same as `shouldBe`, except it checks that the value is `Left`
shouldBeLeft :: (HasCallStack, Show a, Eq a, Show b, MonadIO m) => Either a b -> a -> m ()
shouldBeLeft e x = expectLeft e >>= (`shouldBe` x)

-- | Same as `shouldBeExpr`, except it checks that the value is `Left`
shouldBeLeftExpr :: (HasCallStack, ToExpr a, ToExpr b, Eq a, MonadIO m) => Either a b -> a -> m ()
shouldBeLeftExpr e x = expectLeftExpr e >>= (`shouldBeExpr` x)

expectJust :: (HasCallStack, MonadIO m) => Maybe a -> m a
expectJust (Just x) = pure x
expectJust Nothing = assertFailure "Expected Just, got Nothing"

expectNothingExpr :: (HasCallStack, MonadIO m, ToExpr a) => Maybe a -> m ()
expectNothingExpr (Just x) =
  assertFailure $
    "Expected Nothing, got Just:\n" <> showExpr x
expectNothingExpr Nothing = pure ()

---------------------------
-- MonadGen alternatives --
---------------------------

arbitrary :: (Arbitrary a, MonadGen m) => m a
arbitrary = liftGen H.arbitrary

---------------------------------------------------------------------------
-- This interface will be defined in the next major version of `random` ---
---------------------------------------------------------------------------

class R.StatefulGen g m => HasStatefulGen g m | m -> g where
  askStatefulGen :: m g

class HasGenEnv env g | env -> g where
  getGenEnv :: env -> g

instance
  (HasGenEnv env g, R.StatefulGen g (ReaderT env m), Monad m) =>
  HasStatefulGen g (ReaderT env m)
  where
  askStatefulGen = asks getGenEnv

class HasSubState s where
  type SubState s :: Type
  getSubState :: s -> SubState s
  getSubState = getConst . subStateL Const
  setSubState :: s -> SubState s -> s
  setSubState s a = runIdentity $ subStateL (const $ Identity a) s
  subStateL :: Functor f => (SubState s -> f (SubState s)) -> (s -> f s)
  subStateL k s = setSubState s <$> k (getSubState s)
  {-# MINIMAL subStateL | getSubState, setSubState #-}

setSubStateM :: (HasSubState s, MonadState s m) => SubState s -> m ()
setSubStateM s = subStateM $ const ((), s)
{-# INLINE setSubStateM #-}

-- | Modify the sub-state and return a value, using the supplied function.
-- Similar to the `state` method of `MonadState`.
subStateM :: (HasSubState s, MonadState s m) => (SubState s -> (a, SubState s)) -> m a
subStateM = state . subStateL -- Uses (a,) as the functor for subStateL
{-# INLINE subStateM #-}

uniformM ::
  ( HasStatefulGen g m
  , R.Uniform a
  ) =>
  m a
uniformM = askStatefulGen >>= R.uniformM
{-# INLINE uniformM #-}

uniformRM ::
  ( HasStatefulGen g m
  , R.UniformRange a
  ) =>
  (a, a) ->
  m a
uniformRM r = askStatefulGen >>= R.uniformRM r
{-# INLINE uniformRM #-}

uniformListM ::
  ( HasStatefulGen g m
  , R.Uniform a
  ) =>
  Int ->
  m [a]
uniformListM n = askStatefulGen >>= R.uniformListM n
{-# INLINE uniformListM #-}

uniformListRM ::
  ( HasStatefulGen g m
  , R.UniformRange a
  ) =>
  (a, a) ->
  Int ->
  m [a]
uniformListRM r n = askStatefulGen >>= replicateM n . R.uniformRM r
{-# INLINE uniformListRM #-}

uniformByteStringM :: HasStatefulGen a m => Int -> m ByteString
uniformByteStringM n = askStatefulGen >>= R.uniformByteStringM n
{-# INLINE uniformByteStringM #-}

uniformShortByteStringM :: HasStatefulGen a m => Int -> m ShortByteString
uniformShortByteStringM n = askStatefulGen >>= R.uniformShortByteString n
{-# INLINE uniformShortByteStringM #-}

data StateGenM s = StateGenM

newtype StateGen s = StateGen {unStateGen :: s}
  deriving (Eq, Ord, Show, R.RandomGen, Storable, NFData)

instance HasSubState (StateGen g) where
  type SubState (StateGen g) = g
  getSubState (StateGen g) = g
  {-# INLINE getSubState #-}
  setSubState _ = StateGen
  {-# INLINE setSubState #-}

instance
  (HasSubState s, R.RandomGen (SubState s), MonadState s m) =>
  R.StatefulGen (StateGenM s) m
  where
  uniformWord32R r _ = subStateM (R.genWord32R r)
  {-# INLINE uniformWord32R #-}
  uniformWord64R r _ = subStateM (R.genWord64R r)
  {-# INLINE uniformWord64R #-}
  uniformWord8 _ = subStateM R.genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 _ = subStateM R.genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 _ = subStateM R.genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 _ = subStateM R.genWord64
  {-# INLINE uniformWord64 #-}
  uniformShortByteString n _ = subStateM (R.genShortByteString n)
  {-# INLINE uniformShortByteString #-}
