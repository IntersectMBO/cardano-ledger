{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
-- NOTE: this is for `split` vs. `splitGen` that we haven't had
-- time to fix in `QuickCheck`.
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | This module provides an interface for writing and working with generators
--     that may fail in both recoverable and unrecoverable ways.
module Constrained.GenT (
  -- * Types
  GE (..),
  GenT,
  GenMode (..),

  -- * Writing t`GenT` generators
  MonadGenError (..),
  pureGen,
  genFromGenT,
  suchThatT,
  suchThatWithTryT,
  scaleT,
  firstGenT,
  tryGenT,
  chooseT,
  sizeT,
  withMode,
  frequencyT,
  oneofT,
  vectorOfT,
  listOfUntilLenT,
  listOfT,
  resizeT,
  strictGen,
  looseGen,

  -- * So far undocumented
  fatalError,
  catMessages,
  catMessageList,
  explain,
  errorGE,
  fromGE,
  runGE,
  inspect,
  genError,
  pushGE,
  push,
  dropGen,
  catchGen,
  getMode,
  headGE,
  fromGEProp,
  fromGEDiscard,
  listFromGE,
) where

import Control.Monad
import Data.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Typeable
import GHC.Stack
import System.Random
import Test.QuickCheck hiding (Args, Fun)
import Test.QuickCheck.Gen

-- ==============================================================
-- The GE Monad

-- | This is like an @Error@ monad that distinguishes between two kinds of
-- errors: @FatalError@s and non-fatal @GenError@s.
data GE a
  = FatalError (NonEmpty (NonEmpty String))
  | GenError (NonEmpty (NonEmpty String))
  | Result a
  deriving (Ord, Eq, Show, Functor)

instance Applicative GE where
  pure = Result
  (<*>) = ap

instance Monad GE where
  FatalError es >>= _ = FatalError es
  GenError es >>= _ = GenError es
  Result a >>= k = k a

------------------------------------------------------------------------
-- The GenT monad
-- An environment monad on top of GE
------------------------------------------------------------------------

-- | Generation mode - how strict are we about requiring the generator to
-- succeed. This is necessary because sometimes failing to find a value means
-- there is an actual problem (a generator _should_ be satisfiable but for
-- whatever buggy reason it isn't) and sometimes failing to find a value just
-- means there are no values. The latter case is very relevant when you're
-- generating e.g. lists or sets of values that can be empty.
data GenMode
  = Loose
  | Strict
  deriving (Ord, Eq, Show)

-- | A `Gen` monad wrapper that allows different generation modes and different
-- failure types.
newtype GenT m a = GenT {runGenT :: GenMode -> [NonEmpty String] -> Gen (m a)}
  deriving (Functor)

instance Monad m => Applicative (GenT m) where
  pure a = GenT (\_ _ -> pure @Gen (pure @m a))
  (<*>) = ap

-- I think this might be an inlined use of the Gen monad transformer?
instance Monad m => Monad (GenT m) where
  GenT m >>= k = GenT $ \mode -> \msgs -> MkGen $ \r n -> do
    let (r1, r2) = split r
    a <- unGen (m mode msgs) r1 n
    unGen (runGenT (k a) mode msgs) r2 n

instance MonadGenError m => MonadFail (GenT m) where
  fail s = genError s

------------------------------------------------------------------------
-- The MonadGenError transformer
----------------------------------------------------------------------

-- | A class for different types of errors with a stack of `explain` calls to
-- narrow down problems. The @NonEmpty String@ means one cannot cause an error
-- without at least one string to explain it.
class Monad m => MonadGenError m where
  genErrors :: HasCallStack => NonEmpty (NonEmpty String) -> m a
  fatalErrors :: HasCallStack => NonEmpty (NonEmpty String) -> m a
  genErrorNE :: HasCallStack => NonEmpty String -> m a
  fatalErrorNE :: HasCallStack => NonEmpty String -> m a
  explainNE :: HasCallStack => NonEmpty String -> m a -> m a

-- | A potentially recoverable generation error
genError :: MonadGenError m => String -> m a
genError = genErrorNE . pure

-- | A non-recoverable fatal error
fatalError :: MonadGenError m => String -> m a
fatalError = fatalErrorNE . pure

-- | Attach an explanation to a computation in case of error
explain :: MonadGenError m => String -> m a -> m a
explain s
  | null s = id
  | otherwise = explainNE (pure s)

-- GE instance

instance MonadGenError GE where
  genErrorNE msg = GenError (pure msg)
  genErrors msgs = GenError msgs
  fatalErrorNE msg = FatalError (pure msg)
  fatalErrors msgs = FatalError msgs
  explainNE m (GenError ms) = GenError (m <| ms)
  explainNE m (FatalError ms) = FatalError (m <| ms)
  explainNE _ (Result x) = Result x

-- GenT instance

-- | calls to genError and fatalError, add the stacked messages in the monad.
instance MonadGenError m => MonadGenError (GenT m) where
  genErrorNE e = GenT $ \_ xs -> pure $ genErrors (add e xs)
  genErrors es = GenT $ \_ xs -> pure $ genErrors (cat es xs)

  -- Perhaps we want to turn genError into fatalError, if mode_ is Strict?
  fatalErrorNE e = GenT $ \_ xs -> pure $ fatalErrors (add e xs)
  fatalErrors es = GenT $ \_ xs -> pure $ fatalErrors (cat es xs)

  -- Perhaps we want to turn fatalError into genError, if mode_ is Loose?
  explainNE e (GenT f) = GenT $ \mode es -> fmap (explainNE e) (f mode es)

-- ====================================================
-- useful operations on NonEmpty

add :: NonEmpty a -> [NonEmpty a] -> NonEmpty (NonEmpty a)
add a [] = pure a
add a (x : xs) = a <| (x :| xs)

cat :: NonEmpty (NonEmpty a) -> [NonEmpty a] -> NonEmpty (NonEmpty a)
cat a [] = a
cat a (x : xs) = a <> (x :| xs)

-- | Sometimes we have a bunch of `genError` or `fatalError` messages we want
-- to combine into one big message. This happens when we want to lift one of
-- these into an input for 'error'
catMessages :: NonEmpty (NonEmpty String) -> String
catMessages xs = unlines (NE.toList (catMessageList xs))

-- | Turn each inner @NonEmpty String@ into a String
catMessageList :: NonEmpty (NonEmpty String) -> NonEmpty String
catMessageList = fmap (unlines . NE.toList)

-- ========================================================
-- Useful operations on GE

-- If none of the GE's are FatalError, then concat together all the
-- Results (skipping over GenError). If there is at least one
-- @FatalError xs@ abort, and lift all those @xs@ as errors in the monad @m@.
catGEs :: forall m a. MonadGenError m => [GE a] -> m [a]
catGEs ges0 = go [] ges0
  where
    go acc [] = pure $ reverse acc
    go !acc (g : ges) =
      case g of
        Result a -> go (a : acc) ges
        GenError _ -> go acc ges
        FatalError xs -> fatalErrors xs

-- | Turn @'GE' a@ into @a@ given a function for handling @GenError@, and handle
-- @FatalError@ with 'error'
fromGE :: HasCallStack => (NonEmpty (NonEmpty String) -> a) -> GE a -> a
fromGE f ge = case ge of
  Result a -> a
  GenError xs -> f xs
  FatalError es -> error $ catMessages es

-- | Turn @'GE' a@ into where both @GenError@ and @FatalError@ are handled by
-- using 'error'
errorGE :: GE a -> a
errorGE = fromGE (error . catMessages)

isOk :: GE a -> Bool
isOk ge = case ge of
  GenError {} -> False
  FatalError {} -> False
  Result {} -> True

-- | Convert a `GE` into an arbitrary monad that has an instance of
-- `MonadGenError`
runGE :: forall m r. MonadGenError m => GE r -> m r
runGE ge = case ge of
  GenError es -> genErrors es
  FatalError es -> fatalErrors es
  Result a -> pure a

-- | Turn a `GE` for something testable into a `Property`, failing on any
-- kind of error.
fromGEProp :: Testable p => GE p -> Property
fromGEProp ge = case ge of
  GenError es -> counterexample (catMessages es) False
  FatalError es -> counterexample (catMessages es) False
  Result p -> property p

-- | Turn a `GE` into a property, `discard`ing any failure.
fromGEDiscard :: Testable p => GE p -> Property
fromGEDiscard ge = case ge of
  Result p -> property p
  _ -> discard

-- | Like `Prelude.head` in the `GE` monad
headGE :: Foldable t => t a -> GE a
headGE t
  | x : _ <- toList t = pure x
  | otherwise = fatalError "head of empty structure"

-- | Turn a `GE [a]` to `[a]`, `genError` goes to `[]` and `fatalError` to `error`.
listFromGE :: GE [a] -> [a]
listFromGE = fromGE (const []) . explain "listFromGE"

-- ========================================================
-- Useful operations on GenT

-- | Run a t`GenT` generator in `Strict` mode
strictGen :: GenT m a -> Gen (m a)
strictGen genT = runGenT genT Strict []

-- | Run a t`GenT` generator in `Loose` mode
looseGen :: GenT m a -> Gen (m a)
looseGen genT = runGenT genT Loose []

-- | Turn a t`GenT` generator into a `Gen` generator in `Strict` mode
genFromGenT :: GenT GE a -> Gen a
genFromGenT genT = errorGE <$> strictGen genT

-- | Locally change the generation size
resizeT :: (Int -> Int) -> GenT m a -> GenT m a
resizeT f (GenT gm) = GenT $ \mode msgs -> sized $ \sz -> resize (f sz) (gm mode msgs)

-- | Turn a `Gen` generator into a t`GenT` generator that never fails.
pureGen :: Applicative m => Gen a -> GenT m a
pureGen gen = GenT $ \_ _ -> pure <$> gen

-- | Lift `listOf` to t`GenT`
listOfT :: MonadGenError m => GenT GE a -> GenT m [a]
listOfT gen = do
  lst <- pureGen . listOf $ runGenT gen Loose []
  catGEs lst

-- | Generate a list of elements of length at most @goalLen@, but accepting
-- failure to get that many elements so long as @validLen@ is true.
listOfUntilLenT ::
  (Typeable a, MonadGenError m) =>
  -- | Element generator
  GenT GE a ->
  -- | @goalLen@ goal length
  Int ->
  -- | @validLen@ filter
  (Int -> Bool) ->
  GenT m [a]
listOfUntilLenT gen goalLen validLen =
  genList `suchThatT` validLen . length
  where
    genList = do
      res <- pureGen . vectorOf goalLen $ runGenT gen Loose []
      catGEs res

-- | Lift `vectorOf` to t`GenT`
vectorOfT :: MonadGenError m => Int -> GenT GE a -> GenT m [a]
vectorOfT i gen = GenT $ \mode _ -> do
  res <- fmap sequence . vectorOf i $ runGenT gen Strict []
  case mode of
    Strict -> pure $ runGE res
    Loose -> case res of
      FatalError es -> pure $ genErrors es
      _ -> pure $ runGE res

infixl 2 `suchThatT`

-- | Lift `suchThat` to t`GenT`, equivalent to @`suchThatT` 100@
suchThatT :: (Typeable a, MonadGenError m) => GenT m a -> (a -> Bool) -> GenT m a
suchThatT g p = suchThatWithTryT 100 g p

-- | Lift `suchThat` to t`GenT` with special handling of generation mode. In
-- `Strict` mode @suchThatWithTry tries@ will try @tries@ times and fail with a
-- `fatalError` if unsuccessful.  In `Loose` mode however, we will try only
-- once and generate a `genError`.
suchThatWithTryT ::
  forall a m. (Typeable a, MonadGenError m) => Int -> GenT m a -> (a -> Bool) -> GenT m a
suchThatWithTryT tries g p = do
  mode <- getMode
  let (n, cont) = case mode of
        Strict -> (tries, fatalError)
        Loose -> (1 :: Int, genError) -- TODO: Maybe 1 is not the right number here!
  go n cont
  where
    go 0 cont =
      cont
        ("Ran out of tries (" ++ show tries ++ ") on suchThatWithTryT at type " ++ show (typeRep (Proxy @a)))
    go n cont = do
      a <- g
      if p a then pure a else scaleT (+ 1) $ go (n - 1) cont

-- | Lift `scale` to t`GenT`
scaleT :: (Int -> Int) -> GenT m a -> GenT m a
scaleT sc (GenT gen) = GenT $ \mode msgs -> scale sc $ gen mode msgs

-- | Access the `GenMode` we are running in, useful to decide e.g.  if we want
-- to re-try in case of a `GenError` or give up
getMode :: Applicative m => GenT m GenMode
getMode = GenT $ \mode _ -> pure (pure mode)

getMessages :: Applicative m => GenT m [NonEmpty String]
getMessages = GenT $ \_ msgs -> pure (pure msgs)

-- | Locally change the generation mode
withMode :: GenMode -> GenT m a -> GenT m a
withMode mode gen = GenT $ \_ msgs -> runGenT gen mode msgs

-- | Lift `oneof` to t`GenT`
oneofT :: (Typeable a, MonadGenError m) => [GenT GE a] -> GenT m a
oneofT gs = frequencyT $ map (1,) gs

-- | Lift `frequency` to t`GenT`
frequencyT :: (Typeable a, MonadGenError m) => [(Int, GenT GE a)] -> GenT m a
frequencyT gs = do
  mode <- getMode
  msgs <- getMessages
  r <-
    explain "suchThatT in oneofT" $
      pureGen (frequency [(f, runGenT g mode msgs) | (f, g) <- gs]) `suchThatT` isOk
  runGE r

-- | Lift `choose` to t`GenT`, failing with a `genError` in case of an empty interval
chooseT :: (Random a, Ord a, Show a, MonadGenError m) => (a, a) -> GenT m a
chooseT (a, b)
  | b < a = genError ("chooseT (" ++ show a ++ ", " ++ show b ++ ")")
  | otherwise = pureGen $ choose (a, b)

-- | Get the size provided to the generator
sizeT :: Monad m => GenT m Int
sizeT = GenT $ \mode msgs -> sized $ \n -> runGenT (pure n) mode msgs

-- ==================================================================
-- Reflective analysis of the internal GE structure of (GenT GE x)
-- This allows "catching" internal FatalError and GenError, and allowing
-- the program to control what happens in those cases.

-- | Always succeeds, but returns the internal GE structure for analysis
inspect :: forall m x. MonadGenError m => GenT GE x -> GenT m (GE x)
inspect (GenT f) = GenT g
  where
    g mode msgs = do geThing <- f mode msgs; pure @Gen (pure @m geThing)

-- | Ignore all kinds of Errors, by squashing them into Nothing
tryGenT :: MonadGenError m => GenT GE a -> GenT m (Maybe a)
tryGenT g = do
  r <- inspect g
  case r of
    FatalError _ -> pure Nothing
    GenError _ -> pure Nothing
    Result a -> pure $ Just a

-- Pass on the error messages of both kinds of Errors, by squashing and combining both of them into Left constructor
catchGenT :: MonadGenError m => GenT GE a -> GenT m (Either (NonEmpty (NonEmpty String)) a)
catchGenT g = do
  r <- inspect g
  case r of
    FatalError es -> pure $ Left es
    GenError es -> pure $ Left es
    Result a -> pure $ Right a

-- | Pass on the error messages of both kinds of Errors in the Gen (not the GenT) monad
catchGen :: GenT GE a -> Gen (Either (NonEmpty (NonEmpty String)) a)
catchGen g = genFromGenT (catchGenT g)

-- | Return the first successfull result from a list of computations, if they all fail
--   return a list of the error messages from each one.
firstGenT ::
  forall m a. MonadGenError m => [GenT GE a] -> GenT m (Either [(NonEmpty (NonEmpty String))] a)
firstGenT gs = loop gs []
  where
    loop ::
      [GenT GE a] -> [NonEmpty (NonEmpty String)] -> GenT m (Either [NonEmpty (NonEmpty String)] a)
    loop [] ys = pure (Left (reverse ys))
    loop (x : xs) ys = do
      this <- catchGenT x
      case this of
        Left zs -> loop xs (zs : ys)
        Right a -> pure (Right a)

-- | Drop a @t`GenT` `GE`@ computation into a @t`GenT` m@ computation.
--
-- Depending on the monad @m@ Some error information might be lost as
-- the monad might fold `FatalError`'s and `GenError`'s together.
dropGen :: MonadGenError m => GenT GE a -> GenT m a
dropGen y = do
  r <- inspect y
  case r of
    FatalError es -> fatalErrors es
    GenError es -> genErrors es
    Result a -> pure a

-- ======================================

-- | like explain for GenT, but uses [String] rather than (NonEmpty String)
--   if the list is null, it becomes the identity
push :: forall m a. MonadGenError m => [String] -> m a -> m a
push [] m = m
push (x : xs) m = explainNE (x :| xs) m

-- | like explain for GE, but uses [String] rather than (NonEmpty String)
--   if the list is null, it becomes the identity
pushGE :: forall a. [String] -> GE a -> GE a
pushGE [] x = x
pushGE (x : xs) m = explainNE (x :| xs) m
