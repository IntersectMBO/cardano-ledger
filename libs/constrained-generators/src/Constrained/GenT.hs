{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Constrained.GenT where

import Control.Monad
import Data.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE
import GHC.Stack
import System.Random
import Test.QuickCheck hiding (Args, Fun)
import Test.QuickCheck.Gen

-- ==============================================================
-- The GE Monad

-- | It distinguishes between two kinds of errors: FatalError and GenError
-- and non-fatal errors.
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
  fail s = genError (pure s)

------------------------------------------------------------------------
-- The MonadGenError transformer
----------------------------------------------------------------------

-- | A class for different types of errors with a stack of `explain` calls to
-- narrow down problems. The (NonEmpty String) means one cannot cause an
-- Error without at least 1 string to explain it.
class Monad m => MonadGenError m where
  genError :: HasCallStack => NonEmpty String -> m a
  fatalError :: HasCallStack => NonEmpty String -> m a
  genErrors :: HasCallStack => NonEmpty (NonEmpty String) -> m a
  fatalErrors :: HasCallStack => NonEmpty (NonEmpty String) -> m a
  explain :: HasCallStack => NonEmpty String -> m a -> m a

-- | genError with one line of explanation
genError1 :: MonadGenError m => String -> m a
genError1 s = genError (pure s)

-- | fatalError with one line of explanation
fatalError1 :: MonadGenError m => String -> m a
fatalError1 s = fatalError (pure s)

-- | explain with one line of explanation
explain1 :: MonadGenError m => String -> m a -> m a
explain1 s = explain (pure s)

-- GE instance

instance MonadGenError GE where
  genError msg = GenError (pure msg)
  genErrors msgs = GenError msgs
  fatalError msg = FatalError (pure msg)
  fatalErrors msgs = FatalError msgs
  explain m (GenError ms) = GenError (m <| ms)
  explain m (FatalError ms) = FatalError (m <| ms)
  explain _ (Result x) = Result x

-- GenT instance

-- | calls to genError and fatalError, add the stacked messages in the monad.
instance MonadGenError m => MonadGenError (GenT m) where
  genError e = GenT $ \_ xs -> pure $ genErrors (add e xs)
  genErrors es = GenT $ \_ xs -> pure $ genErrors (cat es xs)

  -- Perhaps we want to turn genError into fatalError, if mode_ is Strict?
  fatalError e = GenT $ \_ xs -> pure $ fatalErrors (add e xs)
  fatalErrors es = GenT $ \_ xs -> pure $ fatalErrors (cat es xs)

  -- Perhaps we want to turn fatalError into genError, if mode_ is Loose?
  explain e (GenT f) = GenT $ \mode es -> fmap (explain e) (f mode es)

-- ====================================================
-- useful operations on NonEmpty

add :: NonEmpty a -> [NonEmpty a] -> NonEmpty (NonEmpty a)
add a [] = pure a
add a (x : xs) = a <| (x :| xs)

cat :: NonEmpty (NonEmpty a) -> [NonEmpty a] -> NonEmpty (NonEmpty a)
cat a [] = a
cat a (x : xs) = a <> (x :| xs)

-- | Sometimes we have a bunch of genError or fatalError
--   messages we want to combine into one big message.
--   This happens when we want to lift one of these into an input for 'error'
catMessages :: NonEmpty (NonEmpty String) -> String
catMessages xs = unlines (NE.toList (catMessageList xs))

-- | Turn each inner (NonEmpty String) into a String
catMessageList :: NonEmpty (NonEmpty String) -> NonEmpty String
catMessageList = fmap (unlines . NE.toList)

-- ========================================================
-- Useful operations on GE

-- If none of the GE's are FatalError, then concat together all the
-- Result's (skipping over GenError). If there is at least one
-- (FatalError xs) abort, and lift all those 'xs' as errors in the monad 'm'
catGEs :: forall m a. MonadGenError m => [GE a] -> m [a]
catGEs ges0 = go [] ges0
  where
    go acc [] = pure $ reverse acc
    go !acc (g : ges) =
      case g of
        Result a -> go (a : acc) ges
        GenError _ -> go acc ges
        FatalError xs -> fatalErrors xs

-- | Given a function for handling GenError,
--   and handling FatalError by using 'error'
--   Turn (GE a) into 'a'
fromGE :: HasCallStack => (NonEmpty (NonEmpty String) -> a) -> GE a -> a
fromGE f ge = case ge of
  Result a -> a
  GenError xs -> f xs
  FatalError es -> error $ catMessages es

-- | Turn (GE a) into a
--   both GenError and FatalErrors are handled by using 'error'
errorGE :: GE a -> a
errorGE ge = case ge of
  FatalError xs -> error $ catMessages xs
  GenError xs -> error $ catMessages xs
  Result x -> x

isOk :: GE a -> Bool
isOk ge = case ge of
  GenError {} -> False
  FatalError {} -> False
  Result {} -> True

runGE :: forall m r. MonadGenError m => GE r -> m r
runGE ge = case ge of
  GenError es -> genErrors es
  FatalError es -> fatalErrors es
  Result a -> pure a

fromGEProp :: Testable p => GE p -> Property
fromGEProp ge = case ge of
  GenError es -> counterexample (catMessages es) False
  FatalError es -> counterexample (catMessages es) False
  Result p -> property p

fromGEDiscard :: Testable p => GE p -> Property
fromGEDiscard ge = case ge of
  Result p -> property p
  _ -> discard

headGE :: Foldable t => t a -> GE a
headGE t
  | x : _ <- toList t = pure x
  | otherwise = fatalError (pure "head of empty structure")

-- | Turn a `GE [a]` to `[a]`, `genError` goes to `[]` and `fatalError` to `error`.
listFromGE :: GE [a] -> [a]
listFromGE = fromGE (const []) . explain1 "listFromGE"

-- ========================================================
-- Useful operations on GenT

strictGen :: GenT m a -> Gen (m a)
strictGen genT = runGenT genT Strict []

genFromGenT :: GenT GE a -> Gen a
genFromGenT genT = errorGE <$> runGenT genT Strict []

resizeT :: (Int -> Int) -> GenT m a -> GenT m a
resizeT f (GenT gm) = GenT $ \mode msgs -> sized $ \sz -> resize (f sz) (gm mode msgs)

pureGen :: Applicative m => Gen a -> GenT m a
pureGen gen = GenT $ \_ _ -> pure <$> gen

listOfT :: MonadGenError m => GenT GE a -> GenT m [a]
listOfT gen = do
  lst <- pureGen . listOf $ runGenT gen Loose []
  catGEs lst

-- | Generate a list of elements of length at most `goalLen`, but accepting failure
-- to get that many elements so long as `validLen` is true.
-- TODO: possibly one could return "more, fewer, ok" in the `validLen` instead
-- of `Bool`
listOfUntilLenT :: MonadGenError m => GenT GE a -> Int -> (Int -> Bool) -> GenT m [a]
listOfUntilLenT gen goalLen validLen =
  genList `suchThatT` validLen . length
  where
    genList = do
      res <- pureGen . vectorOf goalLen $ runGenT gen Loose []
      catGEs res

vectorOfT :: MonadGenError m => Int -> GenT GE a -> GenT m [a]
vectorOfT i gen = GenT $ \mode _ -> do
  res <- fmap sequence . vectorOf i $ runGenT gen Strict []
  case mode of
    Strict -> pure $ runGE res
    Loose -> case res of
      FatalError es -> pure $ genErrors es
      _ -> pure $ runGE res

infixl 2 `suchThatT`
suchThatT :: MonadGenError m => GenT m a -> (a -> Bool) -> GenT m a
suchThatT g p = suchThatWithTryT 100 g p

suchThatWithTryT :: MonadGenError m => Int -> GenT m a -> (a -> Bool) -> GenT m a
suchThatWithTryT tries g p = do
  mode <- getMode
  let (n, cont) = case mode of
        Strict -> (tries, fatalError)
        Loose -> (1 :: Int, genError) -- TODO: Maybe 1 is not the right number here!
  go n cont
  where
    go 0 cont = cont (pure ("Ran out of tries (" ++ show tries ++ ") on suchThatWithTryT"))
    go n cont = do
      a <- g
      if p a then pure a else scaleT (+ 1) $ go (n - 1) cont

scaleT :: (Int -> Int) -> GenT m a -> GenT m a
scaleT sc (GenT gen) = GenT $ \mode msgs -> scale sc $ gen mode msgs

getMode :: Applicative m => GenT m GenMode
getMode = GenT $ \mode _ -> pure (pure mode)

getMessages :: Applicative m => GenT m [NonEmpty String]
getMessages = GenT $ \_ msgs -> pure (pure msgs)

withMode :: GenMode -> GenT m a -> GenT m a
withMode mode gen = GenT $ \_ msgs -> runGenT gen mode msgs

oneofT :: MonadGenError m => [GenT GE a] -> GenT m a
oneofT gs = do
  mode <- getMode
  msgs <- getMessages
  r <-
    explain (pure "suchThatT in oneofT") $
      pureGen (oneof [runGenT g mode msgs | g <- gs]) `suchThatT` isOk
  runGE r

frequencyT :: MonadGenError m => [(Int, GenT GE a)] -> GenT m a
frequencyT gs = do
  mode <- getMode
  msgs <- getMessages
  r <-
    explain (pure "suchThatT in oneofT") $
      pureGen (frequency [(f, runGenT g mode msgs) | (f, g) <- gs]) `suchThatT` isOk
  runGE r

chooseT :: (Random a, Ord a, Show a, MonadGenError m) => (a, a) -> GenT m a
chooseT (a, b)
  | b < a = genError (pure ("chooseT (" ++ show a ++ ", " ++ show b ++ ")"))
  | otherwise = pureGen $ choose (a, b)

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

liftGen :: forall x. (forall m. MonadGenError m => GenT m x) -> GenT GE x
liftGen x = x

-- Drop a (GenT GE) computation into a (GenT m) computation.
-- Depending on the monad 'm' Some error information might be lost as
-- the monad m might fold FatalError's and GenError's together.
dropGen :: MonadGenError m => GenT GE a -> GenT m a
dropGen y = do
  r <- inspect y
  case r of
    FatalError es -> fatalErrors es
    GenError es -> genErrors es
    Result a -> pure a

-- | Run one of the actions with frequency proportional to the count. If it fails, run the other.
frequency2 :: forall m a. MonadGenError m => (Int, GenT GE a) -> (Int, GenT GE a) -> GenT m a
frequency2 (n, g1) (m, g2)
  | n <= 0 && m <= 0 = fatalError (pure ("Non positive frequencies in frequency2 " ++ show (n, m)))
  | n <= 0 = dropGen g2
  | m <= 0 = dropGen g1
  | True = do
      i <- pureGen $ choose (1, n + m)
      ans <- if i <= n then firstGenT [g1, g2] else firstGenT [g2, g1]
      case ans of
        Left _ -> fatalError (pure "Both branches of frequency2 fail")
        Right x -> pure x

-- ======================================

-- | like explain for GenT, but uses [String] rather than (NonEmpty String)
--   if the list is null, it becomes the identity
push :: forall m a. MonadGenError m => [String] -> m a -> m a
push [] m = m
push (x : xs) m = explain (x :| xs) m

-- | like explain for GE, but uses [String] rather than (NonEmpty String)
--   if the list is null, it becomes the identity
pushGE :: forall a. [String] -> GE a -> GE a
pushGE [] x = x
pushGE (x : xs) m = explain (x :| xs) m
