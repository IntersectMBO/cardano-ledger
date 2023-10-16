{-# LANGUAGE AllowAmbiguousTypes #-}
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

import Control.Applicative
import Control.Monad
import Data.Foldable
import GHC.Stack
import System.Random
import Test.QuickCheck hiding (Args, Fun)
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

------------------------------------------------------------------------
-- The Gen Error monad
------------------------------------------------------------------------

-- | A class for different types of errors with a stack of `explain` calls to
-- narrow down problems.
class Monad m => MonadGenError m where
  genError :: HasCallStack => [String] -> m a
  fatalError :: HasCallStack => [String] -> m a
  explain :: HasCallStack => [String] -> m a -> m a

-- | The Gen Error monad, distinguishes between fatal errors
-- and non-fatal errors.
data GE a
  = FatalError [[String]] [String]
  | GenError [[String]] [String]
  | Result [[String]] a
  deriving (Ord, Eq, Show, Functor)

instance Applicative GE where
  pure = Result []
  (<*>) = ap

instance Monad GE where
  FatalError es err >>= _ = FatalError es err
  GenError es err >>= _ = GenError es err
  Result _ a >>= k = k a

instance Alternative GE where
  empty = GenError [] []
  m@FatalError {} <|> _ = m
  GenError {} <|> m = m
  Result es x <|> _ = Result es x

instance MonadGenError GE where
  genError = GenError []
  fatalError = FatalError []
  explain es (GenError es' err) = GenError (es : es') err
  explain es (FatalError es' err) = FatalError (es : es') err
  explain es (Result es' a) = Result (es : es') a

instance MonadGenError m => MonadGenError (GenT m) where
  genError es = GenT $ \_ -> pure $ genError es
  fatalError es = GenT $ \_ -> pure $ fatalError es
  explain es gen = GenT $ \mode -> fmap (explain es) (runGenT gen mode)

instance MonadGenError m => MonadFail (GenT m) where
  fail = genError . (: [])

catGEs :: MonadGenError m => [GE a] -> m [a]
catGEs [] = pure []
catGEs (Result _ a : ges) = (a :) <$> catGEs ges
catGEs (GenError {} : ges) = catGEs ges
catGEs (FatalError es e : _) =
  runGE $ FatalError es e

fromGE :: ([String] -> a) -> GE a -> a
fromGE _ (Result _ a) = a
fromGE a (GenError es e) = a $ concat es ++ e
fromGE _ (FatalError es e) =
  error . unlines $ concat es ++ e

errorGE :: GE a -> a
errorGE = fromGE (error . unlines)

isOk :: GE a -> Bool
isOk GenError {} = False
isOk FatalError {} = False
isOk Result {} = True

runGE :: MonadGenError m => GE r -> m r
runGE (GenError es err) = foldr explain (genError err) es
runGE (FatalError es err) = foldr explain (fatalError err) es
runGE (Result es a) = foldr explain (pure a) es

fromGEProp :: Testable p => GE p -> Property
fromGEProp (GenError es err) = foldr (counterexample . unlines) (counterexample (unlines err) False) es
fromGEProp (FatalError es err) = foldr (counterexample . unlines) (counterexample (unlines $ "Fatal error: " : err) False) es
fromGEProp (Result es p) = foldr (counterexample . unlines) (property p) es

fromGEDiscard :: Testable p => GE p -> Property
fromGEDiscard (Result es p) = foldr (counterexample . unlines) (property p) es
fromGEDiscard _ = discard

headGE :: Foldable t => t a -> GE a
headGE t
  | null t = fatalError ["head of empty structure"]
  | otherwise = pure $ head $ toList t

------------------------------------------------------------------------
-- GenT
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

-- TODO: put a global suchThat fuel parameter in here? To avoid exponential blowup of nested such
-- thats?
newtype GenT m a = GenT {runGenT :: GenMode -> Gen (m a)}
  deriving (Functor)

instance Monad m => Applicative (GenT m) where
  pure x = GenT $ \_ -> pure (pure x)
  (<*>) = ap

instance Monad m => Monad (GenT m) where
  GenT m >>= k = GenT $ \mode -> MkGen $ \r n -> do
    a <- unGen (m mode) (left r) n
    unGen (runGenT (k a) mode) (right r) n

strictGen :: GenT m a -> Gen (m a)
strictGen gen = runGenT gen Strict

resizeT :: (Int -> Int) -> GenT m a -> GenT m a
resizeT f (GenT gm) = GenT $ \mode -> sized $ \sz -> resize (f sz) (gm mode)

pureGen :: Applicative m => Gen a -> GenT m a
pureGen gen = GenT $ \_ -> pure <$> gen

listOfT :: MonadGenError m => GenT GE a -> GenT m [a]
listOfT gen = do
  lst <- pureGen . listOf $ runGenT gen Loose
  catGEs lst

vectorOfT :: MonadGenError m => Int -> GenT GE a -> GenT m [a]
vectorOfT i gen = GenT $ \mode -> do
  res <- fmap sequence . vectorOf i $ runGenT gen Strict
  case mode of
    Strict -> pure $ runGE res
    Loose -> case res of
      FatalError es e -> pure $ runGE (GenError es e)
      _ -> pure $ runGE res

infixl 2 `suchThatT`
suchThatT :: MonadGenError m => GenT m a -> (a -> Bool) -> GenT m a
suchThatT g p = do
  mode <- getMode
  let (n, cont) = case mode of
        Strict -> (100, fatalError)
        Loose -> (1 :: Int, genError) -- TODO: Maybe 1 is not the right number here!
  go n cont
  where
    go 0 cont = cont ["Ran out of tries on suchThatT"]
    go n cont = do
      a <- g
      if p a then pure a else scaleT (+ 1) $ go (n - 1) cont

scaleT :: (Int -> Int) -> GenT m a -> GenT m a
scaleT sc (GenT gen) = GenT $ \mode -> scale sc $ gen mode

getMode :: Applicative m => GenT m GenMode
getMode = GenT $ \mode -> pure (pure mode)

withMode :: GenMode -> GenT m a -> GenT m a
withMode mode gen = GenT $ \_ -> runGenT gen mode

oneofT :: MonadGenError m => [GenT GE a] -> GenT m a
oneofT gs = do
  mode <- getMode
  r <- explain ["suchThatT in oneofT"] $ pureGen (oneof [runGenT g mode | g <- gs]) `suchThatT` isOk
  runGE r

chooseT :: (Random a, Ord a, Show a, MonadGenError m) => (a, a) -> GenT m a
chooseT (a, b)
  | b < a = genError ["chooseT (" ++ show a ++ ", " ++ show b ++ ")"]
  | otherwise = pureGen $ choose (a, b)

sizeT :: Monad m => GenT m Int
sizeT = GenT $ \mode -> sized $ \n -> runGenT (pure n) mode

tryGen :: MonadGenError m => GenT GE a -> GenT m (Maybe a)
tryGen g = do
  r <- pureGen $ runGenT g Loose
  case r of
    FatalError es err -> foldr explain (fatalError err) es
    GenError _ _ -> pure Nothing
    Result _ a -> pure $ Just a
