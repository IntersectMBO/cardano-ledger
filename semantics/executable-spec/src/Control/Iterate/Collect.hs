{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Control.Iterate.Collect where

import qualified Control.Applicative as AP
import qualified Control.Monad as CM
import qualified Data.Map.Strict as Map

-- =========================================================================
-- Sample continuation monad to study. We don't actually use this monad, but
-- we put it here since it is the simplest continuation monad, and studying
-- it, helped me define the Collect monad.

newtype Cont ans x = Cont {runCont :: (x -> ans) -> ans} -- ans is the final result type of the whole computation

instance Functor (Cont ans) where
  fmap f (Cont k2) = Cont (\k1 -> k2 (k1 . f))

instance Applicative (Cont ans) where
  pure x = Cont (\ret -> ret x)
  f <*> x = do g <- f; y <- x; pure (g y)

instance Monad (Cont r) where
  return a = Cont $ \k -> k a -- i.e. return a = \k -> k a
  (Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k) -- i.e. c >>= f = \k -> c (\a -> f a k)

-- ========================================================================
-- Now we want to make the following, more complicated continuation a Monad
-- Here the answer type is completely abstract.

newtype Collect tuple = Collect {runCollect :: forall ans. ans -> (tuple -> ans -> ans) -> ans}

instance Functor Collect where
  fmap f (Collect g) = Collect (\x c -> g x (\t a -> c (f t) a))

-- Playing type tetris find this term    ^----------------------^
-- given
-- f:: t -> s
-- g:: a -> (t -> a -> a) -> a
-- x:: a
-- c:: s -> a -> a

instance Applicative Collect where
  pure x = Collect (\ans f -> f x ans)
  f <*> x = do g <- f; y <- x; pure (g y)

instance Monad Collect where
  (Collect g) >>= f = Collect (\x c -> g x (\t a -> runCollect (f t) a c))

-- Playing type tetris find this term  ^--------------------------------^
-- given
-- g:: a -> (t -> a -> a) -> a
-- f:: t -> (Collect s)
-- x:: a
-- c:: (s -> a -> a)

instance Foldable Collect where
  foldr f z (Collect g) = g z f

-- ===========================================================================
-- Operations on the collect Monad.

-- | A (Collect t) is completely agnostic over how 't's are beging collected.
-- We can make this abstraction concrete by using fixAction.
fixAction :: Collect tuple -> ans -> (tuple -> ans -> ans) -> ans
fixAction = runCollect

mapify :: Ord a => Collect (a, b) -> Map.Map a b
mapify m = runCollect m Map.empty (\(a, b) ans -> Map.insert a b ans)

listify :: Collect (a, b) -> [(a, b)]
listify m = runCollect m [] (:)

count :: Collect (a, b) -> Int
count m = runCollect m 0 (\t n -> n + 1)

-- | Here are several ways to add a new t to what is being collected.

-- | The `one` and `none` interface are used when we want collections with 0 or 1 elements
one :: t -> Collect t
one t = Collect (\a f -> f t a)

none :: Collect t
none = Collect (\a f -> a)

-- | The `front` and `rear` interface can add to either end of the sequence (both in constant time)
front :: t -> Collect t -> Collect t
front t (Collect g) = Collect (\a f -> g (f t a) f)

rear :: Collect t -> t -> Collect t
rear (Collect g) t = Collect (\a f -> f t (g a f))

-- | Conditional collecting
when :: Bool -> Collect ()
when True = Collect (\ans f -> f () ans)
when False = Collect (\ans f -> ans)

takeC :: Int -> Collect t -> [t]
takeC n (Collect f) = fst (f ([], n) next)
  where
    next x (xs, 0) = (xs, 0)
    next x (xs, m) = (x : xs, m -1)

isempty :: Collect t -> Bool
isempty col = runCollect col True (\t a -> False)

nonempty :: Collect t -> Bool
nonempty col = runCollect col False (\t a -> True)

hasElem :: Collect t -> Maybe t
hasElem col = runCollect col Nothing (\t _ -> Just t)

-- | Even though a (Collect t) is a function, if we can (Show t), we can pick an action
-- that collects all the shown t, and turn them into a big multi-line string.
instance Show t => Show (Collect t) where
  show c2 = unlines (runCollect c2 [] (\t ans -> show t : ans))

-- =======================================================
-- Collection with mplus

newtype ColPlus tuple = ColPlus
  { runColPlus :: forall ans. ans -> (tuple -> ans -> ans) -> (ans -> ans -> ans) -> ans
  }

instance Functor ColPlus where
  fmap f (ColPlus g) = ColPlus (\x c m -> g x (\t a -> c (f t) a) m)

instance Applicative ColPlus where
  pure x = ColPlus (\ans f m -> f x ans)
  f <*> x = do g <- f; y <- x; pure (g y)

instance Monad ColPlus where
  (ColPlus g) >>= f = ColPlus (\x c m -> g x (\t a -> runColPlus (f t) a c m) m)

runPlus :: Monoid a => ColPlus t -> a -> (t -> a -> a) -> a
runPlus (ColPlus g) a f = g a f mappend

instance AP.Alternative ColPlus where
  empty = ColPlus (\a h m -> a)
  (<|>) (ColPlus f) (ColPlus g) = ColPlus (\a h m -> m (f a h m) (g a h m))

instance CM.MonadPlus ColPlus where
  mzero = ColPlus (\a h m -> a)
  mplus (ColPlus f) (ColPlus g) = ColPlus (\a h m -> m (f a h m) (g a h m))
