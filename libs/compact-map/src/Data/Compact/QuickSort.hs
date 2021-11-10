{-# LANGUAGE BangPatterns #-}

module Data.Compact.QuickSort where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive.SmallArray

quicksort :: Ord e => SmallArray e -> SmallArray e
quicksort arr =
  runST $ do
    marr <- thawSmallArray arr 0 (sizeofSmallArray arr)
    quicksortM_ marr
    unsafeFreezeSmallArray marr
{-# INLINE quicksort #-}

quicksortBy :: (e -> e -> Ordering) -> SmallArray e -> SmallArray e
quicksortBy f arr =
  runST $ do
    marr <- thawSmallArray arr 0 (sizeofSmallArray arr)
    quicksortByM_ f marr
    unsafeFreezeSmallArray marr
{-# INLINE quicksortBy #-}

quicksortM_ :: (Ord e, PrimMonad m) => SmallMutableArray (PrimState m) e -> m ()
quicksortM_ = quicksortInternalM_ (\e1 e2 -> pure $ e1 < e2) (\e1 e2 -> pure $ e1 == e2)
{-# INLINE quicksortM_ #-}

quicksortByM_ ::
  PrimMonad m =>
  (e -> e -> Ordering) ->
  SmallMutableArray (PrimState m) e ->
  m ()
quicksortByM_ comp =
  quicksortInternalM_ (\x y -> pure (LT == comp x y)) (\x y -> pure (EQ == comp x y))
{-# INLINE quicksortByM_ #-}

quicksortInternalM_ ::
  PrimMonad m =>
  (e -> e -> m Bool) ->
  (e -> e -> m Bool) ->
  SmallMutableArray (PrimState m) e ->
  m ()
quicksortInternalM_ fLT fEQ marr = qsort 0 (sizeofSmallMutableArray marr - 1)
  where
    ltSwap i j = do
      ei <- readSmallArray marr i
      ej <- readSmallArray marr j
      lt <- fLT ei ej
      if lt
        then do
          writeSmallArray marr i ej
          writeSmallArray marr j ei
          pure ei
        else pure ej
    {-# INLINE ltSwap #-}
    getPivot lo hi = do
      let !mid = (hi + lo) `div` 2
      _ <- ltSwap mid lo
      _ <- ltSwap hi lo
      ltSwap mid hi
    {-# INLINE getPivot #-}
    qsort !lo !hi =
      when (lo < hi) $ do
        p <- getPivot lo hi
        l <- unsafeUnstablePartitionRegionM marr (`fLT` p) lo (hi - 1)
        h <- unsafeUnstablePartitionRegionM marr (`fEQ` p) l hi
        qsort lo (l - 1)
        qsort h hi
{-# INLINE quicksortInternalM_ #-}

-- | Partition a segment of a vector. Starting and ending indices are unchecked.
--
-- @since 1.0.0
unsafeUnstablePartitionRegionM ::
  PrimMonad m =>
  SmallMutableArray (PrimState m) e ->
  (e -> m Bool) ->
  -- | Start index of the region
  Int ->
  -- | End index of the region
  Int ->
  m Int
unsafeUnstablePartitionRegionM marr f start end = fromLeft start (end + 1)
  where
    fromLeft i j
      | i == j = pure i
      | otherwise = do
        e <- f =<< readSmallArray marr i
        if e
          then fromLeft (i + 1) j
          else fromRight i (j - 1)
    fromRight i j
      | i == j = pure i
      | otherwise = do
        x <- readSmallArray marr j
        e <- f x
        if e
          then do
            writeSmallArray marr j =<< readSmallArray marr i
            writeSmallArray marr i x
            fromLeft (i + 1) j
          else fromRight i (j - 1)
{-# INLINE unsafeUnstablePartitionRegionM #-}
