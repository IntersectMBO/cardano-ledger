{-# LANGUAGE RankNTypes #-}

module Data.Compact.SmallArray where

import Control.Monad.ST (ST, runST)
import qualified Data.Foldable as Fold (toList)
import qualified Data.Primitive.SmallArray as Small

-- import Debug.Trace

-- ====================================

type PArray = Small.SmallArray

type MArray = Small.SmallMutableArray

index :: PArray a -> Int -> a
isize :: PArray a -> Int
fromlist :: [a] -> PArray a
tolist :: PArray a -> [a]
index = boundsCheck Small.indexSmallArray

isize = Small.sizeofSmallArray

fromlist = Small.smallArrayFromList

tolist = Fold.toList

-- catenate = catArray
-- merge = mergeArray

mindex :: MArray s a -> Int -> ST s a
msize :: MArray s a -> Int
mnew :: Int -> ST s (MArray s a)
mnewInit :: Int -> a -> ST s (MArray s a)
mfreeze :: MArray s a -> ST s (PArray a) -- This should be the unsafe version that does not copy
mwrite :: MArray s a -> Int -> a -> ST s ()
mcopy :: forall s. (forall a. MArray s a -> Int -> PArray a -> Int -> Int -> ST s ())
mindex = mboundsCheck Small.readSmallArray

msize = Small.sizeofSmallMutableArray

mnew size = Small.newSmallArray size (error "uninitialized index, allocated by 'mnew', is referenced")

mnewInit size = Small.newSmallArray size

mfreeze = Small.unsafeFreezeSmallArray

mwrite arr i a =
  if i >= 0 && i < msize arr
    then Small.writeSmallArray arr i a
    else error $ boundsMessage "mwrite" i (msize arr - 1)

mcopy = Small.copySmallArray

mboundsCheck :: (MArray s a -> Int -> p) -> MArray s a -> Int -> p
mboundsCheck indexf arr i | i >= 0 && i < msize arr = indexf arr i
mboundsCheck _ arr i = error $ boundsMessage "mboundscheck" i (msize arr - 1)

boundsCheck :: (PArray a -> Int -> p) -> PArray a -> Int -> p
boundsCheck indexf arr i | i >= 0 && i < isize arr = indexf arr i
boundsCheck _ arr i = error $ boundsMessage "boundscheck" i (isize arr - 1)

withMutArray :: Int -> (forall s. MArray s a -> ST s x) -> (PArray a, x)
withMutArray n process = runST $ do
  marr <- mnew n
  x <- process marr
  arr <- mfreeze marr
  pure (arr, x)

withMutArray_ :: Int -> (forall s. MArray s a -> ST s x) -> PArray a
withMutArray_ n process = fst $ withMutArray n process

boundsMessage :: String -> Int -> Int -> String
boundsMessage funcName i n =
  concat
    [ "Index out of bounds in '",
      funcName,
      "' ",
      show i,
      " not in range (0,",
      show n,
      ")"
    ]
{-# NOINLINE boundsMessage #-}
