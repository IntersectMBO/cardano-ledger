{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- HeapWords for Array and PrimArray
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Compact.Class where

import qualified Data.Array as A
import qualified Data.Primitive.Array as PA
import qualified Data.Array.MArray as MutA
import Data.Primitive.PrimArray
 ( PrimArray, indexPrimArray, primArrayFromList, primArrayToList, sizeofPrimArray, copyPrimArray,
   MutablePrimArray,unsafeFreezePrimArray , newPrimArray,sizeofMutablePrimArray, readPrimArray, writePrimArray,
 )
import Data.Primitive.Types (Prim (..))
import GHC.Arr(STArray(..),unsafeFreezeSTArray)
import Control.Monad.ST (ST, runST)
import Cardano.Prelude (HeapWords (..))

-- ============================================================================================
-- Array like objects which can access elements by their index

class Indexable t a where
  index :: t a -> Int -> a
  isize :: t a -> Int
  fromlist :: [a] -> t a
  tolist :: t a -> [a]

-- Array like objects that store their elements in ascending order dupport Binary search

-- | Find the index of 'k'. Use 'lo' and 'hi' to narrow the scope where 'k' may occur
--   This is possible because we assume 'arr' is maintained in ascending order of keys.
binsearch :: (Ord k, Indexable arr k) => Int -> Int -> k -> arr k -> Maybe Int
binsearch lo hi _k _v | lo > hi = Nothing
binsearch lo hi k v | lo == hi = if index v lo == k then Just lo else Nothing
binsearch lo _hi k v | index v lo == k = Just lo
binsearch _lo hi k v | index v hi == k = Just hi
binsearch lo hi _k _v | lo + 1 == hi = Nothing
binsearch lo hi k v = (if index v mid > k then binsearch lo mid k v else binsearch mid hi k v)
  where
    mid = lo + (div (hi - lo) 2)


-- | Find the index and the value at the least upper bound of 'target'
alub :: (Ord t1, Indexable t2 t1) => (Int, Int) -> t2 t1 -> t1 -> Maybe (Int, t1)
alub (lo, hi) arr target
  | lo > hi = Nothing
  | target <= index arr lo = Just (lo, index arr lo)
  | lo == hi = Nothing
  | lo + 1 == hi && index arr lo < target && target <= index arr hi = Just (hi, index arr hi)
  | True = if target <= index arr mid then (alub (lo, mid) arr target) else (alub (mid, hi) arr target)
  where
    mid = lo + (div (hi - lo) 2)

-- Built in type Instances

instance Indexable PA.Array x where
  index = PA.indexArray
  isize = PA.sizeofArray
  fromlist = PA.arrayFromList
  tolist arr = foldr (:) [] arr
 
instance Prim a => Indexable PrimArray a where
  index = indexPrimArray
  isize = sizeofPrimArray
  fromlist = primArrayFromList
  tolist = primArrayToList

instance Indexable (A.Array Int) a where
  index = (A.!)
  isize arr = (hi - lo) + 1 where (lo, hi) = A.bounds arr
  fromlist xs = (A.listArray (0, length xs -1) xs)
  tolist arr = foldr (:) [] arr

-- ========================================================================
-- Pairs of Mutable Arrays and ImMutable Arrays that can be converted
-- ========================================================================

class Indexable arr a => ArrayPair arr marr a | marr -> arr, arr -> marr where
  mindex :: marr s a -> Int -> ST s a
  msize :: marr s a -> Int
  mnew :: Int -> ST s (marr s a)
  mfreeze :: marr s a -> ST s (arr a) -- This should be the unsafe version that does not copy
  mwrite :: marr s a -> Int -> a -> ST s ()
  mcopy :: forall s. marr s a -> Int -> arr a -> Int -> Int -> ST s ()

-- Built in type instances

instance ArrayPair PA.Array PA.MutableArray a where
  msize = PA.sizeofMutableArray 
  mindex = PA.readArray
  mnew n = PA.newArray n undefined
  mfreeze = PA.unsafeFreezeArray
  mwrite = PA.writeArray
  mcopy = PA.copyArray

instance Prim a => ArrayPair PrimArray MutablePrimArray a where
  msize = sizeofMutablePrimArray
  mindex = readPrimArray
  mnew = newPrimArray  
  mfreeze = unsafeFreezePrimArray
  mwrite = writePrimArray
  mcopy = copyPrimArray

-- | MutArray fixes the index type to Int for the STArray type constructor
newtype MutArray s t = MutArray (STArray s Int t)

instance ArrayPair (A.Array Int) MutArray a where
  msize (MutArray (STArray lo hi _ _)) = hi - lo + 1 
  mindex (MutArray arr) i = MutA.readArray arr i
  mnew n = MutArray <$> (MutA.newArray_ (0,n-1))
  mfreeze (MutArray arr) = unsafeFreezeSTArray arr
  mwrite (MutArray arr) i a = MutA.writeArray arr i a
  mcopy marr startm arr start count = go startm start count
    where go _i _j 0 = pure ()
          go i j n = do
             mwrite marr i (index arr j)
             go (i+1) (j+1) (n-1)

-- | Build a mutable array from a list
mfromlist :: ArrayPair arr marr a => [a] -> ST s (marr s a)
mfromlist xs = do
  marr <- mnew (length xs) 
  let loop _i [] = pure ()
      loop i (y:ys) = mwrite marr i y >> loop (i+1) ys
  loop 0 xs
  pure marr

-- | concatenate a list of array like objects by alocating and then copying them 1 by 1.
catArray :: ArrayPair arr marr a => [arr a] -> arr a
catArray xs = fst(withMutArray total (build 0 xs))
  where total = sum(map isize xs)
        build _next [] _marr = pure ()
        build next (arr: arrs) marr =
           do let size = isize arr
              mcopy marr next arr 0 size
              build (next+size) arrs marr

-- ================================================================
-- Functions for using mutable initialization in a safe manner.
-- Using these functions is the safe way to use the method 'mfreeze'

withMutArray:: ArrayPair arr marr a => Int -> (forall s. marr s a -> ST s x) -> (arr a,x)
withMutArray n process = runST $ do
  marr <- mnew n
  x <- process marr
  arr <- mfreeze marr
  pure (arr, x)

with2MutArray ::
  ( ArrayPair arr1 marr1 a, ArrayPair arr2 marr2 b) =>
  Int ->
  Int ->
  (forall s. marr1 s a -> marr2 s b -> ST s x) ->
  (arr1 a, arr2 b,x)
with2MutArray size1 size2 process = runST $ do
  arr1 <- mnew size1
  arr2 <- mnew size2
  x <- process arr1 arr2
  arr3 <- mfreeze arr1
  arr4 <- mfreeze arr2
  pure (arr3, arr4, x)

-- =======================================================
-- Abtract Searchable types (Arrays stored in ascending order)
-- These will be very usefull when we create maps as parallel arrays
-- the first sorted on key, and the second holdingthe associated value at the
-- same index as it's key.

class Ord key => Search t key where
  search :: key -> t -> Maybe Int

instance Ord key => Search (PA.Array key) key
   where search key v = binsearch 0 (isize v - 1) key v

instance (Prim key,Ord key) => Search (PrimArray key) key
   where search key v = binsearch 0 (isize v - 1) key v

instance Ord key => Search (A.Array Int key) key
   where search key v = binsearch 0 (isize v - 1) key v

-- ==============================================================
-- Overloaded operations on (Map k v)

class Maplike m k v where
  makemap :: [(k,v)] -> m k v
  lookupmap :: Ord k => k -> m k v -> Maybe v
  insertmap :: Ord k => k -> v -> m k v -> m k v

-- =========================================================
-- HeapWords instances

instance (HeapWords v) => HeapWords (A.Array Int v) where
  heapWords arr = foldl accum (3 + n) arr
    where
      accum ans v = ans + heapWords v
      n = isize arr

instance (Prim a, HeapWords a) => HeapWords (PrimArray a) where
  heapWords arr = 2 + (sizeofPrimArray arr * heapWords (index arr 0))

-- =======================================================
-- Encoding lists with the structure of binary numbers

-- | binary encoding of 'n', least significant bit on the front of the list
binary :: Int -> [Int]
binary 0 = []
binary 1 = [(1)]
binary n = (mod n 2) : binary (div n 2)

-- | Compute a sparse list of non-zero Binary digits and their positional weights to represent 'n'
--   For example (sparseBinary 25) returns [(1,1),(1,8),(1,16)], I.e. we need: 1 one,
--   1 eight, and 1 sixteen.  Since this is binary, and we don't store the 0's, the digits are aways 1.
--   and the weights are powers of 2.
sparseBinary :: Int -> [(Int, Int)]
sparseBinary n = fix 1 (binary n)
  where
    fix _ [] = []
    fix m (x : xs) =
      if x == 0
        then fix (m * 2) xs
        else (x, m) : fix (m * 2) xs

-- | Split a list of length 'n' into pieces, each piece has a power of two as its length.
-- For example:  pieces [1..11]  -->  [(1,[1]), (2,[2,3]), (8,[4,5,6,7,8,9,10,11])]
pieces :: [a] -> [(Int, [a])]
pieces xs = chop parts xs
  where
    parts = sparseBinary (length xs)
    chop [] _zs = []
    chop ((_, n) : ys) zs = (n, take n zs) : chop ys (drop n zs)


-- | When a list is represented with the structure of binary numbers, an important
--   property is that every such list has a full prefix. This is a prefix which has
--   contiguous powers of two. For example:
--   splitAtFullPrefix 1 (node 1) [node 1,node 2, node 4, node 8, node 32, node 128]
--      returns
--   (16, [node 1,node 2, node 4, node 8], [node 32,node 128])
--   because [1,2,4,8] is the longest contiguous prefix consisting of adjacent powers of 2.
--   In the worst case the prefix has length 1.
splitAtFullPrefix :: (node -> Int) -> Int -> node -> [node] -> (Int,[node],[node])
splitAtFullPrefix getsize _next node [] = (getsize node,[node],[])
splitAtFullPrefix getsize next node1 (node2:more) =
   let n = getsize node1
       m = getsize node2
   in if next==m
         then case splitAtFullPrefix getsize (next*2) node2 more of
                (count,prefix,rest) -> (count+n, node1:prefix, rest)
         else (n,[node1],node2:more)

