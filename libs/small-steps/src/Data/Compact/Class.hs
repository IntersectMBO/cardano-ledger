{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import qualified Data.Primitive.SmallArray as Small
import Data.Primitive.SmallArray(SmallArray,SmallMutableArray)

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
  catenate :: Int -> [t a] -> t a
  merge :: Ord a => Int -> [t a] -> t a

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

boundsCheck :: Indexable t1 a => (t1 a -> Int -> t2) -> t1 a -> Int -> t2
boundsCheck indexf arr i | i>=0 && i < isize arr = indexf arr i
boundsCheck _ arr i = error ("boundscheck error, "++show i++", not in bounds (0.."++show (isize arr -1)++").")

-- Built in type Instances

instance Indexable PA.Array x where
  index = boundsCheck PA.indexArray
  isize = PA.sizeofArray
  fromlist = PA.arrayFromList
  tolist arr = foldr (:) [] arr
  catenate = catArray
  merge = mergeArray
  
instance Prim a => Indexable PrimArray a where
  index = boundsCheck indexPrimArray
  isize = sizeofPrimArray
  fromlist = primArrayFromList
  tolist = primArrayToList
  catenate = catArray
  merge = mergeArray  

instance Indexable (A.Array Int) a where
  index = (A.!)
  isize arr = (hi - lo) + 1 where (lo, hi) = A.bounds arr
  fromlist xs = (A.listArray (0, length xs -1) xs)
  tolist arr = foldr (:) [] arr
  catenate = catArray
  merge = mergeArray

instance Indexable SmallArray t where
  index = Small.indexSmallArray
  isize = Small.sizeofSmallArray 
  fromlist = Small.smallArrayFromList
  tolist arr = foldr (:) [] arr
  catenate = catArray
  merge = mergeArray

-- ========================================================================
-- Pairs of Mutable Arrays and ImMutable Arrays that can be converted safely
-- ========================================================================


mboundsCheck :: (ArrayPair arr marr a) =>
                (marr s a -> Int -> ST s a) -> marr s a -> Int -> ST s a
mboundsCheck indexf arr i | i>=0 && i < msize arr = indexf arr i
mboundsCheck _ arr i = error ("mboundscheck error, "++show i++", not in bounds (0.."++show (msize arr -1)++").")


class Indexable arr a => ArrayPair arr marr a | marr -> arr, arr -> marr where
  mindex :: marr s a -> Int -> ST s a
  msize :: marr s a -> Int
  mnew :: Int -> ST s (marr s a)
  mfreeze :: marr s a -> ST s (arr a) -- This should be the unsafe version that does not copy
  mwrite :: marr s a -> Int -> a -> ST s ()
  mcopy :: forall s. marr s a -> Int -> arr a -> Int -> Int -> ST s ()

-- Built in type instances

instance ArrayPair SmallArray SmallMutableArray a where
  mindex = mboundsCheck Small.readSmallArray
  msize = Small.sizeofSmallMutableArray
  mnew size = Small.newSmallArray size undefined
  mfreeze = Small.unsafeFreezeSmallArray 
  mwrite arr i a = if i>=0 && i<(msize arr)
       then Small.writeSmallArray arr i a
       else error ("mwrite error, "++show i++", not in bounds (0.."++show (msize arr -1)++").")
  mcopy = Small.copySmallArray

instance ArrayPair PA.Array PA.MutableArray a where
  msize = PA.sizeofMutableArray 
  mindex = mboundsCheck PA.readArray
  mnew n = PA.newArray n undefined
  mfreeze = PA.unsafeFreezeArray
  mwrite arr i a =
    if i>=0 && i<(msize arr)
       then  PA.writeArray arr i a
       else error ("mwrite error, "++show i++", not in bounds (0.."++show (msize arr -1)++").")
  mcopy = PA.copyArray

instance Prim a => ArrayPair PrimArray MutablePrimArray a where
  msize = sizeofMutablePrimArray
  mindex = mboundsCheck readPrimArray
  mnew = newPrimArray  
  mfreeze = unsafeFreezePrimArray
  mwrite arr i a =
    if i>=0 && i<(msize arr)
       then writePrimArray arr i a
       else error ("mwrite error, "++show i++", not in bounds (0.."++show (msize arr -1)++").")
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


-- =======================================================
-- Usefull functions that use Mutable Arrays

-- | Build a mutable array from a list
mfromlist :: ArrayPair arr marr a => [a] -> ST s (marr s a)
mfromlist xs = do
  marr <- mnew (length xs) 
  let loop _i [] = pure ()
      loop i (y:ys) = mwrite marr i y >> loop (i+1) ys
  loop 0 xs
  pure marr

-- | concatenate a list of array like objects by allocating the target and then copying them 1 by 1.
--   catArray maintains index order, but mergeArray maintains ascending oder.
--   catArray   [[2,1],[14],[6,5,11]]  --> [2,1,14,6,5,11]
--   mergeArray [[1,2],[14],[5,6,11]]  --> [1,2,5,6,11,14]
catArray :: ArrayPair arr marr a => Int -> [arr a] -> arr a
catArray totalsize xs = fst(withMutArray totalsize (build 0 xs))
  where build _next [] _marr = pure ()
        build next (arr: arrs) marr =
           do let size = isize arr
              mcopy marr next arr 0 size
              build (next+size) arrs marr

-- | Swap the values at indices 'i' and 'j' in mutable array 'marr'
swap :: ArrayPair arr marr a => marr s a -> Int -> Int -> ST s ()
swap _ i j | i==j = pure ()
swap marr i j = do
  ti <- mindex marr i;
  tj <- mindex marr j;
  mwrite marr i tj
  mwrite marr j ti

mToList :: ArrayPair arr marr a => Int -> marr s a -> ST s [a]
mToList first marr = loop first []
  where hi = (msize marr - 1)
        loop lo xs | lo > hi = pure(reverse xs)
        loop lo xs = do {x <- mindex marr lo; loop (lo+1) (x:xs)}


-- | Extract a slice from an array
slice :: ArrayPair arr2 marr a => Int -> Int -> arr2 a -> arr2 a
slice 0 hi arr | hi == (isize arr -1) = arr
slice lo hi arr = fst(withMutArray size action)
  where size = max (hi - lo + 1) 0
        action marr = mcopy marr 0 arr lo size
{-# INLINE slice #-}

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

instance (Search t key) => Search [t] key where
   search _ [] = Nothing
   search key (x:xs) = 
     case search key x of
       Nothing -> search key xs
       Just i -> Just i

instance Search t key => Search (Node t) key where
   search key (Node _ x) = search key x

-- ==============================================================
-- Overloaded operations on (Map k v)

class Maplike m k v where
  makemap :: [(k,v)] -> m k v
  lookupmap :: Ord k => k -> m k v -> Maybe v
  insertmap :: Ord k => k -> v -> m k v -> m k v

-- ==============================================================
-- Overloaded operations on (Set k)

class Setlike m k where
  makeset :: [k] -> m k 
  elemset :: Ord k => k -> m k -> Bool
  insertset :: Ord k => k -> m k -> m k
  emptyset :: m k

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
binary :: Integral n => n -> [n]
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
--   (16, [node 1,node 1,node 2, node 4, node 8], [node 32,node 128])
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


-- ==============================================================================

-- | A node carries a 'size' and some array-like type 'arr'
data Node arr = Node {-# UNPACK #-} !Int arr
  deriving Show

arrayPart :: Node arr -> arr
arrayPart (Node _ arr) = arr

nodesize :: Node arr -> Int
nodesize (Node i _) = i

-- ==================================================================
-- Merging arrays to maintain ascending order.

-- | Find the index in 'marr' of the smallest 't'. Return a pair (index,the-smallest-t)
--   The function 'smaller' compares two 't' for smallness.
--   'pair' is the smallest (index,t) we have seen so far. 'lo' and 'hi' limit
--   the bounds of where to look.
smallestIndex ::(ArrayPair arr marr t) => (t -> t -> Bool) ->  marr s t -> (Int,t) ->Int -> Int -> ST s (Int,t)
smallestIndex smaller marr initpair initlo hi = loop initpair initlo where
   loop pair lo | lo > hi = pure pair
   loop (pair@(_i,t)) lo = do
     t2 <- mindex marr lo
     if smaller t t2
         then loop pair (lo+1) 
         else loop (lo,t2) (lo+1)

-- | Apply 'action' to each 't' in 'marr' in ascending order as determined by 'smaller'
--  'state' is the current state, and 'lo' and 'hi' limit the bounds of where to look.
--  'markIfDone' might alter 'marr' and return a new 'lo' limit, if the 'lo' index in
--  'marr' has no more 't' objects to offer.
inOrder :: 
   (Int -> Int -> t -> PA.MutableArray s t -> ST s Int) ->
   (t -> t -> Bool) ->
   state ->
   Int ->
   Int ->
   (state -> t -> ST s state) ->
   PA.MutableArray s t ->  -- array of items to be merged, This should be small. At most 20 or so.
   ST s state
inOrder markIfDone smaller initstate initlo hi action marr = loop initlo initstate where
   loop lo  state | lo > hi = pure state
   loop lo state = 
      do t <-  mindex marr lo
         (i,small) <- smallestIndex smaller marr (lo,t) (lo+1) hi
         state' <- action state small
         lo' <- markIfDone lo i small marr
         loop lo' state'

-- | A commonly used 'markIfDone' function. Test if 'next' is still in bounds for 'arr'
--   If so, them mutate 'marr' to indicate that next time we should look at index 'next+1' in arr.
--   If it is out of bounds, then swap the pairs in 'marr' at indexs 'i' and 'lo', and then
--   increment lo, so the pair that has no more to offer, is no longer in an active position.
mark1:: (Indexable arr t) =>
        Int -> Int -> (Int,arr t) -> PA.MutableArray s (Int,arr t) -> ST s Int
mark1 lo i (next,arr) marr =
  do let next' = next+1
     if next' < isize arr
        then mwrite marr i (next',arr) >> pure lo
        else swap marr lo i >> pure(lo+1)

-- | A commonly used 'smaller' function
smaller1 :: (Ord a, Indexable arr a) => (Int, arr a) -> (Int, arr a) -> Bool
smaller1 (i,arr1) (j,arr2) = index arr1 i < index arr2 j

-- | A commonly used 'action' function. Appropriate when the 'arr' is simple with
--   no bells or whistles. Good for PrimArray, PA.Array, A.Array, Any array with a ArrayPair instance.
--   If we use an exotic array with no ArrayPair instance, we can stil merge, but we can't use this
--   action function.
action1:: (ArrayPair arr marr a, Indexable t a) => marr s a -> Int -> (Int, t a) -> ST s Int
action1 marr i (j,arr) = (mwrite marr i (index arr j) >> pure(i+1))


-- | Merge a list of array-like objects using 'action' The 'action' will differ depending on
--   what kind of arrays are begin merged.
mergeWithAction :: forall a arr marr.
  (ArrayPair arr marr a, Ord a) =>
  Int ->
  [arr a] ->
  (forall s. marr s a -> Int -> (Int,arr a) -> ST s Int) ->
  arr a
mergeWithAction size inputs action = fst $ withMutArray size build where
  build:: forall s. marr s a -> ST s Int
  build moutput = do
     minputs <- mfromlist (map (\ x -> (0,x)) inputs)
     inOrder mark1 smaller1 (0::Int) 0 (length inputs-1) (action moutput) minputs

-- | Merge a list of array like objects by allocating the target and then merging the sources.
--   mergeArray maintains ascending order. But catArray maintains index order.
--   mergeArray [[1,2],[14],[5,6,11]]  --> [1,2,5,6,11,14]
--   catArray [[2,1],[14],[6,5,11]]  --> [2,1,14,6,5,11]

mergeArray :: (Ord a,ArrayPair arr marr a) => Int -> [arr a] -> arr a
mergeArray size xs = mergeWithAction size xs action1

testmerge :: PrimArray Int
testmerge = mergeArray (sum(map isize xs)) xs
  where xs = [fromlist[2,7], fromlist[1,6,19], fromlist[4,9], fromlist[3,8,12,17]]

{-
-- | Merge 2 parallel arrays with 'action'. The order of merging depends only on
--   The first list 'keys' , the second is implicit in the the state (Int,'vals').
merge2WithAction :: forall a arr marr arr2 marr2 v.
  ( ArrayPair arr marr a,
    Indexable arr2 v,
    ArrayPair arr2 marr2 v,
    Ord a
  ) =>
  Int ->
  [arr a] ->
  (forall s. marr s a -> marr2 s v -> Int -> (Int,arr a) -> ST s Int) ->
  MapNode arr arr2 a v
merge2WithAction size keys action = node (with2MutArray size size build) where
  node (arr1,arr2,_) = MapNode size arr1 arr2
  build:: forall s. marr s a -> marr2 s v -> ST s Int
  build mkeys mvals = do
     minputs <- mfromlist (map (\ x -> (0,x)) keys)
     inOrder mark1 smaller1 (0::Int) 0 (length keys - 1) (action mkeys mvals) minputs

mergeMapNode :: forall karr varr marr marr2 k v.
 ( ArrayPair karr marr k,
   ArrayPair varr marr2 v,
   Ord k
  ) => Int -> [MapNode karr varr k v] -> MapNode karr varr k v
mergeMapNode size nodes = mergeArray2 size inputs action where
   (inputs,vals) = unzip (map (\ (MapNode _ ks vs) -> (ks,vs)) nodes)
   action:: forall s. marr s k -> marr2 s v -> Int -> (Int,karr k) -> ST s Int
   action mkeys mvals i (n,arrkeys) = undefined

-}


