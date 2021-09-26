{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

-- HeapWords for Array and PrimArray
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Compact where

import qualified Data.Array as A
import qualified Data.Primitive.Array as PA
import qualified Data.Array.MArray as MutA
import GHC.Arr(STArray(..),unsafeFreezeSTArray)
import Data.Primitive.PrimArray
 ( PrimArray, indexPrimArray, primArrayFromList, primArrayToList, sizeofPrimArray,
   MutablePrimArray,unsafeFreezePrimArray , newPrimArray,sizeofMutablePrimArray, readPrimArray, writePrimArray,
 )
import Data.ByteString (ByteString)
import Data.Foldable (foldr')
import Data.Primitive.Types (Prim (..))
import Cardano.Prelude (HeapWords (..))
import GHC.Exts( IsList (toList) )
import Control.Monad.ST (ST, runST)
import Data.List(sort)

-- ============================================================================================
-- Arrays that support Binary search

class Indexable t a where
  index :: t a -> Int -> a
  isize :: t a -> Int
  fromlist :: [a] -> t a
  tolist :: t a -> [a]

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


-- | Find the index of 'target'
search :: (Ord k, Indexable arr k) => k -> arr k -> Maybe Int
search key v = binsearch 0 (isize v - 1) key v

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

-- Instances

instance Indexable PA.Array x where
  index = PA.indexArray
  isize = PA.sizeofArray
  fromlist = PA.arrayFromList
  tolist arr = foldr (:) [] arr

instance Prim a => Indexable PrimArray a where
  index = indexPrimArray
  isize = sizeofPrimArray
  fromlist = primArrayFromList
  tolist = toList

instance Indexable (A.Array Int) a where
  index = (A.!)
  isize arr = (hi - lo) + 1 where (lo, hi) = A.bounds arr
  fromlist xs = (A.listArray (0, length xs -1) xs)
  tolist arr = foldr (:) [] arr

-- ========================================================
-- Pairs of Mutable Arrays and ImMutable Arrays that can be converted

class Indexable arr a => ArrayPair arr marr a | marr -> arr, arr -> marr where
  mindex :: marr s a -> Int -> ST s a
  msize :: marr s a -> Int
  mnew :: Int -> ST s (marr s a)
  mfreeze :: marr s a -> ST s (arr a) -- This should be the unsafe version that does not copy
  mwrite :: marr s a -> Int -> a -> ST s ()

instance ArrayPair PA.Array PA.MutableArray a where
  msize = PA.sizeofMutableArray 
  mindex = PA.readArray
  mnew n = PA.newArray n undefined
  mfreeze = PA.unsafeFreezeArray
  mwrite = PA.writeArray

instance Prim a => ArrayPair PrimArray MutablePrimArray a where
  msize = sizeofMutablePrimArray
  mindex = readPrimArray
  mnew = newPrimArray  
  mfreeze = unsafeFreezePrimArray
  mwrite = writePrimArray

newtype MutArray s t = MutArray (STArray s Int t)

instance ArrayPair (A.Array Int) MutArray a where
  msize (MutArray (STArray lo hi _ _)) = hi - lo + 1 
  mindex (MutArray arr) i = MutA.readArray arr i
  mnew n = MutArray <$> (MutA.newArray_ (0,n-1))
  mfreeze (MutArray arr) = unsafeFreezeSTArray arr
  mwrite (MutArray arr) i a = MutA.writeArray arr i a

-- ================================================================

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

-- ========================================================
-- efficient merge of sorted arrays

-- Mergeing two arrays using an abstract 'cont' to add things
mergeArrWithCont ::
   forall t arr ans. (Ord t,Indexable arr t) =>
      Int ->
      Int ->
      arr t ->
      arr t -> ans -> (ans -> t -> Either Int Int -> ans) -> ans
mergeArrWithCont size1 size2 arr1 arr2 !ans cont = loop 0 0 0 ans
  where
    totsize = size1 + size2
    loop :: Int -> Int -> Int -> ans -> ans
    loop i1 i2 next answer =
      case (i1 < size1, i2 < size2, next < totsize) of
        (True, True, True) ->
          let x1 = index arr1 i1
              x2 = index arr2 i2
          in case compare x1 x2 of
               LT -> loop (i1 + 1) i2 (next + 1) (cont answer x1 (Left i1))
               GT -> loop i1 (i2 + 1) (next + 1) (cont answer x2 (Right i2))
               EQ -> error ("Duplicates in mergeArrWithCont.")
        (True, False, True) -> loop (i1 + 1) i2 (next + 1) (cont answer (index arr1 i1) (Left i1))
        (False, True, True) -> loop i1 (i2 + 1) (next + 1) (cont answer (index arr2 i2) (Right i2))
        _ -> answer

mergeParallel :: (ArrayPair vals mvals v, ArrayPair keys mkeys key, Ord key,Indexable arr key) =>
     Int ->
     Int -> 
     arr key ->
     arr key ->
     vals v ->
     vals v ->
     (keys key, vals v, ())
mergeParallel size1 size2 keys1 keys2 vals1 vals2 =
   with2MutArray (size1+size2) (size1+size2) $
   (\ m1 m2 -> const () <$> mergeArrWithCont size1 size2 keys1 keys2 (pure 0) (action vals1 vals2 m1 m2))

getEither:: (Indexable t1 p) => t1 p -> t1 p -> Either Int Int -> p
getEither v1 v2 (Left i)  = index v1 i
getEither v1 v2 (Right i) = index v2 i

action :: (ArrayPair keys mkeys key, ArrayPair vals mvals v) =>
  vals v ->
  vals v ->
  mkeys s key ->
  mvals s v ->
  ST s Int ->
  key ->
  Either Int Int ->
  ST s Int    
action v1 v2 mkeys mvals indexM k eitheri = do
   !index <- indexM
   mwrite mkeys index k
   mwrite mvals index (getEither v1 v2 eitheri)
   pure(index + 1)

-- ================================================
-- Merging N-sorted arrays

data Node t = Node Int t

mergeMany:: (ArrayPair arr marr k, Ord k) => [(Int, Node (arr k))] -> marr s k -> ST s Int
mergeMany xs arr = inOrder smaller done action xs (pure (0::Int)) where
  smaller (i,Node _ xs) (j,Node _ ys) = index xs i < index ys j
  done (i,node@(Node size _)) = if i+1 < size then Just(i+1,node) else Nothing
  action n (i,Node _ xs) = do { count <- n; mwrite arr count (index xs i); pure(count+1)}

smallest :: (a -> a -> Bool) -> a -> [a] -> [a] -> (a, [a])
smallest _ x [] larger = (x,larger)
smallest smaller x (y:ys) larger =
   if smaller x y
      then smallest smaller x ys (y:larger)
      else smallest smaller y ys (x:larger)

inOrder :: 
   (t -> t -> Bool) ->
   (t -> Maybe t) ->
   (ans -> t -> ans) ->
   [t] -> ans -> ans  
inOrder smaller done action items ans = loop items ans where
   loop [] ans = ans
   loop (x:xs) ans = 
     case smallest smaller x xs [] of
       (small,larger) -> case done small of
          Nothing ->  loop larger (action ans small)
          Just more -> loop (more:larger) (action ans small)

rrr :: [Int]
rrr = inOrder smaller done action [[1,4,7],[3],[11,34,78,99,145],[2,6,8,9]] []
  where smaller (x:xs) (y:ys) = x < y
        done (x:y:zs) = Just(y:zs)
        done _ = Nothing
        action ans (x:_) = x:ans


mergeNodes :: (ArrayPair arr marr key, Ord key) => [Node (arr key)] -> (arr key, Int)
mergeNodes ns = withMutArray (sum (map getsize ns)) (mergeMany (map start ns))
  where start x = (0,x)
        getsize (Node n _) = n

-- ============================================================

newtype CompactPrimSet t = CompactPrimSet [Node (PrimArray t)]


hasElem :: (Prim t,Ord t) => t -> CompactPrimSet t -> Bool
hasElem _t (CompactPrimSet []) = False
hasElem t (CompactPrimSet ((Node siz arr) : more)) =
  case binsearch 0 (siz -1) t arr of
    Nothing -> hasElem t (CompactPrimSet more)
    Just _ -> True

insertElem :: (Prim t, Ord t) => t -> CompactPrimSet t -> CompactPrimSet t
insertElem t (set@(CompactPrimSet nodes)) =
  if hasElem t set
    then set
    else CompactPrimSet (addSetNode (Node 1 (fromlist [t])) nodes)


addSetNode :: (Ord t, Prim t) => Node (PrimArray t) -> [Node (PrimArray t)] -> [Node (PrimArray t)]
addSetNode node [] = [node]
addSetNode (node@(Node n arr1)) (nodes@((Node m arr2) : more))
  | n < m = node : nodes
  | n == m = addSetNode (mergeSetNodes n m arr1 arr2) more
  | True = error ("SetNodes not in ascending Order.")

mergeSetNodes :: forall t. (Ord t, Prim t) => Int -> Int -> PrimArray t -> PrimArray t -> Node (PrimArray t)
mergeSetNodes size1 size2 arr1 arr2 = Node totsize (fst (withMutArray totsize make))
  where
    totsize = size1 + size2
    make :: forall s. MutablePrimArray s t -> ST s ()
    make marr = loop 0 0 0
      where
        loop :: Int -> Int -> Int -> ST s ()
        loop i1 i2 next =
          case (i1 < size1, i2 < size2, next < totsize) of
            (True, True, True) ->
              let x1 = index arr1 i1
                  x2 = index arr2 i2
               in case compare x1 x2 of
                    LT -> do writePrimArray marr next x1; loop (i1 + 1) i2 (next + 1)
                    GT -> do writePrimArray marr next x2; loop i1 (i2 + 1) (next + 1)
                    EQ -> error ("Duplicates in mergeSetNodes.")
            (True, False, True) -> do writePrimArray marr next (index arr1 i1); loop (i1 + 1) i2 (next + 1)
            (False, True, True) -> do writePrimArray marr next (index arr2 i2); loop i1 (i2 + 1) (next + 1)
            _ -> pure ()



ss1, ss2, ss3, ss4, ss5, ss6, ss7, ss8, ss9 :: CompactPrimSet Int
ss1 = CompactPrimSet []
ss2 = insertElem 99 ss1
ss3 = insertElem 33 ss2
ss4 = insertElem 12 ss3
ss5 = insertElem 6 ss4
ss6 = insertElem 22 ss5
ss7 = insertElem 71 ss6
ss8 = insertElem 81 ss7
ss9 = insertElem 51 ss8

makeSet :: (Ord t,Prim t) => [t] -> CompactPrimSet t
makeSet ts = CompactPrimSet (map node nodes) where
    nodes = pieces ts
    node (n, ps) = Node n (fromlist (sort ps))  
      



{-
ttt = CompactPrimSet [Node n arr]
  where  CompactPrimSet nodes = makeSet [7::Int,2,1,4,3,6,5]
         (arr,n) = mergeNodes nodes
-}    


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

-- =========================================================
-- HeapWords instances

instance (HeapWords v) => HeapWords (A.Array Int v) where
  heapWords arr = foldl accum (3 + n) arr
    where
      accum ans v = ans + heapWords v
      n = isize arr

instance (Prim a, HeapWords a) => HeapWords (PrimArray a) where
  heapWords arr = 2 + (sizeofPrimArray arr * heapWords (index arr 0))