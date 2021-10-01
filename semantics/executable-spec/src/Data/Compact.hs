{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- HeapWords for Array and PrimArray
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Compact where

import qualified Data.Array as A
import qualified Data.Primitive.Array as PA
import qualified Data.Array.MArray as MutA
import GHC.Arr(STArray(..),unsafeFreezeSTArray,newSTArray)
import Data.Primitive.PrimArray
 ( PrimArray, indexPrimArray, primArrayFromList, primArrayToList, sizeofPrimArray, copyPrimArray,
   MutablePrimArray,unsafeFreezePrimArray , newPrimArray,sizeofMutablePrimArray, readPrimArray, writePrimArray,
 )
import Data.ByteString.Short (ShortByteString,toShort,fromShort)
import Data.ByteString(ByteString)
import Data.Foldable (foldr',foldl')
import Data.Primitive.Types (Prim (..))
import Cardano.Prelude (HeapWords (..))
import GHC.Exts( IsList (toList) )
import Control.Monad.ST (ST, runST)
import Data.List(sort,sortBy)
import Debug.Trace(trace)
import Cardano.Binary
  ( Encoding,
    FromCBOR (..),
    ToCBOR (..),
    serialize',
    unsafeDeserialize',
  )
import Data.Text(Text,pack)
import Data.STRef(STRef,newSTRef,readSTRef,writeSTRef)

shorten :: ToCBOR t => t -> ShortByteString
shorten x = toShort (serialize' x)


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

newtype MutArray s t = MutArray (STArray s Int t)

instance ArrayPair (A.Array Int) MutArray a where
  msize (MutArray (STArray lo hi _ _)) = hi - lo + 1 
  mindex (MutArray arr) i = MutA.readArray arr i
  mnew n = MutArray <$> (MutA.newArray_ (0,n-1))
  mfreeze (MutArray arr) = unsafeFreezeSTArray arr
  mwrite (MutArray arr) i a = MutA.writeArray arr i a
  mcopy marr startm arr start count = go startm start count
    where go i j 0 = pure ()
          go i j n = do
             mwrite marr i (index arr j)
             go (i+1) (j+1) (n-1)

mfromlist :: ArrayPair arr marr a => [a] -> ST s (marr s a)
mfromlist xs = do
  marr <- mnew (length xs) 
  let loop i [] = pure ()
      loop i (x:xs) = mwrite marr i x >> loop (i+1) xs
  loop 0 xs
  pure marr

catArray :: ArrayPair arr marr a => [arr a] -> arr a
catArray xs = fst(withMutArray total (build 0 xs))
  where total = sum(map isize xs)
        build next [] marr = pure ()
        build next (arr: arrs) marr =
           do let size = isize arr
              mcopy marr next arr 0 size
              build (next+size) arrs marr

testmcopy = catArray xs where
   xs :: [A.Array Int Int]
   xs = map fromlist [[2,6],[2,6,9,8],[],[1]]

sp1 = splitAtFullPrefix nodesize 1 (Node 1 1) [Node 1 1,Node 2 2, Node 4 4, Node 8 8,Node 32 32,Node 128 128]


-- =====================================================
-- Arrays which store their elements in sorted order.
-- can be used with binary search, to quickly test if a key
-- is stored in the array.
-- =====================================================

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

instance Search (arr k) k => Search (NixArr arr k) k where
  search key (NixArr _ arr del) =
    case search key arr of
      Nothing -> Nothing
      Just i -> if hasElem i del then Nothing else (Just i)
      

instance Search t key => Search (t,x)  key where
  search key (t,x) = search key t

-- ================================================
-- Merging N-sorted arrays into 1 sorted array.

-- | Split a list into (smallest-element, larger-elements). 'x' is the smallest seen so far.
smallest :: (a -> a -> Bool) -> a -> [a] -> [a] -> (a, [a])
smallest _ x [] larger = (x,larger)
smallest smaller x (y:ys) larger =
   if smaller x y
      then smallest smaller x ys (y:larger)
      else smallest smaller y ys (x:larger)

-- | Build an 'ans' by applying 'action' in ascending order of 't'.
--   Works by choosing the smallest 't' in each pass (call). The 'done' function
--   returns (Just more) when the 't' object has more things to process.
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

inOrder2 :: Show t =>
   (t -> t -> Bool) ->
   (t -> Maybe t) ->
   (state -> t -> ST s state) ->
   state -> [t] -> ST s state
inOrder2 smaller done action state items = loop items state where
    loop [] state = pure state
    loop (x:xs) state =
       trace ("LOOP "++show (x:xs)) $
       case smallest smaller x xs [] of
         (small,larger) -> do
              state' <- action state small
              case done small of
                Nothing -> loop larger state'
                Just more -> loop (more:larger) state'


mark1:: (Indexable arr t) =>
        Int -> (Int,arr t) -> Int -> (Int,arr t) -> MutArray s (Int,arr t) -> ST s Int
mark1 lo t i (next,arr) marr =
  do let next' = next+1
     if next' < isize arr
        then mwrite marr i (next',arr) >> pure lo
        else mwrite marr i t >> pure(lo+1)

smaller1 :: (Ord a, Indexable arr a) => (Int, arr a) -> (Int, arr a) -> Bool
smaller1 (i,arr1) (j,arr2) = index arr1 i < index arr2 j

mergeArray :: forall a arr marr.
  ( ArrayPair arr marr a,
    Show (arr a),
    Ord a,
    Indexable arr a
  ) =>
  Int ->
  [arr a] ->
  (forall s. marr s a -> Int -> (Int,arr a) -> ST s Int) ->
  arr a
mergeArray size inputs action = fst $ withMutArray size build where
  build:: forall s. marr s a -> ST s Int
  build moutput = do
     minputs <- mfromlist (map (\ x -> (0,x)) inputs)
     inOrder3 mark1 smaller1 (0::Int) 0 (size-1) (action moutput) minputs

mergeArray2 :: forall a arr marr arr2 marr2 v.
  ( ArrayPair arr marr a,
    Indexable arr2 v,
    ArrayPair arr2 marr2 v,
    Show (arr a),
    Ord a
  ) =>
  Int ->
  [arr a] ->
  (forall s. marr s a -> marr2 s v -> Int -> (Int,arr a) -> ST s Int) ->
  MapNode arr arr2 a v
mergeArray2 size inputs action = node (with2MutArray size size build) where
  node (arr1,arr2,_) = MapNode size arr1 arr2
  build:: forall s. marr s a -> marr2 s v -> ST s Int
  build mkeys mvals = do
     minputs <- mfromlist (map (\ x -> (0,x)) inputs)
     inOrder3 mark1 smaller1 (0::Int) 0 (size-1) (action mkeys mvals) minputs


mergeMapNode :: forall karr varr marr marr2 k v.
 ( ArrayPair karr marr k,
   ArrayPair varr marr2 v,
   Show (karr k),
   Ord k
  ) => Int -> [MapNode karr varr k v] -> MapNode karr varr k v
mergeMapNode size nodes = mergeArray2 size inputs action where
   (inputs,vals) = unzip (map (\ (MapNode _ ks vs) -> (ks,vs)) nodes)
   action:: forall s. marr s k -> marr2 s v -> Int -> (Int,karr k) -> ST s Int
   action mkeys mvals i (n,arrkeys) = undefined

-- ==============================================================
-- Overloaded operations on (Map k v)

class Maplike m k v where
  makemap :: [(k,v)] -> m k v
  lookupmap :: Ord k => k -> m k v -> Maybe v
  insertmap :: Ord k => k -> v -> m k v -> m k v

data MapNode karr varr k v where
  MapNode:: (Indexable karr k,Indexable varr v) =>
     {-# UNPACK #-} !Int ->
     karr k ->
     varr v ->
     MapNode karr varr k v

instance (Indexable karr k, Indexable varr v, Show k, Show v) => Show (MapNode karr varr k v) where
  show (MapNode n ks vs) = show n++" "++show(zip (tolist ks) (tolist vs))

mn1 :: MapNode (A.Array Int) (A.Array Int) Int String 
mn1 = makemap [(i,show i) | i <- [0..15]]

getNodeSize (MapNode i _ _) = i

instance  (Ord k, Indexable karr k, Indexable varr v) => Maplike (MapNode karr varr) k v where
  makemap pairs = MapNode (length pairs) (fromlist ks) (fromlist vs) where
     (ks,vs) = unzip (sortBy (\ (k1,_) (k2,_) -> compare k1 k2) pairs)
  lookupmap key (MapNode size karr varr) = (index varr) <$> (binsearch 0 (size-1) key karr)
  insertmap k v (MapNode size karr varr) = error ("NO efficent insertmap for MapNode")
     
newtype FlexMap karr varr k v = FlexMap [MapNode karr varr k v]

instance (Indexable karr k, Indexable varr v, Show k, Show v) => Show(FlexMap karr varr k v) where
  show (FlexMap nodes) = "FlexMap \n"++ unlines (map (("  " ++).show) nodes)

instance
  ( Ord k,
    ArrayPair karr marr k,
    ArrayPair varr marr2 v,
    Show (karr k)
  ) => Maplike (FlexMap karr varr) k v where
   makemap pairs = FlexMap (map node nodes) where
      nodes = pieces (sortBy (\ (k1,_) (k2,_) -> compare k1 k2) pairs)
      node (n, ps) = MapNode n (fromlist ks) (fromlist vs)
         where (ks,vs) = unzip ps
   lookupmap key (FlexMap (x:xs)) =
      case lookupmap key x of
         Nothing -> lookupmap key (FlexMap xs)
         Just v -> Just v
   insertmap k v (FlexMap xs) =
      case splitAtFullPrefix getNodeSize 1 (MapNode 1 (fromlist [k]) (fromlist [v])) xs of
        (n,prefix,appendix) -> FlexMap ((mergeMapNode n prefix):appendix)

pairs = [(i,show i) | i <- [14:: Int,13..0]]
fl1 :: FlexMap PrimArray PA.Array Int String
fl1@(FlexMap nnn) = makemap pairs
      

-- ============================================================
--  Compound Arrays that are constructed from simpler Arrays
-- ============================================================


-- ===========================================================================
-- NixArray. an Array where some indices have been nix'ed, or marked as deleted
-- Convenient since we can both insert, overwrite, and delete efficiently, by
-- using the delete set.

data NixArr arr k where
  NixArr :: Indexable arr k => {-# UNPACK #-} !Int  -> -- Actual elements (size arr - size delete-set)
                               !(arr k) ->
                               !(CompactPrimSet Int) ->
                               NixArr arr k

instance (Show k,Indexable arr k) => Show (NixArr arr k) where
  show x = show (tolist x)

instance (Indexable arr k) => Indexable (NixArr arr) k where
  index (NixArr _ arr del) k = index arr k 
  isize (NixArr _ arr del) = isize arr
  fromlist xs =  NixArr (length xs) (fromlist xs) emptyset
  tolist (NixArr _ arr del) = removeNixedIndices del arr

removeNixedIndices del xs = help (zip [0..] (tolist xs))
   where help ((i,x):more) = if hasElem i del then help more else x : help more
         help [] = []

narr1 :: NixArr PrimArray Int
narr1@(NixArr nsize1 ys _) = fromlist [ i | i <- [0..8]]
narr2 = NixArr (nsize1 - 2) ys (makeSet [3,6])

{-
insertNixMap :: (Prim k,Ord k) => k -> v -> NixMap k v -> NixMap k v
insertNixMap k v (NixMap nodes) =
   case splitAtFullPrefix nodesize 1 (Node 1 (fromlist [k],fromlist[v])) (findAndMark k nodes) of
         (size,prefix,tail) -> NixMap ((mergeNixParallel size prefix):tail)
  
deleteNixMap :: (Prim k, Ord k) => k -> NixMap k v -> NixMap k v
deleteNixMap k (NixMap nodes) = NixMap(findAndMark k nodes)

findAndMark :: (Search (arr1 k) k) => k -> [Node (NixArr arr1 k,arr2 v)] -> [Node (NixArr arr1 k,arr2 v)]
findAndMark k [] = []
findAndMark k (nodes@(node@(Node n (NixArr m ks ds,vs)) : more )) =
  case search k ks of
    Nothing -> node : findAndMark k more
    Just i -> if hasElem i ds
                 then nodes
                 else (Node n (NixArr (m+1) ks (insertElem i ds),vs)) : more

nm1 :: NixMap Int String
nm1 = makemap [(i,show i) | i <- [1..7]]
-}

-- =======================================================================================
-- SerialArray. An array where the elements are stored serialized in groups of fixed size

data SerialArray arr v where
   SerialArray:: (Indexable arr ShortByteString,FromCBOR v) =>
       {-# UNPACK #-} !Int -> -- Nominal size of the arr. Indices run from [0..nominal -1]
       {-# UNPACK #-} !Int -> -- groupsize, so the actual size of the array is (serialSize nominalsize groupsize)
       arr ShortByteString ->
       SerialArray arr v
       
-- These two functions control how to serialize
toBytes :: ToCBOR a => a -> ShortByteString
toBytes x = toShort (serialize' x)

fromBytes :: FromCBOR a => ShortByteString -> a
fromBytes x = (unsafeDeserialize' (fromShort x))

-- | How many slots do you need to store 'nominalsize' elements if they are serialized into
--   groups, where each group is a list of length 'groupsize'. The last group may have fewer items.
serialSize nominalsize groupsize = (nominalsize `div` groupsize) + fix
  where fix = if (nominalsize `mod` groupsize)==0 then 0 else 1

-- | Chop a list into 'groupsize' sub lists
--   Invariant: (length (chopN groupsize xs) == serialSize (length xs))
chopN :: Int -> [t] -> [[t]]
chopN groupsize [] = []
chopN groupsize xs = take groupsize xs : chopN groupsize (drop groupsize xs)

instance Show t => Show (SerialArray arr t) where
   show (SerialArray nominal group arr) = show bytes
     where bytes = concat $ map (fromBytes @[t]) (tolist arr)

instance (Indexable arr ShortByteString,ToCBOR t, FromCBOR t) => Indexable (SerialArray arr) t where
   isize (SerialArray nominal _ _) = nominal
   index (SerialArray nominal groupsize arr) i =
     if (i >=0) && (i < nominal)
        then (fromBytes (index arr (i `div` groupsize))) !! (i `mod` groupsize)
        else error ("index "++show i++" out of SerialArray nominal range (0,"++show(nominal-1)++").")
   fromlist = serialArrayFromlist @t 10
   tolist (SerialArray _ _ arr) = concat (map (fromBytes @[t]) (tolist arr))

serialArrayFromlist :: forall t arr. (ToCBOR t,FromCBOR t,Indexable arr ShortByteString) => Int -> [t] -> SerialArray arr t
serialArrayFromlist groupsize ts = SerialArray nominalsize groupsize arr
  where nominalsize = length ts
        arr = fromlist (map (toBytes @[t]) (chopN groupsize ts))

{-
-- ==================================
-- =================================4
-- ==================================

-- ====================================
groupSize :: Int
groupSize = 4
sa2 :: SerialArray (A.Array Int) Text
sa2 = serialArrayFromlist groupSize (map (pack . show) [8..16])

sa3 :: SerialArray (A.Array Int) Text
sa3@(SerialArray  _ _ bs3) = serialArrayFromlist groupSize (map (pack . show) [2,5,17,21,50,60])


n = isize sa2
m = isize sa3

salist ::  [(Int, Node (PrimArray Int, SerialArray (A.Array Int) Text))]
salist = [(0,Node n (fromlist[1,3,5,7,9,11,13,15,17],sa2)),(0,Node m (fromlist[2,4,6,8,10,12],sa3))]
satest = mergeParSerialNodes salist (n+m) (serialSize groupSize (n+m))
instance (MergeParallel PrimArray Int) where



-- =======================================

{-
inOrder2 :: 
   (t -> t -> Bool) ->
   (t -> Maybe t) ->
   (state -> t -> ST s state) ->
   state -> [t] -> ST s state


satest2 = mergeSerialNodes [(0,Node n sa2),(0,Node m sa3)] 


mergeSerialNodes :: forall arr1 marr1 v.
  ( ArrayPair arr1 marr1 ShortByteString,
    Ord v,
    ToCBOR v,
    FromCBOR v,
    Show v
  ) =>
  [Node (SerialArray arr1 v)] ->
  (arr1 ShortByteString,())
mergeSerialNodes xs = withMutArray (serialSize groupSize total) builder where
  total = sum (map (\ (_,Node n _) -> n) xs)
  builder :: forall s. marr1 s ShortByteString -> ST s ()
  builder marr1 = loop1 xs (0,[])
    where loop1 [] (j,group) = mwrite marr1 j group
          loop1 ((Node n (SerialArray _ _ arr)):more) state =
              do state2 <- loop2 (0,serialSize n groupSize) state
                 loop1 more state2
          loop2 (i,size,arr) vs | i < size =
             do let vs = index 
-}

-- builder :: forall s. marr1 s ShortByteString -> ST s (Int,Int,[v])
-- builder marr1 = inOrder2 smallerS doneS (actionS marr1) (0,0,[]) xs 

smallerS (i,Node _ sarr1) (j,Node _ sarr2) =
    trace ("SMALLER "++show(i,sarr1,j,sarr2)++" "++show(index sarr1 i < index sarr2 j)) $
    index sarr1 i < index sarr2 j
doneS (i,node@(Node size _)) = if i+1 < size then Just(i+1,node) else Nothing
actionS marr1  (i,j,vs) (_size,Node _ (vals@(SerialArray _ _ _))) =
     -- trace ("ACTION "++show (i,j,vs)++"  "++show vals) $
     do { let vs2 = (index vals i) : vs
        ; if length vs == groupSize {- group size might vary -}
             then do { mwrite marr1 j (toBytes (reverse vs)); pure(i+1,j+1,[]) }
             else pure(i+1,j,vs2) }


{-
class Foo arr a v where
  smallerF :: (Ord a, Indexable arr a) => (Int, Node (arr a)) -> (Int, Node (arr a)) -> Bool
  doneF :: (Int, Node (arr a)) -> Maybe (Int, Node (arr a))
  actionF :: forall s marr state. ArrayPair arr marr a =>
    marr s v -> state -> (Int,Node (arr a)) -> ST s state

instance
   ( -- Indexable arr ShortByteString,
     Ord v,
     ToCBOR v,
     FromCBOR v
   ) => Foo (SerialArray arr) v ShortByteString where
  smallerF = smallerS
  doneF = (doneS @v)
  actionF = (actionS @v)
-}  

mergeParSerialNodes ::
  ( ArrayPair arr2 marr2 ShortByteString,
    ArrayPair arr1 marr1 k,
    MergeParallel arr1 k,
    Ord k,
    ToCBOR v
  ) =>
  [(Int, Node (arr1 k, SerialArray arr2 v))] ->
  Int ->
  Int ->
  (arr1 k, arr2 ShortByteString, (Int, Int, [v]))
mergeParSerialNodes xs s1 s2 = with2MutArray s1 s2 (builder xs) where
  builder xs marr1 marr2 = inOrder smallerP doneP (action marr1 marr2) xs (pure(0,0,[]))
  action marr1 marr2 comp (_size,Node _ (keys,vals@(SerialArray _ _ _))) =
     do { (i,j,vs) <- comp
        ; mwrite marr1 i (index keys i)
        ; let vs2 = (index vals i) : vs
        ; if length vs == groupSize {- group size might vary -}
             then do { mwrite marr2 j (toBytes (reverse vs)); pure(i+1,j+1,[]) }
             else pure(i+1,j,vs2) }
            

-- ================================================
-- Merging sorted arrays

class Indexable t a => MergeVector t a where
  smallerV:: Ord a => (Int,Node (t a)) -> (Int,Node (t a)) -> Bool
  smallerV (i1,Node _ arr1) (i2,Node _ arr2) = index arr1 i1 < index arr2 i1
  doneV:: (Int,Node (t a)) -> Maybe (Int,Node (t a))
  doneV (i,node@(Node size _)) = if i+1 < size then Just(i+1,node) else Nothing
  actionV:: ans -> (Int,Node (t a)) -> ans
  actionV ans _ = ans

class Indexable t a => MergeParallel t a where
  smallerP:: Ord a => (Int,Node (t a,b)) -> (Int,Node (t a,b)) -> Bool
  smallerP (i1,Node _ (arr1,_)) (i2,Node _ (arr2,_)) = index arr1 i1 < index arr2 i1
  doneP:: (Int,Node (t a,b)) -> Maybe (Int,Node (t a,b))
  doneP (i,node@(Node size _)) = if i+1 < size then Just(i+1,node) else Nothing
  actionP:: ans -> (Int,Node (t a,b)) -> ans
  actionP ans _ = ans




-- =========================

-- | Merge many Nodes into 1 Node. Each node carries a set of parallel arrays.
--   In the pattern (i,Node size (keys,vals)). 'i' is the next entry
--   in 'keys' and 'vals' that is ready to be merged into the output.
mergeManyParallel::
  ( ArrayPair arr1 marr1 k,
    ArrayPair arr2 marr2 v,
    Ord k,
    Prim k
  ) =>
  [(Int, Node (arr1 k, arr2 v))] -> marr1 s k -> marr2 s v -> ST s Int
mergeManyParallel xs marr1 marr2 = inOrder smaller done action xs (pure (0::Int)) where
  smaller (i,Node _ (xs,_)) (j,Node _ (ys,_)) = index xs i < index ys j
  done (i,node@(Node size _)) = if i+1 < size then Just(i+1,node) else Nothing
  action n (i,Node _ (keys,vals)) =
     do { count <- n
        ; mwrite marr1 count (index keys i)
        ; mwrite marr2 count (index vals i)
        ; pure(count+1)}

-- | Merge 'ns' (which carry parallel arrays) into one Node with 'size' entries.
mergeParallel ::
  ( ArrayPair arr1 marr1 k,
    ArrayPair arr2 marr2 v,
    Ord k,
    Prim k
  ) => Int -> [Node (arr1 k, arr2 v)] -> Node (arr1 k, arr2 v)
mergeParallel size ns = Node size (project(with2MutArray size size (mergeManyParallel (map start ns))))
    where start x = (0,x)
          project (x,y,_) = (x,y)

-- =========================

mergeManyNixParallel::
  ( ArrayPair arr1 marr1 k,
    ArrayPair arr2 marr2 v,
    Ord k,
    Prim k
  ) =>
  [(Int, Node (NixArr arr1 k, arr2 v))] -> marr1 s k -> marr2 s v -> ST s Int
mergeManyNixParallel xs marr1 marr2 = inOrder smaller done action xs (pure (0::Int)) where
  smaller (i,Node _ (NixArr _ xs d1,_)) (j,Node _ (NixArr _ ys d2,_)) = index xs i < index ys j
  done (i,node@(Node size _)) = if i+1 < size then Just(i+1,node) else Nothing
  action n (i,Node _ (NixArr size keys del,vals)) =
     do { count <- n
        ; if hasElem count del
             then pure count
             else do { mwrite marr1 count (index keys i)
                     ; mwrite marr2 count (index vals i)
                     ; pure(count+1) } }

mergeNixParallel ::
  ( ArrayPair arr1 marr1 k,
    ArrayPair arr2 marr2 v,
    Ord k,
    Prim k
  ) => Int -> [Node (NixArr arr1 k, arr2 v)] -> Node (NixArr arr1 k, arr2 v)
mergeNixParallel size ns = Node size (project(with2MutArray size size (mergeManyNixParallel (map start ns))))
    where start x = (0,x)
          actualSize = sum(map (\ (Node _ (NixArr size _ _,_)) -> size) ns)
          project (x,y,_) = (NixArr actualSize x emptyset,y)

-- A simple test
rrr :: [Int]
rrr = inOrder smaller done action [[1,4,7],[3],[11,34,78,99,145],[2,6,8,9]] []
  where smaller (x:xs) (y:ys) = x < y
        done (x:y:zs) = Just(y:zs)
        done _ = Nothing
        action ans (x:_) = x:ans



-- ===========================================================
-- (Map key value) as lists of nodes of ascending size. Each size is a power of 2.
-- In each (Node size (keyArr,valArr)) the 'keyArr' and 'valArr' have 'size' components.
-- These are parallel arrays. The key at index i keyArr, has its associated value at index i valArr
-- The sum of the sizes is the total number of elements in the set. 

newtype PrimMap key val = PrimMap [Node (PrimArray key,PA.Array val)]

instance (Prim k,Show k,Show v) => Show(PrimMap k v) where
  show (PrimMap ts) = "PrimMap\n   "++show keys++"\n   "++show vals
    where unNode(Node _ (x,y)) = (tolist x, tolist y)
          pairs = map unNode ts
          (keys,vals) = unzip pairs

instance (Ord k,Prim k) => Maplike PrimMap k v where
   lookupmap key (PrimMap []) = Nothing
   lookupmap key (PrimMap (Node _ (ks,vs) : more)) =
     case search key ks of
       Nothing -> lookupmap key (PrimMap more)
       Just i -> Just(index vs i)

   insertmap k v (PrimMap nodes) =
      case splitAtFullPrefix nodesize 1 (Node 1 (fromlist [k],fromlist[v])) nodes of
         (size,prefix,tail) -> PrimMap ((mergeParallel size prefix):tail)

   makemap = makeMap

makeMap :: (Prim k,Ord k) => [(k,v)] -> PrimMap k v
makeMap pairs = PrimMap (map node nodes) where
    nodes = pieces pairs
    node (n, ps) = Node n (fromlist ks, fromlist vs)
      where (ks,vs) = unzip (sortBy (\ (k1,_) (k2,_) -> compare k1 k2) ps)

m2,m3 :: PrimMap Int String
m2 = makeMap [(i,show i) | i <- [1..21]]
m3 = foldl accum (PrimMap []) (reverse [(i,show i) | i <- [1..21 ::Int]])
  where accum ans (k,v) = insertmap k v ans

-- =====================================================================
-- NixMap supports deletion, and overwriting insertion

newtype NixMap k v = NixMap [Node (NixArr PrimArray k,PA.Array v)]

instance (Prim k,Show k,Show v) => Show(NixMap k v) where
  show (NixMap ts) = "NixMap\n   "++show keys++"\n   "++show vals
    where unNode(Node _ (x@(NixArr _ _ ds),y)) = (tolist x,removeNixedIndices ds y)
          pairs = map unNode ts
          (keys,vals) = unzip pairs

instance (Ord k,Prim k) => Maplike NixMap k v where
   makemap = makeNixMap
   lookupmap k (NixMap []) = Nothing
   lookupmap k (NixMap ((Node _ (NixArr _ keys ds,vals)):more)) =
      case search k keys of
        Nothing -> lookupmap k (NixMap more)
        Just i -> if hasElem i ds
                     then Nothing
                     else Just(index vals i)
   insertmap = insertNixMap

makeNixMap :: (Prim k,Ord k) => [(k,v)] -> NixMap k v
makeNixMap pairs = NixMap (map node nodes) where
    nodes = pieces pairs
    node (n, ps) = Node n (fromlist ks, fromlist vs)
      where (ks,vs) = unzip (sortBy (\ (k1,_) (k2,_) -> compare k1 k2) ps)


-- ============================================================
-- Code for binary merging of adjacent nodes, instead of n-ary

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



-- =================================================






-- ========================================================
-- efficient merge of 2 sorted arrays

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

mergeParallel2 :: (ArrayPair vals mvals v, ArrayPair keys mkeys key, Ord key,Indexable arr key) =>
     Int ->
     Int -> 
     arr key ->
     arr key ->
     vals v ->
     vals v ->
     (keys key, vals v, ())
mergeParallel2 size1 size2 keys1 keys2 vals1 vals2 =
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

-}