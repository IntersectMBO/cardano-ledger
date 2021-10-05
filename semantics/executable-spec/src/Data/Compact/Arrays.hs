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

{-# OPTIONS_GHC -Wno-unused-imports #-}


-- | Introduces a number of new Array type constructors which are instances of Indexable. These
--   are usefull because they can be used to make small (compact) values with low memory use.
module Data.Compact.Arrays where

-- import qualified Data.Array as A
import qualified Data.Primitive.Array as PA
-- import qualified Data.Array.MArray as MutA
-- import GHC.Arr(STArray(..),unsafeFreezeSTArray,newSTArray)
import Data.Primitive.PrimArray
 ( PrimArray, indexPrimArray, primArrayFromList, primArrayToList, sizeofPrimArray, copyPrimArray,
   MutablePrimArray,unsafeFreezePrimArray , newPrimArray,sizeofMutablePrimArray, readPrimArray, writePrimArray,
 )
import Data.Primitive.Types (Prim (..)) 
import Data.ByteString.Short (ShortByteString,toShort,fromShort)
import Data.ByteString(ByteString)
import Data.Foldable (foldr',foldl')
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
import Data.Compact.Class
import Data.Compact.Set(IntSet, emptyIntSet, elemIntSet, makeIntSet,insertIntSet)

{-
-- ==================================
import Data.Bits ((.&.), (.|.), complement, popCount, unsafeShiftL, unsafeShiftR)
type Hash   = Word
type Bitmap = Word
type Shift  = Int


bitsPerSubkey :: Int
bitsPerSubkey = 4

maxChildren :: Int   -- 16
maxChildren = 1 `unsafeShiftL` bitsPerSubkey

subkeyMask :: Bitmap  -- 15 --> binary (fromIntegral subkeyMask) --> [1,1,1,1]
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1

sparseIndex :: Bitmap -> Bitmap -> Int
sparseIndex b m = popCount (b .&. (m - 1))

mask :: Word -> Shift -> Bitmap
mask w s = 1 `unsafeShiftL` indexB w s
{-# INLINE mask #-}

-- | Mask out the 'bitsPerSubkey' bits used for indexing at this level
-- of the tree.
indexB :: Hash -> Shift -> Int
indexB w s = fromIntegral $ (unsafeShiftR w s) .&. subkeyMask
{-# INLINE indexB #-}

-- | A bitmask with the 'bitsPerSubkey' least significant bits set.
fullNodeMask :: Bitmap
fullNodeMask = complement (complement 0 `unsafeShiftL` maxChildren)
{-# INLINE fullNodeMask #-}
-}
-- =================================================

shorten :: ToCBOR t => t -> ShortByteString
shorten x = toShort (serialize' x)

-- ============================================================
-- FlexArray. A list of arrays with exponentially increasing sizes
-- the size of each array is a power of 2. This allows FlexArrays to grow
-- gracefully by pushing an element on the end. By pushing on the end
-- the index of each element stays the same as the array grows.

data FlexArray arr t where
  FlexArray:: Indexable arr t =>
              {-# UNPACK #-} !Int ->
              [Node(arr t)] ->
              FlexArray arr t

instance (Indexable arr t,Show t) => Show (FlexArray arr t) where
  show (FlexArray _ ns) = concat (map showNode (reverse ns))
    where showNode (Node _ arr) = show (reverse (tolist arr))

instance Indexable arr t => Indexable (FlexArray arr) t where
  index (FlexArray n nodes) i = indexL nodes ((n - i) + 1)
  isize (FlexArray _ nodes) = isizeL nodes
  fromlist xs = FlexArray (length xs) (fromlistL xs)
  tolist (FlexArray _ nodes) = tolistL nodes
  catenate _size _xs = undefined
  merge _size _xs = undefined

indexL :: Indexable arr t => [Node(arr t)] -> Int -> t
indexL [] i = error ("Index, "++show i++", out of bounds on empty [Node (arr t)]")
indexL ((Node n arr):more) i = if i < n then index arr i else indexL more (i - n)

isizeL :: [Node(arr t)] -> Int
isizeL xs = sum(map (\ (Node n _) -> n) xs)

fromlistL :: Indexable t a => [a] -> [Node (t a)]
fromlistL xs = map node pairs
   where pairs = pieces (reverse xs)
         node (n,zs) = Node n (fromlist zs)

tolistL :: Indexable t a => [Node (t a)] -> [a]
tolistL xs = concat(map (tolist . arrayPart) xs)

pushD :: (ArrayPair arr marr t) => FlexArray arr t -> t -> FlexArray arr t
pushD (FlexArray _ []) t = fromlist [t]
pushD (FlexArray siz nodes) t =
   case splitAtFullPrefix nodesize 1 (Node 1 (fromlist [t])) nodes of
     (size,prefix,rest) -> FlexArray siz (Node size (catArray size (map arrayPart prefix)):rest)

flex10, flex11, flex12, flex13, flex14, flex15 :: FlexArray PrimArray Int
flex10 = fromlist [1..19] 
flex11 = pushD flex10 20
flex12 = pushD flex11 21
flex13 = pushD flex12 22
flex14 = pushD flex13 23
flex15 = pushD flex14 24

instance (Indexable arr key,Ord key) => Search (FlexArray arr key) key
   where search key v = binsearch 0 (isize v - 1) key v   

-- =====================================================================================
-- NixNode. An Array where some indices have been nix'ed, or marked as deleted.
-- Like primitive arrays (PrimArray, PA.Array, A.Array) NixNode does not grow. But
-- it is convenient since we can use it in other constructions to make insert, overwrite
-- and delete efficient in compound constructions.
 
data NixNode arr k where
  NixNode :: Indexable arr k =>
       {-# UNPACK #-} !Int  -> -- Actual elements (size arr - size delete-set)
       !(arr k) ->
       !IntSet ->
       NixNode arr k

nix :: Ord k => k -> NixNode arr k -> NixNode arr k
nix k (node@(NixNode n arr del)) =
  case binsearch 0 (isize arr -1) k arr of
    Nothing -> node
    Just i -> NixNode (n-1) arr (insertIntSet i del)

plistf :: (a -> String) -> String -> [a] -> String -> String ->String
plistf f open xs comma close = open++help xs++close
  where help [] = ""
        help [y] = f y
        help (y:ys) = f y ++ comma ++ help ys

instance (Show k,Indexable arr k) => Show (NixNode arr k) where
  show (NixNode _ arr del) = plistf tell "[" (zip [0..] (tolist arr)) "," "]"
    where tell (i,x) = if elemIntSet i del then "_" else show x

instance (Indexable arr k) => Indexable (NixNode arr) k where
  index (NixNode _ arr _del) k = index arr k -- Note this may have been deleted
  isize (NixNode _ arr _del) = isize arr
  fromlist xs =  NixNode (length xs) (fromlist xs) emptyIntSet
  tolist (NixNode _ arr del) = removeNixedIndices del arr
  catenate _size _xs = undefined
  merge _size _xs = undefined


removeNixedIndices :: Indexable t a => IntSet -> t a -> [a]
removeNixedIndices del xs = help (zip [0..] (tolist xs))
   where help ((i,x):more) = if elemIntSet i del then help more else x : help more
         help [] = []

-- | Merge a list of NixNode. Complicated because not every index is logically present
--   because of the delete set. So when ever we want to increment an index 'i' into a NixNode
--   we use (incrementNix i arr del) rather than (i+1)
mergeNixNode :: forall arr marr k.
  (ArrayPair arr marr k, Ord k) =>
  Int -> [NixNode arr k] -> NixNode arr k
mergeNixNode size inputs =  project $ withMutArray size build where
  project (arr,actual) = NixNode actual arr emptyIntSet
  build:: forall s. marr s k -> ST s Int
  build moutput = do
     minputs <- mfromlist (initIndices inputs) -- Some elements might be totally nixed, and dropped here
     inOrder mark2 smaller2 (0::Int) 0 (msize minputs -1) (action1 moutput) minputs
  smaller2 (i,NixNode _ arr1 _) (j,NixNode _ arr2 _) = index arr1 i < index arr2 j
  mark2 lo i (next,node@(NixNode _ arr del)) marr =
    case incrementNix next arr del of
      Nothing -> swap marr lo i >> pure(lo+1)
      Just next' ->
        if next' < isize arr
           then mwrite marr i (next',node) >> pure lo
           else swap marr lo i >> pure(lo+1)

-- | Increment 'i' to the next valid (not Nixed) index in 'arr' using 'del' to discover invalidity
incrementNix :: Indexable t a => Int -> t a -> IntSet -> Maybe Int
incrementNix j arr del = loop (j+1)
  where loop i = if i < (isize arr)
                    then (if elemIntSet i del then loop (i+1) else Just i)
                    else Nothing

-- | Construct a [(n,nixarr)] , where 'n' points to the first not nixed index in 'nixarr'
--   Note that some 'nixarr', that are totally nixed, will be dropped from the output.
initIndices :: [NixNode arr k] -> [(Int,NixNode arr k)]
initIndices xs = foldr accum [] xs where
   accum x ys = case firstNotNixedNode x of {Nothing -> ys; Just node -> node : ys}


firstNotNixedNode :: NixNode arr k -> Maybe (Int, NixNode arr k)
firstNotNixedNode (node@(NixNode _ arr del)) =
   case incrementNix (-1) arr del of
      Nothing -> Nothing
      Just i -> Just(i,node)

inputs3 :: [NixNode PrimArray Int]
inputs3 = [nix 2 $ nix 7 (fromlist[2,7]), nix 6 (fromlist[1,6,19]), fromlist[4,9], nix 3 (fromlist[3,8,12,17])]

nixmerge :: NixNode PrimArray Int
nixmerge = mergeNixNode 7 [nix 2 $ nix 7 (fromlist[2,7]), nix 6 (fromlist[1,6,19]), fromlist[4,9], nix 3 (fromlist[3,8,12,17])]

newtype NixArr arr t = NixArr [NixNode arr t]

instance (Indexable arr t) => Indexable (NixArr arr) t where
  isize (NixArr xs) = sum(map isize xs)
  index (NixArr _xs) _i = undefined
  fromlist xs = NixArr (map node parts) where
     parts = pieces xs
     node (n,ys) = NixNode n (fromlist ys) emptyIntSet
  tolist (NixArr xs) = foldr accum [] xs
     where accum (NixNode _ arr del) ans = removeNixedIndices del arr ++ ans
  catenate _size _xs = undefined
  merge _size _xs = undefined
     

instance (Show t,Indexable arr t) => Show(NixArr arr t) where
  show (NixArr xs) = plistf show "[" xs "," "]"

nixarr2 :: NixArr PrimArray Int
nixarr2 = fromlist [1,6,19,4,9,3,8,12,17]
-- ========================================================================

narr1 :: NixNode PrimArray Int
ys2 :: PrimArray Int
narr2 :: NixNode PrimArray Int
narr1@(NixNode nsize1 ys2 _) = fromlist [ i | i <- [0..8]]
narr2 = NixNode (nsize1 - 2) ys2 (makeIntSet [3,6])
nsize1 :: Int


instance Search (arr k) k => Search (NixNode arr k) k where
  search key (NixNode _ arr del) =
    case search key arr of
      Nothing -> Nothing
      Just i -> if elemIntSet i del then Nothing else (Just i)
      
-- =======================================================================================
-- SerialArray. An array where the elements are stored serialized in groups of fixed size
-- Like primitive arrays (PrimArray, PA.Array, A.Array) SerialArray does not grow. But
-- may be usefull in other constructions. 

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
serialSize :: Int -> Int -> Int
serialSize nominalsize groupsize = (nominalsize `div` groupsize) + fix
  where fix = if (nominalsize `mod` groupsize)==0 then 0 else 1

-- | Chop a list into 'groupsize' sub lists
--   Invariant: (length (chopN groupsize xs) == serialSize (length xs))
chopN :: Int -> [t] -> [[t]]
chopN _groupsize [] = []
chopN groupsize xs = take groupsize xs : chopN groupsize (drop groupsize xs)

instance Show t => Show (SerialArray arr t) where
   show (SerialArray _nominal _group arr) = show bytes
     where bytes = concat $ map (fromBytes @[t]) (tolist arr)

instance (Indexable arr ShortByteString,ToCBOR t, FromCBOR t) => Indexable (SerialArray arr) t where
   isize (SerialArray nominal _ _) = nominal
   index (SerialArray nominal groupsize arr) i =
     if (i >=0) && (i < nominal)
        then (fromBytes (index arr (i `div` groupsize))) !! (i `mod` groupsize)
        else error ("index "++show i++" out of SerialArray nominal range (0,"++show(nominal-1)++").")
   fromlist = serialArrayFromlist @t 10
   tolist (SerialArray _ _ arr) = concat (map (fromBytes @[t]) (tolist arr))
   catenate _size _xs = undefined
   merge _size _xs = undefined   

serialArrayFromlist :: forall t arr. (ToCBOR t,FromCBOR t,Indexable arr ShortByteString) => Int -> [t] -> SerialArray arr t
serialArrayFromlist groupsize ts = SerialArray nominalsize groupsize arr
  where nominalsize = length ts
        arr = fromlist (map (toBytes @[t]) (chopN groupsize ts))
