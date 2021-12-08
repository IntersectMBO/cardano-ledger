{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Compact.KeyMap where

import Cardano.Prelude (Generic, HeapWords (..), ST, runST)
import Control.DeepSeq (NFData (..))
import Data.Bits
  ( Bits,
    clearBit,
    complement,
    popCount,
    setBit,
    shiftR,
    testBit,
    unsafeShiftL,
    zeroBits,
    (.&.),
    (.|.),
  )
import Data.Compact.SmallArray
  ( PArray,
    fromlist,
    index,
    isize,
    mcopy,
    mfreeze,
    mnew,
    mwrite,
    tolist,
    withMutArray,
  )
import Data.Foldable (foldl')
import Data.Primitive.SmallArray ()
import qualified Data.Primitive.SmallArray as Small
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Word (Word64)
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#, (==#))
import Prettyprinter
import qualified Prettyprinter.Internal as Pretty

-- =============================

-- | binary encoding of 'n', LEAST significant bit on the front of the list.
binary :: Integral n => n -> [n]
binary 0 = []
binary 1 = [1]
binary n = mod n 2 : binary (div n 2)

-- | Show 'n' as a binary number with MOST significant bits on the front of the list.
bin :: Integral n => n -> [n]
bin x = reverse (binary x)

-- ==========================================================================
-- bitsPerSegment, Segments, Paths. Breaking a Key into a sequence of small components

-- | Represent a set of small integers, they can range from 0 to 63
type Bitmap = Word64

-- | The number of bits in a segment. Can't be more than 6, because using Word64
--   as Bitmap can only accomodate 2^6 = 64 bits
bitsPerSegment :: Int
bitsPerSegment = 6
{-# INLINE bitsPerSegment #-}

-- | Ints in the range [0.. intSize], represents one 'bitsPerSegment' wide portion of a key
type Segment = Int

-- | Represents a list of 'Segment', which when combined is in 1-1 correspondance with a Key
type Path = [Segment]

-- | The maximum value of a Segment, as an Int
intSize :: Int
intSize = 2 ^ bitsPerSegment
{-# INLINE intSize #-}

-- | The maximum value of a segment, as a Word64
wordSize :: Word64
wordSize = 2 ^ ((fromIntegral bitsPerSegment) :: Word64)
{-# INLINE wordSize #-}

-- | The length of a list of segments representing a key. Need to be
--   carefull if a Key isn't evenly divisible by bitsPerSegment
pathSize :: Word64
pathSize = (if (mod 64 wbits) == 0 then (div 64 wbits) else (div 64 wbits) + 1)
  where
    wbits = fromIntegral bitsPerSegment :: Word64

-- ========================================================================
-- Keys

-- | Represents 32 Bytes, (wordsPerKey * 8) Bytes compactly
data Key
  = Key
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
  deriving (Eq, Ord, Show, NFData, Generic)

-- | The number of Word64 per key
wordsPerKey :: Int
wordsPerKey = 4

-- | The length of a Path for a Key (which might have multiple Word64's inside)
keyPathSize :: Int
keyPathSize = wordsPerKey * (fromIntegral pathSize)

-- | Note that  (mod n wordSize) and (n .&. modMask) are the same
modMask :: Word64
modMask = wordSize - 1

-- | Break up a Word64 into a Path . Equivalent to
--   loop 0 _ ans = ans
--   loop cnt n ans = loop (cnt - 1) (div n wordSize) ((fromIntegral (mod n wordSize)):ans)
--   But much faster.
getpath :: Word64 -> Path
getpath w64 = loop pathSize w64 []
  where
    loop :: Word64 -> Word64 -> [Int] -> [Int]
    loop 0 _ ans = ans
    loop cnt n ans = loop (cnt - 1) (shiftR n bitsPerSegment) ((fromIntegral (n .&. modMask)) : ans)

-- | Break up a Key into a Path
keyPath :: Key -> Path
keyPath (Key w0 w1 w2 w3) = getpath w0 ++ getpath w1 ++ getpath w2 ++ getpath w3

showBM :: Bitmap -> String
showBM bm = show (bitmapToList bm)

bitmapToList :: Bits a => a -> [Int]
bitmapToList bm = loop 63 []
  where
    loop i ans | i < 0 = ans
    loop i ans = if testBit bm i then loop (i - 1) (i : ans) else loop (i - 1) ans

instance HeapWords Key where
  heapWords (Key _ _ _ _) = 5

-- ===============================================================

-- | KeyMap datastructure.
--   Maintains the bitmap invariant that in the Two, BitmapIndexed, and Full constructors,
--   the Bitmap has the same number of bits set as the number of children in the constructor.
data KeyMap v
  = Empty
  | Leaf {-# UNPACK #-} !Key !v
  | One {-# UNPACK #-} !Int !(KeyMap v) -- 1 subtree
  | Two {-# UNPACK #-} !Bitmap !(KeyMap v) !(KeyMap v) -- 2 subtrees
  | BitmapIndexed
      {-# UNPACK #-} !Bitmap -- 3 - (intSize - 1) subtrees
      {-# UNPACK #-} !(Small.SmallArray (KeyMap v))
  | Full {-# UNPACK #-} !(Small.SmallArray (KeyMap v)) -- intSize subtrees
  deriving (NFData, Generic)

notEmpty :: KeyMap v -> Bool
notEmpty Empty = False
notEmpty _ = True

isEmpty :: KeyMap v -> Bool
isEmpty Empty = True
isEmpty _ = False

instance Eq v => Eq (KeyMap v) where
  (==) x y = toList x == toList y

heapAdd :: HeapWords a => a -> Int -> Int
heapAdd x ans = heapWords x + ans

heapPlus :: HeapWords a => Int -> a -> Int
heapPlus ans x = heapWords x + ans

instance HeapWords v => HeapWords (KeyMap v) where
  heapWords Empty = 1
  heapWords (One _ xs) = 3 + heapWords xs
  heapWords (Leaf _ v) = 6 + heapWords v -- Change when Key changes
  heapWords (BitmapIndexed _ arr) = foldl' heapPlus 2 arr
  heapWords (Full arr) = foldl' heapPlus 1 arr
  heapWords (Two _ a b) = 4 + heapWords a + heapWords b

instance HeapWords () where
  heapWords () = 1

tag :: KeyMap v -> String
tag Empty = "Empty"
tag (One _ _xs) = "One"
tag (Leaf _ _v) = "Leaf"
tag (BitmapIndexed _ _arr) = "BitmapedIndexed"
tag (Full _arr) = "Full"
tag (Two _ _a _b) = "Two"

-- ======================================================================
-- Insertion

indexFromSegment :: Bitmap -> Int -> Int
indexFromSegment bmap j = sparseIndex bmap (setBit 0 j)

insertWithKey' :: Int -> (Key -> v -> v -> v) -> Path -> Key -> v -> KeyMap v -> KeyMap v
insertWithKey' n0 combine path k x kmap = go n0 kmap
  where
    go _ Empty = Leaf k x
    go n (One j node) =
      case compare j i of
        EQ -> One j (go (n + 1) node)
        LT -> Two (setBits [i, j]) node (go (n + 1) Empty)
        GT -> Two (setBits [i, j]) (go (n + 1) Empty) node
      where
        i = path !! n
    go n t@(Leaf k2 y)
      | k == k2 =
        if x `ptrEq` y
          then t
          else (Leaf k (combine k x y))
      | otherwise = twoLeaf (drop n (keyPath k2)) t (drop n path) k x
    go n t@(BitmapIndexed bmap arr)
      | not (testBit bmap j) =
        let !arr' = insertAt arr i $! (Leaf k x)
         in buildKeyMap (bmap .|. (setBit 0 j)) arr'
      | otherwise =
        let !st = index arr i
            !st' = go (n + 1) st
         in if st' `ptrEq` st
              then t
              else BitmapIndexed bmap (update arr i st')
      where
        i = indexFromSegment bmap j
        j = (path !! n)
    go n t@(Two bmap x0 x1)
      | not (testBit bmap j) =
        let !arr' = insertAt (fromlist [x0, x1]) i $! (Leaf k x)
         in buildKeyMap (bmap .|. (setBit 0 j)) arr'
      | otherwise =
        let !st = if i == 0 then x0 else x1
            !st' = go (n + 1) st
         in if st' `ptrEq` st
              then t
              else
                if i == 0
                  then Two bmap st' x1
                  else Two bmap x0 st'
      where
        i = indexFromSegment bmap j
        j = path !! n
    go n t@(Full arr) =
      let !st = index arr i
          !st' = go (n + 1) st
       in if st' `ptrEq` st
            then t
            else Full (update arr i st')
      where
        i = indexFromSegment fullNodeMask j
        j = path !! n

twoLeaf :: Path -> KeyMap v -> Path -> Key -> v -> KeyMap v
twoLeaf [] _ _ _ _ = error ("the path ran out of segments in twoLeaf case 1.")
twoLeaf _ _ [] _ _ = error ("the path ran out of segments in twoLeaf case 1.")
twoLeaf (i : is) leaf1 (j : js) k2 v2
  | i == j = One i (twoLeaf is leaf1 js k2 v2)
  | otherwise =
    if i < j
      then Two (setBits [i, j]) leaf1 (Leaf k2 v2)
      else Two (setBits [i, j]) (Leaf k2 v2) leaf1

insertWithKey :: (Key -> v -> v -> v) -> Key -> v -> KeyMap v -> KeyMap v
insertWithKey f k v m = insertWithKey' 0 f (keyPath k) k v m

insertWith :: (t -> t -> t) -> Key -> t -> KeyMap t -> KeyMap t
insertWith f k v m = insertWithKey' 0 (\_ key val -> f key val) (keyPath k) k v m

insert :: Key -> v -> KeyMap v -> KeyMap v
insert k v m = insertWithKey' 0 (\_key new _old -> new) (keyPath k) k v m

fromList :: [(Key, v)] -> KeyMap v
fromList ps = foldl' accum Empty ps
  where
    accum ans (k, v) = insert k v ans

toList :: KeyMap v -> [(Key, v)]
toList km = foldWithDescKey accum [] km
  where
    accum k v ans = (k, v) : ans

-- =================================================================
-- Deletion

delete2 :: Path -> Key -> KeyMap v -> (KeyMap v -> KeyMap v) -> KeyMap v
delete2 path key km continue = case3 (continue Empty) leafF arrayF km
  where
    leafF k2 _ = if key == k2 then continue Empty else continue km
    arrayF bmap arr =
      case path of
        [] -> continue km
        (i : is) ->
          let m = setBit 0 i
              j = sparseIndex bmap m
              newcontinue Empty = continue (buildKeyMap (clearBit bmap i) (remove arr j))
              newcontinue x = continue (buildKeyMap bmap (update arr j x))
           in if testBit bmap i
                then delete2 is key (index arr j) newcontinue
                else continue km

delete :: Key -> KeyMap v -> KeyMap v
delete key km = delete2 (keyPath key) key km id

-- ==================================================================================
-- One of the invariants is that no Empty ever appears in any of the other
-- constructors of KeyMap.  So we make "smart" constructors that remove Empty
-- if it ever occurrs. This is necessary since 'delete' can turn a subtree
-- into Empty. The strategy is to float 'Empty' up the tree, until it can be
-- 'remove'd from one of the constructors with Array like components (One, Two, BitmapInded, Full).

-- Float Empty up over One
oneE :: Int -> KeyMap v -> KeyMap v
oneE _ Empty = Empty
oneE i x = One i x
{-# INLINE oneE #-}

-- Float Empty's up over Two
twoE :: Bitmap -> KeyMap v -> KeyMap v -> KeyMap v
twoE _ Empty Empty = Empty
twoE bmap x Empty = oneE (ith bmap 0) x
twoE bmap Empty x = oneE (ith bmap 1) x
twoE bmap x y = Two bmap x y
{-# INLINE twoE #-}

-- | Create a 'BitmapIndexed' or 'Full' or 'One' or 'Two' node depending on the size of 'arr'
--   and dropping all Empty nodes.  Use this only where things can become empty (delete, intersect, etc)
dropEmpty :: Bitmap -> PArray (KeyMap v) -> KeyMap v
dropEmpty _ arr | isize arr == 0 = Empty
dropEmpty b arr | isize arr == 1 =
  case bitmapToList b of
    (i : _) -> oneE i (index arr 0)
    [] -> error ("Bitmap " ++ show b ++ "has no bits set in 'dropEmpty', this violates the bitmap invariant. It should have 1 bit set.")
dropEmpty b arr | isize arr == 2 = twoE b (index arr 0) (index arr 1)
dropEmpty b arr
  | any notEmpty arr =
    case (filterArrayWithBitmap isEmpty b arr) of
      (arr2, bm2) -> buildKeyMap bm2 arr2
  | b == fullNodeMask = Full arr
  | otherwise = BitmapIndexed b arr
{-# INLINE dropEmpty #-}

-- | Given Bitmap and an array, where some of the array elements meet the predicate 'p'
--   filter out those elements and adjust the Bitmap to show they were removed.
--   It must be the case that the (popCount 'bm') == (isize 'arr).
filterArrayWithBitmap :: (a -> Bool) -> Bitmap -> PArray a -> (PArray a, Bitmap)
filterArrayWithBitmap _p bm arr
  | popCount bm /= isize arr =
    error ("array size " ++ show (isize arr) ++ " and bitmap " ++ show (bitmapToList bm) ++ " don't agree.")
filterArrayWithBitmap p bm0 arr =
  if n == (isize arr)
    then (arr, bm0)
    else withMutArray n (loop 0 0 bm0)
  where
    n = foldl' (\ans x -> if not (p x) then ans + 1 else ans) 0 arr
    -- i ranges over all possible elements of a Bitmap [0..63], only some are found in 'bm'
    -- j ranges over the slots in the new array [0..n-1]
    loop i j bm marr | i < 63 && not (testBit bm0 i) = loop (i + 1) j bm marr -- Skip over those not in 'bm'
    loop i j bm marr
      | i < 63 =
        let slot = indexFromSegment bm0 i -- what is the index in 'arr' for this Bitmap element?
            item = index arr slot -- Get the array item
         in if not (p item) -- if it does not meet the 'p' then move it to the answer.
              then mwrite marr j item >> loop (i + 1) (j + 1) bm marr
              else loop (i + 1) j (clearBit bm i) marr -- if it meets 'p' then don't copy, and clear it from 'bm'
    loop _i _j bm _marr = pure bm

-- ================================================================
-- aggregation in ascending order of keys

foldWithAscKey :: (ans -> Key -> v -> ans) -> ans -> KeyMap v -> ans
foldWithAscKey _ !ans Empty = ans
foldWithAscKey accum !ans (Leaf k v) = accum ans k v
foldWithAscKey accum !ans (One _ x) = foldWithAscKey accum ans x
foldWithAscKey accum !ans (Two _ x y) = foldWithAscKey accum (foldWithAscKey accum ans x) y
foldWithAscKey accum !ans0 (BitmapIndexed _ arr) = loop ans0 0
  where
    n = isize arr
    loop !ans i | i >= n = ans
    loop !ans i = loop (foldWithAscKey accum ans (index arr i)) (i + 1)
foldWithAscKey accum !ans0 (Full arr) = loop ans0 0
  where
    n = isize arr
    loop !ans i | i >= n = ans
    loop !ans i = loop (foldWithAscKey accum ans (index arr i)) (i + 1)

sizeKeyMap :: KeyMap v -> Int
sizeKeyMap x = foldWithAscKey (\ans _k _v -> ans + 1) 0 x

-- ================================================================
-- aggregation in descending order of keys

foldWithDescKey :: (Key -> v -> ans -> ans) -> ans -> KeyMap v -> ans
foldWithDescKey _ !ans Empty = ans
foldWithDescKey accum !ans (Leaf k v) = accum k v ans
foldWithDescKey accum !ans (One _ x) = foldWithDescKey accum ans x
foldWithDescKey accum !ans (Two _ x y) = foldWithDescKey accum (foldWithDescKey accum ans y) x
foldWithDescKey accum !ans0 (BitmapIndexed _ arr) = loop ans0 (n - 1)
  where
    n = isize arr
    loop !ans i | i < 0 = ans
    loop !ans i = loop (foldWithDescKey accum ans (index arr i)) (i - 1)
foldWithDescKey accum !ans0 (Full arr) = loop ans0 (n - 1)
  where
    n = isize arr
    loop !ans i | i < 0 = ans
    loop !ans i = loop (foldWithDescKey accum ans (index arr i)) (i - 1)

-- ==================================================================
-- Lookup a key

lookupHM :: Key -> KeyMap v -> Maybe v
lookupHM key km = go (keyPath key) km
  where
    go _ Empty = Nothing
    go _ (Leaf key2 v) = if key == key2 then Just v else Nothing
    go [] _ = Nothing -- Path is empty, we will never find it.
    go (j : js) (One i x) = if i == j then go js x else Nothing
    go (j : js) (Two bm x0 x1) =
      if testBit bm j
        then (if i == 0 then go js x0 else go js x1)
        else Nothing
      where
        i = indexFromSegment bm j
    go (j : js) (BitmapIndexed bm arr) =
      if testBit bm j
        then go js (index arr i)
        else Nothing
      where
        i = indexFromSegment bm j
    go (j : js) (Full arr) =
      -- Every possible bit is set, so no testBit call necessary
      go js (index arr i)
      where
        i = indexFromSegment fullNodeMask j

searchPath :: Key -> Path -> KeyMap v -> Maybe v
searchPath _key _path Empty = Nothing
searchPath key _path (Leaf key2 v) = if key == key2 then Just v else Nothing
searchPath _key [] _ = Nothing -- Path is empty, we will never find it.
searchPath key (j : js) (One i x) = if i == j then searchPath key js x else Nothing
searchPath key (j : js) (Two bm x0 x1) =
  if testBit bm j
    then (if i == 0 then searchPath key js x0 else searchPath key js x1)
    else Nothing
  where
    i = indexFromSegment bm j
searchPath key (j : js) (BitmapIndexed bm arr) =
  if testBit bm j
    then searchPath key js (index arr i)
    else Nothing
  where
    i = indexFromSegment bm j
searchPath key (j : js) (Full arr) =
  -- Every possible bit is set, so no testBit call necessary
  searchPath key js (index arr i)
  where
    i = indexFromSegment fullNodeMask j

-- =========================================================
-- map

mapWithKey :: (Key -> a -> b) -> KeyMap a -> KeyMap b
mapWithKey _ Empty = Empty
mapWithKey f (Leaf k2 v) = (Leaf k2 (f k2 v))
mapWithKey f (One i x) = One i (mapWithKey f x)
mapWithKey f (Two bm x0 x1) = Two bm (mapWithKey f x0) (mapWithKey f x1)
mapWithKey f (BitmapIndexed bm arr) = BitmapIndexed bm (fmap (mapWithKey f) arr)
mapWithKey f (Full arr) = Full (fmap (mapWithKey f) arr)

instance Functor KeyMap where
  fmap f x = mapWithKey (\_ v -> f v) x

-- =========================================================
-- UnionWith

-- | Make an array of size 1, with 'x' stored at index 0.
array1 :: a -> PArray a
array1 x = fst (withMutArray 1 (\marr -> mwrite marr 0 x))

-- | Make an array of size 2, with 'x' stored at index 0.
array2 :: a -> a -> PArray a
array2 x y = fst (withMutArray 2 (\marr -> mwrite marr 0 x >> mwrite marr 1 y))

union4 :: Int -> (Key -> v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
union4 _n _combine Empty Empty = Empty
union4 n combine x y = case3 emptyC1 leafF1 arrayF1 x
  where
    emptyC1 = y
    leafF1 k v = insertWithKey' n combine (keyPath k) k v y
    arrayF1 bm1 arr1 = case3 emptyC2 leafF2 arrayF2 y
      where
        emptyC2 = x
        -- flip the combine function because the Leaf comes from the right, but in insertWithKey' is is on the left.
        leafF2 k v = insertWithKey' n (\key a b -> combine key b a) (keyPath k) k v x
        arrayF2 bm2 arr2 = buildKeyMap bm (arrayFromBitmap bm actionAt)
          where
            bm = bm1 .|. bm2
            actionAt i =
              case (testBit bm1 i, testBit bm2 i) of
                (True, False) -> index arr1 (indexFromSegment bm1 i)
                (False, True) -> index arr2 (indexFromSegment bm2 i)
                (False, False) -> Empty -- This should be impossible 'i' is in (bm1 .|. bm2). so it must be in bm1 or bm2 or both
                (True, True) ->
                  union4
                    (n + 1)
                    combine
                    (index arr1 (indexFromSegment bm1 i))
                    (index arr2 (indexFromSegment bm2 i))

unionWithKey :: (Key -> v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWithKey comb x y = union4 0 comb x y

unionWith :: (v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWith comb x y = union4 0 (\_k a b -> comb a b) x y

union :: KeyMap v -> KeyMap v -> KeyMap v
union x y = union4 0 (\_k a _b -> a) x y

-- ===========================================
-- intersection operators
-- ==================================================

-- | The (key,value) pairs (i.e. a subset) of 'h1' where key is in the domain of both 'h1' and 'h2'
intersect :: KeyMap v -> KeyMap v -> KeyMap v
intersect map1 map2 =
  case maxMinOf map1 map2 of
    Nothing -> Empty
    Just k -> leapfrog k map1 map2 Empty

-- | Accumulate a new Key map, by adding the key value pairs to 'ans', for
--   the Keys that appear in both maps 'x' and 'y'. The key 'k' should
--   be the smallest key in either 'x' or 'y', used to get started.
leapfrog :: Key -> KeyMap v -> KeyMap v -> KeyMap v -> KeyMap v
leapfrog k x y ans =
  case (lub k x, lub k y) of
    (Just ((k1, v1), h1), Just ((k2, _), h2)) ->
      case maxMinOf h1 h2 of
        Just k3 -> leapfrog k3 h1 h2 (if k1 == k2 then insert k1 v1 ans else ans)
        Nothing -> (if k1 == k2 then insert k1 v1 ans else ans)
    _ -> ans

-- | Get the larger of the two min keys of 'x' and 'y'. Nothing if either is Empty.
maxMinOf :: KeyMap v1 -> KeyMap v2 -> Maybe Key
maxMinOf x y = case (lookupMin x, lookupMin y) of
  (Just (k1, _), Just (k2, _)) -> Just (max k1 k2)
  _ -> Nothing

intersect3 :: Int -> (Key -> u -> v -> w) -> KeyMap u -> KeyMap v -> KeyMap w
intersect3 _ _ Empty Empty = Empty
intersect3 n combine x y = case3 Empty leafF1 arrayF1 x
  where
    leafF1 k v = case searchPath k (drop n (keyPath k)) y of
      Nothing -> Empty
      Just u -> Leaf k (combine k v u)
    arrayF1 bm1 arr1 = case3 Empty leafF2 arrayF2 y
      where
        leafF2 k v =
          case searchPath k (drop n (keyPath k)) x of
            Nothing -> Empty
            Just u -> Leaf k (combine k u v)
        arrayF2 bm2 arr2 = dropEmpty bm (arrayFromBitmap bm actionAt)
          where
            bm = bm1 .&. bm2
            actionAt i =
              intersect3
                (n + 1)
                combine
                (index arr1 (indexFromSegment bm1 i))
                (index arr2 (indexFromSegment bm2 i))

intersectWhenN :: Int -> (Key -> u -> v -> Maybe w) -> KeyMap u -> KeyMap v -> KeyMap w
intersectWhenN _ _ Empty Empty = Empty
intersectWhenN n combine x y = case3 Empty leafF1 arrayF1 x
  where
    leafF1 k v = case searchPath k (drop n (keyPath k)) y of
      Nothing -> Empty
      Just u -> case combine k v u of
        Just w -> Leaf k w
        Nothing -> Empty
    arrayF1 bm1 arr1 = case3 Empty leafF2 arrayF2 y
      where
        leafF2 k v =
          case searchPath k (drop n (keyPath k)) x of
            Nothing -> Empty
            Just u -> case combine k u v of
              Just w -> Leaf k w
              Nothing -> Empty
        arrayF2 bm2 arr2 = dropEmpty bm (arrayFromBitmap bm actionAt)
          where
            bm = bm1 .&. bm2
            actionAt i =
              intersectWhenN
                (n + 1)
                combine
                (index arr1 (indexFromSegment bm1 i))
                (index arr2 (indexFromSegment bm2 i))

intersection :: KeyMap u -> KeyMap v -> KeyMap u
intersection x y = intersect3 0 (\_key a _b -> a) x y

intersectionWith :: (u -> v -> w) -> KeyMap u -> KeyMap v -> KeyMap w
intersectionWith combine x y = intersect3 0 (\_key a b -> combine a b) x y

intersectionWithKey :: (Key -> u -> v -> w) -> KeyMap u -> KeyMap v -> KeyMap w
intersectionWithKey combine x y = intersect3 0 combine x y

-- | Like intersectionWithKey, except if the 'combine' function returns Nothing, the common
--   key is NOT placed in the intersectionWhen result.
intersectionWhen :: (Key -> u -> v -> Maybe w) -> KeyMap u -> KeyMap v -> KeyMap w
intersectionWhen combine x y = intersectWhenN 0 combine x y

foldIntersect2 :: Int -> (ans -> Key -> u -> v -> ans) -> ans -> KeyMap u -> KeyMap v -> ans
foldIntersect2 n accum ans x y = case3 ans leafF1 arrayF1 x
  where
    leafF1 k u = case searchPath k (drop n (keyPath k)) y of
      Nothing -> ans
      Just v -> accum ans k u v
    arrayF1 bm1 arr1 = case3 ans leafF2 arrayF2 y
      where
        leafF2 k v = case searchPath k (drop n (keyPath k)) x of
          Nothing -> ans
          Just u -> accum ans k u v
        arrayF2 bm2 arr2 = foldl' accum2 ans (bitmapToList bm)
          where
            bm = bm1 .&. bm2
            accum2 result i =
              foldIntersect2
                (n + 1)
                accum
                result
                (index arr1 (indexFromSegment bm1 i))
                (index arr2 (indexFromSegment bm2 i))

foldOverIntersection :: (ans -> Key -> u -> v -> ans) -> ans -> KeyMap u -> KeyMap v -> ans
foldOverIntersection accum ans x1 x2 = foldIntersect2 0 accum ans x1 x2

-- =========================================================

-- | Domain restrict 'hm' to those Keys found in 's'. This algorithm
--   assumes the set 's' is small compared to 'hm'.
--   when that is not the case, intersection variants can be used.
restrictKeys :: KeyMap v -> Set Key -> KeyMap v
restrictKeys hm s = Set.foldl' accum Empty s
  where
    accum ans key =
      case lookupHM key hm of
        Nothing -> ans
        Just v -> insert key v ans

withoutKeys :: KeyMap v -> Set Key -> KeyMap v
withoutKeys hm s = Set.foldl' accum hm s
  where
    accum ans key =
      case lookupHM key hm of
        Nothing -> ans
        Just _ -> delete key ans

-- ===========================================================
-- Maximum and Minimum Key

-- | Get the smallest key, NOT the smallest value
lookupMin :: KeyMap v -> Maybe (Key, v)
lookupMin Empty = Nothing
lookupMin (Leaf k v) = Just (k, v)
lookupMin (One _ x) = lookupMin x
lookupMin (Two _ x _) = lookupMin x
lookupMin (BitmapIndexed _ arr) = lookupMin (index arr 0)
lookupMin (Full arr) = lookupMin (index arr 0)

-- | Get the largest key, NOT the largest value
lookupMax :: KeyMap v -> Maybe (Key, v)
lookupMax Empty = Nothing
lookupMax (Leaf k v) = Just (k, v)
lookupMax (One _ x) = lookupMax x
lookupMax (Two _ _ y) = lookupMax y
lookupMax (BitmapIndexed _ arr) = lookupMax (index arr (isize arr - 1))
lookupMax (Full arr) = lookupMax (index arr (isize arr - 1))

-- | The view of the KeyMap of the smallestKey and its value, and the map that results from removing that Leaf.
minViewWithKeyHelp :: KeyMap a -> (KeyMap a -> KeyMap a) -> Maybe ((Key, a), KeyMap a)
minViewWithKeyHelp x continue = case3 Nothing leafF arrayF x
  where
    leafF k v = Just ((k, v), continue Empty)
    arrayF bm arr =
      case bitmapToList bm of
        [] -> error ("Bitmap " ++ show bm ++ "has no bits set in 'minViewWithKeyHelp', this violates the non-empty bitmap invariant.")
        (i : _) -> minViewWithKeyHelp (index arr slicepoint) (continue . largeSide i bmMinusi slicepoint arr)
          where
            slicepoint = 0
            bmMinusi = clearBit bm i

minViewWithKey :: KeyMap a -> Maybe ((Key, a), KeyMap a)
minViewWithKey km = minViewWithKeyHelp km id

-- | The view of the KeyMap of the largestKey and its value, and the map that results from removing that Leaf.
maxViewWithKeyHelp :: KeyMap a -> (KeyMap a -> KeyMap a) -> Maybe ((Key, a), KeyMap a)
maxViewWithKeyHelp x continue = case3 Nothing leafF arrayF x
  where
    leafF k v = Just ((k, v), continue Empty)
    arrayF bm arr = maxViewWithKeyHelp (index arr slicepoint) (continue . smallSide i bmMinusi slicepoint arr)
      where
        slicepoint = (isize arr - 1)
        seglist = bitmapToList bm
        i = last seglist
        bmMinusi = clearBit bm i

maxViewWithKey :: KeyMap a -> Maybe ((Key, a), KeyMap a)
maxViewWithKey km = maxViewWithKeyHelp km id

-- ==========================================================
-- Split a KeyMap into pieces according to different criteria
-- These functins are usefull for divide and conquer algorithms.

-- | Breaks a KeyMap into three parts, Uses two continuations: smallC and largeC
--   which encode how to build the larger answer from a smaller one.
splitHelp2 :: Path -> Key -> KeyMap u -> (KeyMap u -> KeyMap u) -> (KeyMap u -> KeyMap u) -> (KeyMap u, Maybe u, KeyMap u)
splitHelp2 path key x smallC largeC = case3 emptyC leafF arrayF x
  where
    emptyC = (smallC Empty, Nothing, largeC Empty)
    leafF k u = case compare k key of
      EQ -> (smallC Empty, Just u, largeC Empty)
      LT -> (smallC (Leaf k u), Nothing, largeC Empty)
      GT -> (smallC Empty, Nothing, largeC (Leaf k u))
    arrayF bm arr = case path of
      [] -> (smallC Empty, Nothing, largeC Empty)
      (i : is) ->
        let (bmsmall, found, bmlarge) = splitBitmap bm i
            splicepoint = indexFromSegment bm i
         in if found
              then
                splitHelp2
                  is
                  key
                  (index arr splicepoint)
                  (smallC . smallSide i bmsmall splicepoint arr)
                  (largeC . largeSide i bmlarge splicepoint arr)
              else
                let smaller = buildKeyMap bmsmall (slice 0 (splicepoint - 1) arr)
                    larger = buildKeyMap bmlarge (slice splicepoint (isize arr - 1) arr)
                 in (smallC smaller, Nothing, largeC larger)

-- | return (smaller than 'key', has key?, greater than 'key')
splitLookup :: Key -> KeyMap u -> (KeyMap u, Maybe u, KeyMap u)
splitLookup key x = splitHelp2 (keyPath key) key x id id

smallSide :: Int -> Bitmap -> Int -> PArray (KeyMap a1) -> KeyMap a1 -> KeyMap a1
smallSide _i bm point arr Empty = buildKeyMap bm (slice 0 (point - 1) arr)
smallSide i bm point arr x = buildKeyMap (setBit bm i) (lowSlice point arr x)

largeSide :: Int -> Bitmap -> Int -> PArray (KeyMap a1) -> KeyMap a1 -> KeyMap a1
largeSide _i bm point arr Empty = buildKeyMap bm (slice (point + 1) (isize arr - 1) arr)
largeSide i bm point arr x = buildKeyMap (setBit bm i) (highSlice point arr x)

-- ==================================================================================
-- Given a Key, Split a KeyMap into a least upper bound on the Key and everything else
-- greater than the key. Particularly usefull when computing things that involve
-- spliting a KeyMap into pieces.

-- | Find the smallest key <= 'key', and a KeyMap of everything bigger than 'key'
lub :: Key -> KeyMap v -> Maybe ((Key, v), KeyMap v)
lub key hm =
  case splitLookup key hm of
    (_, Just v, Empty) -> Just ((key, v), Empty)
    (_, Just v, hm2) -> Just ((key, v), hm2)
    (_, Nothing, hm1) -> minViewWithKey hm1

-- ==========================================
-- Operations on Bits and Bitmaps

-- | Check if two the two arguments are the same value.  N.B. This
-- function might give false negatives (due to GC moving objects.)
ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y ==# 1#)
{-# INLINE ptrEq #-}

maxChildren :: Int
maxChildren = 1 `unsafeShiftL` bitsPerSegment
{-# INLINE maxChildren #-}

subkeyMask :: Bitmap
subkeyMask = 1 `unsafeShiftL` bitsPerSegment - 1
{-# INLINE subkeyMask #-}

sparseIndex :: Bitmap -> Bitmap -> Int
sparseIndex b m = popCount (b .&. (m - 1))
{-# INLINE sparseIndex #-}

-- | Create a 'BitmapIndexed' or 'Full' or 'One' or 'Two' node depending on the size of 'arr'
buildKeyMap :: Bitmap -> PArray (KeyMap v) -> KeyMap v
buildKeyMap _ arr | isize arr == 0 = Empty
buildKeyMap b arr | isize arr == 1 =
  case (index arr 0, bitmapToList b) of
    (x@(Leaf _ _), _) -> x
    (x, i : _) -> One i x
    (_, []) -> error ("Bitmap " ++ show b ++ "has no bits set in 'buildKeyMap', this violates the bitmap invariant.")
buildKeyMap b arr | isize arr == 2 = Two b (index arr 0) (index arr 1)
buildKeyMap b arr
  | b == fullNodeMask = Full arr
  | otherwise = BitmapIndexed b arr
{-# INLINE buildKeyMap #-}

-- | Split a (KeyMap v) into three logical cases that need to be handled
--    1) The Empty KeyMap
--    2) A Leaf
--    3) A Bitmap and an (PArray (KeyMap v)) (logically handles One, Two, BitmapIndexed and Full)
--       This maintains the bitmap invariant that in the 'arrayF' case the bitmap has the same number
--       of bits set, as the size of the array.
--  In some way, this function is the flip-side of 'buildKeyMap'
case3 :: ans -> (Key -> t -> ans) -> (Bitmap -> PArray (KeyMap t) -> ans) -> KeyMap t -> ans
case3 emptyC leafF arrayF km =
  case km of
    Empty -> emptyC
    (Leaf k v) -> leafF k v
    (One i x) -> arrayF (setBits [i]) (array1 x)
    (Two bm x y) -> arrayF bm (array2 x y)
    (BitmapIndexed bm arr) -> arrayF bm arr
    (Full arr) -> arrayF fullNodeMask arr
{-# INLINE case3 #-}

-- | A bitmask with the 'bitsPerSegment' least significant bits set.
fullNodeMask :: Bitmap
fullNodeMask = complement (complement 0 `unsafeShiftL` maxChildren)
{-# INLINE fullNodeMask #-}

setBits :: [Int] -> Bitmap
setBits xs = foldl' setBit 0 xs

oneBits :: Bitmap
oneBits = (complement (zeroBits :: Word64))

-- | Get the 'ith' element from a Bitmap
ith :: Bitmap -> Int -> Int
ith bmap i = (bitmapToList bmap !! i)

-- | A Bitmap represents a set. Split it into 3 parts (set1,present,set2)
--   where 'set1' is all elements in 'bm' less than 'i'
--         'present' is if 'i' is in the set 'bm'
--         'set2' is all elements in 'bm' greater than 'i'
--   We do this by using the precomputed masks: lessMasks, greaterMasks
splitBitmap :: Bitmap -> Int -> (Bitmap, Bool, Bitmap)
splitBitmap bm i = (bm .&. (index lessMasks i), testBit bm i, bm .&. (index greaterMasks i))

{-
mask            bits set     formula

at position i=0
[0,0,0,0,0]     []           [0 .. i-1]
[1,1,1,1,0]     [1,2,3,4]    [i+1 .. 4]

at position i=1
[0,0,0,0,1]     [0]
[1,1,1,0,0]     [2,3,4]

at position i=2
[0,0,0,1,1]     [0,1]
[1,1,0,0,0]     [3,4]

at position i=3
[0,0,1,1,1]     [0,1,2]
[1,0,0,0,0]     [4]

at position i=4
[0,1,1,1,1]     [0,1,2,3]
[0,0,0,0,0]     []
-}

lessMasks, greaterMasks :: PArray Bitmap
lessMasks = fromlist [setBits [0 .. i - 1] | i <- [0 .. 63]]
greaterMasks = fromlist [setBits [i + 1 .. 63] | i <- [0 .. 63]]

testsplitBitmap :: Int -> ([Int], Bool, [Int])
testsplitBitmap i = (bitmapToList l, b, bitmapToList g)
  where
    (l, b, g) = splitBitmap (complement (zeroBits :: Word64)) i

-- =======================================================================
-- Operations to make new arrays out off old ones with small changes
-- =======================================================================

-- | /O(n)/ Make a copy of an Array that removes the 'i'th element. Decreasing the size by 1.
remove :: PArray a -> Int -> PArray a
remove arr i =
  if i < 0 || i > n
    then error ("index out of bounds in 'remove' " ++ show i ++ " not in range (0," ++ show (isize arr - 1) ++ ")")
    else fst (withMutArray n action)
  where
    n = (isize arr) - 1
    action marr = do
      mcopy marr 0 arr 0 i
      mcopy marr i arr (i + 1) (n - i)
{-# INLINE remove #-}

-- | /O(n)/ Overwrite the element at the given position in this array,
update :: PArray t -> Int -> t -> PArray t
update arr i _
  | i < 0 || i >= (isize arr) =
    error ("index out of bounds in 'update' " ++ show i ++ " not in range (0," ++ show (isize arr - 1) ++ ")")
update arr i t = fst (withMutArray size1 action)
  where
    size1 = isize arr
    action marr = do
      mcopy marr 0 arr 0 i
      mwrite marr i t
      mcopy marr (i + 1) arr (i + 1) (size1 - (i + 1))
{-# INLINE update #-}

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insertM :: PArray e -> Int -> e -> ST s (PArray e)
insertM ary idx b
  | idx < 0 || idx > counter = error ("Bounds check in 'insertAt' " ++ show idx ++ " not in range 0.." ++ show (counter))
  | otherwise = do
    mary <- mnew (counter + 1)
    mcopy mary 0 ary 0 idx
    mwrite mary idx b
    mcopy mary (idx + 1) ary idx (counter - idx)
    mfreeze mary
  where
    !counter = isize ary
{-# INLINE insertM #-}

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insertAt :: PArray e -> Int -> e -> PArray e
insertAt arr idx b = runST (insertM arr idx b)
{-# INLINE insertAt #-}

-- | Create a new Array of size 'n' filled with objects 'a'
arrayOf :: Int -> a -> PArray a
arrayOf n a = runST $ do
  marr <- mnew n
  let loop i
        | i < n = mwrite marr i a >> loop (i + 1)
        | otherwise = pure ()
  loop 0
  arr <- mfreeze marr
  pure arr
{-# INLINE arrayOf #-}

-- | Extract a slice from an array
slice :: Int -> Int -> PArray a -> PArray a
slice 0 hi arr | hi == (isize arr - 1) = arr
slice lo hi arr = fst (withMutArray size action)
  where
    size = max (hi - lo + 1) 0
    action marr = mcopy marr 0 arr lo size
{-# INLINE slice #-}

-- ========================================================================
--The functions lowSlice and highSlice, split an array into two arrays
-- which share different variations of the value of the index 'slicepoint'.
-- arr= [2,5,3,6,7,8,45,6,3]  let the slicepoint be index 3 (with value 6).
--             ^ slicepoint at index 3
-- Then  lowSlice 3 arr (f 6) =  [2,5,3,f 6]
-- and   highSlice 3 arr (g 6) =       [g 6,7,8,45,6,3]

-- | Extract a slice (of size 'n') from 'arr', then put 'x' at index 'n'
--   The total size of the resulting array will be (n+1), and indices less than (n+1) are the same
--   as in the original 'arr'. if 'n' is too large or too small (negative) for the array, 'n' is
--   adjusted to copy everything (too large) or nothing (too small).
lowSlice :: Int -> PArray a -> a -> PArray a
lowSlice slicepoint arr x = fst (withMutArray (m + 1) action)
  where
    m = min (max slicepoint 0) (isize arr) -- if slicepoint<0 then copy zero things, if slicepoint>(isize arr) then copy everything
    action marr =
      mcopy marr 0 arr 0 m
        >> mwrite marr m x

-- | Extract a slice (of size 'slicepoint') from 'arr'. Put 'x' at index '0' in the new slice
--   The total size of the resulting array will be (isize arr - m + 1), and indices greater than 'slicepoint' copied
--   to the new slice at indices [1..(isize arr)]. if 'slicepoint' is too large or too small (negative) for the array,
--   'slicepoint' is adjusted to copy slicepointothing (too large) or everything (too small).
highSlice :: Int -> PArray a -> a -> PArray a
highSlice slicepoint arr x = fst (withMutArray (isize arr - m + 1) action)
  where
    m = min (max (slicepoint + 1) 0) (isize arr) -- if slicepoint<0 then copy zero things, if slicepoint>(isize arr) then copy everything
    action marr =
      mwrite marr 0 x
        >> mcopy marr 1 arr m (isize arr - m)

arrayFromBitmap :: Bitmap -> (Int -> a) -> PArray a
arrayFromBitmap bm f = fst (withMutArray (popCount bm) (loop 0))
  where
    loop n _marr | n >= 64 = pure ()
    loop n marr =
      if testBit bm n
        then mwrite marr (indexFromSegment bm n) (f n) >> loop (n + 1) marr
        else loop (n + 1) marr
{-# INLINE arrayFromBitmap #-}

-- ======================================================================================
-- Helper functions for Pretty Printers

newtype PrettyAnn = Width Int

type Ann = [PrettyAnn]

type PDoc = Doc Ann

class PrettyA t where
  prettyA :: t -> PDoc

instance PrettyA Int where
  prettyA = ppInt

instance PrettyA Word64 where
  prettyA = ppWord64

instance PrettyA v => PrettyA (KeyMap v) where
  prettyA km = ppKeyMap ppKey prettyA km

ppWord64 :: Word64 -> Doc a
ppWord64 = viaShow

ppInt :: Int -> Doc a
ppInt = viaShow

text :: Text -> Doc ann
text = pretty

isEmptyDoc :: Doc ann -> Bool
isEmptyDoc Pretty.Empty = True
isEmptyDoc _ = False

-- | ppSexp x [w,y,z] --> (x w y z)
ppSexp :: Text -> [PDoc] -> PDoc
ppSexp con = ppSexp' (text con)

ppSexp' :: PDoc -> [PDoc] -> PDoc
ppSexp' con fields =
  group $
    flatAlt
      (hang 2 (encloseSep lparen rparen space docs))
      (encloseSep lparen rparen space docs)
  where
    docs = if isEmptyDoc con then fields else con : fields

-- | Vertical layout with commas aligned on the left hand side
puncLeft :: Doc ann -> [Doc ann] -> Doc ann -> Doc ann -> Doc ann
puncLeft open [] _ close = hsep [open, close]
puncLeft open [x] _ close = hsep [open, x, close]
puncLeft open (x : xs) coma close = align (sep ((open <+> x) : help xs))
  where
    help [] = mempty
    help [y] = [hsep [coma, y, close]]
    help (y : ys) = (coma <+> y) : help ys

ppList :: (x -> Doc ann) -> [x] -> Doc ann
ppList p xs =
  group $
    flatAlt
      (puncLeft lbracket (map p xs) comma rbracket)
      (encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) (map p xs))

-- | x == y
equate :: Doc a -> Doc a -> Doc a
equate x y = group (flatAlt (hang 2 (sep [x <+> text "=", y])) (hsep [x, text "=", y]))

ppArray :: (a -> PDoc) -> PArray a -> PDoc
ppArray f arr = ppList f (tolist arr)

-- ====================================
-- Pretty Printer for KeyMap

oneList :: KeyMap v -> [Int] -> (KeyMap v, [Int])
oneList (One i x) is = oneList x (i : is)
oneList x is = (x, reverse is)

ppKey :: Key -> PDoc
ppKey (Key _w0 _ _ _) = ppWord64 _w0

-- ppKey k = viaShow k

ppBitmap :: Word64 -> PDoc
ppBitmap x = text (pack (showBM x))

ppKeyMap :: (Key -> PDoc) -> (v -> PDoc) -> KeyMap v -> PDoc
ppKeyMap k p (Leaf key v) = ppSexp "L" [k key, p v]
ppKeyMap _ _ Empty = text "E"
ppKeyMap k p (m@(One _ _)) = ppSexp "O" [text (pack (show is)), ppKeyMap k p x]
  where
    (x, is) = oneList m []
ppKeyMap k p (Two x m1 m2) = ppSexp "T" [ppBitmap x, ppKeyMap k p m1, ppKeyMap k p m2]
ppKeyMap k p (BitmapIndexed x arr) = ppSexp "B" [ppList q (zip (bitmapToList x) (tolist arr))]
  where
    q (i, a) = ppInt i <+> ppKeyMap k p a
ppKeyMap k p (Full arr) = ppSexp "F" [ppList q (zip (bitmapToList fullNodeMask) (tolist arr))]
  where
    q (i, a) = ppInt i <+> ppKeyMap k p a

instance PrettyA v => Show (KeyMap v) where
  show x = show (ppKeyMap ppKey prettyA x)
  showList xs x = unlines (map (\y -> "\n" ++ show (ppKeyMap ppKey prettyA y)) xs) ++ x
