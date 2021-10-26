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
import Data.Compact.Class
import Data.Foldable (foldl')
import Data.List (sortBy)
import qualified Data.Map as Map
import qualified Data.Primitive.Array as PA
import Data.Primitive.SmallArray ()
import qualified Data.Primitive.SmallArray as Small
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Word (Word64)
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#, (==#))
import Prettyprinter
import qualified Prettyprinter.Internal as Pretty
import System.Random (RandomGen, genWord64, mkStdGen)

type PArray = Small.SmallArray

-- | Show 'n' as a binary number with most significant bits on the left.
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

genKey :: RandomGen b => b -> (Key, b)
genKey g = (Key w0 w1 w2 w3, g4)
  where
    (w0, g1) = genWord64 g
    (w1, g2) = genWord64 g1
    (w2, g3) = genWord64 g2
    (w3, g4) = genWord64 g3

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
    loop i ans = if testBit bm i then loop (i -1) (i : ans) else loop (i -1) ans

instance HeapWords Key where
  heapWords (Key _ _ _ _) = 5

-- ===============================================================

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

instance Eq v => Eq (KeyMap v) where
  (==) x y = toList x == toList y

heapAdd :: HeapWords a => a -> Int -> Int
heapAdd x ans = heapWords x + ans

heapPlus :: HeapWords a => Int -> a -> Int
heapPlus ans x = heapWords x + ans

instance HeapWords t => HeapWords (PA.Array t) where
  heapWords arr = foldl' heapPlus (2 + isize arr) arr

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

insertWithKey' :: (Key -> v -> v -> v) -> Path -> Key -> v -> KeyMap v -> KeyMap v
insertWithKey' combine path k x kmap = go 0 kmap
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
         in bitmapIndexedOrFull (bmap .|. (setBit 0 j)) arr'
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
         in bitmapIndexedOrFull (bmap .|. (setBit 0 j)) arr'
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
insertWithKey f k v m = insertWithKey' f (keyPath k) k v m

insertWith :: (t -> t -> t) -> Key -> t -> KeyMap t -> KeyMap t
insertWith f k v m = insertWithKey' (\_ key val -> f key val) (keyPath k) k v m

insert :: Key -> v -> KeyMap v -> KeyMap v
insert k v m = insertWithKey' (\_key new _old -> new) (keyPath k) k v m

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

-- | Delete the Key encoded in the Path from the KeyMap
delete' :: Path -> Key -> KeyMap v -> KeyMap v
delete' [] _key hm = hm -- Removing a bogus key, leaves 'hm' unchanged
delete' _ _key Empty = Empty
delete' _ k (hm@(Leaf k2 _)) = if k == k2 then Empty else hm
delete' (i : is) k (hm@(One j x)) = if i == j then oneE j (delete' is k x) else hm
delete' (i : is) k (hm@(Two bmap x y)) =
  if testBit bmap i
    then twoE bmap (delete' is k x) (delete' is k y)
    else hm
delete' (i : is) k (hm@(BitmapIndexed bmap arr)) =
  if testBit bmap i
    then
      let m = setBit 0 i
          j = sparseIndex bmap m
          result = delete' is k (index arr j)
       in -- Consume an upwards floating Empty by removing that element from the array
          case result of
            Empty -> bitmapE (clearBit bmap i) (remove arr j)
            _ -> BitmapIndexed bmap (update arr j result)
    else hm
delete' (i : is) k (Full arr) =
  let m = setBit 0 i
      j = sparseIndex fullNodeMask m
      result = delete' is k (index arr j)
   in -- Consume an upwards floating Empty by removing that element from the array
      case result of
        Empty -> BitmapIndexed (clearBit fullNodeMask i) (remove arr j)
        _ -> Full (update arr j result)

delete :: Key -> KeyMap v -> KeyMap v
delete k hm = delete' (keyPath k) k hm

-- One of the invariants is that no Empty ever appears in any of the other
-- constructors of KeyMap.  So we make "smart" constructors that remove Empty
-- if it ever occurrs. This is necessary since 'delete' can turn a subtree
-- into Empty. The strategy is to float 'Empty' up the tree, until it can be
-- 'remove'd from one of the constructors with Array like components (One, Two, BitmapInded, Full).

-- Float Empty up over One
oneE :: Int -> KeyMap v -> KeyMap v
oneE _ Empty = Empty
oneE i x = One i x

-- Float Empty's up over Two
twoE :: Bitmap -> KeyMap v -> KeyMap v -> KeyMap v
twoE _ Empty Empty = Empty
twoE bmap x Empty = oneE (ith bmap 0) x
twoE bmap Empty x = oneE (ith bmap 1) x
twoE bmap x y = Two bmap x y

-- Float Empty's up over BitmpIndexed, Note that if the size of the arr
-- becomes 2, then rebuild with Two rather than BitmapIndexed
bitmapE :: Bitmap -> PArray (KeyMap v) -> KeyMap v
bitmapE bmap arr | isize arr == 2 = twoE bmap (index arr 0) (index arr 1)
bitmapE bmap arr = bitmapIndexedOrFull bmap arr

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
foldWithDescKey accum !ans0 (BitmapIndexed _ arr) = loop ans0 (n -1)
  where
    n = isize arr
    loop !ans i | i < 0 = ans
    loop !ans i = loop (foldWithDescKey accum ans (index arr i)) (i -1)
foldWithDescKey accum !ans0 (Full arr) = loop ans0 (n -1)
  where
    n = isize arr
    loop !ans i | i < 0 = ans
    loop !ans i = loop (foldWithDescKey accum ans (index arr i)) (i -1)

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

-- ==========================================================
-- Split a KeyMap into 3 parts

-- | return (smaller than 'key', has key?, greater than 'key')
splitKeyMap :: Path -> Key -> KeyMap v -> (KeyMap v, Maybe v, KeyMap v)
splitKeyMap [] _key hm = (hm, Nothing, Empty)
splitKeyMap (i : is) key hm =
  case splitBySegment i hm of
    (less, x, greater) ->
      case x of
        Empty -> (build less, Nothing, build greater)
        (Leaf k v) -> (build less, if key == k then (Just v) else Nothing, build greater)
        other -> (reconstruct i less less1, ans, reconstruct i greater greater1)
          where
            (less1, ans, greater1) = splitKeyMap is key other

splitBySegment :: Segment -> KeyMap v -> ([(Segment, KeyMap v)], KeyMap v, [(Segment, KeyMap v)])
splitBySegment i _x | i < 0 = ([], Empty, [])
splitBySegment i _x | i > intSize = ([], Empty, [])
splitBySegment _ Empty = ([], Empty, [])
splitBySegment _ (x@(Leaf _ _)) = ([], x, [])
splitBySegment i (x@(One j y)) =
  case compare i j of
    LT -> ([], Empty, [(i, x)])
    EQ -> ([], y, [])
    GT -> ([(i, x)], Empty, [])
splitBySegment i (Two bmap l h) = splitArrAtSeg i bmap (fromlist [l, h])
splitBySegment i (BitmapIndexed bmap arr) = splitArrAtSeg i bmap arr
splitBySegment i (Full arr) = splitArrAtSeg i fullNodeMask arr

-- | Split an PArray at a particular Segment.
splitArrAtSeg :: Segment -> Bitmap -> PArray (KeyMap v) -> ([(Int, KeyMap v)], KeyMap v, [(Int, KeyMap v)])
splitArrAtSeg i bmap arr = (takeWhile smaller ps, match, dropWhile tooSmall ps)
  where
    ps = zip (bitmapToList bmap) (tolist arr)
    smaller (j, _) = j < i
    tooSmall (j, _) = j <= i
    same (j, _) = i == j
    match = case filter same ps of
      [] -> Empty
      ((_, x) : _) -> x

-- | reconstruct a KeyMap from list of previous Segments, and a single KeyMap from the next Segment
reconstruct :: Segment -> [(Segment, KeyMap v)] -> KeyMap v -> KeyMap v
reconstruct _ xs Empty = build xs
reconstruct seg xs x = build (insertAscending (seg, x) xs)

-- | insert a Segment pair in ascending order of Segments, Keep it sorted.
insertAscending :: (Segment, KeyMap v) -> [(Segment, KeyMap v)] -> [(Segment, KeyMap v)]
insertAscending (i, x) [] = [(i, x)]
insertAscending (i, x) (ws@((y@(j, _)) : ys)) =
  case compare i j of
    LT -> (i, x) : ws
    GT -> y : insertAscending (i, x) ys
    EQ -> (i, x) : ys -- We know that the Segement i should never appear in the list

-- | Build a KeyMap out of a list of Segment pairs.
build :: [(Segment, KeyMap v)] -> KeyMap v
build [] = Empty
build [(_, x)] = x
build [(j, x), (k, y)] = Two (setBits [j, k]) x y
build ps = bitmapIndexedOrFull (setBits (map fst ps)) (fromlist (map snd ps))

testSplit2 :: Int -> IO ()
testSplit2 i = putStrLn (unlines [show hm, " ", show pathx, " ", show a, " ", show b, " ", show c])
  where
    keys = makeKeys 99 1000
    ps = zip (take 12 keys) [0 ..]
    hm :: KeyMap Int
    hm = fromList ps
    pathx = (keyPath (keys !! i))
    (a, b, c) = splitKeyMap pathx (keys !! i) hm

-- =========================================================
-- UnionWith

-- | Make an array of size 1, with 'x' stored at index 0.
array1 :: a -> PArray a
array1 x = fst (withMutArray 1 (\marr -> mwrite marr 0 x))

-- | Make an array of size 2, with 'x' stored at index 0.
array2 :: a -> a -> PArray a
array2 x y = fst (withMutArray 2 (\marr -> mwrite marr 0 x >> mwrite marr 1 y))

-- | Turn a (KeyMap v) into a BitMap and an PArray (KeyMap v)
toSegArray :: Int -> KeyMap v -> (Bitmap, PArray (KeyMap v))
toSegArray _ Empty = error ("not possible: Empty in toSegArray")
toSegArray n (l@(Leaf k _)) = (setBit 0 (keyPath k !! n), array1 l)
toSegArray _ (One i x) = (setBits [i], array1 x)
toSegArray _ (Two bm x y) = (bm, array2 x y)
toSegArray _ (BitmapIndexed bm arr) = (bm, arr)
toSegArray _ (Full arr) = (fullNodeMask, arr)

union2 :: Int -> (Key -> v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
union2 _ _ Empty Empty = Empty
union2 _ _ x Empty = x
union2 _ _ Empty y = y
union2 n combine x y = bitmapIndexedOrFull bmap arrAll
  where
    (bmx, arrx) = toSegArray n x
    (bmy, arry) = toSegArray n y
    (bmap, arrAll) = mergeArrayWithBitMaps union3 bmx arrx bmy arry
    union3 (Leaf k1 v1) (Leaf k2 v2) | k1 == k2 = Leaf k1 (combine k1 v1 v2)
    union3 a b = union2 (n + 1) combine a b

mergeArrayWithBitMaps :: (v -> v -> v) -> Bitmap -> PArray v -> Bitmap -> PArray v -> (Bitmap, PArray v)
mergeArrayWithBitMaps combine bm1 arr1 bm2 arr2 = (bmBoth, fst (withMutArray size action))
  where
    bmBoth = bm1 .|. bm2
    size = popCount bmBoth
    segments = bitmapToList bmBoth
    action marr3 = (loop segments)
      where
        loop [] = pure ()
        loop (i : is) = do
          let j1 = (indexFromSegment bm1 i)
              j2 = (indexFromSegment bm2 i)
              j3 = indexFromSegment bmBoth i
          case (testBit bm1 i, testBit bm2 i) of
            (True, True) -> mwrite marr3 j3 (combine (index arr1 j1) (index arr2 j2))
            (True, False) -> mwrite marr3 j3 (index arr1 j1)
            (False, True) -> mwrite marr3 j3 (index arr2 j2)
            (False, False) -> pure ()
          loop is

bmapA, bmapB :: Bitmap
bmapA = setBits [0, 3, 6, 11, 15]
bmapB = setBits [1, 3, 5, 9, 11, 14]

arrA, arrB :: PArray Int
arrA = fromlist [0, 3, 6, 11, 15]
arrB = fromlist [1, 3, 5, 9, 11, 14]

testmergeBm :: (Bitmap, PArray Int)
testmergeBm = mergeArrayWithBitMaps (+) bmapA arrA bmapB arrB

unionWithKey :: (Key -> v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWithKey comb x y = union2 0 comb x y

unionWith :: (v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWith comb x y = union2 0 (\_k a b -> comb a b) x y

hm10, hm11, hm12 :: KeyMap Int
hm10 = fromList (take 5 pairs)
hm11 = fromList (take 5 (drop 4 pairs))
hm12 = unionWith (+) hm10 hm11

-- ===========================================================
-- Maximum and Minimum Key

-- | Get the smallest key, NOT the smallest value
getMin :: KeyMap v -> Maybe (Key, v)
getMin Empty = Nothing
getMin (Leaf k v) = Just (k, v)
getMin (One _ x) = getMin x
getMin (Two _ x _) = getMin x
getMin (BitmapIndexed _ arr) = getMin (index arr 0)
getMin (Full arr) = getMin (index arr 0)

-- | Get the largest key, NOT the largest value
getMax :: KeyMap v -> Maybe (Key, v)
getMax Empty = Nothing
getMax (Leaf k v) = Just (k, v)
getMax (One _ x) = getMax x
getMax (Two _ _ y) = getMax y
getMax (BitmapIndexed _ arr) = getMax (index arr (isize arr - 1))
getMax (Full arr) = getMax (index arr (isize arr - 1))

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
    (Just (k1, v1, h1), Just (k2, _, h2)) ->
      case maxMinOf h1 h2 of
        Just k3 -> leapfrog k3 h1 h2 (if k1 == k2 then insert k1 v1 ans else ans)
        Nothing -> (if k1 == k2 then insert k1 v1 ans else ans)
    _ -> ans

-- | Get the larger of the two min keys of 'x' and 'y'. Nothing if either is Empty.
maxMinOf :: KeyMap v1 -> KeyMap v2 -> Maybe Key
maxMinOf x y = case (getMin x, getMin y) of
  (Just (k1, _), Just (k2, _)) -> Just (max k1 k2)
  _ -> Nothing

-- ==================================================================================
-- Given a Key, Split a KeyMap into a least upper bound on the Key and everything else
-- greater than the key. Particularly usefull when computing things that involve the
-- intersection over the Key's in two KeyMaps. See eapfrog above for an example.

-- | Find the smallest key <= 'key', and a KeyMap of everything bigger than 'key'
lub :: Key -> KeyMap v -> Maybe (Key, v, KeyMap v)
lub key hm =
  case splitKeyMap (keyPath key) key hm of
    (_, Just v, Empty) -> Just (key, v, Empty)
    (_, Just v, hm2) -> Just (key, v, hm2)
    (_, Nothing, hm1) ->
      case getMin hm1 of
        Just (k, v) -> Just (k, v, hm1)
        Nothing -> Nothing

-- | The smallest (key and value) greater-or-equal to 'key', plus a new KeyMap
--   that includes everything greater than that lub key.
mylub :: Key -> KeyMap v -> Maybe (Key, v, KeyMap v)
mylub key mp = go (keyPath key) mp
  where
    go [] _ = Nothing
    go _ Empty = Nothing
    go _ (Leaf k v) = if k >= key then Just (k, v, Empty) else Nothing
    go (i : is) (One j x) =
      case compare i j of
        EQ -> go is x
        LT -> go is x
        GT -> Nothing
    go path (Two bm x y) = mylubArray path bm (fromlist [x, y])
    go path (BitmapIndexed bm arr) = mylubArray path bm arr
    go path (Full arr) = mylubArray path fullNodeMask arr
    mylubArray [] _ _ = Nothing
    mylubArray (i : is) bm arr =
      case findFirstLargerSegment key arr i bm of
        Nothing -> Nothing
        Just (n, j, newbm) ->
          case go is (index arr n) of
            Nothing -> Nothing
            Just (k, v, Empty) ->
              -- This case occurs only when (index arr n) is a (Leaf kk v)
              if k == key -- And kk >= key, but the two cases: 1) kk=key and kk>key differ
                then
                  let arr2 = (slice (n + 1) (isize arr - 1) arr)
                   in Just (k, v, bitmapIndexedOrFull (clearBit newbm j) arr2)
                else Just (k, v, bitmapIndexedOrFull newbm (suffixAfterIndex n (Leaf k v) arr))
            Just (k, v, keymap) -> Just (k, v, bitmapIndexedOrFull newbm (suffixAfterIndex n keymap arr))

-- | 'seg' is the current Segment in the Path of 'key'. 'bm' is the set of Segments that
--   are stored in 'arr'. We are looking for the index, 'i', of the first KeyMap in 'arr' where
--   there is some key that is greater than or equal to 'key'. Since 'j' is the first segment
--   of things stored at index 'i', we can skip any index whose first segment 'j' is less than 'seg'.
findFirstLargerSegment :: Key -> PArray (KeyMap v) -> Segment -> Bitmap -> Maybe (Int, Segment, Bitmap)
findFirstLargerSegment key arr seg bm
  | not (isize arr == length segmentsFromArray) = error ("bitmp does not describe array")
  | otherwise = loop 0 bm segmentsFromArray
  where
    segmentsFromArray = (bitmapToList bm)
    loop _ _ [] = Nothing
    loop i b (j : js) =
      if (j < seg)
        then loop (i + 1) (clearBit b j) js
        else case getMax (index arr i) of
          Nothing -> loop (i + 1) (clearBit b j) js
          Just (k, _) ->
            if k < key
              then loop (i + 1) (clearBit b j) js
              else Just (i, j, b)

testlub :: [(Int, Bool)]
testlub = [(i, mylub key kmap == lub key kmap) | i <- [0 .. 55], key <- [bpairs !! i]]
  where
    kmap = fromList (take 50 pairs)

kmap12 :: KeyMap Int
kmap12 = fromList (take 12 pairs)

testIntersect :: KeyMap Int
testIntersect = intersect h1x h2x

h1x, h2x :: KeyMap Int
h1x = fromList [pairs !! 3, pairs !! 5, pairs !! 11, pairs !! 6, pairs !! 4]
h2x = fromList [pairs !! 3, pairs !! 7, pairs !! 4, pairs !! 6, pairs !! 8]

-- =========================================================

-- | Domain restrict 'hkm' to those Keys found in 's'. This algorithm
--   assumes the set 's' is small compared to 'hm'.
domainRestrict :: KeyMap v -> Set Key -> KeyMap v
domainRestrict hm s = Set.foldl' accum Empty s
  where
    accum ans key =
      case lookupHM key hm of
        Nothing -> ans
        Just v -> insert key v ans

hmdr :: KeyMap Int
hmdr = fromList (take 10 pairs)

set :: Set Key
set = Set.fromList [bpairs !! 3, bpairs !! 8, bpairs !! 20]

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
bitmapIndexedOrFull :: Bitmap -> PArray (KeyMap v) -> KeyMap v
bitmapIndexedOrFull _ arr | isize arr == 0 = Empty
bitmapIndexedOrFull b arr | isize arr == 1 = One (head (bitmapToList b)) (index arr 0)
bitmapIndexedOrFull b arr | isize arr == 2 = Two b (index arr 0) (index arr 1)
bitmapIndexedOrFull b arr
  | b == fullNodeMask = Full arr
  | otherwise = BitmapIndexed b arr
{-# INLINE bitmapIndexedOrFull #-}

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
lessMasks = fromlist [setBits [0 .. i -1] | i <- [0 .. 63]]
greaterMasks = fromlist [setBits [i + 1 .. 63] | i <- [0 .. 63]]

testsplitBitmap :: Int -> ([Int], Bool, [Int])
testsplitBitmap i = (bitmapToList l, b, bitmapToList g)
  where
    (l, b, g) = splitBitmap (complement (zeroBits :: Word64)) i

-- =======================================================================
-- Operations to make new arrays out off old ones with small changes

-- | /O(n)/ Make a copy of an Array that removes the 'i'th element. Decreasing the size by 1.
remove :: PArray a -> Int -> PArray a
remove arr i =
  if i < 0 || i > n
    then error ("index out of bounds in 'remove' " ++ show i ++ " not in range (0," ++ show (isize arr -1) ++ ")")
    else fst (withMutArray n action)
  where
    n = (isize arr) - 1
    action marr = do
      mcopy marr 0 arr 0 i
      mcopy marr i arr (i + 1) (n - i)

-- | /O(n)/ Overwrite the element at the given position in this array,
update :: PArray t -> Int -> t -> PArray t
update arr i _
  | i < 0 || i >= (isize arr) =
    error ("index out of bounds in 'update' " ++ show i ++ " not in range (0," ++ show (isize arr -1) ++ ")")
update arr i t = fst (withMutArray size1 action)
  where
    size1 = isize arr
    action marr = do
      mcopy marr 0 arr 0 i
      mwrite marr i t
      mcopy marr (i + 1) arr (i + 1) (size1 - (i + 1))

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

-- |  /O(n)/ Make a new array which has a repacement 'v' for index 'n', and copies the values
--   at indices greater than 'n'. The values at indices before 'n' are thrown away.
--   The size of the output, is n smaller than the size of the input.
suffixAfterIndex :: Int -> v -> PArray v -> PArray v
suffixAfterIndex n v arr = fst (withMutArray size action)
  where
    size = ((isize arr) - n)
    action marr = mwrite marr 0 v >> mcopy marr 1 arr (n + 1) (size - 1)
{-# INLINE suffixAfterIndex #-}

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

-- =========================================================================

makeKeys :: Int -> Int -> [Key]
makeKeys seed cnt = loop (mkStdGen seed) cnt []
  where
    loop _g i ans | i <= 0 = ans
    loop g i ans = case genKey g of
      (key, g2) -> loop g2 (i -1) (key : ans)

testt :: Int -> IO ()
testt n = do
  let (hmap, output) = tests n
      histArr = histo hmap
  putStrLn output
  putStrLn ("histogram " ++ show (tolist histArr))

tests :: Int -> (KeyMap Int, String)
tests n =
  ( hashmap,
    unlines
      [ "bits per level = " ++ show bitsPerSegment,
        "num levels = " ++ show keyPathSize,
        "empty = " ++ show empty,
        "leaf  = " ++ show leaf,
        "one   = " ++ show one,
        "two   = " ++ show two,
        "bits  = " ++ show bit,
        "full  = " ++ show full,
        "hwords = " ++ show hwords,
        "mwords = " ++ show mwords,
        "diff   = " ++ show (hwords - mwords) ++ " %" ++ show ((hwords * 100) `div` mwords),
        "depth = " ++ show (hdepth hashmap)
      ]
  )
  where
    hashmap = fromList (take n pairs)
    mapmap = Map.fromList (take n pairs)
    (empty, one, two, leaf, bit, full) = count hashmap
    hwords = heapWords hashmap
    mwords = heapWords mapmap

count :: KeyMap v -> (Int, Int, Int, Stat Int, Stat Int, Int)
count x = go 0 x (0, 0, 0, mempty, mempty, 0)
  where
    go _ Empty (e, o, t, l, b, f) = (e + 1, o, t, l, b, f)
    go d (One _ y) (e, o, t, l, b, f) = go (1 + d) y (e, 1 + o, t, l, b, f)
    go d (Two _ z y) (e, o, t, l, b, f) = go (1 + d) y (go (1 + d) z (e, o, 1 + t, l, b, f))
    go d (Leaf _ _) (e, o, t, l, b, f) = (e, o, t, add d l, b, f)
    go d (BitmapIndexed _ arr) (e, o, t, l, b, f) =
      foldr (go (length arr + d)) (e, o, t, l, add (length arr) b, f) arr
    go d (Full arr) (e, o, t, l, b, f) = foldr (go (length arr + d)) (e, o, t, l, b, f + 1) arr

countIO :: HeapWords a => KeyMap a -> IO ()
countIO hashmap = do
  putStrLn $
    unlines
      [ "bits per level = " ++ show bitsPerSegment,
        "num levels = " ++ show keyPathSize,
        "empty = " ++ show empty,
        "leaf  = " ++ show leaf,
        "one   = " ++ show one,
        "two   = " ++ show two,
        "bits  = " ++ show bit,
        "full  = " ++ show full,
        "hwords = " ++ show hwords,
        "depth = " ++ show (hdepth hashmap),
        "histogram =" ++ show hist
      ]
  where
    (empty, one, two, leaf, bit, full) = count hashmap
    hist = histo hashmap
    hwords = heapWords hashmap

hdepth :: KeyMap v -> Int
hdepth Empty = 0
hdepth (One _ x) = 1 + hdepth x
hdepth (Leaf _ _) = 1
hdepth (BitmapIndexed _ arr) = 1 + maximum (foldr (\x ans -> hdepth x : ans) [] arr)
hdepth (Full arr) = 1 + maximum (foldr (\x ans -> hdepth x : ans) [] arr)
hdepth (Two _ x y) = 1 + max (hdepth x) (hdepth y)

increment :: (ArrayPair arr marr a, Num a) => marr s a -> Int -> ST s ()
increment marr i = do n <- mindex marr i; mwrite marr i (n + 1)

histogram :: KeyMap v -> PA.MutableArray s Int -> ST s ()
histogram Empty _ = pure ()
histogram (One _ x) marr = increment marr 1 >> histogram x marr
histogram (Leaf _ _) _ = pure ()
histogram (BitmapIndexed _ arr) marr = increment marr (isize arr -1) >> mapM_ (\x -> histogram x marr) arr
histogram (Full arr) marr = increment marr (intSize -1) >> mapM_ (\x -> histogram x marr) arr
histogram (Two _ x y) marr = increment marr 2 >> histogram x marr >> histogram y marr

histo :: KeyMap v -> PA.Array Int
histo x = fst (withMutArray intSize process)
  where
    process marr = do initialize (intSize - 1); histogram x marr
      where
        initialize n | n < 0 = pure ()
        initialize n = mwrite marr n 0 >> initialize (n -1)

bpairs :: [Key]
bpairs = makeKeys 99 1500000

-- makeKeys 3 15

pairs :: [(Key, Int)]
pairs = zip bpairs [0 ..]

-- ===================================================

data Stat n = Stat n n (Maybe n) (Maybe n)

liftM :: (t -> t -> t) -> Maybe t -> Maybe t -> Maybe t
liftM f (Just x) (Just y) = Just (f x y)
liftM _ Nothing (Just y) = Just y
liftM _ (Just x) Nothing = Just x
liftM _ Nothing Nothing = Nothing

instance (Ord n, Num n) => Semigroup (Stat n) where
  (Stat c1 s1 mx1 mn1) <> (Stat c2 s2 mx2 mn2) =
    Stat (c1 + c2) (s1 + s2) (liftM max mx1 mx2) (liftM min mn1 mn2)

instance (Ord n, Num n) => Monoid (Stat n) where
  mempty = Stat 0 0 Nothing Nothing

instance (Integral n, Show n) => Show (Stat n) where
  show (Stat c s mx mn) =
    "{count= " ++ show c ++ ", sum=" ++ show s ++ ", max=" ++ show mx
      ++ ", min="
      ++ show mn
      ++ (if c == 0 then "}" else ", avg=" ++ show (div s c) ++ "}")

add :: (Num n, Ord n) => n -> Stat n -> Stat n
add n stat = (Stat 1 n (Just n) (Just n)) <> stat

-- ====================
-- Debugging functions

bug :: Int -> IO (KeyMap Int)
bug n = do
  let ps = take n pairs -- zip (makeKeys 3 n) [0..]
      hh (k@(Key m0 m1 _ _), v) = show m0 ++ " " ++ show m1 ++ " " ++ show (keyPath k) ++ " " ++ show v
  putStrLn (unlines (map hh ps))

  -- putStrLn (show (fromList ps))
  pure (fromList ps)

try :: [(Key, Int)] -> IO ()
try ps = do
  let hh (k@(Key m0 m1 _ _), v) = show m0 ++ " " ++ show m1 ++ " " ++ show (keyPath k) ++ " " ++ show v
  putStrLn (unlines (map hh ps))
  putStrLn (show (fromList ps))

testlookup :: Int -> Int -> Bool
testlookup seed n = all ok results
  where
    ps = zip (makeKeys seed n) [0 ..]
    keymap :: KeyMap Int
    keymap = fromList ps
    results = [(i, lookupHM (fst (ps !! i)) keymap) | i <- [0 .. (n -1)]]
    ok (_, Just _) = True
    ok (i, Nothing) =
      error
        ( "testlookup failure: " ++ show i ++ "   " ++ show pair ++ "\n"
            ++ show (keyPath (fst pair))
            ++ "\n  "
            ++ show keymap
        )
      where
        pair = (ps !! i)

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
  prettyA km = ppKeyMap prettyA km

ppWord64 :: Word64 -> Doc a
ppWord64 = viaShow

ppInt :: Int -> Doc a
ppInt = viaShow

text :: Text -> Doc ann
text = pretty

isEmpty :: Doc ann -> Bool
isEmpty Pretty.Empty = True
isEmpty _ = False

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
    docs = if isEmpty con then fields else con : fields

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

ppArray :: (Indexable arr a) => (a -> PDoc) -> arr a -> PDoc
ppArray f arr = ppList f (tolist arr)

-- ====================================
-- Pretty Printer for KeyMap

ppKey :: Key -> PDoc
ppKey (Key w0 _ _ _) = ppWord64 w0

ppBitmap :: Word64 -> PDoc
ppBitmap x = text (pack (showBM x))

ppKeyMap :: (v -> PDoc) -> KeyMap v -> PDoc
ppKeyMap p (Leaf k v) = ppSexp "L" [ppKey k, p v]
ppKeyMap _ Empty = text "E"
ppKeyMap p (One x mp) = ppSexp "O" [ppInt x, ppKeyMap p mp]
ppKeyMap p (Two x m1 m2) = ppSexp "T" [ppBitmap x, ppKeyMap p m1, ppKeyMap p m2]
ppKeyMap p (BitmapIndexed x arr) = ppSexp "B" [ppList q (zip (bitmapToList x) (tolist arr))]
  where
    q (i, a) = ppInt i <+> ppKeyMap p a
ppKeyMap p (Full arr) = ppSexp "F" [ppList q (zip (bitmapToList fullNodeMask) (tolist arr))]
  where
    q (i, a) = ppInt i <+> ppKeyMap p a

instance PrettyA v => Show (KeyMap v) where
  show x = show (ppKeyMap prettyA x)
  showList xs x = unlines (map (\y -> "\n" ++ show (ppKeyMap prettyA y)) xs) ++ x

-- ====================================================================
-- Bulk insert

{- The input to bulkInsert looks like this. Each row represents one key value pair.
On the left, each column represents the bits from one Segment of the key.
On the right is the Key and 'v' as a (Leaf KeyMap 'v'). The rows are sorted by the [Segment]

([ 2,13,42,25, 3,48,19,53,21,34], (L 2551962348052371621 8))
([ 4,54,49, 6,56,12,18,32,32,41], (L 5598286061563415157 1))

([ 6, 5,40,60,58,61,31,53, 5,27], (L 7019127953809495798 4))
([ 6,51,10, 4,48,59,22,42, 2,59], (L 7839099055843585733 3))

([ 7,57, 1,12,57,50,61,15,47,47], (L 9097609470548048848 5))

([ 9,32,38,58,62,34,17,42, 0,28], (L 10963709726988699431 9))
([ 9,63,16,12,42,50,36,46,46,35], (L 11515759964265834722 2))

([10,53, 8, 5,22,24,58,25, 8,61], (L 12486253495695216508 0))

([14,13,49, 4, 7,37, 4,23, 5,28], (L 16388898632001935134 6))
([14,43,28,10,55,12,51,63,56,24], (L 16923449273545098794 7))

For each column we break the code into groups where the Segment matches
on that column. Above we have grouped the 6's, 9's and 14's together by column 1
-}

-- | Make a (KeyMap v) out of the input. Works by  focusing on a particular range of rows ('lo' .. 'hi')
--   It calls it self recursively, by chooing a smaller range, and increasing the column number 'n' by 1.
bulkInsert :: Int -> PArray (Path, KeyMap v) -> Int -> Int -> KeyMap v
bulkInsert _n arr lo hi
  | lo < 0 || lo > n || hi < 0 || hi > n =
    error ("lo or hi out of bounds (0 .. " ++ show n ++ ") lo=" ++ show lo ++ " hi=" ++ show hi)
  where
    n = isize arr - 1
bulkInsert _n arr lo hi | lo == hi = snd (index arr lo)
bulkInsert n arr lo hi = bitmapIndexedOrFull bmap (fst (withMutArray size (action 0 segmentRanges)))
  where
    (size, segments, bmap) = getBitmap n arr lo hi
    segmentRanges = ranges n arr lo hi segments
    action _j [] _marr = pure ()
    action j ((lox, hix) : more) marr = do
      mwrite marr j (bulkInsert (n + 1) arr lox hix)
      action (j + 1) more marr

-- | get the bitmap of column 'n' for the rows 'lo' to 'hi' of arr.
--   This is a set of all the segments present for that range.
getBitmap :: Int -> PArray (Path, KeyMap v) -> Int -> Int -> (Int, [Segment], Bitmap)
getBitmap n arr lo hi = (size, segments, bitmap)
  where
    accum bm (path, _) = setBit bm (path !! n)
    bitmap = foldRange accum 0 arr lo hi
    segments = bitmapToList bitmap
    size = length segments

-- | Given starting row 'i' find the last row 'j', such that column 'n' has 'val' in all rows 'i' to 'j'
--   Both 'i' and 'j' must be in the range (i .. maxi), which denote the beginning and end of the
--   of the data for the current segment.
contiguous :: Int -> Int -> Int -> Int -> PArray ([Int], b) -> Int
contiguous _n _val i _maxi _arr | i < 0 = i
contiguous _n _val i _maxi arr | i >= isize arr = isize arr - 1
contiguous _n _val i maxi _arr | i > maxi = i -1 -- Do not look outside the valid range for matching val
contiguous n val i maxi arr = if (fst (index arr i) !! n) == val then contiguous n val (i + 1) maxi arr else (i -1)

-- | compute the row ranges where the 'n' column has the same value 'val', we assume the rows are sorted
--   in ascending order, and so is the list of 'vals'
ranges :: Int -> PArray ([Int], b) -> Int -> Int -> [Int] -> [(Int, Int)]
ranges _n _arr _i _hi [] = []
ranges n arr i hi (val : vals) = (i, j) : ranges n arr (j + 1) hi vals
  where
    j = contiguous n val i hi arr

-- like foldl, except we fold only a limited range ('lo' .. 'hi') of the indices of 'arr'
foldRange :: (ans -> t -> ans) -> ans -> PArray t -> Int -> Int -> ans
foldRange _accum ans _arr lo hi | lo > hi = ans
foldRange accum ans arr lo hi = foldRange accum (accum ans (index arr lo)) arr (lo + 1) hi

-- ==========================================
-- test that incremental and bulk loading create the same KeyMap

testbulk :: Int -> Int -> (KeyMap Int, Bool)
testbulk seed n = (bulk, bulk == incremental)
  where
    keys = makeKeys seed n
    f (k, v) = (keyPath k, Leaf k v)
    cmp (p1, _) (p2, _) = compare p1 p2
    pairsb = zip keys [0 ..]
    paths :: [(Path, KeyMap Int)]
    paths = sortBy cmp $ map f pairsb
    pathArr = fromlist paths
    incremental = fromList pairsb
    bulk = bulkInsert 0 pathArr 0 (isize pathArr - 1)

-- ===================================================
-- try and measure that bulk loading allocates less memory
-- Does not count the creation and sorting of the array
-- TODO can we do something kike this with a list rather than an array?
-- or sort the array in place?

keysbulk :: [Key]
keysbulk = makeKeys 199 50000

pairsbulk :: [(Key, Int)]
pairsbulk = zip keysbulk [0 ..]

pathsbulk :: [(Path, KeyMap Int)]
pathsbulk = sortBy cmpbulk $ map fbulk pairsbulk
  where
    fbulk (k, v) = (keyPath k, Leaf k v)
    cmpbulk (p1, _) (p2, _) = compare p1 p2

-- use the ghci command   :set +s   to enable statistics
-- (2.30 secs, 1,159,454,816 bytes) size = 10000
-- (13.16 secs, 6,829,808,992 bytes) size = 50000
-- (1.74 secs, 1,623,304,184 bytes)  size = 50000, no print
tryincremental :: Int
tryincremental = sizeKeyMap (fromList pairsbulk)

-- (1.93 secs, 968,147,688 bytes) size = 10000
-- (11.96 secs, 5,937,089,424 bytes) size = 50000
-- (0.94 secs, 661,637,584 bytes) size = 50000, no print
trybulk :: Int
trybulk = sizeKeyMap (bulkInsert 0 pathArr 0 (isize pathArr - 1))
  where
    pathArr :: PArray (Path, KeyMap Int)
    pathArr = fromlist pathsbulk
