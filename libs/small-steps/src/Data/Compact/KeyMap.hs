{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


{-# OPTIONS_GHC -Wno-orphans #-}


module Data.Compact.KeyMap where

import Data.Foldable (foldl')
import Cardano.Prelude (HeapWords (..),Generic,runST,ST)
import Data.Word(Word64)
import qualified Data.Primitive.Array as PA
import Data.Bits
  ( Bits,(.&.),
    (.|.),
    complement,
    popCount,
    unsafeShiftL,
    setBit,
    testBit,
    clearBit,
    zeroBits,
  )
import Data.Compact.Class
import GHC.Exts ((==#), reallyUnsafePtrEquality#, isTrue# )
import qualified Data.Map as Map
import Control.DeepSeq (NFData(..))
import qualified Data.Primitive.SmallArray as Small
import Data.Primitive.SmallArray()
import System.Random(RandomGen,genWord64,mkStdGen)
import Prettyprinter
import Data.Text(Text,pack)
import qualified Prettyprinter.Internal as Pretty
import Data.Set(Set)
import qualified Data.Set as Set
 

-- type PArray = PA.Array
type PArray = Small.SmallArray

bin :: Integral n => n -> [n]
bin x = reverse (binary x)

-- ==========================================================================
-- bits, Segments, Paths. Breaking a Key into a sequence of small components

-- | Represent a set of small integers, they can range from 0 to 63
type Bitmap = Word64

-- | The number of bits in a segment. Can't be more than 6, because using Word64
--   as Bitmap can only accomodate 2^6 = 64 bits
bits :: Int  
bits = 6

-- | Ints in the range [0..63], represents 'bits' wide portion of a key
type Segment = Int

-- | Represents a list of 'bits', which when combined is in 1-1 correspondance with a Key
type Path = [Segment]

-- | The maximum value of a segment, as an Int
intSize :: Int     
intSize = 2 ^ bits

-- | The maximum value of a segment, as a Word64
wordSize :: Word64
wordSize = 2 ^ ((fromIntegral bits)::Word64)

-- | The length of a list of segments representing a key. Need to be carefull if a Key isn't evenly divisible by bits
pathSize :: Word64
pathSize = (if (mod 64 wbits)==0 then (div 64 wbits) else (div 64 wbits) + 1)
  where wbits = fromIntegral bits :: Word64

-- | Break up a Word64 into a Path
getpath :: Word64 -> Path
getpath w64 = loop pathSize w64 []
  where loop :: Word64 -> Word64 -> [Int] -> [Int]
        loop 0 _ ans = ans
        loop cnt n ans = loop (cnt - 1) (div n wordSize) ((fromIntegral (mod n wordSize)):ans)

-- ========================================================================
-- Keys 

data Key = Key  {-# UNPACK #-} !Word64
                {-# UNPACK #-} !Word64
                {-# UNPACK #-} !Word64
                {-# UNPACK #-} !Word64
  deriving (Eq,Ord,Show,NFData,Generic)

-- | The number of Word64 per key
wordsPerKey :: Int
wordsPerKey = 4

-- | The length of a Path for a Key (which might have multiple Word64's inside)
keyPathSize :: Int
keyPathSize = wordsPerKey * (fromIntegral pathSize)

genKey :: RandomGen b => b -> (Key, b)
genKey g = (Key w0 w1 w2 w3,g4)
  where (w0,g1) = genWord64 g
        (w1,g2) = genWord64 g1
        (w2,g3) = genWord64 g2
        (w3,g4) = genWord64 g3        

-- | Break up a Key into a Path
path :: Key -> Path
path (Key w0 w1 w2 w3) = getpath w0 ++ getpath w1 ++ getpath w2 ++ getpath w3

-- | A pair of a Key and its equivalent Path
data BitState = BitState Path !Key

-- Initialize a BitState from a Key
initBitState :: Key -> BitState
initBitState key = BitState (path key) key

-- | Obtain the Key from a BitState
getBytes :: BitState -> Key
getBytes (BitState _ bs) = bs

-- | Make a new BitState from a Key, using the old BitState to figure out
--   how far down the path have we already gone.
next2 :: BitState -> Key -> BitState
next2 (BitState ps _) key = (BitState (drop n (path key)) key)
   where n = (fromIntegral keyPathSize) - length ps 

showBM :: Bitmap -> String
showBM bm = show(bitmapToList bm)

bitmapToList :: Bits a => a -> [Int]
bitmapToList bm = loop 63 []
  where loop i ans | i < 0 = ans
        loop i ans = if testBit bm i then loop (i-1) (i:ans) else loop (i-1) ans

instance HeapWords Key where
  heapWords (Key _ _ _ _) = 5

instance Show BitState where
  show (BitState p key) = "(BitState "++show p++" "++show key++")"


-- ===============================================================

data KeyMap v
    = Empty
    | Leaf {-# UNPACK #-} !Key v
    | One {-# UNPACK #-} !Int (KeyMap v)                           -- 1 subtree
    | Two {-# UNPACK #-} !Bitmap (KeyMap v) (KeyMap v)            -- 2 subtrees
    | BitmapIndexed {-# UNPACK #-} !Bitmap                          -- 3 - (intSize - 1) subtrees
                    {-# UNPACK #-} !(Small.SmallArray (KeyMap v))  
    | Full {-# UNPACK #-} !(Small.SmallArray (KeyMap v))           -- intSize subtrees
  deriving (NFData,Generic)

heapAdd :: HeapWords a => a -> Int -> Int
heapAdd x ans = heapWords x + ans

heapPlus:: HeapWords a => Int -> a -> Int
heapPlus ans x = heapWords x + ans

instance HeapWords t => HeapWords (PA.Array t) where
   heapWords arr = foldl' heapPlus (2 + isize arr) arr

instance HeapWords v => HeapWords (KeyMap v) where
  heapWords Empty = 1
  heapWords (One _ xs) = 3 + heapWords xs
  heapWords (Leaf _ v) = 6 + heapWords v  -- Change when Key changes
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

insert' :: BitState -> v -> KeyMap v -> KeyMap v
insert' state v m = insertWithKey (\ _k new _old -> new) state v m

insertWithKey :: (Key -> v -> v -> v) -> BitState -> v -> KeyMap v -> KeyMap v
insertWithKey combine bs0 v0 m0 = goR bs0 v0 m0
  where
    goR state val mp = (go state val mp)
    go (BitState _ k) !x Empty = Leaf k x
    go (BitState [] k) _ _ = error ("In insert', ran out of bits for key "++show k)
    go (BitState (i:is) k) x (One j node) =
       case compare j i of
         EQ -> One j (goR (BitState is k) x node)
         LT -> Two (setBits [i,j]) node (goR (BitState is k) x Empty)
         GT -> Two (setBits [i,j]) (goR (BitState is k) x Empty) node
    go (state@(BitState _ k1)) x t@(Leaf k2 y)
      | k1 == k2 = if x `ptrEq` y
                    then t
                    else (Leaf k2 (combine k2 x y))
      | otherwise = makeTwo state t (next2 state k2) x
    go (BitState (j:js) k)  x t@(BitmapIndexed bmap arr)
        | not(testBit bmap j) =
            let !arr' = insertAt arr i $! (Leaf k x)
            in bitmapIndexedOrFull (bmap .|. (setBit 0 j)) arr'
        | otherwise = 
            let !st = index arr i
                !st' = goR (BitState js k) x st
            in if st' `ptrEq` st
                  then t
                  else BitmapIndexed bmap (update arr i st') 
       where i = indexFromSegment bmap j
    go (BitState (j:js) k) x t@(Two bmap x0 x1)
        | not(testBit bmap j) =
            let !arr' = insertAt (fromlist [x0,x1]) i $! (Leaf k x)
            in bitmapIndexedOrFull (bmap .|. (setBit 0 j)) arr'
        | otherwise =
            let !st = if i==0 then x0 else x1  
                !st' = goR (BitState js k) x st
            in if st' `ptrEq` st
                  then t
                  else if i==0
                          then Two bmap st' x1
                          else Two bmap x0 st'
       where i = indexFromSegment bmap j
    go (BitState (j:js) k) x t@(Full arr) =
        let !st = index arr i
            !st' = goR (BitState js k) x st
        in if st' `ptrEq` st
              then t
              else Full (update arr i st') 
       where i = indexFromSegment fullNodeMask j

makeTwo :: BitState -> KeyMap v -> BitState -> v -> KeyMap v
makeTwo (BitState [] k) _leaf _state _val = error ("Case 1. In makeTwo, out of bits for key "++show k)
makeTwo _state _leaf (BitState [] k) _val = error ("Case 2. In makeTwo, out of bits for key "++show k)
makeTwo (BitState (i:is) k1) leaf1 (BitState (j:js) k2) val2 
      | i==j = One i (makeTwo (BitState is k1) leaf1 (BitState js k2) val2)
      | otherwise = if i < j
                       then Two (setBits [i,j]) (Leaf k1 val2) leaf1 
                       else Two (setBits [i,j]) leaf1 (Leaf k1 val2) 

foo :: String -> a -> String
foo s !_ = s

insert :: Key -> v -> KeyMap v -> KeyMap v
insert bs v hashmap = insert' (initBitState bs) v hashmap -- (trace  ("INSERT "++show bs) hashmap)            

fromList :: [(Key,v)] -> KeyMap v
fromList ps = foldl' accum Empty ps
  where accum ans (k,v) = insert k v ans

toList :: KeyMap v -> [(Key,v)]
toList km = foldWithDescKey accum [] km
  where accum k v ans = (k,v):ans

-- =================================================================
-- Deletion

-- | Delete the Key encoded in the BitState from the KeyMap
delete' :: BitState -> KeyMap v -> KeyMap v
delete' (BitState [] _) hm = hm -- Removing a bogus key, leaves 'hm' unchanged
delete' _ Empty = Empty
delete' (BitState _ k) (hm@(Leaf k2 _)) = if k==k2 then Empty else hm
delete' (BitState(i:is) k) (hm@(One j x)) = if i==j then oneE j (delete' (BitState is k) x) else hm
delete' (BitState(i:is) k) (hm@(Two bmap x y)) =
   if testBit bmap i
      then twoE bmap (delete' (BitState is k) x) (delete' (BitState is k) y)
      else hm
delete' (BitState(i:is) k) (hm@(BitmapIndexed bmap arr)) =
   if testBit bmap i
      then let m = setBit 0 i
               j = sparseIndex bmap m
               result = delete' (BitState is k) (index arr j)
           -- Consume an upwards floating Empty by removing that element from the array
           in case result of
               Empty -> bitmapE (clearBit bmap i) (remove arr j)
               _ -> BitmapIndexed bmap (update arr j result)
      else hm
delete' (BitState(i:is) k) (Full arr) =
   let m = setBit 0 i
       j = sparseIndex fullNodeMask m
       result = delete' (BitState is k) (index arr j)
   -- Consume an upwards floating Empty by removing that element from the array
   in case result of
        Empty -> BitmapIndexed (clearBit fullNodeMask i) (remove arr j)
        _ -> Full(update arr j result)

delete :: Key -> KeyMap v -> KeyMap v
delete k hm = delete' (initBitState k) hm

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

-- | The first (smallest) Segment in a BitMap
firstSeg :: Bitmap -> Segment
firstSeg bmap = head(bitmapToList bmap)

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
  where n = isize arr
        loop !ans i | i >= n = ans
        loop !ans i = loop (foldWithAscKey accum ans (index arr i)) (i+1)
foldWithAscKey accum !ans0 (Full arr) = loop ans0 0
  where n = isize arr
        loop !ans i | i >= n = ans
        loop !ans i = loop (foldWithAscKey accum ans (index arr i)) (i+1)        

sizeKeyMap :: KeyMap v -> Int
sizeKeyMap x = foldWithAscKey (\ ans _k _v -> ans+1) 0 x

-- ================================================================
-- aggregation in descending order of keys

foldWithDescKey :: (Key -> v -> ans -> ans) -> ans -> KeyMap v -> ans  
foldWithDescKey _ !ans Empty = ans
foldWithDescKey accum !ans (Leaf k v) = accum k v ans
foldWithDescKey accum !ans (One _ x) = foldWithDescKey accum ans x
foldWithDescKey accum !ans (Two _ x y) = foldWithDescKey accum (foldWithDescKey accum ans y) x
foldWithDescKey accum !ans0 (BitmapIndexed _ arr) = loop ans0 (n-1)
  where n = isize arr
        loop !ans i | i < 0 = ans
        loop !ans i = loop (foldWithDescKey accum ans (index arr i)) (i-1)
foldWithDescKey accum !ans0 (Full arr) = loop ans0 (n-1)
  where n = isize arr
        loop !ans i | i < 0 = ans
        loop !ans i = loop (foldWithDescKey accum ans (index arr i)) (i-1)      

-- ==================================================================
-- Lookup a key 

lookup' :: BitState -> KeyMap v -> Maybe v
lookup' _ Empty = Nothing
lookup' (BitState _ k1) (Leaf k2 v) = if k1==k2 then Just v else Nothing
lookup' (BitState [] k) _ = error ("lookup', out of bits for key "++show k)
lookup' (BitState (j:js) k) (One i x) = if i==j then lookup' (BitState js k) x else Nothing
lookup' (BitState (j:js) k) (Two bm x0 x1) =
    if testBit bm j
       then (if i==0 then lookup' (BitState js k) x0 else lookup' (BitState js k) x1)
       else Nothing
  where i = indexFromSegment bm j
lookup' (BitState (j:js) k) (BitmapIndexed bm arr) =
    if testBit bm j
       then lookup' (BitState js k) (index arr i)
       else Nothing
  where i = indexFromSegment bm j
lookup' (BitState (j:js) k) (Full arr) = -- Every possible bit is set, to no testBit call necessary
    lookup'  (BitState js k) (index arr i)
  where i = indexFromSegment fullNodeMask j

lookupHM :: Key -> KeyMap v -> Maybe v
lookupHM bytes mp = lookup' (initBitState bytes) mp

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
  fmap f x = mapWithKey (\ _ v -> f v) x

-- ==========================================================
-- Split a KeyMap into 3 parts

-- | return (smaller than 'key', has key?, greater than 'key')
splitKeyMap:: BitState -> KeyMap v -> (KeyMap v,Maybe v,KeyMap v)
splitKeyMap (BitState [] _) hm = (hm,Nothing,Empty)
splitKeyMap (BitState (i:is) key) hm =
  case splitBySegment i hm of
    (less,x,greater) ->
      case x of
        Empty -> (build less,Nothing,build greater)
        (Leaf k v) -> (build less,if key==k then (Just v) else Nothing,build greater)
        other ->  (reconstruct i less less1,ans,reconstruct i greater greater1)
          where (less1,ans,greater1) = splitKeyMap (BitState is key) other

splitBySegment :: Segment -> KeyMap v -> ([(Segment,KeyMap v)],KeyMap v, [(Segment,KeyMap v)])
splitBySegment i _x | i < 0 = ([],Empty,[])
splitBySegment i _x | i > intSize =  ([],Empty,[])
splitBySegment _ Empty = ([],Empty,[])
splitBySegment _ (x@(Leaf _ _)) = ([],x,[])
splitBySegment i (x@(One j y)) =
   case compare i j of
     LT -> ([],Empty,[(i,x)])
     EQ -> ([],y,[])
     GT -> ([(i,x)],Empty,[])
splitBySegment i (Two bmap l h) = splitArrAtSeg i bmap (fromlist [l,h])
splitBySegment i (BitmapIndexed bmap arr) = splitArrAtSeg i bmap arr
splitBySegment i (Full arr) = splitArrAtSeg i fullNodeMask arr 

-- | Split an PArray at a particular Segment.
splitArrAtSeg:: Segment -> Bitmap -> PArray (KeyMap v) -> ([(Int, KeyMap v)], KeyMap v, [(Int, KeyMap v)])
splitArrAtSeg i bmap arr = (takeWhile smaller ps, match, dropWhile tooSmall ps)
    where ps = zip (bitmapToList bmap) (tolist arr)
          smaller (j,_) = j < i
          tooSmall (j,_) = j <= i
          same (j,_) = i==j
          match = case filter same ps of
            [] -> Empty
            ((_,x):_) -> x

-- | reconstruct a KeyMap from list of previous Segments, and a single KeyMap from the next Segment 
reconstruct :: Segment -> [(Segment, KeyMap v)] -> KeyMap v -> KeyMap v
reconstruct _ xs Empty = build xs
reconstruct seg xs x = build (insertAscending (seg,x) xs)

-- | insert a Segment pair in ascending order of Segments, Keep it sorted.
insertAscending:: (Segment, KeyMap v) -> [(Segment, KeyMap v)] -> [(Segment, KeyMap v)]
insertAscending (i,x) [] = [(i,x)]
insertAscending (i,x) (ws@((y@(j,_)):ys)) =
  case compare i j of
    LT -> (i,x):ws
    GT -> y : insertAscending (i,x) ys
    EQ -> (i,x):ys -- We know that the Segement i should never appear in the list

-- | Build a KeyMap out of a list of Segment pairs.
build :: [(Segment, KeyMap v)] -> KeyMap v
build [] = Empty
build [(_,x)] = x
build [(j,x),(k,y)] = Two (setBits [j,k]) x y
build ps = bitmapIndexedOrFull (setBits (map fst ps)) (fromlist (map snd ps))


testSplit2 :: Int -> IO ()
testSplit2 i = putStrLn (unlines [show hm, " ",show pathx," ",show a, " ",show b, " ",show c])
  where keys = makeKeys 99 1000
        ps = zip (take 12 keys) [0..]
        hm :: KeyMap Int
        hm = fromList ps
        state@(BitState pathx _) = (initBitState (keys !! i))
        (a,b,c) = splitKeyMap state hm

-- =========================================================
-- UnionWith

toListOfSegments :: Int -> KeyMap v -> [(Segment,KeyMap v)]
toListOfSegments _ Empty = []       
toListOfSegments n (l@(Leaf k _)) = [(path k !! n,l)]
toListOfSegments _ (One i x) = [(i,x)]
toListOfSegments _ (Two bm x y) = zip (bitmapToList bm) [x,y]
toListOfSegments _ (BitmapIndexed bm arr) = zip (bitmapToList bm) (tolist arr)
toListOfSegments _ (Full arr) = zip (bitmapToList fullNodeMask) (tolist arr)


mergeWith:: (KeyMap v -> KeyMap v -> KeyMap v) -> [(Segment,KeyMap v)] -> [(Segment,KeyMap v)] -> [(Segment,KeyMap v)]
mergeWith _combine [] [] = []
mergeWith _combine xs [] = xs
mergeWith _combine [] ys = ys
mergeWith combine (allxs@((i,x):xs)) (allys@((j,y):ys)) =
  case compare i j of
    EQ -> (i,combine x y) : mergeWith combine xs ys
    LT -> (i,x) : mergeWith combine xs allys
    GT -> (j,y) : mergeWith combine allxs ys

unionWithN :: Int -> (Key -> v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWithN _ _ Empty Empty = Empty
unionWithN _ _ x Empty = x
unionWithN _ _ Empty y = y
unionWithN _ combine (Leaf k1 v1) (Leaf k2 v2) | k1==k2 = Leaf k1 (combine k1 v1 v2)
unionWithN _ combine (Leaf k v) y = insertWithKey combine (BitState (path k) k) v y
unionWithN _ combine x (Leaf k v) = insertWithKey combine (BitState (path k) k) v x
unionWithN n combine x y = build (mergeWith (unionWithN (n+1) combine) xpairs ypairs)
  where xpairs = toListOfSegments n x
        ypairs = toListOfSegments n y

unionWithKey :: (Key -> v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWithKey comb x y = unionWithN 0 comb x y

unionWith :: (v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWith comb x y = unionWithN 0 (\ _k a b -> comb a b) x y

hm10, hm11, hm12 :: KeyMap Int
hm10 = fromList (take 5 pairs)
hm11 = fromList (take 5 (drop 4 pairs))
hm12 = unionWith (+) hm10 hm11


-- ===========================================================
-- Maximum and Minimum Key

-- | Get the smallest key, NOT the smallest value
getMin :: KeyMap v -> Maybe (Key,v)
getMin Empty = Nothing
getMin (Leaf k v) = Just (k,v)
getMin (One _ x) = getMin x
getMin (Two _ x _) = getMin x
getMin (BitmapIndexed _ arr) = getMin (index arr 0)
getMin (Full arr) = getMin (index arr 0)

-- | Get the largest key, NOT the largest value
getMax :: KeyMap v -> Maybe (Key,v)
getMax Empty = Nothing
getMax (Leaf k v) = Just (k,v)
getMax (One _ x) = getMax x
getMax (Two _ _ y) = getMax y
getMax (BitmapIndexed _ arr) = getMax (index arr (isize arr - 1))
getMax (Full arr) = getMax (index arr (isize arr - 1))

-- ==================================================

-- | The (key,value) pairs (i.e. a subset) of 'h1' where key is in the domain of both 'h1' and 'h2'
intersect :: KeyMap v -> KeyMap v -> KeyMap v
intersect map1 map2 =
   case next map1 map2 of
     Nothing -> Empty
     Just k -> leapfrog k map1 map2 Empty

leapfrog :: Key -> KeyMap v -> KeyMap v -> KeyMap v -> KeyMap v
leapfrog k x y ans =
  case (lub k x,lub k y) of
       (Just(k1,v1,h1),Just(k2,_,h2)) ->
          case next h1 h2 of
            Just k3 -> leapfrog k3 h1 h2 (if k1==k2 then insert k1 v1 ans else ans)
            Nothing -> (if k1==k2 then insert k1 v1 ans else ans)
       _ -> ans

-- | Find the smallest key <= 'key', and a KeyMap of everything bigger than 'key'
lub :: Key -> KeyMap v -> Maybe (Key, v, KeyMap v)
lub key hm =
  case splitKeyMap (initBitState key) hm of
    (_,Just _,Empty) -> Nothing
    (_,Just v,hm2) -> Just(key,v,hm2)
    (_,Nothing,hm1) ->
       case getMin hm1 of
         Just (k,v) -> Just(k,v,hm1) 
         Nothing -> Nothing

next :: KeyMap v1 -> KeyMap v2 -> Maybe Key
next x y = case (getMin x,getMin y) of
            (Just (k1,_),Just (k2,_)) -> Just(max k1 k2)
            _ -> Nothing

testIntersect :: KeyMap Int
testIntersect = intersect h1x h2x

h1x, h2x :: KeyMap Int
h1x = fromList [pairs !! 3,pairs !! 5, pairs !! 11, pairs !! 6, pairs !! 4]
h2x = fromList [pairs !! 3,pairs !! 7, pairs !! 4, pairs !! 6, pairs !! 8] 
-- =========================================================

-- | Domain restrict 'hkm' to those Keys found in 's'. This algorithm
--   assumes the set 's' is small compared to 'hm'.
domainRestrict :: KeyMap v -> Set Key -> KeyMap v
domainRestrict hm s = Set.foldl' accum Empty s
  where accum ans key =
          case lookupHM key hm of
            Nothing -> ans
            Just v -> insert key v ans

hmdr :: KeyMap Int
hmdr = fromList (take 10 pairs)

set:: Set Key
set = Set.fromList [ bpairs !! 3, bpairs !! 8, bpairs !! 20]

-- ==========================================
-- Operations on Bits and Bitmaps

-- | Check if two the two arguments are the same value.  N.B. This
-- function might give false negatives (due to GC moving objects.)
ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y ==# 1#)
{-# INLINE ptrEq #-}

bitsPerSubkey :: Int
bitsPerSubkey = 4
{-# INLINE bitsPerSubkey #-}

maxChildren :: Int
maxChildren = 1 `unsafeShiftL` bitsPerSubkey
{-# INLINE maxChildren #-}

subkeyMask :: Bitmap
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1
{-# INLINE subkeyMask #-}

sparseIndex :: Bitmap -> Bitmap -> Int
sparseIndex b m = popCount (b .&. (m - 1))
{-# INLINE sparseIndex #-}

-- | Create a 'BitmapIndexed' or 'Full' node.
bitmapIndexedOrFull :: Bitmap -> PArray (KeyMap v) -> KeyMap v
bitmapIndexedOrFull b ary
    | b == fullNodeMask = Full ary
    | otherwise         = BitmapIndexed b ary
{-# INLINE bitmapIndexedOrFull #-}

-- | A bitmask with the 'bitsPerSubkey' least significant bits set.
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
splitBitmap :: Bitmap -> Int -> (Bitmap,Bool,Bitmap)
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
lessMasks = fromlist [ setBits [0 .. i-1] | i <- [0..63]]
greaterMasks = fromlist [ setBits [i+1 .. 63] | i <- [0..63]]

testsplitBitmap :: Int -> ([Int], Bool, [Int])
testsplitBitmap i = (bitmapToList l,b,bitmapToList g)
  where (l,b,g) = splitBitmap (complement (zeroBits :: Word64)) i


-- =======================================================================
-- Operations to make new arrays out off old ones with small changes

-- | /O(n)/ Make a copy of an Array that removes the 'i'th element. Decreasing the size by 1.
remove :: PArray a -> Int -> PArray a
remove arr i = if i<0 || i > n
        then error ("index out of bounds in 'remove' "++show i++" not in range (0,"++show (isize arr -1)++")")
        else  fst(withMutArray n action)
   where n = (isize arr) - 1
         action marr = do
            mcopy marr 0 arr 0 i
            mcopy marr i arr (i+1) (n-i)


-- | /O(n)/ Overwrite the element at the given position in this array,
update :: PArray t -> Int -> t -> PArray t
update arr i _
  | i<0 || i >= (isize arr)
  = error ("index out of bounds in 'update' "++show i++" not in range (0,"++show (isize arr -1)++")")
update arr i t = fst(withMutArray size1 action)
  where size1 = isize arr
        action marr = do
          mcopy marr 0 arr 0 i
          mwrite marr i t         
          mcopy marr (i+1) arr (i+1) (size1 - (i+1))


-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insertM :: PArray e -> Int -> e -> ST s (PArray e)
insertM ary idx b
   | idx < 0 || idx > counter = error ("Bounds check in 'insertAt' "++show idx++" not in range 0.."++show (counter))
   | otherwise = do
           mary <- mnew (counter+1)
           mcopy mary 0 ary 0 idx
           mwrite mary idx b
           mcopy mary (idx+1) ary idx (counter-idx)
           mfreeze mary
  where !counter = isize ary
{-# INLINE insertM #-}

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insertAt :: PArray e -> Int -> e -> PArray e
insertAt arr idx b = runST(insertM arr idx b)
{-# INLINE insertAt #-}

-- | Create a new Array of size 'n' filled with objects 'a'
arrayOf :: Int -> a -> PArray a
arrayOf n a =  runST $ do
  marr <- mnew n
  let loop i
       | i < n = mwrite marr i a >> loop (i+1)
       | otherwise = pure ()
  loop 0
  arr <- mfreeze marr
  pure arr
{-# INLINE arrayOf #-}

-- =========================================================================

makeKeys :: Int -> Int -> [Key]
makeKeys seed cnt = loop (mkStdGen seed) cnt []
  where loop _g i ans | i <= 0 = ans
        loop g i ans = case genKey g of
                         (key,g2) -> loop g2 (i-1) (key : ans)

testt :: Int -> IO ()
testt n = do
   let (hmap,output) = tests n
       histArr = histo hmap
   putStrLn output
   putStrLn ("histogram "++show(tolist histArr))


tests :: Int -> (KeyMap Int, String)
tests n = (hashmap,unlines
      [ "bits per level = "++show bits
      , "num levels = "++show keyPathSize
      , "empty = "++show empty
      , "leaf  = "++show leaf
      , "one   = "++show one
      , "two   = "++show two
      , "bits  = "++show bit     
      , "full  = "++show full
      , "hwords = "++show hwords
      , "mwords = "++show mwords
      , "diff   = "++show(hwords - mwords)++" %"++show((hwords*100) `div` mwords)
      , "depth = "++show (hdepth hashmap)
      ])
  where hashmap = fromList (take n pairs)
        mapmap = Map.fromList (take n pairs)
        (empty,one,two,leaf,bit,full) = count hashmap
        hwords = heapWords hashmap
        mwords = heapWords mapmap

count :: KeyMap v -> (Int,Int,Int,Stat Int,Stat Int,Int)
count x = go 0 x (0,0,0,mempty,mempty,0)
  where go _ Empty (e,o,t,l,b,f) = (e+1,o,t,l,b,f)
        go d (One _ y) (e,o,t,l,b,f) = go (1 + d) y (e,1+o,t,l,b,f)
        go d (Two _ z y) (e,o,t,l,b,f) = go (1+d) y (go (1+d) z (e,o,1+t,l,b,f))
        go d (Leaf _ _) (e,o,t,l,b,f) = (e,o,t,add d l,b,f)
        go d (BitmapIndexed _ arr) (e,o,t,l,b,f) =
          foldr (go (length arr + d)) (e,o,t,l,add (length arr) b,f) arr
        go d (Full arr) (e,o,t,l,b,f) = foldr  (go (length arr + d)) (e,o,t,l,b,f+1) arr

countIO:: HeapWords a => KeyMap a -> IO ()
countIO hashmap = do
   putStrLn $ unlines
      [ "bits per level = "++show bits
      , "num levels = "++show keyPathSize
      , "empty = "++show empty
      , "leaf  = "++show leaf
      , "one   = "++show one
      , "two   = "++show two
      , "bits  = "++show bit     
      , "full  = "++show full
      , "hwords = "++show hwords
      , "depth = "++show (hdepth hashmap)
      , "histogram ="++show hist
      ]
  where (empty,one,two,leaf,bit,full) = count hashmap
        hist = histo hashmap
        hwords = heapWords hashmap
        
hdepth :: KeyMap v -> Int
hdepth Empty = 0
hdepth (One _ x) = 1 + hdepth x
hdepth (Leaf _ _) = 1
hdepth (BitmapIndexed _ arr) = 1+ maximum(foldr (\ x ans -> hdepth x : ans) [] arr)            
hdepth (Full arr) = 1+ maximum(foldr (\ x ans -> hdepth x : ans) [] arr)
hdepth (Two _ x y) = 1 + max (hdepth x) (hdepth y)

increment :: (ArrayPair arr marr a, Num a) => marr s a -> Int -> ST s ()
increment marr i = do { n <- mindex marr i; mwrite marr i (n+1) }

histogram :: KeyMap v -> PA.MutableArray s Int -> ST s ()
histogram Empty _ = pure ()
histogram (One _ x) marr = increment marr 1  >> histogram x marr
histogram (Leaf _ _) _ = pure ()
histogram (BitmapIndexed _ arr) marr = increment marr (isize arr-1) >> mapM_ (\ x -> histogram x marr) arr     
histogram (Full arr) marr = increment marr (intSize-1) >> mapM_ (\ x -> histogram x marr) arr  
histogram (Two _ x y) marr = increment marr 2 >> histogram x marr >> histogram y marr

histo :: KeyMap v -> PA.Array Int
histo x = fst(withMutArray intSize process)
  where process marr = do { initialize (intSize - 1) ; histogram x marr }
           where initialize n | n <0 = pure ()
                 initialize n = mwrite marr n 0 >> initialize (n-1)

bpairs :: [Key]
bpairs = makeKeys 99 1500000
         -- makeKeys 3 15

pairs :: [ (Key,Int) ]
pairs = zip bpairs [0..] 

-- ===================================================

data Stat n = Stat n n (Maybe n) (Maybe n)

liftM:: (t -> t -> t) -> Maybe t -> Maybe t -> Maybe t
liftM f (Just x) (Just y) = Just(f x y)
liftM _ Nothing (Just y) = Just y
liftM _ (Just x) Nothing = Just x
liftM _ Nothing Nothing = Nothing

instance (Ord n,Num n) => Semigroup (Stat n) where
 (Stat c1 s1 mx1 mn1) <> (Stat c2 s2 mx2 mn2) =
    Stat (c1+c2) (s1 + s2) (liftM max mx1 mx2) (liftM min mn1 mn2)

instance (Ord n,Num n) => Monoid (Stat n) where
  mempty = Stat 0 0 Nothing Nothing

instance (Integral n,Show n) => Show (Stat n) where
  show (Stat c s mx mn) = "{count= "++show c++", sum="++show s++", max="++show mx++
                            ", min="++show mn++
                            (if c==0 then "}" else ", avg="++show(div s c)++"}")

add :: (Num n,Ord n) => n -> Stat n -> Stat n
add n stat = (Stat 1 n (Just n) (Just n)) <> stat

-- ====================
-- Debugging functions

bug :: Int -> IO (KeyMap Int)
bug n = do
   let ps = take n pairs -- zip (makeKeys 3 n) [0..]
       hh (k@(Key m0 m1 _ _ ),v) = show m0++" "++show m1++" "++show (path k)++" "++show v
   putStrLn (unlines (map hh ps))

   -- putStrLn (show (fromList ps))
   pure (fromList ps)

try :: [(Key,Int)] -> IO ()
try ps = do
   let  hh (k@(Key m0 m1 _ _),v) = show m0++" "++show m1++" "++show (path k)++" "++show v
   putStrLn (unlines (map hh ps))
   putStrLn (show (fromList ps))


testlookup :: Int -> Int -> Bool
testlookup seed n = all ok results
  where ps = zip (makeKeys seed n) [0..]
        keymap :: KeyMap Int
        keymap = fromList ps
        results = [ (i,lookupHM (fst(ps !! i)) keymap) | i <- [0..(n-1)]]
        ok (_,Just _) = True
        ok (i,Nothing) = error ("testlookup failure: "++show i++"   "++show pair++"\n"++
                                show (path (fst pair))++"\n  "++show keymap)
          where pair = (ps !! i)

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
ppBitmap x = text (pack(showBM x))

ppKeyMap :: (v -> PDoc) -> KeyMap v -> PDoc
ppKeyMap p (Leaf k v) = ppSexp "L" [ppKey k,p v]
ppKeyMap _ Empty = text "E"
ppKeyMap p (One x mp) = ppSexp "O" [ppInt x,ppKeyMap p mp]
ppKeyMap p (Two x m1 m2) = ppSexp "T" [ppBitmap x ,ppKeyMap p m1, ppKeyMap p m2]
ppKeyMap p (BitmapIndexed x arr) = ppSexp "B" [ppList q (zip (bitmapToList x) (tolist arr))]
  where q (i,a) = ppInt i <+> ppKeyMap p a
ppKeyMap p (Full arr) = ppSexp "F" [ppList q (zip (bitmapToList fullNodeMask) (tolist arr))]
  where q (i,a) = ppInt i <+> ppKeyMap p a

instance PrettyA v => Show (KeyMap v) where
   show x = show(ppKeyMap prettyA x)
   showList xs x = unlines (map (\ y -> "\n"++ show(ppKeyMap prettyA y)) xs) ++ x


keysX :: [Key]
keysX =
    [ Key 17900508425448557813 1425228445697650578 4096886001776694930 5607342842136005805
    , Key 6883900645186699936 13296170193789961158 4397314084330617059 8869821626379988209
    , Key 10500005495319713790 2912085157004832622 13426000237053837606 12059657784398898378
    , Key 3923906598994021794 12765791139276487287 816482653431531599 7003511147053802144
    , Key 5166915752834780615 7133194944084009196 13810062108841219641 296498671410031824
    , Key 18030165020800047584 18085286706182838302 16232822895209986816 17388829728381408048
    , Key 5142157305423936627 7231225143269744777 15250651091019539686 14241693248962825662
    , Key 13428671722466854389 16561117437870591512 11235927355594486308 16930552725399654134
    , Key 4838981082210206139 12557487373235351610 6348966276768033248 1499713340968517390
    , Key 336475062096603304 6399910448856822947 3425786324025245994 16363487473709422408
    , Key 16607855275778415913 15113927333656355571 16111805289570157530 7151802073429851699
    , Key 10517657211907470890 1089616862122803787 13992791218083853691 13236284657137382314
    , Key 1782840219730873272 5422922922394198551 6207884257158004626 16093772551099792787
    , Key 17216197724774766219 6382375034658581036 883871178158682222 6551207497782514085
    , Key 292346888039769756 7462467761555063764 6493768272219444322 7737867387963351907
    , Key 6067080276442487773 97011115971541225 17793222466254767399 16164726605331358005
    ]
