{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-orphans #-}


module Data.Compact.KeyMap where



import Data.Foldable (foldl')
import Cardano.Prelude (HeapWords (..),Generic,runST,ST)
import Data.Word(Word64)
import qualified Data.Primitive.Array as PA
import Data.Bits ((.&.), (.|.), complement, popCount, unsafeShiftL,setBit,testBit)
import Data.Compact.Class
import GHC.Exts ((==#), reallyUnsafePtrEquality#, isTrue# )
import qualified Data.Map as Map
import Control.DeepSeq (NFData(..))
import qualified Data.Primitive.SmallArray as Small
import Data.Primitive.SmallArray()
import System.Random(genWord64,mkStdGen)


-- type PArray = PA.Array
type PArray = Small.SmallArray

bin :: Int -> [Int]
bin x = reverse (binary x)

-- =============================================================
-- Keys, BitState and Paths

data Key = Key  {-# UNPACK #-} !Word64
  deriving (Eq,Ord,Show,NFData,Generic)

instance HeapWords Key where
  heapWords (Key _) = 2

type Bitmap = Word64
type Shift  = Int
type Path = [Int]

depth, bits :: Int
depth = 4
bits = 4

showBM :: Bitmap -> String
showBM bm = show(loop 16 [])
  where loop i ans | i <= 0 = ans
        loop i ans = if testBit bm i then loop (i-1) (i:ans) else loop (i-1) ans

size :: Word64
size = 2 ^ ((fromIntegral bits)::Word64)


getpath :: Word64 -> [Int]
getpath w64 = loop (64 `div` (fromIntegral bits)) w64 []
  where loop :: Word64 -> Word64 -> [Int] -> [Int]
        loop 0 _ ans = ans
        loop cnt n ans = loop (cnt - 1) (div n size) ((fromIntegral (mod n size)):ans)

path :: Key -> [Int]
path (Key w) = getpath w

data BitState = BitState [Int] !Key
  
initBitState :: Key -> BitState
initBitState (key@(Key w1)) = BitState (getpath w1) key

instance Show BitState where
  show (BitState p key) = "(BitState "++show p++" "++show key++")"

getBytes :: BitState -> Key
getBytes (BitState _ bs) = bs
       
nextBits :: BitState -> (Int,BitState)
nextBits (BitState (x:xs) key) = (x,BitState xs key)
nextBits (BitState [] key) =
   error ("NextBits out of bounds. The key "++show key++" has run out of bits.")
 
next2 :: BitState -> Key -> BitState
next2 (BitState ps _) key = (BitState (drop n (path key)) key)
   where n = (fromIntegral size) - length ps

-- ===============================================================

data HashMap v
    = Empty
    | Leaf {-# UNPACK #-} !Key v
    | One {-# UNPACK #-} !Int (HashMap v)                           -- 1 subtree
    | Two {-# UNPACK #-} !Bitmap (HashMap v) (HashMap v)            -- 2 subtrees
    | BitmapIndexed {-# UNPACK #-} !Bitmap !(PArray (HashMap v))    -- 3-15 subtrees
    | Full !(PArray (HashMap v))                                    -- 16 subtrees
  deriving (NFData,Generic)

heapAdd :: HeapWords a => a -> Int -> Int
heapAdd x ans = heapWords x + ans

heapPlus:: HeapWords a => Int -> a -> Int
heapPlus ans x = heapWords x + ans

instance HeapWords t => HeapWords (PA.Array t) where
   heapWords arr = foldl' heapPlus (2 + isize arr) arr

instance HeapWords v => HeapWords (HashMap v) where
  heapWords Empty = 1
  heapWords (One _ xs) = 3 + heapWords xs
  heapWords (Leaf _ v) = 3 + heapWords v  -- Change when Key changes
  heapWords (BitmapIndexed _ arr) = foldl' heapPlus 3 arr
  heapWords (Full arr) = foldl' heapPlus 2 arr
  heapWords (Two _ a b) = 4 + heapWords a + heapWords b                       

instance Show v => Show (HashMap v) where
  show Empty = "E"
  show (Leaf s v) = "(L "++show s++" "++show v++")"
  show (Full arr) = "(F "++show(tolist arr)++")"
  show (BitmapIndexed bm arr) = "(B "++showBM bm++" "++show(tolist arr)++")"
  show (One n x) = "(O "++show n++" "++show x++")"
  show (Two bm x y) = "(T "++showBM bm++" "++show x++" "++show y++")"


-- ======================================================================


insert' :: Show v => BitState -> v -> HashMap v -> HashMap v
insert' bs0 v0 m0 = go bs0 v0 m0
  where
    go !state !x Empty = Leaf (getBytes state) x
    go !state x (old@(One j node)) =
        case (nextBits state) of
          (i,state1) ->
            case compare j i of
              EQ -> One j (go state1 x node)
              LT -> Two (setBit (setBit 0 i) j) old (go state1 x Empty)
              GT -> Two (setBit (setBit 0 i) j) (go state1 x Empty) old
    go state x t@(Leaf bs1 y)
      | getBytes state == bs1 = if x `ptrEq` y then t else (Leaf bs1 x)
      | otherwise =  makeTwo state t (next2 state bs1) x
    go state x t@(BitmapIndexed bmap arr)
        | not(testBit bmap tagbits) =
            let !arr' = insertAt arr i $! (Leaf (getBytes state) x)
            in bitmapIndexedOrFull (bmap .|. m) arr'
        | otherwise = 
            let !st = index arr i
                !st' = go state1 x st
            in if st' `ptrEq` st
                  then t
                  else BitmapIndexed bmap (update arr i st') 
       where (!tagbits,!state1) = nextBits state
             m = setBit 0 tagbits
             i = sparseIndex bmap m
    go state x t@(Two bmap x0 x1)
        | not(testBit bmap tagbits) =
            let !arr' = insertAt (fromlist [x0,x1]) i $! (Leaf (getBytes state) x)
            in bitmapIndexedOrFull (bmap .|. m) arr'
        | otherwise =
            let !st = if i==0 then x0 else x1 -- index arr i
                !st' = go state1 x st
            in if st' `ptrEq` st
                  then t
                  else if i==0
                          then Two bmap st' x1
                          else Two bmap x0 st'
       where (!tagbits,!state1) = nextBits state
             m = setBit 0 tagbits
             i = sparseIndex bmap m
    go state x t@(Full arr) =
        let !st = index arr i
            !st' = go state1 x st
        in if st' `ptrEq` st
              then t
              else Full (update arr i st') 
       where (!tagbits,!state1) = nextBits state
             m = setBit 0 tagbits
             i = sparseIndex fullNodeMask m

makeTwo :: Show v => BitState -> HashMap v -> BitState -> v -> HashMap v
makeTwo state1 leaf1 state2 val2 
      | i1==i2 = One i1 (makeTwo state1' leaf1 state2' val2)
      | otherwise = if i1 < i2
                       then Two (setBit (setBit 0 i1) i2) (Leaf (getBytes state1') val2) leaf1
                       else Two (setBit (setBit 0 i1) i2) leaf1 (Leaf (getBytes state1') val2) 
   where (i1,state1') = nextBits state1
         (i2,state2') = nextBits state2

insert :: Show v => Key -> v -> HashMap v -> HashMap v
insert bs v hashmap = insert' (initBitState bs) v hashmap            

fromList :: Show v => [(Key,v)] -> HashMap v
fromList ps = foldl' accum Empty ps
  where accum ans (k,v) = insert k v ans
  
-- ==========================

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
bitmapIndexedOrFull :: Bitmap -> PArray (HashMap v) -> HashMap v
bitmapIndexedOrFull b ary
    | b == fullNodeMask = Full ary
    | otherwise         = BitmapIndexed b ary
{-# INLINE bitmapIndexedOrFull #-}

-- | A bitmask with the 'bitsPerSubkey' least significant bits set.
fullNodeMask :: Bitmap
fullNodeMask = complement (complement 0 `unsafeShiftL` maxChildren)
{-# INLINE fullNodeMask #-}


-- =======================================================================
-- Operations to make new arrays out off old ones with small changes

-- | /O(n)/ Overwrite the element at the given position in this array,
update :: PArray t -> Int -> t -> PArray t
update arr i _t
  | i<0 || i >= (isize arr)
  = error ("index out of bounds in update "++show i++" not in range (0,"++show (isize arr -1)++")")
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
   | idx < 0 || idx > counter = error ("Bounds check in insertAt "++show idx++" not in range 0.."++show (counter))
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


arrayOf :: Int -> a -> PArray a
arrayOf n a =  runST $ do
  marr <- mnew n
  let loop i
       | i < n = mwrite marr i a >> loop (i+1)
       | otherwise = pure ()
  loop 0
  arr <- mfreeze marr
  pure arr

-- =========================================================================


makeKeys :: Int -> Int -> [Key]
makeKeys seed cnt = loop (mkStdGen seed) cnt []
  where loop _g i ans | i <= 0 = ans
        loop g i ans = case genWord64 g of
                         (w64,g2) -> loop g2 (i-1) (Key w64:ans)

testt :: Int -> IO ()
testt n = do
   let (hmap,output) = tests n
       histArr = histo hmap
   -- hbytes <- recursiveSize $!! hmap
   -- putStrLn ("hbytes = "++show hbytes++"\n"++output)
   putStrLn output
   putStrLn ("histogram "++show(tolist histArr))


tests :: Int -> (HashMap Int, String)
tests n = (hashmap,unlines
      [ "empty = "++show empty
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

count :: HashMap v -> (Int,Int,Int,Stat Int,Stat Int,Int)
count x = go 0 x (0,0,0,mempty,mempty,0)
  where go _ Empty (e,o,t,l,b,f) = (e+1,o,t,l,b,f)
        go d (One _ y) (e,o,t,l,b,f) = go (1 + d) y (e,1+o,t,l,b,f)
        go d (Two _ z y) (e,o,t,l,b,f) = go (1+d) y (go (1+d) z (e,o,1+t,l,b,f))
        go d (Leaf _ _) (e,o,t,l,b,f) = (e,o,t,add d l,b,f)
        go d (BitmapIndexed _ arr) (e,o,t,l,b,f) =
          foldr (go (length arr + d)) (e,o,t,l,add (length arr) b,f) arr
        go d (Full arr) (e,o,t,l,b,f) = foldr  (go (length arr + d)) (e,o,t,l,b,f+1) arr



hdepth :: HashMap v -> Int
hdepth Empty = 0
hdepth (One _ x) = 1 + hdepth x
hdepth (Leaf _ _) = 1
hdepth (BitmapIndexed _ arr) = 1+ maximum(foldr (\ x ans -> hdepth x : ans) [] arr)            
hdepth (Full arr) = 1+ maximum(foldr (\ x ans -> hdepth x : ans) [] arr)
hdepth (Two _ x y) = 1 + max (hdepth x) (hdepth y)

increment :: (ArrayPair arr marr a, Num a) => marr s a -> Int -> ST s ()
increment marr i = do { n <- mindex marr i; mwrite marr i (n+1) }

histogram :: HashMap v -> PA.MutableArray s Int -> ST s ()
histogram Empty _ = pure ()
histogram (One _ x) marr = increment marr 1  >> histogram x marr
histogram (Leaf _ _) _ = pure ()
histogram (BitmapIndexed _ arr) marr = increment marr (isize arr) >> mapM_ (\ x -> histogram x marr) arr     
histogram (Full arr) marr = increment marr 15 >> mapM_ (\ x -> histogram x marr) arr  
histogram (Two _ x y) marr = increment marr 2 >> histogram x marr >> histogram y marr

histo :: HashMap v -> PA.Array Int
histo x = fst(withMutArray 16 process)
  where process marr = do (initialize 15) ; histogram x marr
           where initialize n | n <0 = pure ()
                 initialize n = mwrite marr n 0 >> initialize (n-1)



bpairs :: [Key]
bpairs = makeKeys 99 1000000

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

bug :: Int -> IO (HashMap Int)
bug n = do
   let ps = take n pairs
       hh (k@(Key m),v) = show m++" "++show (path k)++" "++show v
   putStrLn (unlines (map hh ps))

   -- putStrLn (show (fromList ps))
   pure (fromList ps)

try :: [(Key,Int)] -> IO ()
try ps = do
   let hh (k@(Key n),v) = show n++" "++show (path k)++" "++show v
   putStrLn (unlines (map hh ps))
   putStrLn (show (fromList ps))