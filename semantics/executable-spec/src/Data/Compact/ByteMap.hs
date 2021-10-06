{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module ByteMap where


import Data.ByteString.Short (ShortByteString,toShort,fromShort,pack,unpack)
import qualified Data.ByteString.Short as SBS
import Data.ByteString(ByteString)
import Data.Foldable (foldr',foldl')
import Cardano.Prelude (HeapWords (..),runST,ST)
import Data.Word(Word8,Word64)
import qualified Data.Primitive.Array as PA
import Data.Bits ((.&.), (.|.), complement, popCount, unsafeShiftL, unsafeShiftR,shiftL, shiftR,setBit,testBit)
import qualified Data.Bits as Bits
import qualified Data.Text as Text
import Data.Text.Encoding(encodeUtf8)
import Data.Compact.Class
import GHC.Exts ((==#), build, reallyUnsafePtrEquality#, inline, isTrue# )
import Debug.Trace
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Char(chr)
-- ===================================================

w1:: Word8
w1 = 196

bin x = reverse (binary x)

bytes :: String -> ShortByteString
bytes str = toShort $ encodeUtf8 $ Text.pack str


-------------------------------------------------------------

depth, bits, size :: Int
depth = 4
bits = 4
size = 2 ^ bits
indices = [0,1] -- [0.. size -1]


lo4bits, hi4bits :: Word8
lo4bits = 15                -- 00001111
hi4bits = shiftL lo4bits 4  -- 11110000

split4 :: Word8 -> (Word8,Word8)
split4 w = (shiftR w 4, w .&. lo4bits)

combine4 :: (Word8,Word8) -> Word8
combine4 (hi,lo) = hi * 16 + lo

-- | Split a ByteString into two parts. The first is a [Word8]
--   its length is depth, and each Word8 is in the range (0.. 2^bits)
--   The second part is the remainder of the byte string
split :: ShortByteString -> ([Int],ShortByteString)
split bs | SBS.length bs >= 3 =
   let (w1 : w2 : more) = unpack bs
       (x1,x2) = split4 w1
       (x3,x4) = split4 w2
   in (map fromIntegral [x1,x2,x3,x4],pack more)
split bs = error ("Byte string is too short to split with a prefix of length "++show depth)

data ByteMap v = ByteMap (PA.Array (PA.Array (PA.Array (PA.Array [(ShortByteString,v)]))))
  deriving Show

update :: PA.Array t -> Int -> t -> PA.Array t
update arr i t | i<0 || i >= (isize arr) = error ("index out of bounds in update "++show i)
update arr i t = fst(withMutArray (isize arr) action)
  where action marr = do
          mcopy marr 0 arr 0 i
          mwrite marr i t
          mcopy marr (i+1) arr (i+1) (isize arr - 1)

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insertM :: PA.Array e -> Int -> e -> ST s (PA.Array e)
insertM ary idx b
   | idx < 0 || idx > count = error ("Bounds check in insertAt "++show idx++" not in range 0.."++show (count))
   | otherwise = do
           mary <- mnew (count+1)
           mcopy mary 0 ary 0 idx
           mwrite mary idx b
           mcopy mary (idx+1) ary idx (count-idx)
           mfreeze mary
  where !count = isize ary
{-# INLINE insertM #-}

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insertAt :: PA.Array e -> Int -> e -> PA.Array e
insertAt arr idx b = runST(insertM arr idx b)


arrayOf :: Int -> a -> PA.Array a
arrayOf n a =  runST $ do
  marr <- PA.newArray n a
  arr <- mfreeze marr
  pure arr

insertBM:: ShortByteString -> v -> ByteMap v -> ByteMap v
insertBM bs v (ByteMap arr1) = ByteMap $
                             update arr1 x1 $
                             update arr2 x2 $
                             update arr3 x3 $
                             update arr4 x4 ((more,v):pairs)
   where ([x1,x2,x3,x4],more) = split bs
         arr2 = index arr1 x1
         arr3 = index arr2 x2
         arr4 = index arr3 x3
         pairs = index arr4 x4



emptyByteMap :: ByteMap v
emptyByteMap = ByteMap arr4
   where arr1 = arrayOf 16 []
         arr2 = arrayOf 16 arr1
         arr3 = arrayOf 16 arr2
         arr4 = arrayOf 16 arr3

m11@(ByteMap a1) = insertBM (bytes "ABCDE") True emptyByteMap
a2 = index a1 4
a3 = index a2 1
a4 = index a3 4

-- ===========================================

type Bitmap = Word64
type Shift  = Int
type Path = [Int]


data BitState
  = Even Path !Int !ShortByteString
  | Odd Path !Int !Int !ShortByteString

initBitState bytes = Even [] 0 bytes

instance Show BitState where
  show (Even path next bs) = "(Even "++show path++" "++show next++" "++show bs++")"
  show (Odd path next bits bs) = 
      "(Odd "++show path ++" "++show next++" "++show (bin bits)++" "++show bs++")"

getBytes :: BitState -> ShortByteString
getBytes (Even _ _ bs) = bs
getBytes (Odd _ _ _ bs) = bs

nextBits :: BitState -> (Int,BitState)
nextBits (Odd path next bits bytes) = (bits,Even (bits:path) next bytes)
nextBits (Even path next bytes)
  | next <0 || next >= SBS.length bytes = error ("nextBits out of bounds")
nextBits (Even path next bytes) =
  let word8 = SBS.index bytes next
      (x1,x2) = split4 word8
      i1 = fromIntegral x1
  in (i1,Odd (i1:path) (next+1) (fromIntegral x2) bytes)

getpath :: ShortByteString -> [Int]
getpath bs = go (initBitState bs) where
  go (Odd path next n bytes) = n : go (Even (n:path) next bytes)
  go (Even path next bytes)
    | next <0 || next >= SBS.length bytes = []
    | otherwise =
        let word8 = SBS.index bytes next
            (x1,x2) = split4 word8
            i1 = fromIntegral x1
        in i1 : go (Odd (i1:path) (next+1) (fromIntegral x2) bytes)

bitsFromCommonPrefix :: BitState -> ShortByteString -> (Bitmap,Int,Int)
bitsFromCommonPrefix (Even bm i bs1) bs2 =
  let word1 = SBS.index bs1 i
      word2 = SBS.index bs2 i
      (x1,_) = split4 word1
      (x2,_) = split4 word2
      i1 = fromIntegral x1
      i2 = fromIntegral x2
  in (setBit (setBit 0 i1) i2,i1,i2)

next2 :: BitState -> ShortByteString -> BitState
next2 (state@(Even path i _)) bs2 = (Even path i bs2)
next2 (Odd path next bits _) bs2 = snd (nextBits (Even (tail path) (next-1) bs2))

-- ===========================

-- | Strict, compact, version of [Int]
data IntList = INil | ICons {-# UNPACK #-} !Int ! IntList

fromIntList :: IntList -> [Int]
fromIntList INil = []
fromIntList (ICons x xs) = x : fromIntList xs

instance HeapWords IntList where
  heapWords INil = 1
  heapWords (ICons _ xs) = 2 + heapWords xs
  
app :: IntList -> IntList -> IntList
app INil xs = xs
app (ICons x xs) ys = ICons x (app xs ys)

one :: Int -> HashMap v -> HashMap v
one n (One path x) = One (ICons n path) x
one n x = One (ICons n INil) x

instance Show IntList where
  show xs = show (fromIntList xs)

data HashMap v
    = Empty
    | BitmapIndexed {-# UNPACK #-} !Bitmap !(PA.Array (HashMap v))
    | Leaf !ShortByteString v
    | Full !(PA.Array (HashMap v))
    | One IntList (HashMap v)

heapAdd :: HeapWords a => a -> Int -> Int
heapAdd x ans = heapWords x + ans

instance HeapWords v => HeapWords (HashMap v) where
  heapWords Empty = 1
  heapWords (One x xs) = 1 + heapWords x + heapWords xs
  heapWords (Leaf bs v) = 3 + heapWords bs + heapWords v
  heapWords (BitmapIndexed _ arr) = foldr heapAdd (3 + isize arr) arr
  heapWords (Full arr) = foldr heapAdd (2 + isize arr) arr
  
count :: HashMap v -> (Int,Int,Int,Int,Int,Int)
count x = go x (0,0,0,0,0,0)
  where go Empty (e,o,l,b,f,m) = (e+1,o,l,b,f,m)
        go (One _ x) (e,o,l,b,f,m) = go x (e,o+1,l,b,f,m)
        go (Leaf _ _) (e,o,l,b,f,m) = (e,o,l+1,b,f,m)
        go (BitmapIndexed _ arr) (e,o,l,b,f,m) = foldr go (e,o,l,b+1,f,max m (length arr)) arr
        go (Full arr) (e,o,l,b,f,m) = foldr go (e,o,l,b,f+1,m) arr

instance Show v => Show (HashMap v) where
  show Empty = "E"
  show (Leaf s v) = "(L "++show s++" "++show v++")"
  show (Full arr) = "(F "++show(tolist arr)++")"
  show (BitmapIndexed bm arr) = "(B "++show(bin bm)++" "++show(tolist arr)++")"
  show (One n x) = "(O "++show n++" "++show x++")"

makeTwo :: BitState -> HashMap v -> BitState -> v -> HashMap v
makeTwo state1 leaf1 state2 val2 
      | i1==i2 =  one i1 (makeTwo state1' leaf1 state2' val2)
                  -- BitmapIndexed (setBit 0 i1) (fromlist[makeTwo state1' leaf1 state2' val2])
      | otherwise = BitmapIndexed (setBit (setBit 0 i1) i2) (fromlist list)
   where (i1,state1') = nextBits state1
         (i2,state2') = nextBits state2
         list = if i1 < i2
                   then [leaf1,Leaf (getBytes state1') val2]
                   else [Leaf (getBytes state1') val2,leaf1]


insert' :: Show v => BitState -> v -> HashMap v -> HashMap v
insert' bs0 v0 m0 = gotrace bs0 v0 m0
  where
    gotrace s x m = -- trace ("\nGO\n  "++show s++"\n  "++show x++"\n  "++show m) $
                    go s x m
    go !state !x Empty = Leaf (getBytes state) x
    go !state !x (old@(One (path1@(ICons j _)) _)) =
       -- trace ("One i="++show i) $
        case compare j i of
          EQ ->  one i (gotrace state1 x old)
          LT -> BitmapIndexed bmap (fromlist [old,new])
          GT -> BitmapIndexed bmap (fromlist [new,old])
      where (!i,!state1) = nextBits state
            new = gotrace state1 x Empty
            bmap = setBit (setBit 0 i) j
            
    go state x t@(Leaf bs1 y)
      | getBytes state == bs1 = if x `ptrEq` y then t else (Leaf bs1 x)
      | otherwise = makeTwo state t (next2 state bs1) x
    go state x t@(BitmapIndexed bmap arr)
        | not(testBit bmap bits) = -- trace ("NOT THERE i="++show i++" bits="++show bits) $
            let !arr' = insertAt arr i $! (Leaf (getBytes state) x)
            in bitmapIndexedOrFull (bmap .|. m) arr'
        | otherwise =
            let !st = -- trace ("BitmapedIndex i="++show i++" arr="++show arr++" state="++show state) $
                      index arr i
                !st' = gotrace state1 x st
            in if st' `ptrEq` st
                  then t
                  else BitmapIndexed bmap (update arr i st') 
       where (!bits,!state1) = nextBits state
             m = setBit 0 bits
             i = sparseIndex bmap m
    go state x t@(Full arr) =
        let !st = index arr i
            !st' = gotrace state1 x st
        in if st' `ptrEq` st
              then t
              else Full (update arr i st') 
       where (!bits,!state1) = nextBits state
             m = setBit 0 bits
             i = sparseIndex fullNodeMask m

insert :: Show v => ShortByteString -> v -> HashMap v -> HashMap v
insert bs v hashmap = insert' (initBitState bs) v hashmap            

fromList :: Show v => [(ShortByteString,v)] -> HashMap v
fromList pairs = foldl' accum Empty pairs
  where accum ans (k,v) = insert k v ans
  

i1 = insert' (initBitState (bytes "ABC")) (99::Int) Empty
i2 = insert' (initBitState (bytes "ABD")) 21 i1
i3 = insert' (initBitState (bytes "wxy")) 49 i2
i4 = insert' (initBitState (bytes "wxz")) 63 i3

m1 = Map.insert (bytes "ABC") (99::Int) Map.empty
m2 = Map.insert (bytes "ABD") 21 m1
m3 = Map.insert (bytes "wxy") 49 m2
m4 = Map.insert (bytes "wxz") 63 m3


j1 = insert' (initBitState (bytes "wxy")) 49 Empty
j2 = insert' (initBitState (bytes "wxz")) 63 j1

-- ==========================

-- | Check if two the two arguments are the same value.  N.B. This
-- function might give false negatives (due to GC moving objects.)
ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y ==# 1#)
{-# INLINE ptrEq #-}


bitsPerSubkey :: Int
bitsPerSubkey = 4

maxChildren :: Int
maxChildren = 1 `unsafeShiftL` bitsPerSubkey

subkeyMask :: Bitmap
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1

sparseIndex :: Bitmap -> Bitmap -> Int
sparseIndex b m = popCount (b .&. (m - 1))

-- | Create a 'BitmapIndexed' or 'Full' node.
bitmapIndexedOrFull :: Bitmap -> PA.Array (HashMap v) -> HashMap v
bitmapIndexedOrFull b ary
    | b == fullNodeMask = Full ary
    | otherwise         = BitmapIndexed b ary
{-# INLINE bitmapIndexedOrFull #-}


{-
mask :: Word -> Shift -> Bitmap
mask w s = 1 `unsafeShiftL` getindex w s
{-# INLINE mask #-}


-- | Mask out the 'bitsPerSubkey' bits used for indexing at this level
-- of the tree.
getindex :: Hash -> Shift -> Int
getindex w s = fromIntegral $ (unsafeShiftR w s) .&. subkeyMask
{-# INLINE getindex #-}
-}


-- | A bitmask with the 'bitsPerSubkey' least significant bits set.
fullNodeMask :: Bitmap
fullNodeMask = complement (complement 0 `unsafeShiftL` maxChildren)
{-# INLINE fullNodeMask #-}


unJust (Just x) = x
unJust Nothing = error "unJust"

manyStrings :: [ShortByteString]
manyStrings = [ bytes $ map chr [c1,c2,c3,c4,c5]
              | c1 <- [4::Int, 9, 35, 89, 124]
              , c2 <- [44,60,19,102,88]
              , c3 <- [33, 110, 66, 95, 118]
              , c4 <- [65,75,85,95,105]
              , c5 <- [76,86,96,106,116]
              ]

pairs = zip manyStrings [0..] 

map10:: Map ShortByteString Int
map10 = Map.fromList pairs

hash10 :: HashMap Int
hash10 = fromList pairs

foo :: (Int,Int)
foo = (heapWords map10, heapWords hash10)

