{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where


import Data.ByteString.Short (ShortByteString,toShort,fromShort,pack,unpack)
import qualified Data.ByteString.Short as SBS
import Data.ByteString(ByteString)
import Data.Foldable (foldr',foldl')
import Cardano.Prelude (HeapWords (..),Generic,runST,ST)
import Data.Word(Word8,Word64)
import qualified Data.Primitive.Array as PA
import qualified Data.Array as A
import qualified Data.Array.MArray as MutA
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
import Data.Hashable(hash)
import Data.List(sort,nub)
import Data.Semigroup(Semigroup(..))
import Data.Monoid(Monoid(..))
import Control.DeepSeq (NFData, force, ($!!))
import qualified GHC.Prim as Prim
import qualified Data.Primitive.SmallArray as Small
import Data.Primitive.SmallArray(SmallArray)
import System.Random(genShortByteString,genWord64,mkStdGen)

makeByteStrings :: Int -> Int -> Int -> [ShortByteString]
makeByteStrings seed size count = loop (mkStdGen seed) count []
  where loop g i ans | i <= 0 = ans
        loop g i ans = case genShortByteString size g of
                         (bytes,g2) -> loop g2 (i-1) (bytes:ans)
           

{-
import GHC.DataSize(recursiveSize)
                     , ghc-datasize
                     , ghc-lib-parser
                     , ghc-heap
-}                     
recursiveSize :: a -> IO Int
recursiveSize x = pure 99

-- type PArray = PA.Array
type PArray = Small.SmallArray
-- ===================================================

data Stat n = Stat n n (Maybe n) (Maybe n)

liftM:: (t -> t -> t) -> Maybe t -> Maybe t -> Maybe t
liftM f (Just x) (Just y) = Just(f x y)
liftM f Nothing (Just y) = Just y
liftM f (Just x) Nothing = Just x
liftM f Nothing Nothing = Nothing

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

data ByteMap v = ByteMap (PArray (PArray (PArray (PArray [(ShortByteString,v)]))))
  deriving Show

-- | /O(n)/ Overwrite the element at the given position in this array,
update :: PArray t -> Int -> t -> PArray t
update arr i t
  | i<0 || i >= (isize arr)
  = error ("index out of bounds in update "++show i++" not in range (0,"++show (isize arr -1)++")")
update arr i t = fst(withMutArray size action)
  where size = isize arr
        action marr = do
          mcopy marr 0 arr 0 i
          mwrite marr i t         
          mcopy marr (i+1) arr (i+1) (size - (i+1))

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insertM :: PArray e -> Int -> e -> ST s (PArray e)
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
      "(Odd "++show path ++" "++show next++" "++show bits {- (bin bits) -}++" "++show bs++")"

getBytes :: BitState -> ShortByteString
getBytes (Even _ _ bs) = bs
getBytes (Odd _ _ _ bs) = bs

nextBits :: BitState -> (Int,BitState)
nextBits (Odd path next bits bytes) = (bits,Even (bits:path) next bytes)
nextBits (Even path next bytes)
  | next <0 || next >= SBS.length bytes =
      error ("nextBits out of bounds: "++show next++" not in range (0.."++show((SBS.length bytes)-1)++")\n  "++show(getpath bytes)++"\n  "++show bytes)
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



next2 :: BitState -> ShortByteString -> BitState
next2 (state@(Even path i _)) bs2 = (Even path i bs2)
next2 (Odd path next bits _) bs2 = snd (nextBits (Even (tail path) (next-1) bs2))

-- ============================================================

data HashMap v
    = Empty
    | Leaf !ShortByteString v
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
  heapWords (One x xs) = 3 + heapWords xs
  heapWords (Leaf bs v) = 3 + heapWords bs + heapWords v
  heapWords (BitmapIndexed _ arr) = foldl' heapPlus 3 arr
  heapWords (Full arr) = foldl' heapPlus 2 arr
  heapWords (Two x a b) = 4 + heapWords a + heapWords b                       
                         
instance Show v => Show (HashMap v) where
  show Empty = "E"
  show (Leaf s v) = "(L "++show s++" "++show v++")"
  show (Full arr) = "(F "++show(tolist arr)++")"
  show (BitmapIndexed bm arr) = "(B "++show(bin bm)++" "++show(tolist arr)++")"
  show (One n x) = "(O "++show n++" "++show x++")"
  show (Two n x y) = "(O "++show n++" "++show x++" "++show y++")"

makeTwo :: Show v => BitState -> HashMap v -> BitState -> v -> HashMap v
makeTwo state1 leaf1 state2 val2 
      | i1==i2 = One i1 (makeTwo state1' leaf1 state2' val2)
      | otherwise = if i1 < i2
                       then Two (setBit (setBit 0 i1) i2) (Leaf (getBytes state1') val2) leaf1
                       else Two (setBit (setBit 0 i1) i2) leaf1 (Leaf (getBytes state1') val2) 
   where (i1,state1') = nextBits state1
         (i2,state2') = nextBits state2


testgo = makeTwo state1 leaf1 state2 val2
  where val2 = 1
        leaf1 = Leaf bytes1 0
        bytes1 = bytes "\NUL\SOH\NUL\SOH"
        bytes2 = bytes "\SOH\SOH\NUL\SOH"
        state1 = Even [] 0 bytes2
        state2 = Even [] 0 bytes1


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
      | otherwise = makeTwo state t (next2 state bs1) x
    go state x t@(BitmapIndexed bmap arr)
        | not(testBit bmap bits) =
            let !arr' = insertAt arr i $! (Leaf (getBytes state) x)
            in bitmapIndexedOrFull (bmap .|. m) arr'
        | otherwise =
            let !st = index arr i
                !st' = go state1 x st
            in if st' `ptrEq` st
                  then t
                  else BitmapIndexed bmap (update arr i st') 
       where (!bits,!state1) = nextBits state
             m = setBit 0 bits
             i = sparseIndex bmap m
    go state x t@(Two bmap x0 x1)
        | not(testBit bmap bits) =
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
       where (!bits,!state1) = nextBits state
             m = setBit 0 bits
             i = sparseIndex bmap m
    go state x t@(Full arr) =
        let !st = index arr i
            !st' = go state1 x st
        in if st' `ptrEq` st
              then t
              else Full (update arr i st') 
       where (!bits,!state1) = nextBits state
             m = setBit 0 bits
             i = sparseIndex fullNodeMask m

indexFromStateAndBitmap :: BitState -> Bitmap -> (BitState,Int)
indexFromStateAndBitmap state bmap = (state1,sparseIndex bmap m)
   where  (!bits,!state1) = nextBits state
          m = setBit 0 bits

insert :: Show v => ShortByteString -> v -> HashMap v -> HashMap v
insert bs v hashmap = insert' (initBitState bs) v hashmap            

lookup' :: BitState -> HashMap v -> Maybe v
lookup' _ Empty = Nothing
lookup' state (Leaf bs v) = if (getBytes state)==bs then Just v else Nothing
lookup' state (One i x) = if i==j then lookup' state' x else Nothing
  where (j,state') = nextBits state
lookup' state (Two bm x0 x1) = if i==0 then lookup' state' x0 else lookup' state' x1
  where (state',i) = indexFromStateAndBitmap state bm
lookup' state (BitmapIndexed bm arr) = lookup' state' (index arr i)
  where (state',i) = indexFromStateAndBitmap state bm
lookup' state (Full arr) = lookup' state' (index arr i)
  where (state',i) = indexFromStateAndBitmap state fullNodeMask    

lookupHM :: ShortByteString -> HashMap v -> Maybe v
lookupHM bytes mp = lookup' (initBitState bytes) mp

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

-- =============================================

unJust (Just x) = x
unJust Nothing = error "unJust"

manyStrings :: [ShortByteString]
manyStrings = [ bytes $ map chr [c1,c2,c3,c4,c5]
              | c1 <- [4::Int, 9, 35, 45, 89, 124, 128]
              , c2 <- [44,60,19,102,88, 90, 103]
              , c3 <- [33, 66, 88, 110, 66, 95, 118]
              , c4 <- [40, 52,65,75,85,95,105]
              , c5 <- [66, 70,76,86,96,106,116]
              ]

pairs = zip bpairs [0..] 

map10:: Map ShortByteString Int
map10 = Map.fromList pairs

hash10 :: HashMap Int
hash10 = fromList (take 10 pairs)

hash1000 = fromList (take 1000 pairs)

testt n = do
   let (hmap,output) = tests n
   hbytes <- recursiveSize $!! hmap
   putStrLn ("hbytes = "++show hbytes++"\n"++output)

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
  where go d Empty (e,o,t,l,b,f) = (e+1,o,t,l,b,f)
        go d (One i x) (e,o,t,l,b,f) = go (1 + d) x (e,1+o,t,l,b,f)
        go d (Two i x y) (e,o,t,l,b,f) = go (1+d) y (go (1+d) x (e,o,1+t,l,b,f))
        go d (Leaf _ _) (e,o,t,l,b,f) = (e,o,t,add d l,b,f)
        go d (BitmapIndexed _ arr) (e,o,t,l,b,f) =
          foldr (go (length arr + d)) (e,o,t,l,add (length arr) b,f) arr
        go d (Full arr) (e,o,t,l,b,f) = foldr  (go (length arr + d)) (e,o,t,l,b,f+1) arr



hdepth :: HashMap v -> Int
hdepth Empty = 0
hdepth (One xs x) = 1 + hdepth x
hdepth (Leaf _ _) = 1
hdepth (BitmapIndexed _ arr) = 1+ maximum(foldr (\ x ans -> hdepth x : ans) [] arr)            
hdepth (Full arr) = 1+ maximum(foldr (\ x ans -> hdepth x : ans) [] arr)
hdepth (Two _ x y) = 1 + max (hdepth x) (hdepth y)

breadth:: HashMap v -> (String,[HashMap v])
breadth Empty = ("E",[])
breadth (One xs x) = ("O",[x])
breadth (Two _  x y) = ("T",[x,y])
breadth (Leaf _ _) = ("L",[])
breadth (BitmapIndexed _ arr) = ("B",foldr (:) [] arr)         
breadth (Full arr) = ("F",foldr (:) [] arr)

plistf :: (a -> String) -> String -> [a] -> String -> String ->String
plistf f open xs comma close = open++help xs++close
  where help [] = ""
        help [y] = f y
        help (y:ys) = f y ++ comma ++ help ys

myunlines xs = trace ("UNLINES "++show xs) $ unlines (filter (\ x -> not(x=="\n") && not(x=="")) xs)

indent n str = [' ' | i <- [1..n]]++ str

hview :: Int -> Int -> HashMap v -> [String]
hview tot 0 hm = []
hview tot n hm = case breadth hm of
    (nm,[]) -> [indent ((tot - n)+3) nm]
    (nm,xs) -> let pairs = map breadth xs
                   proj (n,ys) = n
                   row = indent (2 * (tot - n)) (nm++plistf proj "[" pairs "" "]")
                   reccall (_,ys) = map (hview tot (n-1)) ys
                   rows = concat $ map (concat . reccall) pairs
               in (row:rows) 


foo :: (Int,Int)
foo = (heapWords map10, heapWords hash10)


ipairs :: [(Int,Int)]
ipairs = [ (i,j) | i <- [1 .. 500], j <-  [1000, 999 .. 501]]

hpairs :: [Int]
hpairs = map hash ipairs

makeBytes :: Int -> ShortByteString
makeBytes n1 = pack (map fromIntegral[x1,x2,x3,x4])
   where x1 = mod n1 256
         n2 = div n1 256
         x2 = mod n2 256
         n3 = div n2 256
         x3 = mod n3 256
         n4 = div n3 246
         x4 = mod n4 256

bpairs = -- map makeBytes (sort hpairs)
         makeByteStrings 99 4 250000


-- ===========================

-- | Strict, compact, version of [Int]
data IntList = INil | ICons {-# UNPACK #-} !Int ! IntList

fromIntList :: IntList -> [Int]
fromIntList INil = []
fromIntList (ICons x xs) = x : fromIntList xs

intlen :: IntList -> Int
intlen xs = length (fromIntList xs)

instance HeapWords IntList where
  heapWords INil = 1
  heapWords (ICons _ xs) = 2 + heapWords xs
  
app :: IntList -> IntList -> IntList
app INil xs = xs
app (ICons x xs) ys = ICons x (app xs ys)

instance Show IntList where
  show xs = show (fromIntList xs)


sizes n = do 
    let hashmap = fromList (take n pairs)
        mapmap = Map.fromList (take n pairs)
        hwords = heapWords hashmap
        mwords = heapWords mapmap
    hbytes <-  recursiveSize $!! hashmap
    mbytes <- recursiveSize $!! mapmap
    putStrLn (unlines
      [ "hash words = "++show hwords
      , "map words = "++show mwords
      , "hash bytes = "++show hbytes
      , "map bytes = "++show mbytes
      , "word ratio = "++show ((hwords * 100) `div` mwords)
      , "byte ratio = "++show ((hbytes * 100) `div` mbytes)   
      ] )


makeSmall :: Int -> a -> Small.SmallArray a
makeSmall size i = runST $ do
   marr <- Small.newSmallArray size i
   Small.unsafeFreezeSmallArray marr

main = testt 63536