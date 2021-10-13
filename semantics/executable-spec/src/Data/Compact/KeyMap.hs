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
import Data.Bits ((.&.), (.|.), complement, popCount, unsafeShiftL,setBit,testBit)
import Data.Compact.Class
import GHC.Exts ((==#), reallyUnsafePtrEquality#, isTrue# )
import qualified Data.Map as Map
import Control.DeepSeq (NFData(..))
import qualified Data.Primitive.SmallArray as Small
import Data.Primitive.SmallArray()
import System.Random(genWord64,mkStdGen)
import Prettyprinter
import Prettyprinter.Internal (Doc)
import Data.Text(Text,pack)
import qualified Prettyprinter.Internal as Pretty

import Debug.Trace

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
bits = 3

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

w64 :: Word64
w64 = 15857176389017222826

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
  deriving (Eq,Ord,Show,NFData,Generic)

-- | The number of Word64 per key
wordsPerKey :: Int
wordsPerKey = 2

-- | The length of a Path for a Key (which might have multiple Word64's inside)
keyPathSize :: Int
keyPathSize = wordsPerKey * (fromIntegral pathSize)

genKey g = (Key w0 w1,g2)
  where (w0,g1) = genWord64 g
        (w1,g2) = genWord64 g1

-- | Break up a Key into a Path
path :: Key -> Path
path (Key w0 w1) = getpath w0 ++ getpath w1

-- | A pair of a Key and its equivalent Path
data BitState = BitState Path !Key

-- Initialize a BitState from a Key
initBitState :: Key -> BitState
initBitState key = BitState (path key) key

-- | Obtain the Key from a BitState
getBytes :: BitState -> Key
getBytes (BitState _ bs) = bs

-- | Consume the next segment from a path 
nextBits :: String -> BitState -> (Int,BitState)
nextBits _ (BitState (x:xs) key) = (x,BitState xs key)
nextBits message (BitState [] key) =
   error ("NextBits out of bounds. The key "++show key++" has run out of bits."++message)
 
next2 :: BitState -> Key -> BitState
next2 (st@(BitState ps _)) key = -- trace ("NEXT2 n="++show n++"\n st="++show st++"\n key="++show key) $
                            (BitState (drop n (path key)) key)
   where n = (fromIntegral keyPathSize) - length ps 

showBM :: Bitmap -> String
showBM bm = show(loop 63 [])
  where loop i ans | i < 0 = ans
        loop i ans = if testBit bm i then loop (i-1) (i:ans) else loop (i-1) ans

instance HeapWords Key where
  heapWords (Key _ _ ) = 3

instance Show BitState where
  show (BitState p key) = "(BitState "++show p++" "++show key++")"


-- ===============================================================

data HashMap v
    = Empty
    | Leaf {-# UNPACK #-} !Key v
    | One {-# UNPACK #-} !Int (HashMap v)                           -- 1 subtree
    | Two {-# UNPACK #-} !Bitmap (HashMap v) (HashMap v)            -- 2 subtrees
    | BitmapIndexed {-# UNPACK #-} !Bitmap !(PArray (HashMap v))    -- 3 - (intSize - 1) subtrees
    | Full !(PArray (HashMap v))                                    -- intSize subtrees
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

{-
instance Show v => Show (HashMap v) where
  show Empty = "E"
  show (Leaf s v) = "(L "++show s++" "++show v++")"
  show (Full arr) = "(F "++show(tolist arr)++")"
  show (BitmapIndexed bm arr) = "(B "++showBM bm++" "++show(tolist arr)++")"
  show (One n x) = "(O "++show n++" "++show x++")"
  show (Two bm x y) = "(T "++showBM bm++" "++show x++" "++show y++")"
-}

-- ======================================================================


insert' :: Show v => BitState -> v -> HashMap v -> HashMap v
insert' bs0 v0 m0 = go bs0 v0 m0
  where
    go !state !x Empty = Leaf (getBytes state) x
    go !state x (old@(One j node)) =
        case (nextBits "One" state) of
          (i,state1) ->
            case compare j i of
              EQ -> One j (go state1 x node)
              LT -> Two (setBit (setBit 0 i) j) node (go state1 x Empty)
              GT -> Two (setBit (setBit 0 i) j) (go state1 x Empty) node
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
       where (!tagbits,!state1) = nextBits "BitmapIndexed" state
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
       where (!tagbits,!state1) = nextBits "Two" state
             m = setBit 0 tagbits
             i = sparseIndex bmap m
    go state x t@(Full arr) =
        let !st = index arr i
            !st' = go state1 x st
        in if st' `ptrEq` st
              then t
              else Full (update arr i st') 
       where (!tagbits,!state1) = nextBits "Full" state
             m = setBit 0 tagbits
             i = sparseIndex fullNodeMask m

makeTwo :: Show v => BitState -> HashMap v -> BitState -> v -> HashMap v
makeTwo state1 leaf1 state2 val2 
      | i1==i2 = One i1 (makeTwo state1' leaf1 state2' val2)
      | otherwise = -- trace ("MAKETWO (i1,i2)="++show(i1,i2)++"\n  state1="++show state1++"\n  state2="++show state2) $
                    if i1 < i2
                       then Two (setBit (setBit 0 i1) i2) (Leaf (getBytes state1') val2) leaf1
                       else Two (setBit (setBit 0 i1) i2) leaf1 (Leaf (getBytes state1') val2) 
   where (i1,state1') = nextBits "makeTwo1" state1
         (i2,state2') = nextBits ("makeTwo2 "++"\n  "++show state1++"\n  "++show state2) state2

insert :: Show v => Key -> v -> HashMap v -> HashMap v
insert bs v hashmap = insert' (initBitState bs) v hashmap            

fromList :: Show v => [(Key,v)] -> HashMap v
fromList ps = foldl' accum Empty ps
  where accum ans (k,v) = insert k v ans


indexFromStateAndBitmap :: BitState -> Bitmap -> (BitState,Int)
indexFromStateAndBitmap state bmap = (state1,sparseIndex bmap m)
   where  (!bits,!state1) = nextBits "indexFromStateAndBitmap" state
          m = setBit 0 bits


lookup' :: PrettyA v => BitState -> HashMap v -> Maybe v
lookup' _ Empty = Nothing
lookup' state (Leaf bs v) = if (getBytes state)==bs then Just v else Nothing
lookup' state (One i x) = if i==j then lookup' state' x else Nothing
  where (j,state') = nextBits "ONE in lookup" state
lookup' state (Two bm x0 x1) = if i==0 then lookup' state' x0 else lookup' state' x1
  where (state',i) = indexFromStateAndBitmap state bm
lookup' state (BitmapIndexed bm arr) = lookup' state' (index arr i)
  where (state',i) = indexFromStateAndBitmap state bm
lookup' state (Full arr) = lookup' state' (index arr i)
  where (state',i) = indexFromStateAndBitmap state fullNodeMask    

lookupHM :: PrettyA v => Key -> HashMap v -> Maybe v
lookupHM bytes mp = lookup' (initBitState bytes) mp


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
        loop g i ans = case genKey g of
                         (key,g2) -> loop g2 (i-1) (key : ans)

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
histogram (BitmapIndexed _ arr) marr = increment marr (isize arr-1) >> mapM_ (\ x -> histogram x marr) arr     
histogram (Full arr) marr = increment marr (intSize-1) >> mapM_ (\ x -> histogram x marr) arr  
histogram (Two _ x y) marr = increment marr 2 >> histogram x marr >> histogram y marr

histo :: HashMap v -> PA.Array Int
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

bug :: Int -> IO (HashMap Int)
bug n = do
   let ps = take n pairs -- zip (makeKeys 3 n) [0..]
       hh (k@(Key m0 m1),v) = show m0++" "++show m1++" "++show (path k)++" "++show v
   putStrLn (unlines (map hh ps))

   -- putStrLn (show (fromList ps))
   pure (fromList ps)

try :: [(Key,Int)] -> IO ()
try ps = do
   let  hh (k@(Key m0 m1),v) = show m0++" "++show m1++" "++show (path k)++" "++show v
   putStrLn (unlines (map hh ps))
   putStrLn (show (fromList ps))


testlookup seed n = all ok results
  where ps = zip (makeKeys seed n) [0..]
        keymap :: HashMap Int
        keymap = fromList ps
        results = [ (i,lookupHM (fst(ps !! i)) keymap) | i <- [0..(n-1)]]
        ok (i,Just _) = True
        ok (i,Nothing) = error ("testlookup failure: "++show i++"   "++show pair++"\n"++
                                show (path (fst pair))++"\n  "++show keymap)
          where pair = (ps !! i)




-- ======================================================================================

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

-- | ppRecord name [("a",x),("b",y),("c",z)] --> name { a = x, b = y, c = z }
ppRecord :: Text -> [(Text, PDoc)] -> PDoc
ppRecord con = ppRecord' (text con)

ppRecord' :: PDoc -> [(Text, PDoc)] -> PDoc
ppRecord' con fields =
  group $
    flatAlt
      (hang 1 (vcat [con, puncLeft lbrace (map (\(x, y) -> equate (text x) y) fields) comma rbrace]))
      (con <> encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) (map (\(x, y) -> equate (text x) y) fields))

-- | Vertical layout with commas aligned on the left hand side
puncLeft :: Doc ann -> [Doc ann] -> Doc ann -> Doc ann -> Doc ann
puncLeft open [] _ close = hsep [open, close]
puncLeft open [x] _ close = hsep [open, x, close]
puncLeft open (x : xs) coma close = align (sep ((open <+> x) : help xs))
  where
    help [] = mempty
    help [y] = [hsep [coma, y, close]]
    help (y : ys) = (coma <+> y) : help ys

-- ppSet :: (x -> Doc ann) -> Set x -> Doc ann
-- ppSet p xs = encloseSep lbrace rbrace comma (map p (toList xs))

ppList :: (x -> Doc ann) -> [x] -> Doc ann
ppList p xs =
  group $
    flatAlt
      (puncLeft lbracket (map p xs) comma rbracket)
      (encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) (map p xs))

newtype PrettyAnn = Width Int

type Ann = [PrettyAnn]

type PDoc = Doc Ann

text :: Text -> Doc ann
text = pretty

isEmpty :: Doc ann -> Bool
isEmpty Pretty.Empty = True
isEmpty _ = False

-- | x == y
equate :: Doc a -> Doc a -> Doc a
equate x y = group (flatAlt (hang 2 (sep [x <+> text "=", y])) (hsep [x, text "=", y]))

ppArray :: (Indexable arr a) => (a -> PDoc) -> arr a -> PDoc
ppArray f arr = ppList f (tolist arr)

ppKey :: Key -> PDoc
ppKey (Key w0 w1) = ppWord64 w0

ppBitmap :: Word64 -> PDoc
ppBitmap x = text (pack(showBM x))

ppHashMap :: (v -> PDoc) -> HashMap v -> PDoc
ppHashMap p (Leaf k v) = ppSexp "L" [ppKey k,p v]
ppHashMap p Empty = text "E"
ppHashMap p (One x mp) = ppSexp "O" [ppInt x,ppHashMap p mp]
ppHashMap p (Two x m1 m2) = ppSexp "T" [ppBitmap x ,ppHashMap p m1, ppHashMap p m2]
ppHashMap p (BitmapIndexed x arr) = ppSexp "B" [ppBitmap x, ppArray (ppHashMap p) arr]
ppHashMap p (Full arr) = ppSexp "F" [ppArray (ppHashMap p) arr]



ppWord64 :: Word64 -> Doc a
ppWord64 = viaShow

ppInt :: Int -> Doc a
ppInt = viaShow

class PrettyA t where
  prettyA :: t -> PDoc

instance PrettyA Int where
  prettyA = ppInt

instance PrettyA Word64 where
  prettyA = ppWord64

instance PrettyA v => Show (HashMap v) where
   show x = show(ppHashMap prettyA x)


{-

[7,35,37,39,33,32,55,17,17,41,32,7,40,4,17,54,17,59,54,53,16,13]
  (F
   [ (B 0
        [3,4,5,16,20,25,26,43,44,54,56]
        [ (L 57518414112576775 9)
        , (L 79161844310149552 230)
        , (L 98037892909917831 57)
        , (L 289587315466625341 85)
        , (L 361576054454028671 169)
        , (L 465516706021833058 165)
        , (L 469248930710233714 103)
        , (L 775844444642771318 116)
        , (L 807462187389463236 78)
        , (L 978346011746192157 132)
        , (L 1017339447627065800 225) ])
   , (B 1
        [2,4,7,10,21,22,23,24,25,28,30,31,39,55,56,61]
        [ (L 1203856813509640105 119)
        , (L 1236851003849005022 220)
        , (T [22,46] (L 1285344350358277756 162) (L 1292147947718786568 197))
        , (L 1334664866556199165 42)
        , (L 1544280647218138136 184)
        , (L 1563222312429746588 18)
        , (L 1583488883435192445 60)
        , (L 1592604182415922927 10)
        , (L 1617635987495876897 209)
        , (L 1666155074360690202 36)
        , (L 1703570667790261248 20)
        , (L 1725248650990346024 137)
        , (L 1856650865672868387 83)
        , (L 2159908387576321810 247)
        , (L 2177435999704097471 212)
        , (L 2269075143845594343 193) ])
   , (B 2
        [1,2,10,11,14,18,21,28,29,32,38,44,46,50,51,55,57,60,61]
        [ (L 2329472699964605258 63)
        , (L 2346676812779263221 72)
        , (L 2499804900447733724 154)
        , (L 2510433961290480493 91)
        , (L 2558840784794223868 26)
        , (L 2638760800934756383 179)
        , (L 2697863414882185642 111)
        , (L 2810251288725716440 200)
        , (T [37,54] (L 2838716920538874962 45) (L 2843511855584341842 155))
        , (L 2888216952139341863 168)
        , (L 2996620230064525971 228)
        , (L 3101906674616480511 240)
        , (T [2,13] (L 3135141178682686146 109) (L 3138402511406753062 229))
        , (L 3211613688267995163 215)
        , (L 3227142790200447171 121)
        , (L 3309393645584686769 131)
        , (L 3334114561084316694 118)
        , (L 3387644813653541004 30)
        , (L 3414238547548212628 221) ])
   , (B 3
        [7,8,9,11,20,22,29,30,35,36,37,43,46,51,53,54,55]
        [ (L 3597325232245202014 16)
        , (L 3617890454774200018 8)
        , (L 3626808784048768660 49)
        , (L 3666236866859606001 110)
        , (T [5,55] (L 3820597323462828461 52) (L 3834604538989342349 90))
        , (L 3866668609701600919 123)
        , (T [26,55] (L 3988765614336798211 56) (L 3996733424096793634 34))
        , (L 4005220472664322930 142)
        , (L 4090530311814183187 199)
        , (L 4115940976025508840 202)
        , (L 4132759924848597702 32)
        , (L 4239004744761485884 50)
        , (L 4300201023290284666 249)
        , (L 4382870014997944371 47)
        , (L 4423187909528534906 195)
        , (L 4445817334513325610 37)
        , (L 4463646245369540898 164) ])
   , (B 4
        [0,3,10,15,28,30,40,46,62]
        [ (L 4617522403979787684 223)
        , (L 4682078959067470032 35)
        , (L 4804231651351807491 3)
        , (L 4889317643552605283 140)
        , (L 5121575073183431395 206)
        , (L 5162580252158008570 244)
        , (L 5342589992548318716 98)
        , (L 5455277825186539030 226)
        , (L 5743748699829036056 239) ])
   , (B 5
        [1,2,7,8,13,15,18,19,23,27,33,34,53,54,56,58,63]
        [ (B
             [26,39,54]
             [ (L 5790207239766293568 139)
             , (T
                  [25,40]
                  (L 5793710529195663417 145)
                  (L 5793778283362933183 182))
             , (L 5798069027847828359 233) ])
        , (L 5812452206004160359 211)
        , (L 5894943135454736701 198)
        , (T [12,19] (L 5912369939478525317 71) (L 5914164751630216140 227))
        , (L 6015856508099050855 53)
        , (L 6051420878571073132 136)
        , (L 6092942864932358720 181)
        , (L 6118127737775688697 126)
        , (L 6179887585201247729 188)
        , (L 6252789785754261033 29)
        , (L 6364911225130164844 147)
        , (L 6379429005556642133 43)
        , (L 6719427973711746798 214)
        , (L 6750161089585005816 38)
        , (L 6776956135045145773 6)
        , (L 6816157026003846852 128)
        , (L 6910391580097842928 125) ])
   , (B 6
        [5,15,19,20,25,26,34,36,40,42,43,46,47,48,52,60,63]
        [ (L 7009794495276693398 28)
        , (L 7193495244436990754 112)
        , (O
             17
             (T [10,40] (L 7264634616559000384 1) (L 7264766555639728494 93)))
        , (L 7285390446855245622 134)
        , (T [13,25] (L 7371653566273470482 194) (L 7375058465498704623 94))
        , (L 7399938358501860357 24)
        , (L 7544474792767341272 101)
        , (L 7580690424433997145 62)
        , (L 7646821726517624094 33)
        , (L 7682757470817706036 86)
        , (L 7696165222433332195 4)
        , (L 7747908467003179735 180)
        , (L 7781662560152792066 97)
        , (L 7792537786188511772 159)
        , (L 7862234592991449041 114)
        , (L 8008829769479306349 23)
        , (T [4,21] (L 8053618295678634088 231) (L 8058529308055761883 148)) ])

--  (Key 8711542881061771872 8792230869434323981,2)
-- [7,35,37,39,33,32,55,17,17,41,32,7,40,4,17,54,17,59,54,53,16,13]
   , (B 7
        [2,5,11,13,19,21,35,37,39,43,57]
        [ (L 8113054085486348325 68)
        , (L 8160582094941124349 96)
        , (L 8274670832435728453 88)
        , (L 8314843442592392249 65)
        , (L 8418683400440831027 161)
        , (L 8465049282651060387 144)
        , (O
             35
             (T [25,37] (L 8708016982096121964 12) (L 8711542881061771872 2)))
        , (L 8744117925681161294 55)
        , (L 8776103184358612948 107)
        , (L 8849903612813549654 92)
        , (L 9113638603762076480 117) ])
   , (B 8
        [4,6,9,11,12,16,19,20,32,33,41,48,55,57,59]
        [ (L 9295751140287025676 158)
        , (L 9344351964414737186 76)
        , (L 9400646064964664204 217)
        , (L 9436719900190150312 66)
        , (L 9445918857912215984 15)
        , (L 9523419482886262617 222)
        , (L 9575286135634934334 143)
        , (L 9588136836208120035 242)
        , (L 9799930526101845883 135)
        , (L 9825722696997107003 104)
        , (L 9963134405638791223 21)
        , (L 10095155389655054397 0)
        , (L 10220937877125099325 235)
        , (L 10250977703885838829 99)
        , (L 10300367377856589513 191) ])
   , (B 9
        [7,21,22,26,27,28,30,31,34,46,49,55,59,63]
        [ (L 10512239979332789613 81)
        , (T [52,63] (L 10769434564237489532 190) (L 10772346711024201067 234))
        , (L 10784851303677588590 243)
        , (T [33,54] (L 10854010214713699070 245) (L 10860042242407247075 100))
        , (T [16,36] (L 10867290717340440279 54) (L 10872818210492027632 205))
        , (L 10883206686473657755 74)
        , (T [3,16] (L 10917643526048067358 124) (L 10921489670507410219 22))
        , (L 10945623465899106931 204)
        , (T [29,53] (L 10996995086343767283 67) (L 11003939006808226909 11))
        , (L 11222880884708115211 213)
        , (L 11264779755256165318 149)
        , (L 11380934722048177051 13)
        , (B
             [5,44,61]
             [ (L 11440724930997895819 172)
             , (L 11451537670513739451 238)
             , (L 11456562320202098202 146) ])
        , (L 11515486489542076333 166) ])
   , (B 10
        [6,8,16,18,23,42,45,48,55,60]
        [ (L 11644703189750918566 232)
        , (L 11682554722624814719 177)
        , (L 11825308128967544131 89)
        , (L 11856158429891202193 105)
        , (L 11949090328852710194 70)
        , (L 12303062724129835025 77)
        , (L 12355530404747712920 73)
        , (L 12396289550292543183 248)
        , (L 12534178848023252542 174)
        , (L 12625827705106718998 216) ])
   , (B 11
        [11,15,27,29,30,33,40,42,53,54,59,60,62]
        [ (L 12891735293121972673 130)
        , (L 12968614728892176257 58)
        , (L 13184065293823873233 51)
        , (L 13220262783806469405 108)
        , (L 13238630230733480790 48)
        , (L 13280149406226489112 7)
        , (L 13403176864786536048 106)
        , (L 13442773820359233541 14)
        , (L 13639272581626794398 171)
        , (T [14,49] (L 13659093752097018961 95) (L 13668847969639169601 178))
        , (L 13748560489236403874 163)
        , (L 13768702315267227527 150)
        , (L 13807245874246393822 141) ])
   , (B 12
        [6,14,18,20,25,32,33,39,42,45,47,52,56,58,59,61]
        [ (L 13949007747585375094 246)
        , (L 14095975780925543077 192)
        , (L 14176719258489481077 183)
        , (L 14198864465386621631 196)
        , (L 14297092152125609691 224)
        , (L 14415550580003575478 80)
        , (T [1,50] (L 14430000219523966376 219) (L 14443652709945326894 236))
        , (T [8,53] (L 14539978440096846316 153) (L 14552671640088369608 25))
        , (L 14601796361801899988 133)
        , (L 14656501092542937360 84)
        , (T [50,56] (L 14695980450478104512 138) (L 14697730579752275356 186))
        , (L 14783875937692760131 46)
        , (L 14844791574935431106 218)
        , (L 14880261783622357016 40)
        , (T [46,61] (L 14911101540900695863 129) (L 14915292582735134102 185))
        , (L 14934653003117056318 79) ])
   , (B 13
        [7,10,14,15,20,25,30,36,38,43,44,49,54,58,61]
        [ (L 15125790821711528494 115)
        , (T [7,36] (L 15170106289025782505 201) (L 15178489974245758294 17))
        , (L 15257815241456052469 64)
        , (L 15273704248219139637 27)
        , (L 15348835242365225136 41)
        , (L 15454857340363826009 237)
        , (L 15531135364945179560 189)
        , (T [8,59] (L 15638952972742377129 152) (L 15653142677227594415 187))
        , (L 15676530559245415943 44)
        , (L 15766419539877647334 87)
        , (L 15792928835882236715 120)
        , (L 15886521512254272386 151)
        , (L 15962924016092043088 59)
        , (L 16045079169508044665 160)
        , (L 16088214167997259433 208) ])
   , (B 14
        [5,12,16,20,31,37,38,45,53,54,55,60,61,62]
        [ (L 16234141810701750009 69)
        , (L 16368087467838533097 167)
        , (L 16445123074285557320 175)
        , (L 16516887326899631973 241)
        , (L 16714917742314473792 113)
        , (L 16823590792259789829 176)
        , (L 16831042691064554302 5)
        , (L 16960122482309037238 75)
        , (L 17111372505109033041 122)
        , (L 17120376993596023111 127)
        , (L 17135854862143603551 203)
        , (L 17233359898529174107 156)
        , (O
             7
             (T
                [29,59]
                (L 17241881143273282943 19)
                (L 17242009592616102734 207)))
        , (L 17267540842470077262 39) ])
   , (B 15
        [11,37,52,53,55,61]
        [ (L 17500965053172476000 82)
        , (L 17960712826341302250 170)
        , (T [23,42] (L 18237124392238187220 173) (L 18242617319282301894 102))
        , (L 18257281565556272370 210)
        , (L 18296906201173532572 31)
        , (T
             [46,61]
             (L 18405833735424979648 157)
             (L 18410149248234285848 61)) ]) ])
-}