{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Cardano.Ledger.Alonzo.CompactMap where

import Cardano.Binary
  ( Encoding,
    FromCBOR (..),
    ToCBOR (..),
    serialize',
    unsafeDeserialize',
  )
import Cardano.Crypto.Hash.Class (Hash (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
-- https://downloads.haskell.org/~ghc/8.10.4/docs/html/libraries/base-4.14.1.0/GHC-Exts.html#v:indexWord32Array-35-

{-
    indexWord8ArrayAsWord16#,
    readWord8ArrayAsWord16#,
    writeWord8ArrayAsWord16#,
    indexWord16OffAddr#,
    readWord16OffAddr#,
    writeWord16OffAddr#,
-}

-- Arbitrary instance Value
-- HeapWords instance CompactValue

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (Compactible (..))
import qualified Cardano.Ledger.Core as Core (Value)
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.DescribeEras as E (Witness (..))
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Mary.Value (Value (..))
import Cardano.Ledger.SafeHash (extractHash)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Val (Val (..))
import Cardano.Prelude (HeapWords (..))
-- (PrimArray,primArrayFromList)
import Control.Monad.Primitive
import Control.Monad.ST (ST, runST)
import qualified Data.Array as A
import qualified Data.Array.MArray as MutA
import Data.Array.ST (STArray)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as Short
import Data.Char (chr)
import Data.CompactMap
import Data.Foldable (foldl')
import Data.List (sort, sortBy)
import qualified Data.Map as Map
import Data.Messages
import Data.Primitive.PrimArray
import Data.Primitive.Types (Prim (..), defaultSetByteArray#, defaultSetOffAddr#)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Debug.Trace
import GHC.Base (Int (I#))
import GHC.Exts
  ( Int#,
    IsList (..),
    indexWord32OffAddr#,
    indexWord64OffAddr#,
    indexWord8ArrayAsWord32#,
    indexWord8ArrayAsWord64#,
    readWord32OffAddr#,
    readWord64OffAddr#,
    readWord8ArrayAsWord32#,
    readWord8ArrayAsWord64#,
    writeWord32OffAddr#,
    writeWord64OffAddr#,
    writeWord8ArrayAsWord32#,
    writeWord8ArrayAsWord64#,
    (*#),
    (+#),
  )
import GHC.Word (Word32 (..), Word64 (..))
import Shelley.Spec.Ledger.CompactAddr (CompactAddr (..))
import Shelley.Spec.Ledger.Tx (TxId (..), TxIn (..), TxOut)
import Shelley.Spec.Ledger.TxBody (TxOut (..))
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.QuickCheck.Gen (frequency, vectorOf)
import Test.Shelley.Spec.Ledger.PackedBytes (PackedBytes (..), packBytes)
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, generate, vector)
import qualified Data.Primitive.Array as PA

-- import Test.Tasty
-- import Cardano.Binary.Deserialize(unsafeDeserialize)
-- import Shelley.Spec.Ledger.UTxO(UTxO(..))
-- import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C,C_Crypto)
-- import qualified Data.ByteString as BS
-- import Data.ByteString.Conversion.From(FromByteString(parser),runParser)
-- import qualified Data.Vector.Generic as VGen
-- import qualified Data.Vector.Unboxed as VUnbox
-- import Data.Primitive.ByteArray

-- ================================================

class Indexable arr a => MutIndexable arr marr a | marr -> arr where
  mindex :: marr s a -> Int -> ST s a
  msize :: marr s a -> Int
  mnew :: Int -> ST s (marr s a)
  mfreeze :: marr s a -> ST s (arr a)

instance Indexable PA.Array x where
  index = PA.indexArray
  isize = PA.sizeofArray
  fromlist = PA.arrayFromList
  tolist arr = foldr (:) [] arr

instance MutIndexable PA.Array PA.MutableArray a where
  msize = PA.sizeofMutableArray 
  mindex = PA.readArray
  mnew n = PA.newArray n undefined
  mfreeze = PA.unsafeFreezeArray 


instance Prim a => MutIndexable PrimArray MutablePrimArray a where
  msize = sizeofMutablePrimArray
  mindex = readPrimArray
  mnew = newPrimArray  
  mfreeze = unsafeFreezePrimArray 

-- MutA.newArray_ (0, valsize - 1)
-- newPrimArray primsize

withSTArray :: Int -> (forall s. STArray s Int a -> ST s x) -> (A.Array Int a, x)
withSTArray _size process = runST $ do
  marr <- MutA.newArray_ (0, _size - 1)
  x <- process marr
  arr <- MutA.freeze marr
  pure (arr, x)

withPrimArray :: Prim a => Int -> (forall s. MutablePrimArray s a -> ST s x) -> (PrimArray a, x)
withPrimArray siz process = runST $ do
  marr <- newPrimArray siz
  x <- process marr
  arr <- unsafeFreezePrimArray marr
  pure (arr, x)

withBoth ::
  Prim a =>
  Int ->
  Int ->
  (forall s. MutablePrimArray s a -> STArray s Int b -> ST s ()) ->
  (PrimArray a, A.Array Int b)
withBoth primsize normsize process = runST $ do
  arr1 <- newPrimArray primsize
  arr2 <- MutA.newArray_ (0, normsize - 1)
  process arr1 arr2
  arr3 <- unsafeFreezePrimArray arr1
  arr4 <- MutA.freeze arr2
  pure (arr3, arr4)


withParallel :: (MutIndexable arr1 marr1 k,MutIndexable arr2 marr2 v) =>
   Int ->
   Int ->
   (forall s x. marr1 s k -> marr2 s v -> ST s x) ->
   (arr1 k, arr2 v)
withParallel keysize valsize process = runST $ do
  arr1 <- mnew keysize
  arr2 <- mnew valsize
  process arr1 arr2
  arr3 <- mfreeze arr1
  arr4 <- mfreeze arr2
  pure (arr3, arr4)


-- ==============================

arraySize :: A.Array Int e -> Int
arraySize arr = (hi - lo) + 1 where (lo, hi) = A.bounds arr

-- | In order to save space, we store things as (Array Int ByteString) and deserialize a [v] of length
--   'groupsize' from the array, chosing the right thing usingg modular arithmetic on the index 'i'
readFromGroup :: forall v. FromCBOR v => Int -> A.Array Int ByteString -> Int -> v
readFromGroup groupsize arr i = (vals !! (i `mod` groupsize))
  where
    vals :: [v]
    vals = unsafeDeserialize' (index arr (i `div` groupsize))

-- | In order to save space, we store things as (Array Int ByteString) and deserialize a [v] of length
--   'groupsize' from the array, chosing the right thing using modular arithmetic on the index 'i'
--   If we read all the values this way (in increasing key order), we will make 'groupsize' consecutive
--   reads from the same group, so it is worth it to deserialize each group only once, and use Array
--   indexing from that group rather than list indexing as is done in 'readFromGroup'
data CachedArr v where
  CachedArr ::
    FromCBOR v =>
    Int -> -- group size
    Int -> -- current index of the cache
    A.Array Int v -> -- the current cache, usually of size group size,
    -- but if its the last group it might be smaller.
    A.Array Int ByteString -> -- the compressed array storing ByteStrings
    CachedArr v

instance Show v => Show (CachedArr v) where
  show (CachedArr gs gnum current _) = "(Cached " ++ show gs ++ " " ++ show gnum ++ " " ++ show (foldr (:) [] current) ++ ")"

-- | Create a new cache, primed to read from group 0 on the first 'lookupCached'
newCache :: forall v. FromCBOR v => Int -> A.Array Int ByteString -> CachedArr v
newCache groupsize values = CachedArr groupsize 0 current values
  where
    list = (unsafeDeserialize' (index values 0)) :: [v]
    current = A.listArray (0, length list -1) list

-- | Lookup a value 'v' using index 'i', This works best when we repeatedly read values from consequtive keys
lookupCached :: forall v. Int -> CachedArr v -> (CachedArr v, v)
lookupCached i (cache@(CachedArr groupsize currentgroup current compressed)) =
  let groupnum = i `div` groupsize
   in if groupnum == currentgroup
        then (cache, index current (i `mod` groupsize))
        else
          let list = (unsafeDeserialize' (index compressed groupnum) :: [v])
              group = A.listArray (0, length list -1) list
           in (CachedArr groupsize groupnum group compressed, index group (i `mod` groupsize))

-- | If the 'group' is full, serialise it, and then write it to 'mvalues' a index 'jv'
--   Aways return the next index and the next group. Most times the group grows
--   but the index says the same. The index grows only when the group becomes full.
resetNewGroup :: (ToCBOR v) => Int -> v -> STArray s Int ByteString -> Int -> [v] -> ST s (Int, [v])
resetNewGroup groupsize v mvalues jv group = do
  if length group == groupsize - 1
    then do MutA.writeArray mvalues jv (serialize' newgroup); pure (jv + 1, [])
    else pure (jv, v : group)
  where
    newgroup = (reverse (v : group))

-- | Compute the size of new set of keys. If the key of the message appears in the
--   keys of the parallel array, subtract 1 if delete, add 1 if not, and add 0 if
--   it does not appear in the keys.
accumSize2 :: (Ord k, Indexable arr k, Num p) => arr k -> p -> (k, Message v) -> p
accumSize2 arr ans (key, v) =
  case binsearch 0 (isize arr - 1) key arr of
    Just _ -> (case v of Delete -> ans -1; _ -> ans)
    _ -> (case v of Delete -> ans; _ -> ans + 1)

flush ::
  forall a v.
  (ToCBOR v, FromCBOR v, Show v, Ord a, Prim a, Show a) =>
  Int ->
  PrimArray a ->
  A.Array Int ByteString ->
  [(a, Message v)] ->
  (PrimArray a, A.Array Int ByteString)
flush groupsize keys values list = trace ("Sizes " ++ show (newsize, normsize)) $ withBoth newsize normsize process
  where
    keysize = sizeofPrimArray keys
    newsize = keysize + foldl' (accumSize2 keys) 0 list
    normsize =
      if newsize `mod` groupsize == 0
        then newsize `div` groupsize
        else (newsize `div` groupsize) + 1
    sortedlist = sortBy (\x y -> compare (fst x) (fst y)) list
    cachedValues = newCache @v groupsize values
    process :: forall s. MutablePrimArray s a -> STArray s Int ByteString -> ST s ()
    process mkeys mvalues = loop 0 cachedValues 0 0 sortedlist []
      where
        pushFromList :: Int -> CachedArr v -> Int -> Int -> (a, v) -> [(a, Message v)] -> [v] -> ST s ()
        pushFromList
          ik -- current index into keys
          cache -- cached group of values (accessed by 'ik' using 'lookupCached')
          jk -- next index into mkeys
          jv -- next index into mvalues
          (k, v)
          vs
          newgroup = do
            writePrimArray mkeys jk k
            (jv', newgroup') <- resetNewGroup groupsize v mvalues jv newgroup
            loop ik cache (jk + 1) jv' vs newgroup'
        pushFromArray ik cache jk jv (k, v) vs newgroup = do
          writePrimArray mkeys jk k
          (jv', newgroup') <- resetNewGroup groupsize v mvalues jv newgroup
          loop (ik + 1) cache (jk + 1) jv' vs newgroup'
        loop :: Int -> CachedArr v -> Int -> Int -> [(a, Message v)] -> [v] -> ST s ()
        loop ik cache jk jv xs newgroup =
          trace
            ( "\n  " ++ show (ik, jk, jv, xs, newgroup)
                ++ "\n  OTHER "
                ++ show (ik < keysize, jk < newsize, xs)
            )
            $ case (ik < keysize, jk < newsize, xs) of
              (True, True, pairs2) ->
                let k2 = indexPrimArray keys ik
                    (cache', v2) = lookupCached ik cache
                 in case pairs2 of
                      [] -> pushFromArray ik cache' jk jv (k2, v2) [] newgroup
                      ((k, v) : ys) ->
                        case compare k k2 of
                          EQ -> trace ("EQ " ++ show (k, k2, v, v2)) $
                            case v of
                              Delete -> loop (ik + 1) cache jk jv ys newgroup -- Skip over index ik, because its is deleted
                              Edit u -> pushFromList ik cache' jk jv (k, u) ys newgroup
                              _ -> error ("NO Upsert 1.")
                          LT -> trace ("LT " ++ show (k, k2, v, v2)) $
                            case v of
                              Delete -> loop ik cache jk jv ys newgroup -- Do nothing, deleting key that doesn't exist
                              Edit u -> pushFromList ik cache' jk jv (k, u) ys newgroup
                              _ -> error ("NO Upsert 2.")
                          GT -> pushFromArray ik cache' jk jv (k2, v2) xs newgroup
              (False, _, ((k, Edit v) : xs1)) -> pushFromList ik cache jk jv (k, v) xs1 newgroup
              (False, _, ((_k, Delete) : xs2)) -> loop ik cache jk jv xs2 newgroup
              -- This case should only happen when we have run out of things to process
              -- (ik >= keysize), so there is nothing left to copy from 'arr'
              -- or xs is null , so there is nothing left to copy from 'list'
              -- It might happen that he last action did not fill the newgroup so then
              -- we need to write the partial newgroup to the mutable array 'mvalues'
              _ ->
                if null newgroup
                  then pure ()
                  else do MutA.writeArray mvalues jv (serialize' (reverse newgroup))

foldl'2 :: forall k v ans. (Ord k, FromCBOR v) => (ans -> k -> v -> ans) -> ans -> Par2 k v -> ans
foldl'2 accum ans (Par2 groupsize keys values deltam) = loop 0 cache list ans
  where
    list = Map.toList deltam
    cache = newCache @v groupsize values
    keysize = sizeofPrimArray keys
    loop :: Int -> CachedArr v -> [(k, Message v)] -> ans -> ans
    loop ik cache1 xs ans1 =
      case (ik < keysize, xs) of
        (False, []) -> ans
        (False, pairs2) -> foldl' acc ans1 pairs2
          where
            acc ans2 (k, Edit u) = accum ans2 k u
            acc ans2 (_k, Delete) = ans2
            acc _ (_, Upsert _) = error ("No Upsert 3.")
        (True, pairs2) ->
          let k2 = indexPrimArray keys ik
              (cache2, v2) = lookupCached ik cache1
           in case pairs2 of
                [] -> loop (ik + 1) cache2 [] (accum ans k2 v2)
                ((k, m) : ys) ->
                  case (compare k k2, m) of
                    (EQ, Delete) -> loop (ik + 1) cache2 ys ans1 -- Skip over index ik, because its key: k, is deleted
                    (EQ, Edit u) -> loop (ik + 1) cache2 ys (accum ans1 k u) -- accumulate k and u
                    (LT, Delete) -> loop ik cache1 ys ans -- Skip over key k, because its is deleted
                    (LT, Edit u) -> loop ik cache1 ys (accum ans1 k u) -- accumulate k and u
                    (GT, _) -> loop (ik + 1) cache2 pairs2 (accum ans1 k2 v2) -- accumulate k2 and v2
                    _ -> error ("No Upsert 4.")

testfold :: Par2 Int Text -> [(Int, Text)]
testfold par2 = foldl'2 accum [] par2
  where
    accum ans key v = (key, v) : ans

testflush :: Par2 Int Text -> Par2 Int Text
testflush (Par2 groupsize keys values deltam) = Par2 groupsize ks vs Map.empty
  where
    (ks, vs) = flush groupsize keys values (Map.toList deltam)

m2 :: Map.Map Int Text
m2 = Map.fromList [(1 :: Int, pack "a"), (2, pack "b"), (9, pack "d"), (5, pack "c")]

-- (Par2 gs1 ks1 vs1 _) = (toPar2 3 m2)

p2 :: Par2 Int Text
p2 = delete2 9 (insert2 10 "e" (insert2 12 "f" (toPar2 3 m2)))

instance ToCBOR v => ToCBOR (Message v) where
  toCBOR (Edit v) = toCBOR v
  toCBOR _ = error "No ToCBOR for (Message v) alternative"

instance FromCBOR v => FromCBOR (Message v) where
  fromCBOR = Edit <$> fromCBOR

-- =========================================

accumSize :: (Ord k, Indexable arr k, Num p) => arr k -> p -> k -> p
accumSize arr ans key =
  case binsearch 0 (isize arr - 1) key arr of Just _ -> ans; _ -> ans + 1

-- | Merge 'list' (of type [a]) into 'arr' (a sorted (PrimArray a)) creating a new (PrimArray a)
--   The 'arr' should be sorted, and then the result will be sorted.
mergePrimArray :: forall a. (Ord a, Prim a) => PrimArray a -> [a] -> PrimArray a
mergePrimArray arr list = arr2
  where
    (arr2, _) = withPrimArray newsize process
    oldsize = sizeofPrimArray arr
    newsize = oldsize + foldl' (accumSize arr) 0 list
    sortedlist = sort list
    process :: forall s. MutablePrimArray s a -> ST s ()
    process mutarr = do
      let loop i next ys =
            case (i < oldsize, next < newsize, ys) of
              (True, True, (x : xs)) ->
                do
                  let y = indexPrimArray arr i
                  case compare x y of
                    EQ -> do
                      writePrimArray mutarr next x
                      loop (i + 1) (next + 1) xs
                    LT -> do
                      writePrimArray mutarr next x
                      loop i (next + 1) xs
                    GT -> do
                      writePrimArray mutarr next y
                      loop (i + 1) (next + 1) (x : xs)
              (True, True, []) ->
                copyPrimArray mutarr next arr i (oldsize - i)
              (False, True, (x : xs)) ->
                do
                  writePrimArray mutarr next x
                  loop i (next + 1) xs
              -- This case should only happen when we have run out of things to process
              -- (i >= oldsize), so there is nothing left to copy from 'arr'
              -- or xs is null , so there is nothing left to copy from 'list'
              _ -> pure ()
      loop 0 0 sortedlist

t21 :: PrimArray Int
t21 = primArrayFromList [3, 6, 9, 12 :: Int]

test :: [Int] -> PrimArray Int
test xs = mergePrimArray t21 xs

-- ==========================================

-- ==========================================
data TT = TT Word64 Word64 Word64 Word32 Word32 deriving (Eq, Ord, Show)

txInToTT :: TxIn StandardCrypto -> TT
txInToTT txin =
  let TxInCompact (TxId safe) w5 = txin
      UnsafeHash bytes = extractHash safe
   in case (packBytes bytes) :: PackedBytes 28 of
        PackedBytes28 w1 w2 w3 w4 -> TT w1 w2 w3 w4 (fromIntegral w5)
        _ -> error ("BAD TxIn")

-- | Offsets (in Bytes) of the arguments (TT w1 w2 w3 w4 w5)
w1offset, w2offset, w3offset, w4offset, w5offset :: Int
w1offset = 0
w2offset = 8
w3offset = 16
w4offset = 24
w5offset = 28

instance Prim TT where
  sizeOf# _ = (3# *# sizeOf# (undefined :: Word64) +# 2# *# sizeOf# (undefined :: Word32))
  alignment# x = sizeOf# x -- Pack as tight as possible.
  indexByteArray# arr# i# =
    let i2# = i# *# sizeOf# (undefined :: TT)
     in TT
          (W64# (indexWord8ArrayAsWord64# arr# (i2# +# unInt w1offset)))
          (W64# (indexWord8ArrayAsWord64# arr# (i2# +# unInt w2offset)))
          (W64# (indexWord8ArrayAsWord64# arr# (i2# +# unInt w3offset)))
          (W32# (indexWord8ArrayAsWord32# arr# (i2# +# unInt w4offset)))
          (W32# (indexWord8ArrayAsWord32# arr# (i2# +# unInt w5offset)))

  readByteArray# arr# i# =
    \s0 -> case readWord8ArrayAsWord64# arr# (i2# +# unInt w1offset) s0 of
      (# s1, w1 #) -> case readWord8ArrayAsWord64# arr# (i2# +# unInt w2offset) s1 of
        (# s2, w2 #) -> case readWord8ArrayAsWord64# arr# (i2# +# unInt w3offset) s2 of
          (# s3, w3 #) -> case readWord8ArrayAsWord32# arr# (i2# +# unInt w4offset) s3 of
            (# s4, w4 #) -> case readWord8ArrayAsWord32# arr# (i2# +# unInt w5offset) s4 of
              (# s5, w5 #) -> (# s5, TT (W64# w1) (W64# w2) (W64# w3) (W32# w4) (W32# w5) #)
    where
      i2# = i# *# sizeOf# (undefined :: TT)

  writeByteArray# arr# i# (TT (W64# w1) (W64# w2) (W64# w3) (W32# w4) (W32# w5)) =
    \s0 -> case writeWord8ArrayAsWord64# arr# (i2# +# unInt w1offset) w1 s0 of
      s1 -> case writeWord8ArrayAsWord64# arr# (i2# +# unInt w2offset) w2 s1 of
        s2 -> case writeWord8ArrayAsWord64# arr# (i2# +# unInt w3offset) w3 s2 of
          s3 -> case writeWord8ArrayAsWord32# arr# (i2# +# unInt w4offset) w4 s3 of
            s4 -> case writeWord8ArrayAsWord32# arr# (i2# +# unInt w5offset) w5 s4 of
              s5 -> s5
    where
      i2# = i# *# sizeOf# (undefined :: TT)

  setByteArray# arr n m a state = defaultSetByteArray# arr n m a state

  indexOffAddr# arr# i# =
    let i2# = i# *# sizeOf# (undefined :: TT)
     in TT
          (W64# (indexWord64OffAddr# arr# (i2# +# unInt w1offset)))
          (W64# (indexWord64OffAddr# arr# (i2# +# unInt w2offset)))
          (W64# (indexWord64OffAddr# arr# (i2# +# unInt w3offset)))
          (W32# (indexWord32OffAddr# arr# (i2# +# unInt w4offset)))
          (W32# (indexWord32OffAddr# arr# (i2# +# unInt w5offset)))

  readOffAddr# arr# i# =
    \s0 -> case readWord64OffAddr# arr# (i2# +# unInt w1offset) s0 of
      (# s1, w1 #) -> case readWord64OffAddr# arr# (i2# +# unInt w2offset) s1 of
        (# s2, w2 #) -> case readWord64OffAddr# arr# (i2# +# unInt w3offset) s2 of
          (# s3, w3 #) -> case readWord32OffAddr# arr# (i2# +# unInt w4offset) s3 of
            (# s4, w4 #) -> case readWord32OffAddr# arr# (i2# +# unInt w5offset) s4 of
              (# s5, w5 #) -> (# s5, TT (W64# w1) (W64# w2) (W64# w3) (W32# w4) (W32# w5) #)
    where
      i2# = i# *# sizeOf# (undefined :: TT)

  writeOffAddr# arr# i# (TT (W64# w1) (W64# w2) (W64# w3) (W32# w4) (W32# w5)) =
    \s0 -> case writeWord64OffAddr# arr# (i2# +# unInt w1offset) w1 s0 of
      s1 -> case writeWord64OffAddr# arr# (i2# +# unInt w2offset) w2 s1 of
        s2 -> case writeWord64OffAddr# arr# (i2# +# unInt w3offset) w3 s2 of
          s3 -> case writeWord32OffAddr# arr# (i2# +# unInt w4offset) w4 s3 of
            s4 -> case writeWord32OffAddr# arr# (i2# +# unInt w5offset) w5 s4 of
              s5 -> s5
    where
      i2# = i# *# sizeOf# (undefined :: TT)

  setOffAddr# = defaultSetOffAddr#

unInt :: Int -> Int#
unInt (I# x) = x

tt :: TT
tt = TT 1 2 3 4 6

pa :: PrimArray TT
pa = primArrayFromList [TT 1 2 3 4 99, TT 8 7 6 5 21, TT 1 1 1 1 4]

-- ===========================================
data ParVector k v where
  ParVector :: (Prim k) => (PrimArray k) -> (A.Array Int v) -> ParVector k v

instance (Show k, Show v, Prim k) => Show (ParVector k v) where
  show (ParVector ks vs) = show ks ++ "\n" ++ show vs

toPar :: Prim k => Map.Map k v -> ParVector k v
toPar m = ParVector keys values
  where
    pairs1 = Map.toAscList m
    keys = primArrayFromList (map fst pairs1)
    values = A.listArray (0, isize keys - 1) (map snd pairs1)

m1 :: Map.Map Int Char
m1 = Map.fromList [(1 :: Int, 'a'), (2, 'b'), (9, 'c'), (5, 'd')]

look :: Ord k => k -> ParVector k v -> Maybe v
look k (ParVector keys values) =
  case search k keys of
    Just i -> Just $ index values i
    Nothing -> Nothing

-- ==================================================
{-
*Test.Shelley.Spec.Ledger.CompactMap> main
Size 49250 entries 49250
Normal  1576000 words
Compact 648388 words
Percent 41.141370558375634
Parallel HW PARVECTOR 295502  679045
974550 words
Percent 61.836928934010146
-}

instance HeapWords TT where
  heapWords (TT _ _ _ _ _) = 4

instance (HeapWords k, HeapWords v) => HeapWords (ParVector k v) where
  heapWords (ParVector k v) = (3 + hwk + hwv)
    where
      hwk = heapWords k
      hwv = heapWords v

-- ==================================================
type Alonzo = AlonzoEra StandardCrypto

percent :: Integral n => n -> n -> String
percent new old = show (round (((fromIntegral new / fromIntegral old) * 100) :: Double) :: Int) ++ "%"

hasTokens :: E.Witness era -> TxOut era -> Bool
hasTokens E.Alonzo (TxOut _ (Value _ m)) = not (Map.null m)
hasTokens E.Mary (TxOut _ (Value _ m)) = not (Map.null m)
hasTokens _era _txout = False

tokenSize :: E.Witness era -> Int -> TxOut era -> Int
tokenSize E.Shelley ans _ = ans
tokenSize E.Allegra ans _ = ans
tokenSize E.Mary ans (TxOutCompact _ x) =
  case fromCompact x of
    (Value _ m) -> if Map.null m then ans else heapWords x + ans - 1
tokenSize E.Alonzo ans (TxOutCompact _ x) =
  case fromCompact x of
    (Value _ m) -> if Map.null m then ans else heapWords x + ans - 1

serialTxOut :: E.Witness era -> TxOut era -> ByteString
serialTxOut E.Shelley (txout@(TxOutCompact _ _)) = serialize' txout
serialTxOut E.Allegra (txout@(TxOutCompact _ _)) = serialize' txout
serialTxOut E.Mary (txout@(TxOutCompact _ _)) = serialize' txout
serialTxOut E.Alonzo (txout@(TxOutCompact _ _)) = serialize' txout

genTxOut :: E.Witness era -> Int -> Gen (TxOut era)
genTxOut E.Alonzo perCent =
  TxOut <$> arbitrary
    <*> frequency [(perCent, arbitrary), ((100 - perCent), inject <$> arbitrary)]
genTxOut E.Mary perCent =
  TxOut <$> arbitrary
    <*> frequency [(perCent, arbitrary), ((100 - perCent), inject <$> arbitrary)]
genTxOut E.Shelley perCent =
  TxOut <$> arbitrary
    <*> frequency [(perCent, arbitrary), ((100 - perCent), inject <$> arbitrary)]
genTxOut E.Allegra perCent =
  TxOut <$> arbitrary
    <*> frequency [(perCent, arbitrary), ((100 - perCent), inject <$> arbitrary)]

main ::
  forall era.
  ( Era era,
    ToCBOR (Core.Value era),
    HeapWords (CompactForm (Core.Value era))
  ) =>
  E.Witness era ->
  IO ()
main wit = do
  pairs1 <- generate (vectorOf 100000 ((,) <$> arbitrary <*> genTxOut wit 1))
  let m = Map.fromList pairs1
      withnewkeys = Map.mapKeys txInToTT m
      m2' = Map.map (serialTxOut wit) $ withnewkeys
      pm = toPar m2'
      pm2 = toPar2 30 withnewkeys
      keys = Set.fromList $ map fst (take 100 pairs1)
      norm = (heapWords m)
      compact = (heapWords (initMap m keys))
      par = (heapWords pm)
      par2 = (heapWords pm2)
      tokens = Map.foldl' (\ans txout -> if hasTokens wit txout then ans + 1 else ans) (0 :: Int) m
      totaltokenwords = Map.foldl' (tokenSize wit) 0 m

  putStrLn
    ( unlines
        [ show wit ++ " Era.",
          "Size " ++ show (Map.size m) ++ " entries " ++ show (Map.size m2),
          "Number of entries with Tokens = " ++ show tokens ++ "  " ++ percent tokens (Map.size m)
            ++ " of entries have tokens,  total words from tokens "
            ++ show totaltokenwords,
          "Normal  " ++ show norm ++ " words",
          "Compact " ++ show compact ++ " words",
          "Percent " ++ percent compact norm,
          "Parallel " ++ show par ++ " words",
          "Percent " ++ percent par norm,
          "Parallel2 " ++ show par2 ++ " words",
          "Percent " ++ percent par2 norm
        ]
    )

aa :: IO (Short.ShortByteString, [Word64], Int)
aa = do
  txin <- generate (arbitrary :: Gen (TxIn StandardCrypto))
  let TxInCompact (TxId safe) _ = txin
      UnsafeHash bytes = extractHash safe
  case (packBytes bytes) :: PackedBytes 32 of
    PackedBytes32 w1 w2 w3 w4 -> pure (bytes, [w1, w2, w3, w4], Short.length bytes)
    _ -> putStrLn ("BAD ") >> pure (bytes, [], Short.length bytes)

bb :: IO (Int, Int)
bb = do
  txouts <- generate (vector 10 :: Gen [(TxOut Alonzo)])
  putStrLn (show (toCBOR txouts))
  putStrLn ""
  putStrLn (unlines (map (show . toCBOR) txouts))
  pure (heapWords (serialize' txouts), foldr (\x ans -> heapWords (serialize' x) + ans) 0 txouts)

cc :: IO Encoding
cc = do
  txouts <- generate (vector 4 :: Gen [(TxOut Alonzo)])
  pure (toCBOR txouts)

dd :: IO [Int]
dd =
  -- Average 12 Tokens per Value
  do
    l <- generate (vector 100 :: Gen [TxOut Alonzo])
    let baz :: TxOut Alonzo -> Int
        baz (TxOut _ (Value _ mm)) = Map.size mm
    pure (fmap baz l)

-- =======================================

foo :: TxOut era -> Short.ShortByteString
foo (TxOutCompact (UnsafeCompactAddr bytes1) _) = bytes1

-- =========================================

ex2, ex3, ex4 :: Par2 Int Text
ex2 = toPar2 5 (Map.fromList [(i, pack (show i)) | i <- [0 :: Int .. 21]])
ex3 = delete2 5 ex2
ex4 = insert2 12 (pack "99") ex3

instance Exp Text where
  plus x y = x <> y

-- ==============================================

-- | Pair if arrays with the same size. The keys are sorted
--   and if keys[i]=key, then values[i] is its associated value
data Compact1 k v where
  Compact1 ::
    Prim k =>
    (PrimArray k) -> -- Sorted Unboxed array of keys
    (A.Array Int v) -> -- Array of values
    Compact1 k v

lookup1 :: Ord k => k -> Compact1 k v -> Maybe v
lookup1 key (Compact1 keys values) =
  case binsearch 0 (isize keys) key keys of
    Nothing -> Nothing
    Just i -> Just (index values i)

-- ==========================================================

data Node3 k v where
  Node3 ::
    (Prim k) =>
    Int -> -- Size of key Array (power of 2)
    (PrimArray k) -> -- Sorted Unboxed array of keys
    (A.Array Int v) -> -- Array of values, with same index as the associated key
    Node3 k v

instance (Show k, Show v) => Show (Node3 k v) where
  show (Node3 n ks vs) = "\nNode3 " ++ show n ++ "\n   " ++ show (toList ks) ++ "\n   " ++ show (foldr (:) [] vs)

data Compact3 k v = Compact3 [Node3 k v]

instance (Show k, Show v) => Show (Compact3 k v) where
  show (Compact3 nodes) = concat (map show nodes)

-- | binary encoding of 'n', least significant bit on the front of the list
binary :: Int -> [Int]
binary 0 = []
binary 1 = [(1)]
binary n = (mod n 2) : binary (div n 2)

-- | Compute a sparse list of non-zero Binary digits and their positional weights to represent 'n'
--   For eample (sparseBinary 25) returns [(1,1),(1,8),(1,16)], I.e. we need: 1 one,
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

pairs :: Int -> [(Int, [Char])]
pairs n = [(i, [chr (i + 64)]) | i <- [1 .. n]]

pairs3 :: [(Int, [Char])]
pairs3 = pairs 25

makeCompact3 :: Prim k => [(k, v)] -> Compact3 k v
makeCompact3 xs = Compact3 (map node nodes)
  where
    nodes = pieces xs
    node (n, ps) = Node3 n (primArrayFromList keys) (A.listArray (0, n -1) values)
      where
        (keys, values) = unzip ps

arrayFromList :: [e] -> A.Array Int e
arrayFromList xs = (A.listArray (0, length xs -1) xs)

insert1 :: (Ord k, Prim k) => k -> v -> Compact3 k v -> Compact3 k v
insert1 k v (Compact3 nodes) = Compact3 (addNode (Node3 1 (fromlist [k]) (fromlist [v])) nodes)

addNode :: (Ord k) => Node3 k v -> [Node3 k v] -> [Node3 k v]
addNode node [] = [node]
addNode (x@(Node3 n keys1 vals1)) (ys@((Node3 m keys2 vals2) : more))
  | n < m = x : ys
  | n == m = addNode (mergeArr keys1 keys2 vals1 vals2) more
  | True = error ("Nodes not in order of ascending size.")

mergeArr :: forall k v. (Prim k, Ord k) => PrimArray k -> PrimArray k -> A.Array Int v -> A.Array Int v -> Node3 k v
mergeArr k1 k2 v1 v2 = Node3 size1 (fromlist keys) (fromlist values)
  where
    l1 = tolist k1
    l2 = tolist k2
    loop :: Int -> Int -> [k] -> [k] -> ([k], [v]) -> (Int, [k], [v])
    loop next1 next2 (ks1@(key1 : keys1)) (ks2@(key2 : keys2)) (ansk, ansv) =
      case compare key1 key2 of
        LT -> loop (next1 + 1) next2 keys1 ks2 (key1 : ansk, index v1 next1 : ansv)
        GT -> loop next1 (next2 + 1) ks1 keys2 (key2 : ansk, index v2 next2 : ansv)
        EQ -> error ("mergeArr finds duplicate.")
    loop next1 next2 [] (key2 : keys2) (ansk, ansv) = loop next1 (next2 + 1) [] keys2 (key2 : ansk, index v2 next2 : ansv)
    loop next1 next2 (key1 : keys1) [] (ansk, ansv) = loop (next1 + 1) next2 keys1 [] (key1 : ansk, index v1 next1 : ansv)
    loop next1 next2 _ _ (ks1, ks2) = (next1 + next2, reverse ks1, reverse ks2)
    (size1, keys, values) = loop 0 0 l1 l2 ([], [])

c1, c2, c3, c4, c5, c6, c7 :: Compact3 Int [Char]
c1 = makeCompact3 (pairs 8)
c2 = insert1 9 "Z9" c1
c3 = insert1 10 "Z10" c2
c4 = insert1 11 "Z11" c3
c5 = insert1 12 "Z12" c4
c6 = insert1 13 "Z13" c5
c7 = insert1 14 "Z14" c6

lookup3 :: Ord key => key -> Compact3 key v -> Maybe v
lookup3 _key (Compact3 []) = Nothing
lookup3 key (Compact3 ((Node3 n keys values) : more)) =
  case binsearch 0 (n -1) key keys of
    Nothing -> lookup3 key (Compact3 more)
    Just i -> Just (index values i)

-- =========================================================

data Diff v = Del | Change v

data Node4 k v where
  Node4 ::
    (Prim k) =>
    Int -> -- Size of key Array (power of 2)
    (PrimArray k) -> -- Sorted Unboxed array of keys
    (A.Array Int v) -> -- Array of values, with same index as the associated key
    (Map.Map k (Diff v)) -> -- For some (hopefully) small set of keys either (Del) or (Change v)
    Int -> -- The actual number of active pairs (Array size minus number of Deletes)
    Node4 k v

data Compact4 k v = Compact4 [Node4 k v]

lookup4 :: Ord key => key -> Compact4 key v -> Maybe v
lookup4 _key (Compact4 []) = Nothing
lookup4 key (Compact4 ((Node4 n keys values diff _) : more)) =
  case binsearch 0 (n -1) key keys of
    Nothing -> lookup4 key (Compact4 more)
    Just i -> case Map.lookup key diff of
      Nothing -> Just (index values i)
      Just Del -> Nothing
      Just (Change v) -> Just v

-- =================================================================

data Node5 k v where
  Node5 ::
    (Prim k, FromCBOR v) =>
    Int -> -- Size of key Array (power of 2)
    (PrimArray k) -> -- Sorted Unboxed array of keys
    (A.Array Int ByteString) -> -- Array of serialized values,
    (Map.Map k (Diff v)) -> -- For some (hopefully) small set of keys
    Int -> -- The actual number of active pairs
    Node5 k v

data Compact5 k v = Compact5 [Node5 k v]

-- =========================================================

data Node6 k v where
  Node6 ::
    (Prim k, FromCBOR v) =>
    Int -> -- Size of key Array (power of 2)
    (PrimArray k) -> -- Sorted Unboxed array of keys
    Int -> -- size of a group
    (A.Array Int ByteString) -> -- Array of groups of serialized values,
    (Map.Map k (Diff v)) -> -- For some (hopefully) small set of keys
    Int -> -- The actual number of active pairs
    Node6 k v

data Compact6 k v = Compact6 [Node6 k v]

-- =========================================================

data SetNode t where
  SetNode :: Prim t => Int -> (PrimArray t) -> SetNode t

instance Show t => Show (SetNode t) where
  show (SetNode i arr) = "\nSetNode " ++ show i ++ "\n   " ++ show (toList arr)

instance Show t => Show (CompactPrimSet t) where
  show (CompactPrimSet ns) = concat (map show ns)

newtype CompactPrimSet t = CompactPrimSet [SetNode t]

hasElem :: Ord t => t -> CompactPrimSet t -> Bool
hasElem _t (CompactPrimSet []) = False
hasElem t (CompactPrimSet ((SetNode siz arr) : more)) =
  case binsearch 0 (siz -1) t arr of
    Nothing -> hasElem t (CompactPrimSet more)
    Just _ -> True

insertElem :: (Prim t, Ord t) => t -> CompactPrimSet t -> CompactPrimSet t
insertElem t (set@(CompactPrimSet nodes)) =
  if hasElem t set
    then set
    else CompactPrimSet (addSetNode (SetNode 1 (fromlist [t])) nodes)

addSetNode :: (Ord t, Prim t) => SetNode t -> [SetNode t] -> [SetNode t]
addSetNode node [] = [node]
addSetNode (node@(SetNode n arr1)) (nodes@((SetNode m arr2) : more))
  | n < m = node : nodes
  | n == m = addSetNode (mergeSetNodes n m arr1 arr2) more
  | True = error ("SetNodes not in ascending Order.")

mergeSetNodes :: forall t. (Ord t, Prim t) => Int -> Int -> PrimArray t -> PrimArray t -> SetNode t
mergeSetNodes size1 size2 arr1 arr2 = SetNode totsize (fst (withPrimArray totsize make))
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
    node (n, ps) = SetNode n (primArrayFromList (sort ps))  
      

-- ---------------------------------------------------

data ArrayNode t where
  ArrayNode :: Prim t => Int -> (PrimArray t) -> ArrayNode t

instance Show t => Show (ArrayNode t) where
  show (ArrayNode _ arr) = show (reverse (toList arr))

instance Show t => Show (CompactPrimArray t) where
  show (CompactPrimArray _ ns) = concat (map show (reverse ns))

data CompactPrimArray t = CompactPrimArray Int [ArrayNode t]

-- | Assumption that 'i' has been tested for in bounds.
indexCompactPrimArray :: Show t => CompactPrimArray t -> Int -> t
indexCompactPrimArray (CompactPrimArray _ []) i = error ("index out of bounds for CompactPrimArray: " ++ show i)
indexCompactPrimArray (CompactPrimArray m ((ArrayNode n arr) : more)) i =
  if i < n
    then index arr i
    else indexCompactPrimArray (CompactPrimArray (m - n) more) (i - n)

pushD :: Prim t => CompactPrimArray t -> t -> CompactPrimArray t
pushD (CompactPrimArray _ []) t = CompactPrimArray 1 [ArrayNode 1 (fromList [t])]
pushD (CompactPrimArray siz nodes) t = CompactPrimArray (siz + 1) (addArrayNode (ArrayNode 1 (fromList [t])) nodes)

addArrayNode :: ArrayNode t -> [ArrayNode t] -> [ArrayNode t]
addArrayNode node [] = [node]
addArrayNode (node@(ArrayNode n arr1)) (nodes@((ArrayNode m arr2) : more))
  | n < m = node : nodes
  | n == m = addArrayNode (mergeArrayNode n arr1 m arr2) more
  | True = error ("ArrayNodes not in ascending Order.")

mergeArrayNode :: Prim t => Int -> PrimArray t -> Int -> PrimArray t -> ArrayNode t
mergeArrayNode n arr1 m arr2 = ArrayNode (n + m) (arr1 <> arr2) -- Uses the Monoid instance, which should be fast.

instance (Show t, Prim t) => Indexable CompactPrimArray t where
  index (xs@(CompactPrimArray m _)) i =
    if i < 0 || i >= m
      then error ("index out of bounds for CompactPrimArray: " ++ show i ++ " not in range [0.." ++ show (m -1) ++ "]")
      else indexCompactPrimArray xs (isize xs -1 - i)
  isize (CompactPrimArray m _) = m
  tolist (CompactPrimArray _ ns) = concat (map (\(ArrayNode _ arr) -> tolist arr) ns)
  fromlist xs = CompactPrimArray (length xs) (map makeArrNode (pieces (reverse xs)))
    where
      makeArrNode (siz, zs) = ArrayNode siz (fromlist zs)

a0, a1, a2, a3, a4, a20 :: CompactPrimArray Int
a0 = fromlist [0, 1, 2, 3, 4, 5]
a1 = pushD a0 6
a2 = pushD a1 7
a3 = pushD a2 8
a4 = pushD a3 9
a20 = fromlist [2, 6, 8, 23, 6, 45, 9, 12, 99, 132, 44, 15, 2]

-- =============================================================


mergeArrWithCont ::
   forall t ans. (Ord t, Prim t) =>
      Int ->
      Int ->
      PrimArray t ->
      PrimArray t -> ans -> (ans -> t -> Either Int Int -> ans) -> ans
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



mergeParallel size1 size2 keys1 keys2 vals1 vals2 =
   withBoth (size1+size2) (size1+size2) $
   (\ m1 m2 -> const () <$> mergeArrWithCont size1 size2 keys1 keys2 (pure 0) (action vals1 vals2 m1 m2))

getEither v1 v2 (Left i)  = index v1 i
getEither v1 v2 (Right i) = index v2 i

action:: (Show k, Show v,Prim k) => A.Array Int v -> A.Array Int v ->
         MutablePrimArray s k -> STArray s Int v ->
         ST s Int -> k -> Either Int Int -> ST s Int
action v1 v2 mkeys mvals indexM k eitheri =
          do !index <- indexM
             trace ("Action "++show(index,k,eitheri,sizeofMutablePrimArray mkeys)) $ writePrimArray mkeys index k
             MutA.writeArray mvals index (getEither v1 v2 eitheri)
             pure(index + 1)


mergePrim :: forall t. (Ord t, Prim t) => Int -> Int -> PrimArray t -> PrimArray t -> (PrimArray t,Int)
mergePrim size1 size2 arr1 arr2 =
    withPrimArray (size1+size2) (mergeArrWithCont size1 size2 arr1 arr2 (pure 0) . action)
  where action marr indexM t eitheri =
          do !index <- indexM
             writePrimArray marr index t
             pure(index + 1)


(.*.) f g x y = f (g x y)


qqq = mergeParallel 2 3 (fromlist [2::Int,8]) (fromlist [3,6,9]) (fromlist ["two"::String,"eight"]) (fromlist ["three","six","nine"])

-- ================================================================

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

mergeMany :: Ord t => [(Int, SetNode t)] -> MutablePrimArray s t -> ST s Int
mergeMany xs arr = inOrder smaller done action xs (pure (0::Int)) where
  smaller (i,SetNode _ xs) (j,SetNode _ ys) = index xs i < index ys j
  done (i,node@(SetNode size _)) = if i+1 < size then Just(i+1,node) else Nothing
  action n (i,SetNode _ xs) = do { count <- n; writePrimArray arr count (index xs i); pure(count+1)}

mergeNodes :: (Prim t, Ord t) => [SetNode t] -> (PrimArray t, Int)
mergeNodes ns = withPrimArray (sum (map getsize ns)) (mergeMany (map start ns))
  where start x = (0,x)
        getsize (SetNode n _) = n

ttt = CompactPrimSet [SetNode n arr]
  where  CompactPrimSet nodes = makeSet [7::Int,2,1,4,3,6,5]
         (arr,n) = mergeNodes nodes
    