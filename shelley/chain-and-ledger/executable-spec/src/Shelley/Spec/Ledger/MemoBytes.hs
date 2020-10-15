{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | MemoBytes is an abstration for a datetype that encodes its own seriialization.
--   The idea is to use a newtype around a MemoBytes non-memoizing version.
--   For example:   newtype Foo = Foo(MemoBytes NonMemoizingFoo)
--   This way all the instances for Foo (Eq,Show,Ord,ToCBOR,FromCBOR,NoThunks,Generic)
--   can be derived for free.
module Shelley.Spec.Ledger.MemoBytes
  ( MemoBytes (..),
    Encode (..),
    Decode (..),
    Wrapped (..),
    (!>),
    (<!),
    Mem,
    encode,
    decode,
    runE,
    decodeClosed,
    memoBytes,
    shorten,
    decodeList,
    decodeSeq,
    decodeStrictSeq,
    decodeSet,
    encodeList,
    encodeSeq,
    encodeStrictSeq,
    encodeSet,
    showMemo,
    printMemo,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    encodeListLen,
    encodePreEncoded,
    encodeWord,
    withSlice,
  )
import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Write (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Typeable
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (invalidKey)
import Shelley.Spec.Ledger.Serialization
  ( decodeList,
    decodeRecordNamed,
    decodeRecordSum,
    decodeSeq,
    decodeSet,
    decodeStrictSeq,
    encodeFoldable,
  )
import Prelude hiding (span)

-- ========================================================================

data MemoBytes t = Memo {memotype :: !t, memobytes :: {-# UNPACK #-} !ShortByteString}
  deriving (NoThunks) via AllowThunksIn '["memobytes"] (MemoBytes t)

deriving instance Generic (MemoBytes t)

instance (Typeable t) => ToCBOR (MemoBytes t) where
  toCBOR (Memo _ bytes) = encodePreEncoded (fromShort bytes)

instance (Typeable t, FromCBOR (Annotator t)) => FromCBOR (Annotator (MemoBytes t)) where
  fromCBOR = do
    (Annotator getT, Annotator getBytes) <- withSlice fromCBOR
    pure (Annotator (\fullbytes -> Memo (getT fullbytes) (toShort (toStrict (getBytes fullbytes)))))

instance Eq t => Eq (MemoBytes t) where (Memo x _) == (Memo y _) = x == y

instance Show t => Show (MemoBytes t) where show (Memo y _) = show y

instance Ord t => Ord (MemoBytes t) where compare (Memo x _) (Memo y _) = compare x y

shorten :: Lazy.ByteString -> ShortByteString
shorten x = toShort (toStrict x)

-- | Useful when deriving FromCBOR(Annotator T)
-- deriving via (Mem T) instance (Era era) => FromCBOR (Annotator T)
type Mem t = Annotator (MemoBytes t)

showMemo :: Show t => MemoBytes t -> String
showMemo (Memo t b) = "(Memo " ++ show t ++ "  " ++ show b ++ ")"

printMemo :: Show t => MemoBytes t -> IO ()
printMemo x = putStrLn (showMemo x)

-- ===============================================================================
-- Encode and Decode are typed data structures which specify encoders and decoders.
-- The types keep one from making mistakes, and count the correct number fields
-- in an encoding and decoding. They are somewhat dual, and are designed that visual
-- inspection of a Encode and its dual Decode can help the user conclude that the
-- two are self-consistent. They are also reusable abstractions that can be defined
-- once, and then used many places.
--
-- (Encode t) is a data structure from which 3 things can be recovered
-- Given:    x :: Encode t
-- 1) get a value of type t
-- 2) get an Encoding for that value, which correctly encodes the number of "fields"
--    written to the ByteString. Care must still be taken that the tags are correct.
-- 3) get a (MemoBytes t)
-- The advantage of using Encode with a MemoBytes, is we don't have to make a ToCBOR
-- instance. Instead the "instance" is spread amongst the pattern constuctors by using
-- (memoBytes encoding) in the where clause of the pattern contructor.
-- See some examples of this see the file Timelocks.hs
--
-- The Encode and Decode mechanism can also be used to encode Algebraic datatypes
-- in a uniform way. (Decode t) is dual to (Encode t). A decoder can be extracted
-- from it. And it will consistently decode it's dual. We now give some examples.
-- In the examples Let  Int and C have ToCBOR instances, and
-- encodeB :: B -> Encoding, and decodeB :: Decoder s B
{-
-- An example with 1 constructor (a record) uses Rec and RecD

data A = ACon Int B C

encodeA :: A -> Encode 'Closed A
encodeA (ACon i b c) = Rec ACon !> To i !> E encodeB b !> To c

decodeA :: Decode 'Closed A
decodeA = RecD ACon <! From <! D decodeB <! From

instance ToCBOR A   where toCBOR x = encode(encodeA x)
instance FromCBOR A where fromCBOR = decode decodeA

-- An example with multiple constructors uses Sum, SumD, and Summands

data M = M1 Int | M2 B Bool | M3 A

encodeM :: M -> Encode 'Open M
encodeM (M1 i)    = Sum M1 0 !> To i
encodeM (M2 b tf) = Sum M2 1 !> E encodeB b  !> To tf
encodeM (M3 a)    = Sum M3 2 !> To a

decodeM :: Decode 'Closed M
decodeM = Summands "M" decodeMx
  where decodeMx 0 = SumD M1 <! From
        decodeMx 1 = SumD M2 <! D decodeB <! From
        decodeMx 3 = SumD M3 <! From
        decodeMx k = Invalid k

instance ToCBOR M   where toCBOR x = encode(encodeM x)
instance FromCBOR M where fromCBOR = decode decodeM
-}
-- For more examples writing CBOR instances using Encode and Decode see the test file
-- shelley-spec-ledger-test/test/Test/Shelley/Spec/Ledger/MemoBytes.hs

-- ========================================================

-- | Some CBOR instances wrap encoding sequences with prefixes and suffixes. I.e.
--  prefix , encode, encode, encode , ... , suffix
--  A type that MUST do this is called 'Open. Other types are called 'Closed.
--  The point is that a Closed type may have a prefix, but it can still be 'inlined'
--  by using the context it appears in, inside another Closed type. It does so by
--  sharing the prefix of its containing type, and using positional context.
--  In terms of Open and Closed, a datatype with more than one constructor is Open,
--  and one with only one constructor is Closed (we call these records). Primitive
--  types with no constructors can also be inlined, so we mark them as closed.
data Wrapped = Open | Closed

-- ===========================================================

data Encode (w :: Wrapped) t where
  Sum :: t -> Word -> Encode 'Open t
  Rec :: t -> Encode 'Closed t
  To :: ToCBOR a => a -> Encode 'Closed a
  E :: (t -> Encoding) -> t -> Encode 'Closed t
  ApplyE :: Encode w (a -> t) -> Encode 'Closed a -> Encode w t

-- The Wrapped index of ApplyE is determined by the index
-- at the bottom of its left spine. The LEFT arg of ApplyE
-- must be a function type, and the only Encode with function
-- types are (Sum c tag) and (Rec c). So if the leftmost spine
-- is (Sum c tag) it is 'Open, and if is (Rec c) it is 'Closed.
-- The RIGHT arg of ApplyE must be 'Closed. This allows us to
-- inline anything in a RIGHT arg, supporting CBORGroup capability.

infixl 4 !>

(!>) :: Encode w (a -> t) -> Encode 'Closed a -> Encode w t
x !> y = ApplyE x y

runE :: Encode w t -> t
runE (Sum c _) = c
runE (Rec c) = c
runE (ApplyE f x) = runE f (runE x)
runE (To x) = x
runE (E _ x) = x

gsize :: Encode w t -> Word
gsize (Sum _ _) = 0
gsize (Rec _) = 0
gsize (To _) = 1
gsize (E _ _) = 1
gsize (ApplyE f x) = gsize f + gsize x

encode :: Encode w t -> Encoding
encode sym = encodeCountPrefix 0 sym
  where
    encodeCountPrefix :: Word -> Encode w t -> Encoding
    encodeCountPrefix n (Sum _ tag) = encodeListLen (n + 1) <> encodeWord tag
    encodeCountPrefix n (Rec _) = encodeListLen n
    -- n is the number of fields we must write in the prefx.
    encodeCountPrefix _ (To x) = toCBOR x
    encodeCountPrefix _ (E enc x) = enc x
    encodeCountPrefix n (ApplyE f x) =
      encodeCountPrefix (n + gsize x) f <> encodeClosed x
    -- The RIGHT arg may be any 'Closed Encode, and is inlined
    -- by design. Its left spine must end in a (Rec c). We count (gsize x)
    -- the 'fields' in x, and add them to the number things we
    -- must add to the prefix of the enclosing type.

    encodeClosed :: Encode 'Closed t -> Encoding
    -- encodeClosed (Sum _ _) -- By design this case is unreachable by type considerations.
    encodeClosed (Rec _) = mempty
    encodeClosed (To x) = toCBOR x
    encodeClosed (E enc x) = enc x
    encodeClosed (ApplyE f x) = encodeClosed f <> encodeClosed x

memoBytes :: Encode w t -> MemoBytes t
memoBytes t = Memo (runE t) (shorten (toLazyByteString (encode t)))

-- =====================

data Decode (w :: Wrapped) t where
  Summands :: String -> (Word -> Decode 'Open t) -> Decode 'Closed t
  SumD :: t -> Decode 'Open t
  RecD :: t -> Decode 'Closed t
  From :: FromCBOR t => Decode 'Closed t
  D :: (forall s. Decoder s t) -> Decode 'Closed t
  ApplyD :: Decode w (a -> t) -> Decode 'Closed a -> Decode w t
  Invalid :: Word -> Decode w t
  Map :: (a -> b) -> Decode w a -> Decode w b

infixl 4 <!

(<!) :: Decode w (a -> t) -> Decode 'Closed a -> Decode w t
x <! y = ApplyD x y

hsize :: Decode w t -> Int
hsize (Summands _ _) = 1
hsize (SumD _) = 0
hsize (RecD _) = 0
hsize From = 1
hsize (D _) = 1
hsize (ApplyD f x) = hsize f + hsize x
hsize (Invalid _) = 0
hsize (Map _ x) = hsize x

decode :: Decode w t -> Decoder s t
decode x = fmap snd (decodE x)

decodE :: Decode w t -> Decoder s (Int, t)
decodE x = decodeCount x 0

decodeCount :: Decode w t -> Int -> Decoder s (Int, t)
decodeCount (Summands nm f) n = (n + 1,) <$> decodeRecordSum nm (\x -> decodE (f x))
decodeCount (SumD c) n = pure (n + 1, c)
decodeCount (RecD c) n = decodeRecordNamed "RecD" (const n) (pure (n, c))
decodeCount From n = do x <- fromCBOR; pure (n, x)
decodeCount (D dec) n = do x <- dec; pure (n, x)
decodeCount (ApplyD c g) n = do
  (i, f) <- decodeCount c (n + hsize g)
  y <- decodeClosed g
  pure (i, f y)
decodeCount (Invalid k) _ = invalidKey k
decodeCount (Map f x) n = do (m, y) <- decodeCount x n; pure (m, f y)

decodeClosed :: Decode 'Closed t -> Decoder s t
decodeClosed (Summands nm f) = decodeRecordSum nm (\x -> decodE (f x))
-- decodeClosed (SumD _) = undefined -- This case, by design, is unreachable by type considerations
decodeClosed (RecD c) = pure c
decodeClosed From = do x <- fromCBOR; pure x
decodeClosed (D dec) = do x <- dec; pure x
decodeClosed (ApplyD c g) = do
  f <- decodeClosed c
  y <- decodeClosed g
  pure (f y)
decodeClosed (Invalid k) = invalidKey k
decodeClosed (Map f x) = f <$> decodeClosed x

instance Functor (Decode w) where
  fmap f (Map g x) = Map (f . g) x
  fmap f x = Map f x

-- ===========================================================================================
-- These functions are the dual analogs to
-- Shelley.Spec.Ledger.Serialization(decodeList, decodeSeq, decodeStrictSeq, decodeSet)
-- It is not well documented how to use encodeFoldable.
-- They are provided here as compatible pairs for use with the (E x) and (D x) constructors
-- of the Encode and Decode types. (E encodeList xs) and (D (decodeList fromCBOR)) should be duals.

encodeList :: ToCBOR a => [a] -> Encoding
encodeList = encodeFoldable

encodeStrictSeq :: ToCBOR a => StrictSeq a -> Encoding
encodeStrictSeq = encodeFoldable

encodeSeq :: ToCBOR a => Seq a -> Encoding
encodeSeq = encodeFoldable

encodeSet :: ToCBOR a => Set a -> Encoding
encodeSet = encodeFoldable

-- ===========================================
-- For a worked out EXAMPLE see the testfile:
-- cardano-ledger-specs/shelley/chain-and-ledger/shelley-spec-ledger-test/test/Test/Shelley/Spec/Ledger/MemoBytes.hs
