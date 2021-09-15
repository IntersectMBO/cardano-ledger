{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# OPTIONS_GHC  -fno-warn-orphans #-}

-- | MemoBytes is an abstration for a datetype that encodes its own seriialization.
--   The idea is to use a newtype around a MemoBytes non-memoizing version.
--   For example:   newtype Foo = Foo(MemoBytes NonMemoizingFoo)
--   This way all the instances for Foo (Eq,Show,Ord,ToCBOR,FromCBOR,NoThunks,Generic)
--   can be derived for free.
module Data.Coders
  ( Encode (..),
    Decode (..),
    (!>),
    (<!),
    (<*!),
    (<?),
    Density (..),
    Wrapped (..),
    Annotator (..),
    Dual (..),
    Field (..),
    field,
    fieldA,
    fieldAA,
    encode,
    decode,
    runE, -- Used in testing
    decodeList,
    decodePair,
    decodeSeq,
    decodeStrictSeq,
    decodeSet,
    decodeAnnSet,
    decodeRecordNamed,
    decodeRecordSum,
    invalidKey,
    unusedRequiredKeys,
    duplicateKey,
    wrapCBORArray,
    encodePair,
    encodeFoldable,
    encodeFoldableMapPairs,
    decodeCollectionWithLen,
    decodeCollection,
    encodeFoldableEncoder,
    encodeMap,
    wrapCBORMap,
    decodeMap,
    decodeMapByKey,
    decodeMapContents,
    decodeMapTraverse,
    decodeMapContentsTraverse,
    dualList, -- Dual values for export
    dualSeq,
    dualSet,
    dualMaybeAsList,
    dualMaybeAsNull,
    dualText,
    dualStrictSeq,
    dualCBOR,
    to,
    from,
    Decoder,
    Encoding,
    encodeNullMaybe,
    decodeNullMaybe,
    decodeSparse,
    mapEncode,
    mapDecode,
    mapDecodeA,
    setEncode,
    setDecode,
    setDecodeA,
    listEncode,
    listDecode,
    listDecodeA,
    pairDecodeA,

    -- * Utility functions
    cborError,
  )
where

import Cardano.Binary
  ( Annotator (..),
    DecoderError (DecoderErrorCustom),
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    TokenType (..),
    decodeBreakOr,
    decodeListLenOrIndef,
    decodeMapLenOrIndef,
    decodeNull,
    decodeWord,
    encodeBreak,
    encodeListLen,
    encodeListLenIndef,
    encodeMapLen,
    encodeMapLenIndef,
    encodeNull,
    encodeWord,
    matchSize,
    peekTokenType,
  )
import Codec.CBOR.Decoding (Decoder, decodeTag, decodeTag64)
import Codec.CBOR.Encoding (Encoding, encodeTag)
import Control.Applicative (liftA2)
import Control.Monad (replicateM, unless, when)
import Data.Foldable (foldl')
import Data.Functor.Compose (Compose (..))
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set, insert, member)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Typeable
import Formatting (build, formatToString)
import Formatting.Buildable (Buildable)
import Numeric.Natural (Natural)

-- ====================================================================

decodeRecordNamed :: Text.Text -> (a -> Int) -> Decoder s a -> Decoder s a
decodeRecordNamed name getRecordSize decoder = do
  lenOrIndef <- decodeListLenOrIndef
  x <- decoder
  case lenOrIndef of
    Just n -> matchSize (Text.pack "\nRecord " <> name) n (getRecordSize x)
    Nothing -> do
      isBreak <- decodeBreakOr
      unless isBreak $ cborError $ DecoderErrorCustom name "Excess terms in array"
  pure x

decodeRecordSum :: String -> (Word -> Decoder s (Int, a)) -> Decoder s a
decodeRecordSum name decoder = do
  lenOrIndef <- decodeListLenOrIndef
  tag <- decodeWord
  (size, x) <- decoder tag -- we decode all the stuff we want
  case lenOrIndef of
    Just n -> matchSize (Text.pack ("\nSum " ++ name ++ "\nreturned=" ++ show size ++ " actually read= " ++ show n)) size n
    Nothing -> do
      isBreak <- decodeBreakOr -- if there is stuff left, it is unnecessary extra stuff
      unless isBreak $ cborError $ DecoderErrorCustom (Text.pack name) "Excess terms in array"
  pure x

encodeNullMaybe :: (a -> Encoding) -> Maybe a -> Encoding
encodeNullMaybe _ Nothing = encodeNull
encodeNullMaybe encoder (Just x) = encoder x

decodeNullMaybe :: Decoder s a -> Decoder s (Maybe a)
decodeNullMaybe decoder = do
  peekTokenType >>= \case
    TypeNull -> do
      decodeNull
      pure Nothing
    _ -> Just <$> decoder

decodePair :: Decoder s a -> Decoder s b -> Decoder s (a, b)
decodePair first second = decodeRecordNamed "pair" (const 2) $ do
  a <- first
  b <- second
  pure (a, b)

encodePair :: (a -> Encoding) -> (b -> Encoding) -> (a, b) -> Encoding
encodePair encodeFirst encodeSecond (x, y) =
  encodeListLen 2
    <> encodeFirst x
    <> encodeSecond y

invalidKey :: Word -> Decoder s a
invalidKey k = cborError $ DecoderErrorCustom "not a valid key:" (Text.pack $ show k)

duplicateKey :: String -> Word -> Decoder s a
duplicateKey name k = cborError $ DecoderErrorCustom "Duplicate key:" (Text.pack $ show k ++ " while decoding type " ++ name)

unusedRequiredKeys :: Set Word -> [(Word, String)] -> String -> Decoder s a
unusedRequiredKeys used required name =
  cborError $ DecoderErrorCustom (Text.pack ("value of type " ++ name)) (Text.pack (message (filter bad required)))
  where
    bad (k, _) = not (member k used)
    message [] = ", not decoded."
    message [pair] = report pair ++ message []
    message (pair : more) = report pair ++ ", and " ++ message more
    report (k, f) = ("field " ++ f ++ " with key " ++ show k)

decodeList :: Decoder s a -> Decoder s [a]
decodeList = decodeCollection decodeListLenOrIndef

decodeSeq :: Decoder s a -> Decoder s (Seq a)
decodeSeq decoder = Seq.fromList <$> decodeList decoder

decodeStrictSeq :: Decoder s a -> Decoder s (StrictSeq a)
decodeStrictSeq decoder = StrictSeq.fromList <$> decodeList decoder

decodeSet :: Ord a => Decoder s a -> Decoder s (Set a)
decodeSet decoder = Set.fromList <$> decodeList decoder

decodeAnnSet :: Ord t => Decoder s (Annotator t) -> Decoder s (Annotator (Set t))
decodeAnnSet dec = do xs <- decodeList dec; pure (Set.fromList <$> (sequence xs))

decodeCollection :: Decoder s (Maybe Int) -> Decoder s a -> Decoder s [a]
decodeCollection lenOrIndef el = snd <$> decodeCollectionWithLen lenOrIndef el

decodeCollectionWithLen ::
  Decoder s (Maybe Int) ->
  Decoder s a ->
  Decoder s (Int, [a])
decodeCollectionWithLen lenOrIndef el = do
  lenOrIndef >>= \case
    Just len -> (,) len <$> replicateM len el
    Nothing -> loop (0, []) (not <$> decodeBreakOr) el
  where
    loop (n, acc) condition action =
      condition >>= \case
        False -> pure (n, reverse acc)
        True -> action >>= \v -> loop (n + 1, (v : acc)) condition action

encodeFoldable :: (ToCBOR a, Foldable f) => f a -> Encoding
encodeFoldable = encodeFoldableEncoder toCBOR

-- Encodes a sequence of pairs as a cbor map
encodeFoldableMapPairs :: (ToCBOR a, ToCBOR b, Foldable f) => f (a, b) -> Encoding
encodeFoldableMapPairs = encodeFoldableEncoderAs wrapCBORMap $
  \(a, b) -> toCBOR a <> toCBOR b

encodeFoldableEncoder :: (Foldable f) => (a -> Encoding) -> f a -> Encoding
encodeFoldableEncoder = encodeFoldableEncoderAs wrapCBORArray

encodeFoldableEncoderAs ::
  (Foldable f) =>
  (Word -> Encoding -> Encoding) ->
  (a -> Encoding) ->
  f a ->
  Encoding
encodeFoldableEncoderAs wrap encoder xs = wrap len contents
  where
    (len, contents) = foldl' go (0, mempty) xs
    go (!l, !enc) next = (l + 1, enc <> encoder next)

wrapCBORArray :: Word -> Encoding -> Encoding
wrapCBORArray len contents =
  if len <= 23
    then encodeListLen len <> contents
    else encodeListLenIndef <> contents <> encodeBreak

-- ===============================================================
-- We want to make a uniform way of encoding and decoding Map.Map
-- Unfortuantely the ToCBOR and FromCBOR instances date to Byron
-- Era, which are not always cannonical. We want to make these
-- cannonical improvements easy to use.

encodeMap :: (a -> Encoding) -> (b -> Encoding) -> Map.Map a b -> Encoding
encodeMap encodeKey encodeValue m =
  let l = fromIntegral $ Map.size m
      contents = Map.foldMapWithKey (\k v -> encodeKey k <> encodeValue v) m
   in wrapCBORMap l contents

wrapCBORMap :: Word -> Encoding -> Encoding
wrapCBORMap len contents =
  if len <= 23
    then encodeMapLen len <> contents
    else encodeMapLenIndef <> contents <> encodeBreak

decodeMap :: Ord a => Decoder s a -> Decoder s b -> Decoder s (Map.Map a b)
decodeMap decodeKey decodeValue = decodeMapByKey decodeKey (const decodeValue)

decodeMapByKey :: Ord a => Decoder s a -> (a -> Decoder s b) -> Decoder s (Map.Map a b)
decodeMapByKey decodeKey decodeValueFor =
  Map.fromList
    <$> decodeMapContents decodeInlinedPair
  where
    decodeInlinedPair = do
      key <- decodeKey
      value <- decodeValueFor key
      pure (key, value)

decodeMapContents :: Decoder s a -> Decoder s [a]
decodeMapContents = decodeCollection decodeMapLenOrIndef

decodeMapTraverse ::
  (Ord a, Applicative t) =>
  Decoder s (t a) ->
  Decoder s (t b) ->
  Decoder s (t (Map.Map a b))
decodeMapTraverse decodeKey decodeValue =
  fmap Map.fromList <$> decodeMapContentsTraverse decodeKey decodeValue

decodeMapContentsTraverse ::
  (Applicative t) =>
  Decoder s (t a) ->
  Decoder s (t b) ->
  Decoder s (t [(a, b)])
decodeMapContentsTraverse decodeKey decodeValue =
  sequenceA <$> decodeMapContents decodeInlinedPair
  where
    decodeInlinedPair = getCompose $ (,) <$> Compose decodeKey <*> Compose decodeValue

-- ===============================================================================
-- Encode and Decode are typed data structures which specify encoders and decoders
-- for Algebraic data structures written in Haskell. They exploit types and count
-- the correct number fields in an encoding and decoding, which are automatically computed.
-- They are somewhat dual, and are designed so that visual inspection of a Encode and
-- its dual Decode can help the user conclude that the two are self-consistent.
-- They are also reusable abstractions that can be defined once, and then used many places.
--
-- (Encode t) is a data structure from which 3 things can be recovered
-- Given:    x :: Encode t
-- 1) get a value of type t
-- 2) get an Encoding for that value, which correctly encodes the number of "fields"
--    written to the ByteString. Care must still be taken that the Keys are correct.
-- 3) get a (MemoBytes t)
-- The advantage of using Encode with a MemoBytes, is we don't have to make a ToCBOR
-- instance. Instead the "instance" is spread amongst the pattern constuctors by using
-- (memoBytes encoding) in the where clause of the pattern contructor.
-- See some examples of this see the file Timelocks.hs
--
-- The Encode and Decode mechanism are meant to specify the encoding and decoding of
-- Algebraic datatypes in a uniform way. (Decode t) is dual to (Encode t). In some cases
-- a decoder can be extracted from an encoder by visual inspection. We now give some
-- examples. In the examples Let Int and C have ToCBOR instances, and dualB :: Dual B
{-
-- An example with 1 constructor (a record) uses Rec and RecD

data C = C Text.Text
instance ToCBOR C where toCBOR (C t) = toCBOR t
instance FromCBOR C where fromCBOR = C <$> fromCBOR

data B = B Text.Text
dualB = Dual (\ (B t) ->toCBOR t) (B <$> fromCBOR)

data A = ACon Int B C

encodeA :: A -> Encode ('Closed 'Dense) A
encodeA (ACon i b c) = Rec ACon !> To i !> ED dualB b !> To c

decodeA :: Decode ('Closed 'Dense) A
decodeA = RecD ACon <! From <! DD dualB <! From

instance ToCBOR A   where toCBOR x = encode(encodeA x)
instance FromCBOR A where fromCBOR = decode decodeA

-- An example with multiple constructors uses Sum, SumD, and Summands

data N = N1 Int | N2 B Bool | N3 A

encodeN :: N -> Encode 'Open N
encodeN (N1 i)    = Sum N1 0 !> To i
encodeN (N2 b tf) = Sum N2 1 !> ED dualB b  !> To tf
encodeN (N3 a)    = Sum N3 2 !> To a

decodeN :: Decode ('Closed 'Dense) N    -- Note each clause has an 'Open decoder,
decodeN = Summands "N" decodeNx         -- But Summands returns a ('Closed 'Dense) decoder
  where decodeNx 0 = SumD N1 <! From
        decodeNx 1 = SumD N2 <! DD dualB <! From
        decodeNx 3 = SumD N3 <! From
        decodeNx k = Invalid k

instance ToCBOR N   where toCBOR x = encode(encodeN x)
instance FromCBOR N where fromCBOR = decode decodeN
-}
-- For more examples writing CBOR instances using Encode and Decode, including
-- ones using Sparse encoding, see the test file
-- shelley-spec-ledger-test/test/Test/Shelley/Spec/Ledger/Coders.hs

-- ========================================================
-- Subsidary classes and datatype used in the Coders scheme
-- =========================================================

-- | Some CBOR instances wrap encoding sequences with prefixes and suffixes. I.e.
--  prefix , encode, encode, encode , ... , suffix.
--  There are two kinds of wrapping coders: Nary sums, and Sparsely encoded products.
--  Coders in these classes can only be decoded when they are wrapped by their
--  closing forms Summand and SparseKeyed. In another dimension records can be
--  encoded densely (all their fields serialised) or sparsely (only some of their
--  fields). We use indexes to types to try and mark (and enforce) these distinctions.

-- | Record density (all the fields) vs (some of the fields)
data Density = Dense | Sparse

data Wrapped where
  Open :: Wrapped -- Needs some type-wide wrapping
  Closed :: Density -> Wrapped -- Does not need type-wide wrapping,
  -- But may need field-wide wrapping, when Density is 'Sparse

-- | Analogous to paired ToCBOR and FromCBOR instances with out freezing out
--   alternate ways to code. Unlike ToCBOR and FromCBOR where there is only
--   one instance per type. There can be multiple Duals with the same type.
data Dual t = Dual (t -> Encoding) (forall s. Decoder s t)

-- | A Field pairs an update function and a decoder for one field of a Sparse record.
data Field t where
  Field :: (x -> t -> t) -> (forall s. Decoder s x) -> Field t

field :: (x -> t -> t) -> Decode ('Closed d) x -> Field t
field update dec = Field update (decode dec)

-- In order to sparse decode something with a (FromCBOR (Annotator t)) instance
-- we can use these 'field' like functions.

fieldA :: Applicative ann => (x -> t -> t) -> Decode ('Closed d) x -> Field (ann t)
fieldA update dec = Field (liftA2 update) (pure <$> decode dec)

fieldAA :: Applicative ann => (x -> t -> t) -> Decode ('Closed d) (ann x) -> Field (ann t)
fieldAA update dec = Field (liftA2 update) (decode dec)

-- ===========================================================
-- The coders and the decoders as GADT datatypes
-- ===========================================================

data Encode (w :: Wrapped) t where
  Rec :: t -> Encode ('Closed 'Dense) t -- Constructor of normal Record (1 constructor)
  Sum :: t -> Word -> Encode 'Open t -- One Constructor of many
  Keyed :: t -> Encode ('Closed 'Sparse) t -- One Constructor with sparse encoding
  To :: ToCBOR a => a -> Encode ('Closed 'Dense) a
  E :: (t -> Encoding) -> t -> Encode ('Closed 'Dense) t
  ED :: Dual t -> t -> Encode ('Closed 'Dense) t
  OmitC :: t -> Encode w t
  Tag :: Word -> Encode ('Closed x) t -> Encode ('Closed x) t
  Omit :: (t -> Bool) -> Encode ('Closed 'Sparse) t -> Encode ('Closed 'Sparse) t
  Key :: Word -> Encode ('Closed 'Dense) t -> Encode ('Closed 'Sparse) t
  ApplyE :: Encode w (a -> t) -> Encode ('Closed r) a -> Encode w t

-- The Wrapped index of ApplyE is determined by the index
-- at the bottom of its left spine. The choices are 'Open (Sum c tag),
-- ('Closed 'Dense) (Rec c), and ('Closed 'Sparse) (Keyed c).

infixl 4 !>

(!>) :: Encode w (a -> t) -> Encode ('Closed r) a -> Encode w t
x !> y = ApplyE x y

runE :: Encode w t -> t
runE (Sum cn _) = cn
runE (Rec cn) = cn
runE (ApplyE f x) = runE f (runE x)
runE (To x) = x
runE (E _ x) = x
runE (ED _ x) = x
runE (OmitC x) = x
runE (Omit _ x) = runE x
runE (Tag _ x) = runE x
runE (Key _ x) = runE x
runE (Keyed cn) = cn

gsize :: Encode w t -> Word
gsize (Sum _ _) = 0
gsize (Rec _) = 0
gsize (To _) = 1
gsize (E _ _) = 1
gsize (ApplyE f x) = gsize f + gsize x
gsize (ED _ _) = 1
gsize (OmitC _) = 0
gsize (Omit p x) = if p (runE x) then 0 else gsize x
gsize (Tag _ _) = 1
gsize (Key _ x) = gsize x
gsize (Keyed _) = 0

encode :: Encode w t -> Encoding
encode sym = encodeCountPrefix 0 sym
  where
    encodeCountPrefix :: Word -> Encode w t -> Encoding
    -- n is the number of fields we must write in the prefix.
    encodeCountPrefix n (Sum _ tag) = encodeListLen (n + 1) <> encodeWord tag
    encodeCountPrefix n (Keyed _) = encodeMapLen n
    encodeCountPrefix n (Rec _) = encodeListLen n
    encodeCountPrefix _ (To x) = toCBOR x
    encodeCountPrefix _ (E enc x) = enc x
    encodeCountPrefix _ (ED (Dual enc _) x) = enc x
    encodeCountPrefix _ (OmitC _) = mempty
    encodeCountPrefix n (Tag tag x) = encodeTag tag <> encodeCountPrefix n x
    encodeCountPrefix n (Key tag x) = encodeWord tag <> encodeCountPrefix n x
    encodeCountPrefix n (Omit p x) =
      if p (runE x) then mempty else encodeCountPrefix n x
    encodeCountPrefix n (ApplyE ff xx) = encodeCountPrefix (n + gsize xx) ff <> encodeClosed xx
      where
        encodeClosed :: Encode ('Closed d) t -> Encoding
        encodeClosed (Rec _) = mempty
        encodeClosed (To x) = toCBOR x
        encodeClosed (E enc x) = enc x
        encodeClosed (ApplyE f x) = encodeClosed f <> encodeClosed x
        encodeClosed (ED (Dual enc _) x) = enc x
        encodeClosed (OmitC _) = mempty
        encodeClosed (Omit p x) =
          if p (runE x) then mempty else encodeClosed x
        encodeClosed (Tag tag x) = encodeTag tag <> encodeClosed x
        encodeClosed (Key tag x) = encodeWord tag <> encodeClosed x
        encodeClosed (Keyed _) = mempty

-- ==================================================================
-- Decode
-- ===================================================================

data Decode (w :: Wrapped) t where
  Summands :: String -> (Word -> Decode 'Open t) -> Decode ('Closed 'Dense) t
  SparseKeyed :: Typeable t => String -> t -> (Word -> Field t) -> [(Word, String)] -> Decode ('Closed 'Dense) t
  SumD :: t -> Decode 'Open t
  RecD :: t -> Decode ('Closed 'Dense) t
  KeyedD :: t -> Decode ('Closed 'Sparse) t
  From :: FromCBOR t => Decode w t
  D :: (forall s. Decoder s t) -> Decode ('Closed 'Dense) t
  ApplyD :: Decode w1 (a -> t) -> Decode ('Closed d) a -> Decode w1 t
  Invalid :: Word -> Decode w t
  Map :: (a -> b) -> Decode w a -> Decode w b
  DD :: Dual t -> Decode ('Closed 'Dense) t
  TagD :: Word -> Decode ('Closed x) t -> Decode ('Closed x) t
  Emit :: t -> Decode w t
  -- The next two could be generalized to any (Applicative f) rather than Annotator
  Ann :: Decode w t -> Decode w (Annotator t)
  ApplyAnn :: Decode w1 (Annotator (a -> t)) -> Decode ('Closed d) (Annotator a) -> Decode w1 (Annotator t)
  -- A function to Either can raise an error when applied by returning (Left errorMessage)
  ApplyErr :: Decode w1 (a -> Either String t) -> Decode ('Closed d) a -> Decode w1 t

infixl 4 <!

infixl 4 <*!

infixl 4 <?

(<!) :: Decode w1 (a -> t) -> Decode ('Closed w) a -> Decode w1 t
x <! y = ApplyD x y

(<*!) :: Decode w1 (Annotator (a -> t)) -> Decode ('Closed d) (Annotator a) -> Decode w1 (Annotator t)
x <*! y = ApplyAnn x y

(<?) :: Decode w1 (a -> Either String t) -> Decode ('Closed d) a -> Decode w1 t
f <? y = ApplyErr f y

hsize :: Decode w t -> Int
hsize (Summands _ _) = 1
hsize (SumD _) = 0
hsize (RecD _) = 0
hsize (KeyedD _) = 0
hsize From = 1
hsize (D _) = 1
hsize (DD _) = 1
hsize (ApplyD f x) = hsize f + hsize x
hsize (Invalid _) = 0
hsize (Map _ x) = hsize x
hsize (Emit _) = 0
hsize (SparseKeyed _ _ _ _) = 1
hsize (TagD _ _) = 1
hsize (Ann x) = hsize x
hsize (ApplyAnn f x) = hsize f + hsize x
hsize (ApplyErr f x) = hsize f + hsize x

decode :: Decode w t -> Decoder s t
decode x = fmap snd (decodE x)

decodE :: Decode w t -> Decoder s (Int, t)
decodE x = decodeCount x 0

decodeCount :: forall (w :: Wrapped) s t. Decode w t -> Int -> Decoder s (Int, t)
decodeCount (Summands nm f) n = (n + 1,) <$> decodeRecordSum nm (\x -> decodE (f x))
decodeCount (SumD cn) n = pure (n + 1, cn)
decodeCount (KeyedD cn) n = pure (n + 1, cn)
decodeCount (RecD cn) n = decodeRecordNamed "RecD" (const n) (pure (n, cn))
decodeCount From n = (n,) <$> fromCBOR
decodeCount (D dec) n = (n,) <$> dec
decodeCount (Invalid k) _ = invalidKey k
decodeCount (Map f x) n = do (m, y) <- decodeCount x n; pure (m, f y)
decodeCount (DD (Dual _enc dec)) n = (n,) <$> dec
decodeCount (Emit x) n = pure (n, x)
decodeCount (TagD expectedTag decoder) n = do
  assertTag expectedTag
  decodeCount decoder n
decodeCount (SparseKeyed name initial pick required) n =
  (n + 1,) <$> decodeSparse name initial pick required
decodeCount (Ann x) n = do (m, y) <- decodeCount x n; pure (m, pure y)
decodeCount (ApplyAnn g x) n = do
  (i, f) <- decodeCount g (n + hsize x)
  y <- decodeClosed x
  pure (i, f <*> y)
decodeCount (ApplyD cn g) n = do
  (i, f) <- decodeCount cn (n + hsize g)
  y <- decodeClosed g
  pure (i, f y)
decodeCount (ApplyErr cn g) n = do
  (i, f) <- decodeCount cn (n + hsize g)
  y <- decodeClosed g
  case f y of
    Right z -> pure (i, z)
    Left message -> cborError $ DecoderErrorCustom "decoding error:" (Text.pack $ message)

-- The type of DecodeClosed precludes pattern match against (SumD c) as the types are different.

decodeClosed :: Decode ('Closed d) t -> Decoder s t
decodeClosed (Summands nm f) = decodeRecordSum nm (\x -> decodE (f x))
decodeClosed (KeyedD cn) = pure cn
decodeClosed (RecD cn) = pure cn
decodeClosed From = do x <- fromCBOR; pure x
decodeClosed (D dec) = dec
decodeClosed (ApplyD cn g) = do
  f <- decodeClosed cn
  y <- decodeClosed g
  pure (f y)
decodeClosed (Invalid k) = invalidKey k
decodeClosed (Map f x) = f <$> decodeClosed x
decodeClosed (DD (Dual _enc dec)) = dec
decodeClosed (Emit n) = pure n
decodeClosed (TagD expectedTag decoder) = do
  assertTag expectedTag
  decodeClosed decoder
decodeClosed (SparseKeyed name initial pick required) =
  decodeSparse name initial pick required
decodeClosed (Ann x) = fmap pure (decodeClosed x)
decodeClosed (ApplyAnn g x) = do
  f <- decodeClosed g
  y <- decodeClosed x
  pure (f <*> y)
decodeClosed (ApplyErr cn g) = do
  f <- decodeClosed cn
  y <- decodeClosed g
  case f y of
    Right z -> pure z
    Left message -> cborError $ DecoderErrorCustom "decoding error:" (Text.pack $ message)

decodeSparse ::
  Typeable a =>
  String ->
  a ->
  (Word -> Field a) ->
  [(Word, String)] ->
  Decoder s a
decodeSparse name initial pick required = do
  lenOrIndef <- decodeMapLenOrIndef
  (!v, used) <- getSparseBlock lenOrIndef initial pick (Set.empty) name
  if all (\(key, _name) -> member key used) required
    then pure v
    else unusedRequiredKeys used required (show (typeOf initial))

-- | Given a function that picks a Field from a key, decodes that field
--   and returns a (t -> t) transformer, which when applied, will
--   update the record with the value decoded.
applyField :: (Word -> Field t) -> Set Word -> String -> Decoder s (t -> t, Set Word)
applyField f seen name = do
  tag <- decodeWord
  if Set.member tag seen
    then duplicateKey name tag
    else case f tag of
      Field update d -> do v <- d; pure (update v, insert tag seen)

-- | Decode a Map Block of key encoded data for type t
--   given a function that picks the right box for a given key, and an
--   initial value for the record (usually starts filled with default values).
--   The Block can be either len-encoded or block-encoded.
getSparseBlock :: Maybe Int -> t -> (Word -> Field t) -> Set Word -> String -> Decoder s (t, Set Word)
getSparseBlock (Just 0) initial _pick seen _name = pure (initial, seen)
getSparseBlock (Just n) initial pick seen name = do
  (transform, seen2) <- applyField pick seen name
  getSparseBlock (Just (n -1)) (transform initial) pick seen2 name
getSparseBlock Nothing initial pick seen name =
  decodeBreakOr >>= \case
    True -> pure (initial, seen)
    False -> do
      (transform, seen2) <- applyField pick seen name
      getSparseBlock Nothing (transform initial) pick seen2 name

-- ======================================================
-- (Decode ('Closed 'Dense)) and (Decode ('Closed 'Sparse)) are applicative
-- (Decode 'Open) is not applicative since there is no
-- (Applys 'Open 'Open) instance. And there should never be one.

instance Functor (Decode w) where
  fmap f (Map g x) = Map (f . g) x
  fmap f x = Map f x

instance Applicative (Decode ('Closed d)) where
  pure x = Emit x
  f <*> x = ApplyD f x

-- ===========================================================================================
-- A Dual pairs an Encoding and a Decoder with a roundtrip property.
-- They are used with the (E and D) constructors of Encode and Decode
-- If you are trying to code something not in the CBOR classes
-- or you want something not traditional, make you own Dual and use E or D

-- data Dual t = Dual (t -> Encoding) (forall s . Decoder s t)

dualList :: (ToCBOR a, FromCBOR a) => Dual [a]
dualList = Dual encodeFoldable (decodeList fromCBOR)

dualSeq :: (ToCBOR a, FromCBOR a) => Dual (Seq a)
dualSeq = Dual encodeFoldable (decodeSeq fromCBOR)

dualSet :: (Ord a, ToCBOR a, FromCBOR a) => Dual (Set a)
dualSet = Dual encodeFoldable (decodeSet fromCBOR)

-- | Good for encoding (Maybe t) if is another Maybe. Uses more space than dualMaybeAsNull
dualMaybeAsList :: (ToCBOR a, FromCBOR a) => Dual (Maybe a)
dualMaybeAsList = Dual toCBOR fromCBOR

-- | Good for encoding (Maybe T) as long as T isn't another Maybe
dualMaybeAsNull :: (ToCBOR a, FromCBOR a) => Dual (Maybe a)
dualMaybeAsNull = Dual (encodeNullMaybe toCBOR) (decodeNullMaybe fromCBOR)

dualStrictSeq :: (ToCBOR a, FromCBOR a) => Dual (StrictSeq a)
dualStrictSeq = Dual encodeFoldable (decodeStrictSeq fromCBOR)

dualText :: Dual Text.Text
dualText = Dual toCBOR fromCBOR

dualCBOR :: (ToCBOR a, FromCBOR a) => Dual a
dualCBOR = Dual toCBOR fromCBOR

-- Use to and from, when you want to guarantee that a type has both
-- ToCBOR and FromCBR instances.

to :: (ToCBOR t, FromCBOR t) => t -> Encode ('Closed 'Dense) t
to xs = ED dualCBOR xs

from :: (ToCBOR t, FromCBOR t) => Decode ('Closed 'Dense) t
from = DD dualCBOR

-- ==================================================================
-- Combinators for building ({En|De}code ('Closed 'Dense) x) objects.
-- The use of "encodeFoldable" is not self-documenting at all (and
-- not even correct for Maps, even though Map is an instance of Foldable)
-- So instead of writing:  (E encodeFoldable x), we want people to write:
-- 1) (mapEncode x)   if x is a Map
-- 2) (setEncode x)   if x is a Set
-- 3) (listEncode x)  if x is a List
--
-- To decode one of these foldable instances, should use one of the aptly named Duals
--
-- 1) mapDecode   if x is a Map
-- 2) setDecode   if x is a Set
-- 3) listDecode  if x is a List
--
-- If one needs an Annotated decoder, one can use (explained further below)
--
-- 1) mapDecodeA   if x is a Map
-- 2) setDecodeA   if x is a Set
-- 3) listDecodeA  if x is a List
-- 4) pairDecodeA  if x is a Pair like (Int,Bool)

-- | (mapEncode x)  is self-documenting, correct way to encode Map. use mapDecode as its dual
mapEncode :: (ToCBOR k, ToCBOR v) => Map.Map k v -> Encode ('Closed 'Dense) (Map.Map k v)
mapEncode x = E (encodeMap toCBOR toCBOR) x

-- | (mapDecode) is the Dual for (mapEncode x)
mapDecode :: (Ord k, FromCBOR k, FromCBOR v) => Decode ('Closed 'Dense) (Map.Map k v)
mapDecode = D (decodeMap fromCBOR fromCBOR)

-- | (setEncode x) is self-documenting (E encodeFoldable x), use setDecode as its dual
setEncode :: (ToCBOR v) => Set.Set v -> Encode ('Closed 'Dense) (Set.Set v)
setEncode x = E encodeFoldable x

-- | (setDecode) is the Dual for (setEncode x)
setDecode :: (Ord v, FromCBOR v) => Decode ('Closed 'Dense) (Set.Set v)
setDecode = D (decodeSet fromCBOR)

-- | (listEncode x) is self-documenting (E encodeFoldable x), use listDecode as its dual
listEncode :: (ToCBOR v) => [v] -> Encode ('Closed 'Dense) [v]
listEncode x = E encodeFoldable x

-- | (listDecode) is the Dual for (listEncode x)
listDecode :: (FromCBOR v) => Decode ('Closed 'Dense) [v]
listDecode = D (decodeList fromCBOR)

-- =============================================================================
-- Combinators for building (Decode ('Closed 'Dense) (Annotator x)) objects. Unlike
-- the combinators above (setDecode, mapDecode, ListDecode) for Non-Annotator types,
-- these combinators take explicit (Decode  ('Closed 'Dense) i) objects as parameters
-- rather than relying on FromCBOR instances as implicit parameters. To get the
-- annotator version, just add 'A' to the end of the non-annotator version decode function.
-- E.g.  setDecodeA, listDecodeA, mapDecodeA. Suppose I want to decode x:: Map [A] (B,C)
-- and I only have Annotator instances of A and C, then the following decodes x.
-- mapDecodeA (listDecodeA From) (pairDecodeA (Ann From) From).
--                                             ^^^^^^^^
-- One can always lift x::(Decode w T) by using Ann. so (Ann x)::(Decode w (Annotator T)).

pairDecodeA :: Decode ('Closed 'Dense) (Annotator x) -> Decode ('Closed 'Dense) (Annotator y) -> Decode ('Closed 'Dense) (Annotator (x, y))
pairDecodeA x y = D (do (xA, yA) <- decodePair (decode x) (decode y); pure (do x' <- xA; y' <- yA; pure (x', y')))

listDecodeA :: Decode ('Closed 'Dense) (Annotator x) -> Decode ('Closed 'Dense) (Annotator [x])
listDecodeA dx = D (do listXA <- decodeList (decode dx); pure (sequence listXA))

setDecodeA :: Ord x => Decode ('Closed 'Dense) (Annotator x) -> Decode ('Closed 'Dense) (Annotator (Set x))
setDecodeA dx = D (decodeAnnSet (decode dx))

mapDecodeA ::
  Ord k =>
  Decode ('Closed 'Dense) (Annotator k) ->
  Decode ('Closed 'Dense) (Annotator v) ->
  Decode ('Closed 'Dense) (Annotator (Map.Map k v))
mapDecodeA k v = D (decodeMapTraverse (decode k) (decode v))

-- ==================================================================
-- A Guide to Visual inspection of Duality in Encode and Decode
--
-- 1) (Sum c)     and (SumD c)    are duals
-- 2) (Rec c)     and (RecD c)    are duals
-- 3) (Keyed c)   and (KeyedD c)  are duals
-- 4) (OmitC x)   and (Emit x)    are duals
-- 5) (Omit p ..) and (Emit x)    are duals if (p x) is True
-- 6) (To x)      and (From)      are duals if (x::T) and (forall (y::T). isRight (roundTrip y))
-- 7) (E enc x)   and (D dec)     are duals if (forall x . isRight (roundTrip' enc dec x))
-- 6) (ED d x)    and (DD f)      are duals as long as d=(Dual enc dec) and (forall x . isRight (roundTrip' enc dec x))
-- 7) f !> x      and g <! y      are duals if (f and g are duals) and (x and y are duals)
--
-- The duality of (Summands name decodeT) depends on the duality of the range of decodeT with the endoder of T
-- A some property also holds for (SparseKeyed name (init::T) pick required) depending on the keys of pick and the Sparse encoder of T

--------------------------------------------------------------------------------
-- Utility functions for working with CBOR
--------------------------------------------------------------------------------

assertTag :: Word -> Decoder s ()
assertTag tag = do
  t <-
    peekTokenType >>= \case
      TypeTag -> fromIntegral <$> decodeTag
      TypeTag64 -> fromIntegral <$> decodeTag64
      _ -> cborError ("expected tag" :: String)
  when (t /= (fromIntegral tag :: Natural)) $
    cborError ("expecteg tag " <> show tag <> " but got tag " <> show t)

-- | Convert a @Buildable@ error into a 'cborg' decoder error
cborError :: Buildable e => e -> Decoder s a
cborError = fail . formatToString build
