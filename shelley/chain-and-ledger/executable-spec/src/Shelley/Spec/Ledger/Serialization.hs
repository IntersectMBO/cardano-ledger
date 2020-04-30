{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shelley.Spec.Ledger.Serialization
  ( ToCBORGroup (..)
  , FromCBORGroup (..)
  , CBORGroup (..)
  , CborSeq (..)
  , unwrapCborStrictSeq
  , decodeList
  , decodeSeq
  , decodeSet
  , decodeMap
  , decodeMapContents
  , decodeMapTraverse
  , decodeMaybe
  , decodeRecordNamed
  , decodeNullMaybe
  , encodeFoldable
  , encodeFoldableEncoder
  , encodeFoldableMapEncoder
  , encodeNullMaybe
  , groupRecord
  , rationalToCBOR
  , rationalFromCBOR
  , mapToCBOR
  , mapFromCBOR
  )
where

import           Cardano.Binary (Decoder, DecoderError (..), Encoding, FromCBOR (..), ToCBOR (..),
                     TokenType (TypeNull), decodeBreakOr, decodeListLenOrIndef,
                     decodeMapLenOrIndef, decodeNull, decodeTag, encodeBreak, encodeListLen,
                     encodeListLenIndef, encodeMapLen, encodeMapLenIndef, encodeNull, encodeTag,
                     matchSize, peekTokenType)
import           Cardano.Prelude (Text, cborError)
import           Control.Monad (replicateM, unless)
import           Data.Foldable (foldl')
import           Data.Functor.Compose (Compose (..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ratio (Rational, denominator, numerator, (%))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable

class Typeable a => ToCBORGroup a where
  toCBORGroup :: a -> Encoding
  listLen     :: a -> Word

newtype CBORGroup a = CBORGroup a

instance ToCBORGroup a => ToCBOR (CBORGroup a) where
  toCBOR (CBORGroup x) = encodeListLen (listLen x) <> toCBORGroup x

class Typeable a => FromCBORGroup a where
  fromCBORGroup :: Decoder s a

instance (FromCBORGroup a, ToCBORGroup a) => FromCBOR (CBORGroup a) where
  fromCBOR = CBORGroup <$> groupRecord

decodeRecordNamed :: Text -> (a -> Int) -> Decoder s a -> Decoder s a
decodeRecordNamed name getRecordSize decode = do
  lenOrIndef <- decodeListLenOrIndef
  x <- decode
  case lenOrIndef of
    Just n -> matchSize name (getRecordSize x) n
    Nothing -> do
      isBreak <- decodeBreakOr
      unless isBreak $ cborError $ DecoderErrorCustom name "Excess terms in array"
  pure x

groupRecord :: forall a s. (ToCBORGroup a, FromCBORGroup a) => Decoder s a
groupRecord = decodeRecordNamed "CBORGroup" (fromIntegral . toInteger . listLen) fromCBORGroup

mapToCBOR :: (ToCBOR a, ToCBOR b) => Map a b -> Encoding
mapToCBOR m =
    let l = fromIntegral $ Map.size m
        contents = Map.foldMapWithKey (\k v -> toCBOR k <> toCBOR v) m
    in wrapCBORMap l contents

mapFromCBOR :: (Ord a, FromCBOR a, FromCBOR b) => Decoder s (Map a b)
mapFromCBOR = decodeMap fromCBOR fromCBOR

decodeMap :: Ord a => Decoder s a -> Decoder s b -> Decoder s (Map a b)
decodeMap decodeKey decodeValue = Map.fromList
    <$> decodeMapContents decodePair
    where
    decodePair = (,) <$> decodeKey <*> decodeValue

decodeMapTraverse
   :: (Ord a, Applicative t)
   => Decoder s (t a)
   -> Decoder s (t b)
   -> Decoder s (t (Map a b))
decodeMapTraverse decodeKey decodeValue = fmap Map.fromList . sequenceA
    <$> decodeMapContents decodePair
    where
    decodePair = getCompose $ (,) <$> Compose decodeKey <*> Compose decodeValue

newtype CborSeq a = CborSeq { unwrapCborSeq :: Seq a }
  deriving Foldable

instance ToCBOR a => ToCBOR (CborSeq a) where
  toCBOR (CborSeq xs) =
    let l = fromIntegral $ Seq.length xs
        contents = foldMap toCBOR xs
    in wrapCBORArray l contents

instance FromCBOR a => FromCBOR (CborSeq a) where
  fromCBOR = CborSeq <$> decodeSeq fromCBOR

decodeSeq :: Decoder s a -> Decoder s (Seq a)
decodeSeq decoder = Seq.fromList <$> decodeList decoder

unwrapCborStrictSeq :: CborSeq a -> StrictSeq a
unwrapCborStrictSeq = StrictSeq.toStrict . unwrapCborSeq

decodeSet :: Ord a => Decoder s a -> Decoder s (Set a)
decodeSet decoder = Set.fromList <$> decodeList decoder

encodeFoldable :: (ToCBOR a, Foldable f) => f a -> Encoding
encodeFoldable = encodeFoldableEncoder toCBOR

encodeFoldableEncoder :: (Foldable f) => (a -> Encoding) -> f a -> Encoding
encodeFoldableEncoder encode xs = wrapCBORArray len contents
  where
    (len, contents) = foldl' go (0, mempty) xs
    go (!l, !enc) next = (l+1, enc <> encode next)

encodeFoldableMapEncoder
  :: Foldable f
  => (Word -> a -> Maybe Encoding)
  -> f a
  -> Encoding
encodeFoldableMapEncoder encode xs = wrapCBORMap len contents
  where
    (len, _, contents) = foldl' go (0, 0, mempty) xs
    go (!l, !i, !enc) next = case encode i next of
      Nothing -> (l, i+1, enc)
      Just e -> (l+1, i+1, enc <> e)

wrapCBORArray :: Word -> Encoding -> Encoding
wrapCBORArray len contents =
  if len <= 23
    then encodeListLen len <> contents
    else encodeListLenIndef <> contents <> encodeBreak

wrapCBORMap :: Word -> Encoding -> Encoding
wrapCBORMap len contents =
  if len <= 23
    then encodeMapLen len <> contents
    else encodeMapLenIndef <> contents <> encodeBreak

decodeList :: Decoder s a -> Decoder s [a]
decodeList = decodeCollection decodeListLenOrIndef

decodeMaybe :: Decoder s a -> Decoder s (Maybe a)
decodeMaybe d = decodeList d >>= \case
  [] -> pure Nothing
  [x] -> pure $ Just x
  _ -> cborError $ DecoderErrorCustom "Maybe"
         "Expected an array of length 0 or 1"

decodeMapContents :: Decoder s a -> Decoder s [a]
decodeMapContents = decodeCollection decodeMapLenOrIndef

decodeCollection :: Decoder s (Maybe Int) -> Decoder s a -> Decoder s [a]
decodeCollection lenOrIndef el = snd <$> decodeCollectionWithLen lenOrIndef el

decodeCollectionWithLen
  :: Decoder s (Maybe Int)
  -> Decoder s a
  -> Decoder s (Int,[a])
decodeCollectionWithLen lenOrIndef el = do
  lenOrIndef >>= \case
    Just len -> (,) len <$> replicateM len el
    Nothing -> loop (0,[]) (not <$> decodeBreakOr) el
  where
  loop (n,acc) condition action = condition >>= \case
      False -> pure (n,reverse acc)
      True -> action >>= \v -> loop (n+1, (v:acc)) condition action

rationalToCBOR :: Rational -> Encoding
rationalToCBOR r = encodeTag 30
  <> encodeListLen 2 <> toCBOR (numerator r) <> toCBOR (denominator r)

rationalFromCBOR :: Decoder s Rational
rationalFromCBOR = do
    t <- decodeTag
    unless (t == 30) $ cborError $ DecoderErrorCustom "rational" "expected tag 30"
    (numInts, ints) <- decodeCollectionWithLen (decodeListLenOrIndef) fromCBOR
    case ints of
      n:d:[] -> pure $ n % d
      _ -> cborError $ DecoderErrorSizeMismatch "rational" 2 numInts

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
