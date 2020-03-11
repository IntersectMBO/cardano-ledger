{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Shelley.Spec.Ledger.Serialization
  ( ToCBORGroup (..)
  , FromCBORGroup (..)
  , CBORGroup (..)
  , CborSeq (..)
  , CBORMap (..)
  , decodeList
  , decodeMapContents
  , encodeFoldable
  , groupRecord
  , rationalToCBOR
  , rationalFromCBOR
  )
where

import           Cardano.Binary (Decoder, DecoderError (..), Encoding, FromCBOR (..), ToCBOR (..),
                     decodeBreakOr, decodeListLenOrIndef, decodeMapLenOrIndef, decodeTag,
                     encodeBreak, encodeListLen, encodeListLenIndef, encodeMapLen,
                     encodeMapLenIndef, encodeTag, matchSize)
import           Cardano.Prelude (cborError)
import           Control.Monad (replicateM, unless, void)
import           Data.Foldable (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ratio (Rational, denominator, numerator, (%))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
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

decodeRecord :: (a -> Int) -> Decoder s a -> Decoder s a
decodeRecord getRecordSize decode = do
  lenOrIndef <- decodeListLenOrIndef
  x <- decode
  case lenOrIndef of
    Just n -> matchSize "CBORGroup" (getRecordSize x) n
    Nothing -> void decodeBreakOr -- TODO: make this give better errors
  pure x

groupRecord :: forall a s. (ToCBORGroup a, FromCBORGroup a) => Decoder s a
groupRecord = decodeRecord (fromIntegral . toInteger . listLen) fromCBORGroup

newtype CBORMap a b = CBORMap { unwrapCBORMap :: Map a b }

instance (ToCBOR a, ToCBOR b) => ToCBOR (CBORMap a b) where
  toCBOR (CBORMap m) =
    let l = fromIntegral $ Map.size m
        contents = Map.foldMapWithKey (\k v -> toCBOR k <> toCBOR v) m
    in
    if l <= 23
    then encodeMapLen l <> contents
    else encodeMapLenIndef <> contents <> encodeBreak

instance (Ord a, FromCBOR a, FromCBOR b) => FromCBOR (CBORMap a b) where
  fromCBOR = CBORMap . Map.fromList
    <$> decodeMapContents decodePair
    where
    decodePair = (,) <$> fromCBOR <*> fromCBOR

newtype CborSeq a = CborSeq { unwrapCborSeq :: Seq a }
  deriving Foldable

instance ToCBOR a => ToCBOR (CborSeq a) where
  toCBOR (CborSeq xs) =
    let l = fromIntegral $ Seq.length xs
        contents = foldMap toCBOR xs
    in wrapCBORArray l contents

instance FromCBOR a => FromCBOR (CborSeq a) where
  fromCBOR = CborSeq . Seq.fromList <$> decodeList fromCBOR

encodeFoldable :: (ToCBOR a, Foldable f) => f a -> Encoding
encodeFoldable xs = wrapCBORArray len contents
  where
    (len, contents) = foldl' go (0, mempty) xs
    go (!l, !enc) next = (l+1,enc <> toCBOR next)

wrapCBORArray :: Word -> Encoding -> Encoding
wrapCBORArray len contents =
  if len <= 23
    then encodeListLen len <> contents
    else encodeListLenIndef <> contents <> encodeBreak


decodeList :: Decoder s a -> Decoder s [a]
decodeList = decodeCollection decodeListLenOrIndef

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
      False -> pure (n,acc)
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
