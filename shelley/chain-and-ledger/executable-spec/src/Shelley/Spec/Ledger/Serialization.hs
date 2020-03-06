{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Shelley.Spec.Ledger.Serialization
  ( ToCBORGroup (..)
  , FromCBORGroup (..)
  , CBORGroup (..)
  , CborSeq (..)
  , CBORMap (..)
  , decodeList
  , decodeMapContents
  , encodeFoldable
  )
where

import           Cardano.Binary (Decoder, Encoding, FromCBOR (..), ToCBOR (..), decodeBreakOr,
                     decodeListLen, decodeListLenOrIndef, decodeMapLenOrIndef, encodeBreak,
                     encodeListLen, encodeListLenIndef, encodeMapLen, encodeMapLenIndef, matchSize)
import           Control.Monad (replicateM)
import           Data.Foldable (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
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
  fromCBOR = do
    n <- decodeListLen
    x <- fromCBORGroup
    matchSize "CBORGroup" ((fromIntegral . toInteger . listLen) x) n
    pure $ CBORGroup x


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
