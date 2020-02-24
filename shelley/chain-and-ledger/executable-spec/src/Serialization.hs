{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Serialization
  ( ToCBORGroup (..)
  , FromCBORGroup (..)
  , CBORGroup (..)
  , CborSeq (..)
  , CBORMap (..)
  , decodeCollection
  , decodeCollectionWithLen
  , mapHelper
  )
where

import           Cardano.Binary (Decoder, Encoding, FromCBOR (..), ToCBOR (..), decodeBreakOr,
                     decodeListLen, decodeListLenOrIndef, decodeMapLenOrIndef, encodeBreak,
                     encodeListLen, encodeListLenIndef, encodeMapLen, encodeMapLenIndef, matchSize)
import           Control.Monad (replicateM)
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
    <$> decodeCollection decodeMapLenOrIndef decodePair
    where
    decodePair = (,) <$> fromCBOR <*> fromCBOR

newtype CborSeq a = CborSeq { unwrapCborSeq :: Seq a }
  deriving Foldable

instance ToCBOR a => ToCBOR (CborSeq a) where
  toCBOR (CborSeq xs) =
    let l = fromIntegral $ Seq.length xs
        contents = foldMap toCBOR xs
    in
    if l <= 23
    then encodeListLen l <> contents
    else encodeListLenIndef <> contents <> encodeBreak

instance FromCBOR a => FromCBOR (CborSeq a) where
  fromCBOR = CborSeq . Seq.fromList
    <$> decodeCollection decodeListLenOrIndef fromCBOR

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

mapHelper :: Decoder s b -> Decoder s [b]
mapHelper decodePart = decodeMapLenOrIndef >>= \case
  Just len -> replicateM len decodePart
  Nothing  -> loop [] (not <$> decodeBreakOr) decodePart
  where
  loop acc condition action = condition >>= \case
    False -> pure acc
    True -> action >>= \v -> loop (v:acc) condition action
