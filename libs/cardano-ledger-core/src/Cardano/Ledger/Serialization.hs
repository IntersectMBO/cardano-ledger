{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Cardano.Ledger.Serialization
  ( ToCBORGroup (..),
    FromCBORGroup (..),
    CBORGroup (..),
    CborSeq (..),
    decodeList,
    decodeSeq,
    decodeStrictSeq,
    decodeSet,
    decodeMap,
    decodeMapContents,
    decodeMapTraverse,
    decodeMaybe,
    decodeRecordNamed,
    decodeRecordNamedT,
    decodeRecordSum,
    decodeNullMaybe,
    encodeFoldable,
    encodeFoldableEncoder,
    encodeFoldableMapEncoder,
    encodeNullMaybe,
    encodeMap,
    groupRecord,
    ratioToCBOR,
    ratioFromCBOR,
    mapToCBOR,
    mapFromCBOR,
    translateViaCBORAnn,
    -- IPv4
    ipv4ToBytes,
    ipv4ToCBOR,
    ipv4FromCBOR,
    -- IPv6
    ipv6ToBytes,
    ipv6ToCBOR,
    ipv6FromCBOR,
    -- Raw
    listLenInt,
    runByteBuilder,
    -- UTC Time
    utcTimeToCBOR,
    utcTimeFromCBOR,
    -- This abstraction can/should be moved into cardano-binary
    Sized (..),
    mkSized,
    sizedDecoder,
    toSizedL,
  )
where

import Cardano.Binary
  ( Annotated (..),
    ByteSpan (..),
    Decoder,
    DecoderError (..),
    Encoding,
    FromCBOR (..),
    Size,
    ToCBOR (..),
    annotatedDecoder,
    decodeAnnotator,
    decodeListLenOrIndef,
    decodeTag,
    encodeListLen,
    encodeTag,
    serialize,
    withWordSize,
  )
import Control.DeepSeq
import Control.Monad (unless, when)
import Control.Monad.Except (Except, MonadError (throwError))
import Data.Binary.Get (Get, getWord32le, runGetOrFail)
import Data.Binary.Put (putWord32le, runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Coders
  ( Annotator,
    cborError,
    decodeCollectionWithLen,
    decodeList,
    decodeMap,
    decodeMapContents,
    decodeMapTraverse,
    decodeNullMaybe,
    decodeRecordNamed,
    decodeRecordNamedT,
    decodeRecordSum,
    decodeSeq,
    decodeSet,
    decodeStrictSeq,
    encodeFoldable,
    encodeFoldableEncoder,
    encodeMap,
    encodeNullMaybe,
    wrapCBORArray,
    wrapCBORMap,
  )
import Data.Foldable (foldl')
import Data.IP
  ( IPv4,
    IPv6,
    fromHostAddress,
    fromHostAddress6,
    toHostAddress,
    toHostAddress6,
  )
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Ratio (Ratio, denominator, numerator, (%))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime (..))
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate, toOrdinalDate)
import Data.Time.Clock (diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Typeable
import GHC.Generics
import Lens.Micro
import Network.Socket (HostAddress6)
import NoThunks.Class (NoThunks)
import Prelude

class Typeable a => ToCBORGroup a where
  toCBORGroup :: a -> Encoding
  encodedGroupSizeExpr ::
    (forall x. ToCBOR x => Proxy x -> Size) ->
    Proxy a ->
    Size

  listLen :: a -> Word

  -- | an upper bound for 'listLen', used in 'Size' expressions.
  listLenBound :: Proxy a -> Word

listLenInt :: ToCBORGroup a => a -> Int
listLenInt x = fromIntegral (listLen x)

newtype CBORGroup a = CBORGroup {unCBORGroup :: a}

instance ToCBORGroup a => ToCBOR (CBORGroup a) where
  toCBOR (CBORGroup x) = encodeListLen (listLen x) <> toCBORGroup x
  encodedSizeExpr size proxy =
    fromInteger (withWordSize (listLenBound proxy'))
      + encodedGroupSizeExpr size proxy'
    where
      proxy' = unCBORGroup <$> proxy

class Typeable a => FromCBORGroup a where
  fromCBORGroup :: Decoder s a

instance (FromCBORGroup a, ToCBORGroup a) => FromCBOR (CBORGroup a) where
  fromCBOR = CBORGroup <$> groupRecord

groupRecord :: forall a s. (ToCBORGroup a, FromCBORGroup a) => Decoder s a
groupRecord = decodeRecordNamed "CBORGroup" (fromIntegral . toInteger . listLen) fromCBORGroup

mapToCBOR :: (ToCBOR a, ToCBOR b) => Map a b -> Encoding
mapToCBOR = encodeMap toCBOR toCBOR
{-# DEPRECATED mapToCBOR "Use `toCBOR` instead" #-}

mapFromCBOR :: (Ord a, FromCBOR a, FromCBOR b) => Decoder s (Map a b)
mapFromCBOR = decodeMap fromCBOR fromCBOR
{-# DEPRECATED mapFromCBOR "Use `fromCBOR` instead" #-}

newtype CborSeq a = CborSeq {unwrapCborSeq :: Seq a}
  deriving (Foldable)

instance ToCBOR a => ToCBOR (CborSeq a) where
  toCBOR (CborSeq xs) =
    let l = fromIntegral $ Seq.length xs
        contents = foldMap toCBOR xs
     in wrapCBORArray l contents

instance FromCBOR a => FromCBOR (CborSeq a) where
  fromCBOR = CborSeq <$> decodeSeq fromCBOR


-- TODO: This is only used for TxSeq. Create a specialized `StrictSeq` as Map
-- encoder. length is O(1) for StrictSeq, no need to recompute it here.
encodeFoldableMapEncoder ::
  Foldable f =>
  (Word -> a -> Maybe Encoding) ->
  f a ->
  Encoding
encodeFoldableMapEncoder encode xs = wrapCBORMap len contents
  where
    (len, _, contents) = foldl' go (0, 0, mempty) xs
    go (!l, !i, !enc) next = case encode i next of
      Nothing -> (l, i + 1, enc)
      Just e -> (l + 1, i + 1, enc <> e)

ratioToCBOR :: ToCBOR a => Ratio a -> Encoding
ratioToCBOR r =
  encodeTag 30
    <> encodeListLen 2
    <> toCBOR (numerator r)
    <> toCBOR (denominator r)
{-# DEPRECATED ratioToCBOR "Use `ToCBOR` instead" #-}

ratioFromCBOR :: (Bounded a, Integral a, FromCBOR a) => Decoder s (Ratio a)
ratioFromCBOR = decodeFraction fromCBOR
{-# DEPRECATED ratioFromCBOR "Use `FromCBOR` instead" #-}

ipv4FromBytes :: BS.ByteString -> Either String IPv4
ipv4FromBytes b =
  case runGetOrFail getWord32le (BSL.fromStrict b) of
    Left (_, _, err) -> Left err
    Right (_, _, ha) -> Right $ fromHostAddress ha

ipv4ToCBOR :: IPv4 -> Encoding
ipv4ToCBOR = toCBOR . ipv4ToBytes
{-# DEPRECATED ipv4ToCBOR "Use `Cardano.Ledger.Binary.Encoding.Encoder.encodeIPv4` instead" #-}

byteDecoderToDecoder :: Text -> (BS.ByteString -> Either String a) -> Decoder s a
byteDecoderToDecoder name fromBytes = do
  b <- fromCBOR
  case fromBytes b of
    Left err -> cborError $ DecoderErrorCustom name (Text.pack err)
    Right ip -> pure ip

ipv4FromCBOR :: Decoder s IPv4
ipv4FromCBOR = byteDecoderToDecoder "IPv4" ipv4FromBytes
{-# DEPRECATED ipv4FromCBOR "Use `Cardano.Ledger.Binary.Decoding.Decoder.decodeIPv4` instead" #-}

getHostAddress6 :: Get HostAddress6
getHostAddress6 = do
  w1 <- getWord32le
  w2 <- getWord32le
  w3 <- getWord32le
  w4 <- getWord32le
  return (w1, w2, w3, w4)

ipv6FromBytes :: BS.ByteString -> Either String IPv6
ipv6FromBytes b =
  case runGetOrFail getHostAddress6 (BSL.fromStrict b) of
    Left (_, _, err) -> Left err
    Right (_, _, ha) -> Right $ fromHostAddress6 ha

ipv6ToCBOR :: IPv6 -> Encoding
ipv6ToCBOR = toCBOR . ipv6ToBytes
{-# DEPRECATED ipv6ToCBOR "Use `Cardano.Ledger.Binary.Encoding.encodeIPv6` instead" #-}

ipv6FromCBOR :: Decoder s IPv6
ipv6FromCBOR = byteDecoderToDecoder "IPv6" ipv6FromBytes
{-# DEPRECATED ipv6FromCBOR "Use `Cardano.Ledger.Binary.Decoding.decodeIPv6` instead" #-}

--
-- Raw serialisation
--

utcTimeToCBOR :: UTCTime -> Encoding
utcTimeToCBOR t =
  encodeListLen 3
    <> toCBOR year
    <> toCBOR dayOfYear
    <> (toCBOR . diffTimeToPicoseconds . utctDayTime) t
  where
    (year, dayOfYear) = toOrdinalDate . utctDay $ t
{-# DEPRECATED ipv6FromCBOR "Use `Cardano.Ledger.Binary.Encoding.encodeUTCTime` instead" #-}

utcTimeFromCBOR :: Decoder s UTCTime
utcTimeFromCBOR = do
  decodeRecordNamed "UTCTime" (const 3) $ do
    year <- fromCBOR
    dayOfYear <- fromCBOR
    diff <- fromCBOR
    pure $
      UTCTime
        (fromOrdinalDate year dayOfYear)
        (picosecondsToDiffTime diff)
{-# DEPRECATED
utcTimeFromCBOR "Use `Cardano.Ledger.Binary.Decoding.decodeUTCTime` with version 2 at least"
 #-}

translateViaCBORAnn :: (ToCBOR a, FromCBOR (Annotator b)) => Text -> a -> Except DecoderError b
translateViaCBORAnn name x =
  case decodeAnnotator name fromCBOR (serialize x) of
    Right newx -> pure newx
    Left decoderError -> throwError decoderError
{-# DEPRECATED
translateViaCBORAnn "Use `Cardano.Ledger.Binary.translateViaCBORAnnotator`"
 #-}
