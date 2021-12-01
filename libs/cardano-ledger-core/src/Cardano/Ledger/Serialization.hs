{-# LANGUAGE BangPatterns #-}
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
    -- IPv4
    ipv4ToBytes,
    ipv4FromBytes,
    ipv4ToCBOR,
    ipv4FromCBOR,
    -- IPv6
    ipv6ToBytes,
    ipv6FromBytes,
    ipv6ToCBOR,
    ipv6FromCBOR,
    -- Raw
    listLenInt,
    runByteBuilder,
    -- UTC Time
    utcTimeToCBOR,
    utcTimeFromCBOR,
  )
where

import Cardano.Binary
  ( Decoder,
    DecoderError (..),
    Encoding,
    FromCBOR (..),
    Size,
    ToCBOR (..),
    decodeListLenOrIndef,
    decodeTag,
    encodeListLen,
    encodeTag,
    withWordSize,
  )
import Cardano.Prelude (cborError)
import Control.Monad (unless, when)
import Data.Binary.Get (Get, getWord32le, runGetOrFail)
import Data.Binary.Put (putWord32le, runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Coders
  ( decodeCollectionWithLen,
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
import Network.Socket (HostAddress6)
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

mapFromCBOR :: (Ord a, FromCBOR a, FromCBOR b) => Decoder s (Map a b)
mapFromCBOR = decodeMap fromCBOR fromCBOR

newtype CborSeq a = CborSeq {unwrapCborSeq :: Seq a}
  deriving (Foldable)

instance ToCBOR a => ToCBOR (CborSeq a) where
  toCBOR (CborSeq xs) =
    let l = fromIntegral $ Seq.length xs
        contents = foldMap toCBOR xs
     in wrapCBORArray l contents

instance FromCBOR a => FromCBOR (CborSeq a) where
  fromCBOR = CborSeq <$> decodeSeq fromCBOR

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

decodeMaybe :: Decoder s a -> Decoder s (Maybe a)
decodeMaybe d =
  decodeList d >>= \case
    [] -> pure Nothing
    [x] -> pure $ Just x
    _ ->
      cborError $
        DecoderErrorCustom
          "Maybe"
          "Expected an array of length 0 or 1"

ratioToCBOR :: ToCBOR a => Ratio a -> Encoding
ratioToCBOR r =
  encodeTag 30
    <> encodeListLen 2
    <> toCBOR (numerator r)
    <> toCBOR (denominator r)

ratioFromCBOR :: (Bounded a, Integral a, FromCBOR a) => Decoder s (Ratio a)
ratioFromCBOR = decodeFraction fromCBOR

decodeFraction :: Integral a => Decoder s a -> Decoder s (Ratio a)
decodeFraction decoder = do
  t <- decodeTag
  unless (t == 30) $ cborError $ DecoderErrorCustom "rational" "expected tag 30"
  (numValues, values) <- decodeCollectionWithLen decodeListLenOrIndef decoder
  case values of
    [n, d] -> do
      when (d == 0) (fail "denominator cannot be 0")
      pure $ n % d
    _ -> cborError $ DecoderErrorSizeMismatch "rational" 2 numValues

ipv4ToBytes :: IPv4 -> BS.ByteString
ipv4ToBytes = BSL.toStrict . runPut . putWord32le . toHostAddress

ipv4FromBytes :: BS.ByteString -> Either String IPv4
ipv4FromBytes b =
  case runGetOrFail getWord32le (BSL.fromStrict b) of
    Left (_, _, err) -> Left err
    Right (_, _, ha) -> Right $ fromHostAddress ha

ipv4ToCBOR :: IPv4 -> Encoding
ipv4ToCBOR = toCBOR . ipv4ToBytes

byteDecoderToDecoder :: Text -> (BS.ByteString -> Either String a) -> Decoder s a
byteDecoderToDecoder name fromBytes = do
  b <- fromCBOR
  case fromBytes b of
    Left err -> cborError $ DecoderErrorCustom name (Text.pack err)
    Right ip -> pure ip

ipv4FromCBOR :: Decoder s IPv4
ipv4FromCBOR = byteDecoderToDecoder "IPv4" ipv4FromBytes

ipv6ToBytes :: IPv6 -> BS.ByteString
ipv6ToBytes ipv6 = BSL.toStrict . runPut $ do
  let (w1, w2, w3, w4) = toHostAddress6 ipv6
  putWord32le w1
  putWord32le w2
  putWord32le w3
  putWord32le w4

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

ipv6FromCBOR :: Decoder s IPv6
ipv6FromCBOR = byteDecoderToDecoder "IPv6" ipv6FromBytes

--
-- Raw serialisation
--

-- | Run a ByteString 'BS.Builder' using a strategy aimed at making smaller
-- things efficiently.
--
-- It takes a size hint and produces a strict 'ByteString'. This will be fast
-- when the size hint is the same or slightly bigger than the true size.
runByteBuilder :: Int -> BS.Builder -> BS.ByteString
runByteBuilder !sizeHint =
  BSL.toStrict
    . BS.toLazyByteStringWith
      (BS.safeStrategy sizeHint (2 * sizeHint))
      mempty
{-# NOINLINE runByteBuilder #-}

utcTimeToCBOR :: UTCTime -> Encoding
utcTimeToCBOR t =
  encodeListLen 3
    <> toCBOR year
    <> toCBOR dayOfYear
    <> (toCBOR . diffTimeToPicoseconds . utctDayTime) t
  where
    (year, dayOfYear) = toOrdinalDate . utctDay $ t

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
