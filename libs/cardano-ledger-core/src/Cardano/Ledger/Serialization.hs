{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.Ledger.Serialization
  {-# DEPRECATED "Use `Cardano.Ledger.Binary` from 'cardano-ledger-binary' package instead" #-} (
  EncCBORGroup (..),
  DecCBORGroup (..),
  CBORGroup (..),
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
  ratioEncCBOR,
  ratioDecCBOR,
  mapEncCBOR,
  mapDecCBOR,
  translateViaCBORAnnotator,
  -- IPv4
  ipv4ToBytes,
  ipv4FromBytes,
  ipv4EncCBOR,
  ipv4DecCBOR,
  -- IPv6
  ipv6ToBytes,
  ipv6FromBytes,
  ipv6EncCBOR,
  ipv6DecCBOR,
  -- Raw
  listLenInt,
  runByteBuilder,
  -- UTC Time
  utcTimeEncCBOR,
  utcTimeDecCBOR,
  -- This abstraction can/should be moved into cardano-binary
  Sized (..),
  mkSized,
  sizedDecoder,
  toSizedL,
)
where

import Cardano.Ledger.Binary (
  CBORGroup (..),
  DecCBOR (..),
  DecCBORGroup (..),
  Decoder,
  DecoderError (..),
  EncCBOR (..),
  EncCBORGroup (..),
  Encoding,
  Sized (..),
  assertTag,
  cborError,
  decodeIPv4,
  decodeIPv6,
  decodeList,
  decodeMap,
  decodeMapContents,
  decodeMapTraverse,
  decodeMaybe,
  decodeNullMaybe,
  decodeSeq,
  decodeSet,
  decodeStrictSeq,
  decodeUTCTime,
  encodeFoldableEncoder,
  encodeFoldableMapEncoder,
  encodeIPv4,
  encodeIPv6,
  encodeMap,
  encodeNullMaybe,
  encodeRatio,
  encodeUTCTime,
  enforceDecoderVersion,
  groupRecord,
  ipv4ToBytes,
  ipv6ToBytes,
  listLenInt,
  mkSized,
  natVersion,
  runByteBuilder,
  sizedDecoder,
  toSizedL,
  translateViaCBORAnnotator,
 )
import Cardano.Ledger.Binary.Coders (
  decodeRecordNamed,
  decodeRecordNamedT,
  decodeRecordSum,
 )
import Control.Monad (when)
import Data.Binary.Get (Get, getWord32le, runGetOrFail)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IP (
  IPv4,
  IPv6,
  fromHostAddress,
  fromHostAddress6,
 )
import Data.Map.Strict (Map)
import Data.Ratio (Ratio, (%))
import Data.Time (UTCTime (..))
import Network.Socket (HostAddress6)

mapEncCBOR :: (EncCBOR a, EncCBOR b) => Map a b -> Encoding
mapEncCBOR = encodeMap encCBOR encCBOR

mapDecCBOR :: (Ord a, DecCBOR a, DecCBOR b) => Decoder s (Map a b)
mapDecCBOR = decodeMap decCBOR decCBOR

ratioEncCBOR :: EncCBOR a => Ratio a -> Encoding
ratioEncCBOR = encodeRatio encCBOR

ratioDecCBOR :: (Integral a, DecCBOR a) => Decoder s (Ratio a)
ratioDecCBOR = decodeFraction decCBOR

decodeFraction :: Integral a => Decoder s a -> Decoder s (Ratio a)
decodeFraction decoder = do
  assertTag 30
  values <- decodeList decoder
  case values of
    [n, d] -> do
      when (d == 0) (fail "Denominator cannot be 0")
      pure $! n % d
    _ -> cborError $ DecoderErrorSizeMismatch "Rational" 2 (length values)

ipv4FromBytes :: BS.ByteString -> Either String IPv4
ipv4FromBytes b =
  case runGetOrFail getWord32le (BSL.fromStrict b) of
    Left (_, _, err) -> Left err
    Right (_, _, ha) -> Right $ fromHostAddress ha

ipv4EncCBOR :: IPv4 -> Encoding
ipv4EncCBOR = encodeIPv4

ipv4DecCBOR :: Decoder s IPv4
ipv4DecCBOR = enforceDecoderVersion (natVersion @2) decodeIPv4

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

ipv6EncCBOR :: IPv6 -> Encoding
ipv6EncCBOR = encodeIPv6

ipv6DecCBOR :: Decoder s IPv6
ipv6DecCBOR = enforceDecoderVersion (natVersion @2) decodeIPv6

utcTimeEncCBOR :: UTCTime -> Encoding
utcTimeEncCBOR = encodeUTCTime

utcTimeDecCBOR :: Decoder s UTCTime
utcTimeDecCBOR = decodeUTCTime

encodeFoldable :: (EncCBOR a, Foldable f) => f a -> Encoding
encodeFoldable = encodeFoldableEncoder encCBOR
