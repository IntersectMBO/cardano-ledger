{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

-- | (De)serialisation primitives built on top of the @Bi@ typeclass

module Cardano.Binary.Class.Primitive
  ( serialize
  , serializeWith
  , serialize'
  , serializeBuilder
  , serializeEncoding

       -- * Deserialize inside the Decoder monad
  , deserialize
  , deserialize'

       -- * Unsafe deserialization
  , unsafeDeserialize
  , unsafeDeserialize'
  , CBOR.Write.toStrictByteString
  , Raw(..)

       -- * Temporary functions
  , biSize

       -- * Backward-compatible functions
  , decodeFull
  , decodeFull'
  , decodeFullDecoder

       -- * CBOR in CBOR
  , encodeKnownCborDataItem
  , encodeUnknownCborDataItem
  , knownCborDataItemSizeExpr
  , unknownCborDataItemSizeExpr
  , decodeKnownCborDataItem
  , decodeUnknownCborDataItem

       -- * Cyclic redundancy check
  , encodeCrcProtected
  , encodedCrcProtectedSizeExpr
  , decodeCrcProtected
  )
where

import Cardano.Prelude

import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import qualified Codec.CBOR.Read as Read
import qualified Codec.CBOR.Write as CBOR.Write
import Control.Exception.Safe (impureThrow)
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import Data.Digest.CRC32 (CRC32(..))
import Data.Typeable (typeOf)
import Formatting (Format, sformat, shown)

import Cardano.Binary.Class.Core
  (Bi(..), DecoderError(..), Size, apMono, enforceSize, withWordSize)


-- | Serialize a Haskell value to an external binary representation.
--
--   The output is represented as a lazy 'BSL.ByteString' and is constructed
--   incrementally.
serialize :: Bi a => a -> BSL.ByteString
-- 1024 is the size of the first buffer, 4096 is the size of subsequent
-- buffers. Chosen because they seem to give good performance. They are not
-- sacred.
serialize = serializeWith 1024 4096

-- | Serialize a Haskell value to an external binary representation.
--
--   The output is represented as a strict 'BS.ByteString'.
serialize' :: Bi a => a -> BS.ByteString
serialize' = BSL.toStrict . serialize

-- | Serialize into a Builder. Useful if you want to throw other ByteStrings
--   around it.
serializeBuilder :: Bi a => a -> Builder
serializeBuilder = CBOR.Write.toBuilder . encode

-- | Serialize using the safe allocation strategy with a given first and
--   subsequent chunk size.
serializeWith :: Bi a => Int -> Int -> a -> BSL.ByteString
serializeWith firstChunk nextChunk =
  Builder.toLazyByteStringWith strategy mempty . serializeBuilder
  where strategy = Builder.safeStrategy firstChunk nextChunk

serializeEncoding :: E.Encoding -> BSL.ByteString
serializeEncoding =
  Builder.toLazyByteStringWith strategy mempty . CBOR.Write.toBuilder
  where strategy = Builder.safeStrategy 1024 4096

-- | Deserialize a Haskell value from the external binary representation
--   (which must have been made using 'serialize' or related function).
--
--   /Throws/: @'Read.DeserialiseFailure'@ if the given external
--   representation is invalid or does not correspond to a value of the
--   expected type.
unsafeDeserialize :: Bi a => BSL.ByteString -> a
unsafeDeserialize =
  either impureThrow identity . bimap fst fst . deserialiseDecoder decode

-- | Strict variant of 'deserialize'.
unsafeDeserialize' :: Bi a => BS.ByteString -> a
unsafeDeserialize' = unsafeDeserialize . BSL.fromStrict

-- | Run `decodeFull` in the `Decoder` monad, failing (using Decoder.fail) in
--   case the process failed or not the whole input was consumed.
--   We are not generalising this function to any monad as doing so would allow
--   us to cheat, as not all the monads have a sensible `fail` implementation.
--   Expect the whole input to be consumed.
deserialize :: Bi a => BSL.ByteString -> D.Decoder s a
deserialize = toCborError . decodeFull

-- | Strict version of `deserialize`.
deserialize' :: Bi a => BS.ByteString -> D.Decoder s a
deserialize' = deserialize . BSL.fromStrict

-- | Deserialize a Haskell value from the external binary representation,
--   failing if there are leftovers. In a nutshell, the `full` here implies
--   the contract of this function is that what you feed as input needs to
--   be consumed entirely.
decodeFull :: forall a . Bi a => BSL.ByteString -> Either DecoderError a
decodeFull = decodeFullDecoder (label $ Proxy @a) decode

decodeFull' :: forall a . Bi a => BS.ByteString -> Either DecoderError a
decodeFull' = decodeFull . BSL.fromStrict

decodeFullDecoder
  :: Text
  -- ^ Label for error reporting
  -> (forall s . D.Decoder s a)
  -- ^ The parser for the @ByteString@ to decode. It should decode the given
  -- @ByteString@ into a value of type @a@
  -> BSL.ByteString
  -- ^ The @ByteString@ to decode
  -> Either DecoderError a
decodeFullDecoder lbl decoder bs0 = case deserialiseDecoder decoder bs0 of
  Right (x, leftover) -> if BS.null leftover
    then pure x
    else Left $ DecoderErrorLeftover lbl leftover
  Left (e, _) -> Left $ DecoderErrorDeserialiseFailure lbl e

-- | Deserialise a 'BSL.ByteString' incrementally using the provided 'Decoder'
deserialiseDecoder
  :: (forall s . D.Decoder s a)
  -> BSL.ByteString
  -> Either (Read.DeserialiseFailure, BS.ByteString) (a, BS.ByteString)
deserialiseDecoder decoder bs0 =
  runST (supplyAllInput bs0 =<< Read.deserialiseIncremental decoder)

supplyAllInput
  :: BSL.ByteString
  -> Read.IDecode s a
  -> ST
       s
       (Either (Read.DeserialiseFailure, BS.ByteString) (a, BS.ByteString))
supplyAllInput bs' (Read.Done bs _ x) =
  return (Right (x, bs <> BSL.toStrict bs'))
supplyAllInput bs (Read.Partial k) = case bs of
  BSL.Chunk chunk bs' -> k (Just chunk) >>= supplyAllInput bs'
  BSL.Empty           -> k Nothing >>= supplyAllInput BSL.Empty
supplyAllInput _ (Read.Fail bs _ exn) = return (Left (exn, bs))


--------------------------------------------------------------------------------
-- Raw
--------------------------------------------------------------------------------

-- | A wrapper over 'ByteString' for signalling that a bytestring should be
--   processed as a sequence of bytes, not as a separate entity. It's used in
--   crypto and binary code.
newtype Raw =
  Raw ByteString
  deriving (Bi, Eq, Ord, Show, NFData)


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Compute size of something serializable in bytes.
biSize :: Bi a => a -> Natural
biSize = fromIntegral . BSL.length . serialize
{-# INLINE biSize #-}


--------------------------------------------------------------------------------
-- CBORDataItem
-- https://tools.ietf.org/html/rfc7049#section-2.4.4.1
--------------------------------------------------------------------------------

-- | Encode and serialise the given `a` and sorrounds it with the semantic tag 24.
-- In CBOR diagnostic notation:
-- >>> 24(h'DEADBEEF')
encodeKnownCborDataItem :: Bi a => a -> E.Encoding
encodeKnownCborDataItem = encodeUnknownCborDataItem . serialize

-- | Like `encodeKnownCborDataItem`, but assumes nothing about the shape of
-- input object, so that it must be passed as a binary `ByteString` blob.
-- It's the caller responsibility to ensure the input `ByteString` correspond
-- indeed to valid, previously-serialised CBOR data.
encodeUnknownCborDataItem :: BSL.ByteString -> E.Encoding
encodeUnknownCborDataItem x = E.encodeTag 24 <> encode x

knownCborDataItemSizeExpr :: Size -> Size
knownCborDataItemSizeExpr x = 2 + apMono "withWordSize" withWordSize x + x

unknownCborDataItemSizeExpr :: Size -> Size
unknownCborDataItemSizeExpr x = 2 + apMono "withWordSize" withWordSize x + x

-- | Remove the the semantic tag 24 from the enclosed CBOR data item,
-- failing if the tag cannot be found.
decodeCborDataItemTag :: D.Decoder s ()
decodeCborDataItemTag = do
  t <- D.decodeTag
  when (t /= 24) $ cborError $ DecoderErrorUnknownTag
    "decodeCborDataItem"
    (fromIntegral t)

-- | Remove the the semantic tag 24 from the enclosed CBOR data item,
-- decoding back the inner `ByteString` as a proper Haskell type.
-- Consume its input in full.
decodeKnownCborDataItem :: Bi a => D.Decoder s a
decodeKnownCborDataItem = do
  bs <- decodeUnknownCborDataItem
  toCborError $ decodeFull' bs

-- | Like `decodeKnownCborDataItem`, but assumes nothing about the Haskell
-- type we want to deserialise back, therefore it yields the `ByteString`
-- Tag 24 surrounded (stripping such tag away).
-- In CBOR notation, if the data was serialised as:
-- >>> 24(h'DEADBEEF')
-- then `decodeUnknownCborDataItem` yields the inner 'DEADBEEF', unchanged.
decodeUnknownCborDataItem :: D.Decoder s ByteString
decodeUnknownCborDataItem = do
  decodeCborDataItemTag
  D.decodeBytes

-- | Encodes a type `a` , protecting it from tampering/network-transport-alteration by
-- protecting it with a CRC.
encodeCrcProtected :: Bi a => a -> E.Encoding
encodeCrcProtected x =
  E.encodeListLen 2 <> encodeUnknownCborDataItem body <> encode (crc32 body)
  where body = serialize x

encodedCrcProtectedSizeExpr
  :: forall a . Bi a => (forall t . Bi t => Proxy t -> Size) -> Proxy a -> Size
encodedCrcProtectedSizeExpr size pxy =
  2 + unknownCborDataItemSizeExpr (size pxy) + size
    (pure $ crc32 (serialize (panic "unused" :: a)))

-- | Decodes a CBOR blob into a type `a`, checking the serialised CRC corresponds to the computed one.
decodeCrcProtected :: forall s a . Bi a => D.Decoder s a
decodeCrcProtected = do
  enforceSize ("decodeCrcProtected: " <> show (typeOf (Proxy @a))) 2
  body        <- decodeUnknownCborDataItem
  expectedCrc <- decode
  let
    actualCrc :: Word32
    actualCrc = crc32 body
  let
    crcErrorFmt :: Format r (Word32 -> Word32 -> r)
    crcErrorFmt =
      "decodeCrcProtected, expected CRC "
        . shown
        . " was not the computed one, which was "
        . shown
  when (actualCrc /= expectedCrc)
    $ cborError (sformat crcErrorFmt expectedCrc actualCrc)
  toCborError $ decodeFull' body
