{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Binary.Encoding
  ( -- * Running decoders
    serialize,
    serialize',
    serializeBuilder,
    serializeEncoding,
    serializeEncoding',

    -- ** Hash
    hashWithEncoder,
    hashToCBOR,
    module Cardano.Ledger.Binary.Version,
    module Cardano.Ledger.Binary.Encoding.Encoder,
    module Cardano.Ledger.Binary.Encoding.ToCBOR,

    -- * Nested CBOR-in-CBOR
    encodeNestedCbor,
    encodeNestedCborBytes,
    nestedCborSizeExpr,
    nestedCborBytesSizeExpr,

    -- * Tools
    runByteBuilder,

    -- * Deprecated
    toCBORMaybe,
  )
where

import qualified Cardano.Crypto.Hash.Class as C
import Cardano.Ledger.Binary.Encoding.Encoder
import Cardano.Ledger.Binary.Encoding.ToCBOR
import Cardano.Ledger.Binary.Version
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder.Extra (safeStrategy, toLazyByteStringWith)
import qualified Data.ByteString.Lazy as BSL

-- | Serialize a Haskell value with a 'ToCBOR' instance to an external binary
--   representation.
--
--   The output is represented as a lazy 'LByteString' and is constructed
--   incrementally.
serialize :: ToCBOR a => Version -> a -> BSL.ByteString
serialize version = serializeEncoding version . toCBOR

-- | Serialize a Haskell value to an external binary representation.
--
--   The output is represented as a strict 'ByteString'.
serialize' :: ToCBOR a => Version -> a -> BS.ByteString
serialize' version = BSL.toStrict . serialize version

-- | Serialize into a Builder. Useful if you want to throw other ByteStrings
--   around it.
serializeBuilder :: ToCBOR a => Version -> a -> Builder
serializeBuilder version = toBuilder version . toCBOR

-- | Serialize a Haskell value to an external binary representation using the
--   provided CBOR 'Encoding'
--
--   The output is represented as an 'LByteString' and is constructed
--   incrementally.
serializeEncoding :: Version -> Encoding -> BSL.ByteString
serializeEncoding version =
  toLazyByteStringWith strategy mempty . toBuilder version
  where
    -- 1024 is the size of the first buffer, 4096 is the size of subsequent
    -- buffers. Chosen because they seem to give good performance. They are not
    -- sacred.
    strategy = safeStrategy 1024 4096

-- | A strict version of 'serializeEncoding'
serializeEncoding' :: Version -> Encoding -> BS.ByteString
serializeEncoding' version = BSL.toStrict . serializeEncoding version

--------------------------------------------------------------------------------
-- Hashing
--------------------------------------------------------------------------------

hashWithEncoder :: forall h a. C.HashAlgorithm h => Version -> (a -> Encoding) -> a -> C.Hash h a
hashWithEncoder version toEnc = C.hashWith (serializeEncoding' version . toEnc)

hashToCBOR :: forall h a. (C.HashAlgorithm h, ToCBOR a) => Version -> a -> C.Hash h a
hashToCBOR version = hashWithEncoder version toCBOR

--------------------------------------------------------------------------------
-- Nested CBOR-in-CBOR
-- https://tools.ietf.org/html/rfc7049#section-2.4.4.1
--------------------------------------------------------------------------------

-- | Encode and serialise the given `a` and sorround it with the semantic tag 24
--   In CBOR diagnostic notation:
--   >>> 24(h'DEADBEEF')
encodeNestedCbor :: ToCBOR a => a -> Encoding
encodeNestedCbor value =
  encodeTag 24
    <> withCurrentEncodingVersion (\version -> toCBOR (serialize version value))

-- | Like `encodeNestedCbor`, but assumes nothing about the shape of
--   input object, so that it must be passed as a binary `ByteString` blob. It's
--   the caller responsibility to ensure the input `ByteString` correspond
--   indeed to valid, previously-serialised CBOR data.
encodeNestedCborBytes :: BSL.ByteString -> Encoding
encodeNestedCborBytes x = encodeTag 24 <> toCBOR x

nestedCborSizeExpr :: Size -> Size
nestedCborSizeExpr x = 2 + apMono "withWordSize" withWordSize x + x

nestedCborBytesSizeExpr :: Size -> Size
nestedCborBytesSizeExpr x = 2 + apMono "withWordSize" withWordSize x + x

--------------------------------------------------------------------------------
-- Tools
--------------------------------------------------------------------------------

-- | Run a ByteString 'Builder' using a strategy aimed at making smaller
-- things efficiently.
--
-- It takes a size hint and produces a strict 'ByteString'. This will be fast
-- when the size hint is the same or slightly bigger than the true size.
runByteBuilder :: Int -> Builder -> BS.ByteString
runByteBuilder !sizeHint =
  BSL.toStrict . toLazyByteStringWith (safeStrategy sizeHint (2 * sizeHint)) mempty
{-# NOINLINE runByteBuilder #-}

--------------------------------------------------------------------------------
-- Deprecations
--------------------------------------------------------------------------------

toCBORMaybe :: (a -> Encoding) -> (Maybe a -> Encoding)
toCBORMaybe = encodeMaybe
{-# DEPRECATED toCBORMaybe "In favor of `encodeMaybe`" #-}
