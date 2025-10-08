{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | CBOR encoding utilities needed for the Byron transaction format
module Cardano.Chain.Common.CBOR (
  -- * CBOR in CBOR

  -- | These utilities are is used in the Byron-era chain encodings in cases
  -- where there are extensible parts of the encoding. In thse cases we have to
  -- be able to handle unknown extensions and thus decode values where we do
  -- not know the concrete type.
  --
  -- To solve this, the serialised representation uses nested CBOR-in-CBOR
  -- <https://tools.ietf.org/html/rfc7049#section-2.4.4.1>. The nesting means
  -- that the size is known without having to decode the body in those cases
  -- where we cannot decode the body.
  --
  -- The functions in this module handle the encoding and decoding for the
  -- cases of the known and unknown types.
  encodeKnownCborDataItem,
  encodeUnknownCborDataItem,
  knownCborDataItemSizeExpr,
  unknownCborDataItemSizeExpr,
  decodeKnownCborDataItem,
  decodeUnknownCborDataItem,

  -- * Cyclic redundancy check

  -- | The Byron era address format includes a CRC to help resist accidental
  -- corruption. These functions deal with encoding and decoding the format
  -- that is used.
  encodeCrcProtected,
  encodedCrcProtectedSizeExpr,
  decodeCrcProtected,
) where

import qualified Cardano.Binary as Plain (
  Encoding,
  encodeListLen,
  encodeNestedCborBytes,
  nestedCborBytesSizeExpr,
  nestedCborSizeExpr,
  serialize,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  Decoder,
  EncCBOR (..),
  Encoding,
  Size,
  ToCBOR (..),
  byronProtVer,
  cborError,
  decodeFull',
  decodeNestedCbor,
  decodeNestedCborBytes,
  encodeNestedCbor,
  enforceSize,
  toCborError,
 )
import Cardano.Prelude hiding (cborError, toCborError)
import Data.Digest.CRC32 (CRC32 (..))
import Formatting (Format, sformat, shown)

-- | This is an alias for 'encodeNestedCbor'.
--
-- This function is used to handle the case of a known type, but compatible
-- with the encoding used by 'encodeUnknownCborDataItem'.
encodeKnownCborDataItem :: EncCBOR a => a -> Encoding
encodeKnownCborDataItem = encodeNestedCbor

-- | This is an alias for 'Plain.encodeNestedCborBytes', so all its details apply.
--
-- This function is used to handle the case of an unknown type, so it takes an
-- opaque blob that is the representation of the value of the unknown type.
encodeUnknownCborDataItem :: LByteString -> Plain.Encoding
encodeUnknownCborDataItem = Plain.encodeNestedCborBytes

knownCborDataItemSizeExpr :: Size -> Size
knownCborDataItemSizeExpr = Plain.nestedCborSizeExpr

unknownCborDataItemSizeExpr :: Size -> Size
unknownCborDataItemSizeExpr = Plain.nestedCborBytesSizeExpr

-- | This is an alias for 'decodeNestedCbor'.
--
-- This function is used to handle the case of a known type, but compatible
-- with the encoding used by 'decodeUnknownCborDataItem'.
decodeKnownCborDataItem :: DecCBOR a => Decoder s a
decodeKnownCborDataItem = decodeNestedCbor

-- | This is an alias for 'decodeNestedCborBytes', so all its details apply.
--
-- This function is used to handle the case of an unknown type, so it returns
-- an opaque blob that is the representation of the value of the unknown type.
decodeUnknownCborDataItem :: Decoder s ByteString
decodeUnknownCborDataItem = decodeNestedCborBytes

--------------------------------------------------------------------------------
-- Cyclic redundancy check
--------------------------------------------------------------------------------

-- | Encodes a value of type @a@, protecting it from accidental corruption by
-- protecting it with a CRC.
encodeCrcProtected :: ToCBOR a => a -> Plain.Encoding
encodeCrcProtected x =
  Plain.encodeListLen 2 <> encodeUnknownCborDataItem body <> toCBOR (crc32 body)
  where
    body = Plain.serialize x

encodedCrcProtectedSizeExpr ::
  forall a.
  ToCBOR a =>
  (forall t. ToCBOR t => Proxy t -> Size) ->
  Proxy a ->
  Size
encodedCrcProtectedSizeExpr size pxy =
  2
    + unknownCborDataItemSizeExpr (size pxy)
    + size (pure $ crc32 (Plain.serialize (panic "unused" :: a)))

-- | Decodes a CBOR blob into a value of type @a@, checking the serialised CRC
--   corresponds to the computed one
decodeCrcProtected :: forall s a. DecCBOR a => Decoder s a
decodeCrcProtected = do
  enforceSize ("decodeCrcProtected: " <> show (typeOf (Proxy :: Proxy a))) 2
  body <- decodeUnknownCborDataItem
  expectedCrc <- decCBOR
  let actualCrc :: Word32
      actualCrc = crc32 body
  let crcErrorFmt :: Format r (Word32 -> Word32 -> r)
      crcErrorFmt =
        "decodeCrcProtected, expected CRC "
          . shown
          . " was not the computed one, which was "
          . shown
  when (actualCrc /= expectedCrc)
    $ cborError (sformat crcErrorFmt expectedCrc actualCrc)
  toCborError $ decodeFull' byronProtVer body
