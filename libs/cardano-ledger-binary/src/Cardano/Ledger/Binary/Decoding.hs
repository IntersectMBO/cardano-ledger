{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Binary.Decoding
  ( -- * Running decoders
    decodeFull,
    decodeFull',
    decodeFullDecoder,
    decodeFullDecoder',
    decodeFullAnnotator,
    decodeFullAnnotatedBytes,
    module Cardano.Ledger.Binary.Decoding.FromCBOR,
    module Cardano.Ledger.Binary.Decoding.Sharing,
    module Cardano.Ledger.Binary.Decoding.Decoder,
    module Cardano.Ledger.Binary.Decoding.Annotated,
    module Cardano.Ledger.Binary.Decoding.Sized,
    module Cardano.Ledger.Binary.Decoding.Drop,

    -- * Nested CBOR in CBOR
    decodeNestedCbor,
    decodeNestedCborBytes,

    -- * Unsafe deserialization
    unsafeDeserialize,
    unsafeDeserialize',

    -- * Helpers
    toStrictByteString,

    -- * Deprecated
    decodeAnnotator,
    fromCBORMaybe,
  )
where

import Cardano.Ledger.Binary.Decoding.Annotated
import Cardano.Ledger.Binary.Decoding.Decoder
import Cardano.Ledger.Binary.Decoding.Drop
import Cardano.Ledger.Binary.Decoding.FromCBOR
import Cardano.Ledger.Binary.Decoding.Sharing
import Cardano.Ledger.Binary.Decoding.Sized
import Codec.CBOR.Read as Read (DeserialiseFailure, IDecode (..), deserialiseIncremental)
import Codec.CBOR.Write (toStrictByteString)
import Control.Exception (displayException)
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)

-- | Deserialize a Haskell value from the external binary representation
--   (which must have been made using 'serialize' or related function).
--
--   /Throws/: @'Read.DeserialiseFailure'@ if the given external
--   representation is invalid or does not correspond to a value of the
--   expected type.
unsafeDeserialize :: FromCBOR a => Version -> BSL.ByteString -> a
unsafeDeserialize version =
  either (error . displayException) id . bimap fst fst . deserialiseDecoder version fromCBOR

-- | Strict variant of 'deserialize'.
unsafeDeserialize' :: FromCBOR a => Version -> BS.ByteString -> a
unsafeDeserialize' version = unsafeDeserialize version . BSL.fromStrict

-- | Deserialize a Haskell value from the external binary representation,
--   failing if there are leftovers. In a nutshell, the `full` here implies
--   the contract of this function is that what you feed as input needs to
--   be consumed entirely.
decodeFull :: forall a. FromCBOR a => Version -> BSL.ByteString -> Either DecoderError a
decodeFull version = decodeFullDecoder version (label $ Proxy @a) fromCBOR

decodeFull' :: forall a. FromCBOR a => Version -> BS.ByteString -> Either DecoderError a
decodeFull' version = decodeFull version . BSL.fromStrict

decodeFullDecoder ::
  Version ->
  -- | Label for error reporting
  Text ->
  -- | The parser for the @ByteString@ to decode. It should decode the given
  -- @ByteString@ into a value of type @a@
  (forall s. Decoder s a) ->
  -- | The @ByteString@ to decode
  BSL.ByteString ->
  Either DecoderError a
decodeFullDecoder version lbl decoder bs =
  case deserialiseDecoder version decoder bs of
    Right (x, leftover) ->
      if BS.null leftover
        then pure x
        else Left $ DecoderErrorLeftover lbl leftover
    Left (e, _) -> Left $ DecoderErrorDeserialiseFailure lbl e

-- | Same as `decodeFullDecoder`, except works on strict `BS.ByteString`
decodeFullDecoder' ::
  Version ->
  Text ->
  (forall s. Decoder s a) ->
  BS.ByteString ->
  Either DecoderError a
decodeFullDecoder' version lbl decoder = decodeFullDecoder version lbl decoder . BSL.fromStrict

-- | Deserialise a 'LByteString' incrementally using the provided 'Decoder'
deserialiseDecoder ::
  Version ->
  (forall s. Decoder s a) ->
  BSL.ByteString ->
  Either (Read.DeserialiseFailure, BS.ByteString) (a, BS.ByteString)
deserialiseDecoder version decoder bs0 =
  runST (supplyAllInput bs0 =<< Read.deserialiseIncremental (toPlainDecoder version decoder))

supplyAllInput ::
  BSL.ByteString ->
  Read.IDecode s a ->
  ST s (Either (Read.DeserialiseFailure, BS.ByteString) (a, BS.ByteString))
supplyAllInput bs' (Read.Done bs _ x) =
  return (Right (x, bs <> BSL.toStrict bs'))
supplyAllInput bs (Read.Partial k) = case bs of
  BSL.Chunk chunk bs' -> k (Just chunk) >>= supplyAllInput bs'
  BSL.Empty -> k Nothing >>= supplyAllInput BSL.Empty
supplyAllInput _ (Read.Fail bs _ exn) = return (Left (exn, bs))

--------------------------------------------------------------------------------
-- Annotator
--------------------------------------------------------------------------------

-- | Supplies the bytestring argument to both the decoder and the produced annotator.
decodeFullAnnotator ::
  Version ->
  Text ->
  (forall s. Decoder s (Annotator a)) ->
  BSL.ByteString ->
  Either DecoderError a
decodeFullAnnotator v lbl decoder bytes =
  (\x -> runAnnotator x (Full bytes)) <$> decodeFullDecoder v lbl decoder bytes

-- | Decodes a value from a ByteString, requiring that the full ByteString is consumed, and
-- replaces ByteSpan annotations with the corresponding substrings of the input string.
decodeFullAnnotatedBytes ::
  Functor f =>
  Version ->
  Text ->
  (forall s. Decoder s (f ByteSpan)) ->
  BSL.ByteString ->
  Either DecoderError (f BS.ByteString)
decodeFullAnnotatedBytes v lbl decoder bytes =
  annotationBytes bytes <$> decodeFullDecoder v lbl decoder bytes

--------------------------------------------------------------------------------
-- Nested CBOR-in-CBOR
-- https://tools.ietf.org/html/rfc7049#section-2.4.4.1
--------------------------------------------------------------------------------

-- | Remove the the semantic tag 24 from the enclosed CBOR data item,
-- failing if the tag cannot be found.
decodeNestedCborTag :: Decoder s ()
decodeNestedCborTag = do
  t <- decodeTag
  when (t /= 24) $
    cborError $
      DecoderErrorUnknownTag
        "decodeNestedCborTag"
        (fromIntegral t)

-- | Remove the the semantic tag 24 from the enclosed CBOR data item,
-- decoding back the inner `ByteString` as a proper Haskell type.
-- Consume its input in full.
decodeNestedCbor :: FromCBOR a => Decoder s a
decodeNestedCbor = do
  bs <- decodeNestedCborBytes
  version <- getDecoderVersion
  either cborError pure $ decodeFull' version bs

-- | Like `decodeKnownCborDataItem`, but assumes nothing about the Haskell
-- type we want to deserialise back, therefore it yields the `ByteString`
-- Tag 24 surrounded (stripping such tag away).
--
-- In CBOR notation, if the data was serialised as:
--
-- >>> 24(h'DEADBEEF')
--
-- then `decodeNestedCborBytes` yields the inner 'DEADBEEF', unchanged.
decodeNestedCborBytes :: Decoder s BS.ByteString
decodeNestedCborBytes = do
  decodeNestedCborTag
  decodeBytes

decodeAnnotator ::
  Version ->
  Text ->
  (forall s. Decoder s (Annotator a)) ->
  BSL.ByteString ->
  Either DecoderError a
decodeAnnotator = decodeFullAnnotator
{-# DEPRECATED decodeAnnotator "In favor of `decodeFullAnnotator`" #-}

fromCBORMaybe :: Decoder s a -> Decoder s (Maybe a)
fromCBORMaybe = decodeMaybe
{-# DEPRECATED fromCBORMaybe "In favor of `decodeMaybe`" #-}
