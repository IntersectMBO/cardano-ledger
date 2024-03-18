{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Binary.Decoding (
  -- * Running decoders
  decodeFull,
  decodeFull',
  decodeFullDecoder,
  decodeFullDecoder',
  decodeFullAnnotator,
  decodeFullAnnotatedBytes,
  decodeFullAnnotatorFromHexText,
  module Cardano.Ledger.Binary.Version,
  module Cardano.Ledger.Binary.Decoding.DecCBOR,
  module Cardano.Ledger.Binary.Decoding.Sharing,
  module Cardano.Ledger.Binary.Decoding.Decoder,
  module Cardano.Ledger.Binary.Decoding.Sized,
  module Cardano.Ledger.Binary.Decoding.Drop,

  -- * Annotated

  --
  -- $annotated
  module Cardano.Ledger.Binary.Decoding.Annotated,

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
  decCBORMaybe,
)
where

import Cardano.Ledger.Binary.Decoding.Annotated
import Cardano.Ledger.Binary.Decoding.DecCBOR
import Cardano.Ledger.Binary.Decoding.Decoder
import Cardano.Ledger.Binary.Decoding.Drop
import Cardano.Ledger.Binary.Decoding.Sharing
import Cardano.Ledger.Binary.Decoding.Sized
import Cardano.Ledger.Binary.Plain (withHexText)
import Cardano.Ledger.Binary.Version
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

-- | Deserialize a Haskell value from the external binary representation, which
-- have been made using 'serialize' or a matching serialization functionilty in
-- another language that uses CBOR format. Accepts lazy `BSL.ByteString` as
-- input, for strict variant use `unsafeDeserialize'` instead.
--
-- This deserializaer is not safe for these reasons:
--
-- *  /Throws/: @'Read.DeserialiseFailure'@ if the given external
--   representation is invalid or does not correspond to a value of the
--   expected type.
--
-- * Decoding will not fail when the binary input is not consumed in full.
unsafeDeserialize ::
  DecCBOR a =>
  Version ->
  BSL.ByteString ->
  a
unsafeDeserialize version =
  either (error . displayException) id . bimap fst fst . deserialiseDecoder version decCBOR

-- | Variant of 'unsafeDeserialize' that accepts a strict `BS.ByteString` as
-- input.
unsafeDeserialize' :: DecCBOR a => Version -> BS.ByteString -> a
unsafeDeserialize' version = unsafeDeserialize version . BSL.fromStrict

-- | Deserialize a Haskell value from a binary CBOR representation, failing if
--   there are leftovers. In other words, the __Full__ here implies the contract
--   on this function that the input must be consumed in its entirety by the
--   decoder specified in `DecCBOR`.
decodeFull :: forall a. DecCBOR a => Version -> BSL.ByteString -> Either DecoderError a
decodeFull version = decodeFullDecoder version (label $ Proxy @a) decCBOR
{-# INLINE decodeFull #-}

-- | Same as `decodeFull`, except accepts a strict `BS.ByteString` as input
-- instead of the lazy one.
decodeFull' :: forall a. DecCBOR a => Version -> BS.ByteString -> Either DecoderError a
decodeFull' version = decodeFull version . BSL.fromStrict
{-# INLINE decodeFull' #-}

-- | Same as `decodeFull`, except instead of relying on the `DecCBOR` instance
-- the `Decoder` must be suplied manually.
decodeFullDecoder ::
  -- | Protocol version to be used during decoding.
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
{-# INLINE decodeFullDecoder #-}

-- | Same as `decodeFullDecoder`, except works on strict `BS.ByteString`
decodeFullDecoder' ::
  Version ->
  Text ->
  (forall s. Decoder s a) ->
  BS.ByteString ->
  Either DecoderError a
decodeFullDecoder' version lbl decoder = decodeFullDecoder version lbl decoder . BSL.fromStrict
{-# INLINE decodeFullDecoder' #-}

-- | Deserialise a Haskell type from a 'BSL.ByteString' input, which be consumed
-- incrementally using the provided 'Decoder'. Unlike `decodeFullDecoder` there
-- is no requiremenet to consume all input, therefore left over binary input is
-- returned upon sucessfull or failed deserialization.
deserialiseDecoder ::
  Version ->
  (forall s. Decoder s a) ->
  BSL.ByteString ->
  Either (Read.DeserialiseFailure, BS.ByteString) (a, BS.ByteString)
deserialiseDecoder version decoder bs0 =
  runST (supplyAllInput bs0 =<< Read.deserialiseIncremental (toPlainDecoder version decoder))
{-# INLINE deserialiseDecoder #-}

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
{-# INLINE supplyAllInput #-}

--------------------------------------------------------------------------------
-- Annotator
--------------------------------------------------------------------------------

-- | Same as `decodeFullDecoder`, except it provdes the means of passing portion or all
-- of the `BSL.ByteString` input argument to the decoding `Annotator`.
decodeFullAnnotator ::
  Version ->
  Text ->
  (forall s. Decoder s (Annotator a)) ->
  BSL.ByteString ->
  Either DecoderError a
decodeFullAnnotator v lbl decoder bytes =
  (\x -> runAnnotator x (Full bytes)) <$> decodeFullDecoder v lbl decoder bytes
{-# INLINE decodeFullAnnotator #-}

-- | Same as `decodeFullDecoder`, decodes a Haskell value from a lazy
-- `BSL.ByteString`, requiring that the full ByteString is consumed, and
-- replaces `ByteSpan` annotations with the corresponding slice of the input as
-- a strict `BS.ByteString`.
decodeFullAnnotatedBytes ::
  Functor f =>
  Version ->
  Text ->
  (forall s. Decoder s (f ByteSpan)) ->
  BSL.ByteString ->
  Either DecoderError (f BS.ByteString)
decodeFullAnnotatedBytes v lbl decoder bytes =
  annotationBytes bytes <$> decodeFullDecoder v lbl decoder bytes
{-# INLINE decodeFullAnnotatedBytes #-}

decodeFullAnnotatorFromHexText ::
  Version ->
  Text ->
  (forall s. Decoder s (Annotator a)) ->
  Text ->
  Either DecoderError a
decodeFullAnnotatorFromHexText v desc dec =
  withHexText $ decodeFullAnnotator v desc dec . BSL.fromStrict
{-# INLINE decodeFullAnnotatorFromHexText #-}

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
{-# INLINE decodeNestedCborTag #-}

-- | Remove the the semantic tag 24 from the enclosed CBOR data item,
-- decoding back the inner `ByteString` as a proper Haskell type.
-- Consume its input in full.
decodeNestedCbor :: DecCBOR a => Decoder s a
decodeNestedCbor = do
  bs <- decodeNestedCborBytes
  version <- getDecoderVersion
  either cborError pure $ decodeFull' version bs
{-# INLINE decodeNestedCbor #-}

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
decodeNestedCborBytes = decodeNestedCborTag >> decodeBytes
{-# INLINE decodeNestedCborBytes #-}

decodeAnnotator ::
  Version ->
  Text ->
  (forall s. Decoder s (Annotator a)) ->
  BSL.ByteString ->
  Either DecoderError a
decodeAnnotator = decodeFullAnnotator
{-# DEPRECATED decodeAnnotator "In favor of `decodeFullAnnotator`" #-}

decCBORMaybe :: Decoder s a -> Decoder s (Maybe a)
decCBORMaybe = decodeMaybe
{-# DEPRECATED decCBORMaybe "In favor of `decodeMaybe`" #-}

-- $annotated
--
-- The regular CBOR 'Decoder' does not support access to the original 'BSL.ByteString' that is
-- being read during deserialization. The 'Annotator' and 'Annotated' recover this ability.
--
-- 1. 'ByteSpan'  A pair of indexes into a bytestring, indicating a substring.
-- 2. 'Annotated'  Used in practice to pair a value with a 'ByteSpan'. Mostly used in Byron codebase.
-- 3. 'FullByteString' A newtype (around a bytestring) used to store the original bytestring being deserialized.
-- 4. 'Annotator' An explict reader monad whose environment is a 'FullByteString'
--
-- The basic idea is, for a given type @t@, where we need the original 'BSL.ByteString', either
--
-- 1. To complete the deserialization, or
-- 2. To combine the deserialized answer with the original 'BSL.ByteString'.
--
-- We should proceed as follows: Define instances @('DecCBOR' ('Annotator' t))@ instead
-- of @('DecCBOR' t)@. When making this instance we may freely use that both 'Decoder'
-- and 'Annotator' are both monads, and that functions 'withSlice' and 'annotatorSlice'
-- provide access to the original bytes, or portions thereof, inside of decoders.  Then,
-- to actually decode a value of type @t@, we use something similar to the following code
-- fragment.
--
-- @
-- howToUseFullBytes bytes = do
--   Annotator f <- decodeFullDecoder \"DecodingAnnotator\" (decCBOR :: forall s. Decoder s (Annotator t)) bytes
--   pure (f (Full bytes))
-- @
--
-- Decode the bytes to get an @('Annotator' f)@ where f is a function that when given
-- original bytes produces a value of type @t@, then apply @f@ to @('Full' bytes)@ to get
-- the answer.
