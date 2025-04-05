{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Binary.Annotator (
  decodeFullAnnotator,
  decodeFullAnnotatedBytes,
  decodeFullAnnotatorFromHexText,
  Annotated (..),
  decodeAnnotated,
  ByteSpan (..),
  Decoded (..),
  annotatedDecoder,
  slice,
  decCBORAnnotated,
  reAnnotate,
  Annotator (..),
  annotatorSlice,
  withSlice,
  FullByteString (..),
  decodeAnnSet,
  translateViaCBORAnnotator,
)
where

import Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Codec.Serialise as Serialise (decode)
import Control.Monad.Except (Except, MonadError (throwError))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified PlutusLedgerApi.V1 as PV1

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
  Plain.withHexText $ decodeFullAnnotator v desc dec . BSL.fromStrict
{-# INLINE decodeFullAnnotatorFromHexText #-}

-- | Translation function between values through a related binary representation. This
-- function allows you to translate one type into another (or the same one) through their
-- common binary format. It is possible for the source type to be encoded with a different
-- version than the version that will be used for decoding. This is useful for types that
-- build upon one another and are "upgradeable" through their binary representation. It is
-- important to note that the deserialization will happen with `Annotator`, since that is
-- usually the way we deserialize upgradeable types that live on chain. Moreover, encoding
-- does not require a version, because memoized types that were decoded with annotation
-- will have the bytes retained and thus will have the `ToCBOR` instance.
translateViaCBORAnnotator ::
  (ToCBOR a, DecCBOR (Annotator b)) =>
  -- | Version that will be used for deserialization
  Version ->
  Text ->
  a ->
  Except DecoderError b
translateViaCBORAnnotator versionDeserialize name x =
  case decodeFullAnnotator versionDeserialize name decCBOR (Plain.serialize x) of
    Right newx -> pure newx
    Left decoderError -> throwError decoderError

instance DecCBOR (Annotator PV1.Data) where
  decCBOR = pure <$> fromPlainDecoder Serialise.decode
  {-# INLINE decCBOR #-}
