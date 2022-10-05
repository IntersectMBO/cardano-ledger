{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Binary.Decoding.Annotated
  ( C.Annotated (..),
    C.ByteSpan (..),
    C.Decoded (..),
    C.annotationBytes,
    annotatedDecoder,
    C.slice,
    fromCBORAnnotated,
    reAnnotate,
    C.Annotator (..),
    annotatorSlice,
    withSlice,
    C.FullByteString (..),
    C.serializeEncoding,
    encodePreEncoded,
    decodeAnnSet,
  )
where

import qualified Data.Set as Set
import qualified Cardano.Binary as C
import Cardano.Ledger.Binary.Decoding.Decoder (Decoder, withPlainDecoder, decodeList)
import Cardano.Ledger.Binary.Decoding.FromCBOR (FromCBOR (..))
import Codec.CBOR.Encoding (encodePreEncoded)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Functor ((<&>))

-- | A decoder for a value paired with an annotation specifying the start and end
-- of the consumed bytes.
annotatedDecoder :: Decoder s a -> Decoder s (C.Annotated a C.ByteSpan)
annotatedDecoder vd =
  withPlainDecoder
    vd
    ( \d ->
        C.decodeWithByteSpan d <&> \(x, start, end) ->
          C.Annotated x (C.ByteSpan start end)
    )

-- | A decoder for a value paired with an annotation specifying the start and end
-- of the consumed bytes.
fromCBORAnnotated :: FromCBOR a => Decoder s (C.Annotated a C.ByteSpan)
fromCBORAnnotated = annotatedDecoder fromCBOR

-- | Reconstruct an annotation by re-serialising the payload to a ByteString.
reAnnotate :: C.ToCBOR a => C.Annotated a b -> C.Annotated a BS.ByteString
reAnnotate (C.Annotated x _) = C.Annotated x (C.serialize' x)

-------------------------------------------------------------------------
-- Annotator
-------------------------------------------------------------------------

-- | The argument is a decoder for a annotator that needs access to the bytes that
-- | were decoded. This function constructs and supplies the relevant piece.
annotatorSlice ::
  Decoder s (C.Annotator (BSL.ByteString -> a)) ->
  Decoder s (C.Annotator a)
annotatorSlice dec = do
  (k, bytes) <- withSlice dec
  pure $ k <*> bytes

-- | Pairs the decoder result with an annotator that can be used to construct the exact
-- bytes used to decode the result.
withSlice :: Decoder s a -> Decoder s (a, C.Annotator BSL.ByteString)
withSlice dec = do
  C.Annotated r byteSpan <- annotatedDecoder dec
  return (r, C.Annotator (\(C.Full bsl) -> C.slice bsl byteSpan))


-- TODO: Implement version that matches the length of a list with the size of the Set in
-- order to enforce no duplicates.
decodeAnnSet :: Ord t => Decoder s (C.Annotator t) -> Decoder s (C.Annotator (Set.Set t))
decodeAnnSet dec = do
  xs <- decodeList dec
  pure (Set.fromList <$> sequence xs)
