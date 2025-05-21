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
) where

import Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import Control.Monad.Except (Except, MonadError (throwError))
import Data.Text (Text)

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
