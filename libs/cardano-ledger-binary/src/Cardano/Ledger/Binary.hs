{-# LANGUAGE FlexibleContexts #-}

module Cardano.Ledger.Binary
  ( module Cardano.Ledger.Binary.Decoding,
    module Cardano.Ledger.Binary.Encoding,
    module Cardano.Ledger.Binary.Group,
    module Cardano.Ledger.Binary.Version,
    Term (..),
    C.DeserialiseFailure (..),
    translateViaCBORAnnotator,
  )
where

import Cardano.Ledger.Binary.Decoding
import Cardano.Ledger.Binary.Encoding
import Cardano.Ledger.Binary.Group
import Cardano.Ledger.Binary.Version
import qualified Codec.CBOR.Read as C (DeserialiseFailure (..))
import Codec.CBOR.Term (Term (..))
import Control.Monad.Except (Except, MonadError (throwError))
import Data.Text (Text)

-- | Translation function between values through a related binary representation. This
-- function allows you to translate one type into another (or the same one) through their
-- common binary format. It is possible for the source type to be encoded with a different
-- version than the version that will be used for decoding.. This is useful for types that
-- build upon one another and are "upgradeable". It is important to note that the
-- deserialization will happen with `Annotator`, since that is usually the way we
-- deserialize upgradeable types that live on chain.
translateViaCBORAnnotator ::
  (ToCBOR a, FromCBOR (Annotator b)) =>
  -- | Version that will be used for serialization
  Version ->
  -- | Version that will be used for deserialization
  Version ->
  Text ->
  a ->
  Except DecoderError b
translateViaCBORAnnotator versionSerialize versionDeserialize name x =
  case decodeFullAnnotator versionDeserialize name fromCBOR (serialize versionSerialize x) of
    Right newx -> pure newx
    Left decoderError -> throwError decoderError
