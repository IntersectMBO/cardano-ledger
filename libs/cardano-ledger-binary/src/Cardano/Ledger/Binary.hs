{-# LANGUAGE FlexibleContexts #-}

module Cardano.Ledger.Binary
  ( module Cardano.Ledger.Binary.Decoding,
    module Cardano.Ledger.Binary.Encoding,
    module Cardano.Ledger.Binary.Version,
    Term (..),
    C.DeserialiseFailure (..),
    translateViaCBORAnnotator,
  )
where

import Cardano.Ledger.Binary.Decoding
import Cardano.Ledger.Binary.Encoding
import Cardano.Ledger.Binary.Version
import qualified Codec.CBOR.Read as C (DeserialiseFailure (..))
import Codec.CBOR.Term (Term (..))
import Control.Monad.Except (Except, MonadError (throwError))
import Data.Text (Text)

-- | Translate one type into another through their common binary format. This is
-- useful for types that build upon one another and are "upgradeable". It is
-- important to note that the deserialization will happen with `Annotator`,
-- since that is usually the way to deserialize upgradeable types that live on
-- chain.
translateViaCBORAnnotator ::
  (ToCBOR a, FromCBOR (Annotator b)) =>
  Version ->
  Text ->
  a ->
  Except DecoderError b
translateViaCBORAnnotator version name x =
  case decodeFullAnnotator version name fromCBOR (serialize version x) of
    Right newx -> pure newx
    Left decoderError -> throwError decoderError
