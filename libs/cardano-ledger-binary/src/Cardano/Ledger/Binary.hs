{-# LANGUAGE FlexibleContexts #-}

module Cardano.Ledger.Binary
  ( module Cardano.Ledger.Binary.Decoding,
    module Cardano.Ledger.Binary.Encoding,
    Term (..),
    C.DeserialiseFailure (..),
    translateViaCBORAnnotator,
  )
where

import Cardano.Ledger.Binary.Decoding
import Cardano.Ledger.Binary.Encoding
import qualified Codec.CBOR.Read as C (DeserialiseFailure (..))
import Codec.CBOR.Term (Term (..))
import Control.Monad.Except (Except, MonadError (throwError))
import Data.Text (Text)

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
