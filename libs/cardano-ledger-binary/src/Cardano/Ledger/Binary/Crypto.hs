{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Binary.Crypto (
  encFixed,
  decFixed,
  decodeSignedDSIGN,
  encodeSignedDSIGN,
) where

import qualified Cardano.Crypto.DSIGN.Class as C
import Cardano.Crypto.FixedSizeBytes (FixedSizeBytes (..))
import Cardano.Ledger.Binary.Decoding.Decoder (
  Decoder,
  DecoderError (..),
  cborError,
  decodeBytes,
  fromPlainDecoder,
 )
import Cardano.Ledger.Binary.Encoding.Encoder (Encoding, fromPlainEncoding)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Typeable (typeRep)
import GHC.TypeLits (natVal)

encodeSignedDSIGN :: C.DSIGNAlgorithm v => C.SignedDSIGN v a -> Encoding
encodeSignedDSIGN = fromPlainEncoding . C.encodeSignedDSIGN
{-# INLINE encodeSignedDSIGN #-}

decodeSignedDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SignedDSIGN v a)
decodeSignedDSIGN = fromPlainDecoder C.decodeSignedDSIGN
{-# INLINE decodeSignedDSIGN #-}

encFixed :: FixedSizeBytes a => a -> Encoding
encFixed = fromPlainEncoding . encodeFixed

decFixed :: forall a s. FixedSizeBytes a => Decoder s a
decFixed = do
  let p = Proxy @a
  bs <- decodeBytes
  case rawDecodeFixed bs of
    Just vk -> pure vk
    Nothing ->
      cborError $
        DecoderErrorSizeMismatch
          (T.pack . show $ typeRep p)
          (fromIntegral . natVal $ Proxy @(FixedSize a))
          (BS.length bs)
