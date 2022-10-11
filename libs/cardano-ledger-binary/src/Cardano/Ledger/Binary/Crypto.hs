{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Binary.Crypto
  ( encodeVerKeyDSIGN,
    decodeVerKeyDSIGN,
    encodeSignKeyDSIGN,
    decodeSignKeyDSIGN,
    encodeSigDSIGN,
    decodeSigDSIGN,
    encodeSignedDSIGN,
    decodeSignedDSIGN,
  )
where

import qualified Cardano.Crypto.DSIGN.Class as C
import Cardano.Ledger.Binary.Decoding.Decoder (Decoder, fromPlainDecoder)
import Cardano.Ledger.Binary.Encoding.Encoder (Encoding, fromPlainEncoding)

encodeVerKeyDSIGN :: C.DSIGNAlgorithm v => C.VerKeyDSIGN v -> Encoding
encodeVerKeyDSIGN = fromPlainEncoding . C.encodeVerKeyDSIGN

decodeVerKeyDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.VerKeyDSIGN v)
decodeVerKeyDSIGN = fromPlainDecoder C.decodeVerKeyDSIGN

encodeSignKeyDSIGN :: C.DSIGNAlgorithm v => C.SignKeyDSIGN v -> Encoding
encodeSignKeyDSIGN = fromPlainEncoding . C.encodeSignKeyDSIGN

decodeSignKeyDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SignKeyDSIGN v)
decodeSignKeyDSIGN = fromPlainDecoder C.decodeSignKeyDSIGN

encodeSigDSIGN :: C.DSIGNAlgorithm v => C.SigDSIGN v -> Encoding
encodeSigDSIGN = fromPlainEncoding . C.encodeSigDSIGN

decodeSigDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SigDSIGN v)
decodeSigDSIGN = fromPlainDecoder C.decodeSigDSIGN

encodeSignedDSIGN :: C.DSIGNAlgorithm v => C.SignedDSIGN v a -> Encoding
encodeSignedDSIGN = fromPlainEncoding . C.encodeSignedDSIGN

decodeSignedDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SignedDSIGN v a)
decodeSignedDSIGN = fromPlainDecoder C.decodeSignedDSIGN


