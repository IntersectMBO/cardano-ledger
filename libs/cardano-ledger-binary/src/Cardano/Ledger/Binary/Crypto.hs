{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Binary.Crypto (
  -- * DSIGN
  encodeVerKeyDSIGN,
  decodeVerKeyDSIGN,
  encodeSignKeyDSIGN,
  decodeSignKeyDSIGN,
  encodeSigDSIGN,
  decodeSigDSIGN,
  encodeSignedDSIGN,
  decodeSignedDSIGN,

  -- * KES
  encodeVerKeyKES,
  decodeVerKeyKES,
  encodeSignKeyKES,
  decodeSignKeyKES,
  encodeSigKES,
  decodeSigKES,
  encodeSignedKES,
  decodeSignedKES,

  -- * VRF
  encodeVerKeyVRF,
  decodeVerKeyVRF,
  encodeSignKeyVRF,
  decodeSignKeyVRF,
  encodeCertVRF,
  decodeCertVRF,
)
where

import qualified Cardano.Crypto.DSIGN.Class as C
import qualified Cardano.Crypto.KES.Class as C
import qualified Cardano.Crypto.VRF.Class as C
import Cardano.Ledger.Binary.Decoding.Decoder (Decoder, fromPlainDecoder)
import Cardano.Ledger.Binary.Encoding.Encoder (Encoding, fromPlainEncoding)

--------------------------------------------------------------------------------
-- DSIGN
--------------------------------------------------------------------------------

encodeVerKeyDSIGN :: C.DSIGNAlgorithm v => C.VerKeyDSIGN v -> Encoding
encodeVerKeyDSIGN = fromPlainEncoding . C.encodeVerKeyDSIGN
{-# INLINE encodeVerKeyDSIGN #-}

decodeVerKeyDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.VerKeyDSIGN v)
decodeVerKeyDSIGN = fromPlainDecoder C.decodeVerKeyDSIGN
{-# INLINE decodeVerKeyDSIGN #-}

encodeSignKeyDSIGN :: C.DSIGNAlgorithm v => C.SignKeyDSIGN v -> Encoding
encodeSignKeyDSIGN = fromPlainEncoding . C.encodeSignKeyDSIGN
{-# INLINE encodeSignKeyDSIGN #-}

decodeSignKeyDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SignKeyDSIGN v)
decodeSignKeyDSIGN = fromPlainDecoder C.decodeSignKeyDSIGN
{-# INLINE decodeSignKeyDSIGN #-}

encodeSigDSIGN :: C.DSIGNAlgorithm v => C.SigDSIGN v -> Encoding
encodeSigDSIGN = fromPlainEncoding . C.encodeSigDSIGN
{-# INLINE encodeSigDSIGN #-}

decodeSigDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SigDSIGN v)
decodeSigDSIGN = fromPlainDecoder C.decodeSigDSIGN
{-# INLINE decodeSigDSIGN #-}

encodeSignedDSIGN :: C.DSIGNAlgorithm v => C.SignedDSIGN v a -> Encoding
encodeSignedDSIGN = fromPlainEncoding . C.encodeSignedDSIGN
{-# INLINE encodeSignedDSIGN #-}

decodeSignedDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SignedDSIGN v a)
decodeSignedDSIGN = fromPlainDecoder C.decodeSignedDSIGN
{-# INLINE decodeSignedDSIGN #-}

--------------------------------------------------------------------------------
-- KES
--------------------------------------------------------------------------------

encodeVerKeyKES :: C.KESAlgorithm v => C.VerKeyKES v -> Encoding
encodeVerKeyKES = fromPlainEncoding . C.encodeVerKeyKES
{-# INLINE encodeVerKeyKES #-}

decodeVerKeyKES :: C.KESAlgorithm v => Decoder s (C.VerKeyKES v)
decodeVerKeyKES = fromPlainDecoder C.decodeVerKeyKES
{-# INLINE decodeVerKeyKES #-}

encodeSignKeyKES :: C.KESAlgorithm v => C.SignKeyKES v -> Encoding
encodeSignKeyKES = fromPlainEncoding . C.encodeSignKeyKES
{-# INLINE encodeSignKeyKES #-}

decodeSignKeyKES :: C.KESAlgorithm v => Decoder s (C.SignKeyKES v)
decodeSignKeyKES = fromPlainDecoder C.decodeSignKeyKES
{-# INLINE decodeSignKeyKES #-}

encodeSigKES :: C.KESAlgorithm v => C.SigKES v -> Encoding
encodeSigKES = fromPlainEncoding . C.encodeSigKES
{-# INLINE encodeSigKES #-}

decodeSigKES :: C.KESAlgorithm v => Decoder s (C.SigKES v)
decodeSigKES = fromPlainDecoder C.decodeSigKES
{-# INLINE decodeSigKES #-}

encodeSignedKES :: C.KESAlgorithm v => C.SignedKES v a -> Encoding
encodeSignedKES = fromPlainEncoding . C.encodeSignedKES
{-# INLINE encodeSignedKES #-}

decodeSignedKES :: C.KESAlgorithm v => Decoder s (C.SignedKES v a)
decodeSignedKES = fromPlainDecoder C.decodeSignedKES
{-# INLINE decodeSignedKES #-}

--------------------------------------------------------------------------------
-- VRF
--------------------------------------------------------------------------------

encodeVerKeyVRF :: C.VRFAlgorithm v => C.VerKeyVRF v -> Encoding
encodeVerKeyVRF = fromPlainEncoding . C.encodeVerKeyVRF
{-# INLINE encodeVerKeyVRF #-}

decodeVerKeyVRF :: C.VRFAlgorithm v => Decoder s (C.VerKeyVRF v)
decodeVerKeyVRF = fromPlainDecoder C.decodeVerKeyVRF
{-# INLINE decodeVerKeyVRF #-}

encodeSignKeyVRF :: C.VRFAlgorithm v => C.SignKeyVRF v -> Encoding
encodeSignKeyVRF = fromPlainEncoding . C.encodeSignKeyVRF
{-# INLINE encodeSignKeyVRF #-}

decodeSignKeyVRF :: C.VRFAlgorithm v => Decoder s (C.SignKeyVRF v)
decodeSignKeyVRF = fromPlainDecoder C.decodeSignKeyVRF
{-# INLINE decodeSignKeyVRF #-}

encodeCertVRF :: C.VRFAlgorithm v => C.CertVRF v -> Encoding
encodeCertVRF = fromPlainEncoding . C.encodeCertVRF
{-# INLINE encodeCertVRF #-}

decodeCertVRF :: C.VRFAlgorithm v => Decoder s (C.CertVRF v)
decodeCertVRF = fromPlainDecoder C.decodeCertVRF
{-# INLINE decodeCertVRF #-}
