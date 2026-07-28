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

  -- * Leios
  encodeLeiosCert,
  decodeLeiosCert,
) where

import Cardano.Binary.FixedSizeCodec (decodeFixedSized, encodeFixedSized)
import qualified Cardano.Crypto.DSIGN.Class as C
import qualified Cardano.Crypto.KES.Class as C
import qualified Cardano.Crypto.Leios as C
import qualified Cardano.Crypto.VRF.Class as C
import Cardano.Ledger.Binary.Decoding.Decoder (Decoder, fromPlainDecoder)
import Cardano.Ledger.Binary.Encoding.Encoder (Encoding, fromPlainEncoding)

--------------------------------------------------------------------------------
-- DSIGN
--------------------------------------------------------------------------------

encodeVerKeyDSIGN :: C.DSIGNAlgorithm v => C.VerKeyDSIGN v -> Encoding
encodeVerKeyDSIGN = fromPlainEncoding . encodeFixedSized
{-# INLINE encodeVerKeyDSIGN #-}

decodeVerKeyDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.VerKeyDSIGN v)
decodeVerKeyDSIGN = fromPlainDecoder decodeFixedSized
{-# INLINE decodeVerKeyDSIGN #-}

encodeSignKeyDSIGN :: C.DSIGNAlgorithm v => C.SignKeyDSIGN v -> Encoding
encodeSignKeyDSIGN = fromPlainEncoding . encodeFixedSized
{-# INLINE encodeSignKeyDSIGN #-}

decodeSignKeyDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SignKeyDSIGN v)
decodeSignKeyDSIGN = fromPlainDecoder decodeFixedSized
{-# INLINE decodeSignKeyDSIGN #-}

encodeSigDSIGN :: C.DSIGNAlgorithm v => C.SigDSIGN v -> Encoding
encodeSigDSIGN = fromPlainEncoding . encodeFixedSized
{-# INLINE encodeSigDSIGN #-}

decodeSigDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SigDSIGN v)
decodeSigDSIGN = fromPlainDecoder decodeFixedSized
{-# INLINE decodeSigDSIGN #-}

encodeSignedDSIGN :: C.DSIGNAlgorithm v => C.SignedDSIGN v a -> Encoding
encodeSignedDSIGN = fromPlainEncoding . encodeFixedSized
{-# INLINE encodeSignedDSIGN #-}

decodeSignedDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SignedDSIGN v a)
decodeSignedDSIGN = fromPlainDecoder decodeFixedSized
{-# INLINE decodeSignedDSIGN #-}

--------------------------------------------------------------------------------
-- KES
--------------------------------------------------------------------------------

encodeVerKeyKES :: C.KESAlgorithm v => C.VerKeyKES v -> Encoding
encodeVerKeyKES = fromPlainEncoding . encodeFixedSized
{-# INLINE encodeVerKeyKES #-}

decodeVerKeyKES :: C.KESAlgorithm v => Decoder s (C.VerKeyKES v)
decodeVerKeyKES = fromPlainDecoder decodeFixedSized
{-# INLINE decodeVerKeyKES #-}

encodeSigKES :: C.KESAlgorithm v => C.SigKES v -> Encoding
encodeSigKES = fromPlainEncoding . encodeFixedSized
{-# INLINE encodeSigKES #-}

decodeSigKES :: C.KESAlgorithm v => Decoder s (C.SigKES v)
decodeSigKES = fromPlainDecoder decodeFixedSized
{-# INLINE decodeSigKES #-}

encodeSignedKES :: C.KESAlgorithm v => C.SignedKES v a -> Encoding
encodeSignedKES = fromPlainEncoding . encodeFixedSized
{-# INLINE encodeSignedKES #-}

decodeSignedKES :: C.KESAlgorithm v => Decoder s (C.SignedKES v a)
decodeSignedKES = fromPlainDecoder decodeFixedSized
{-# INLINE decodeSignedKES #-}

--------------------------------------------------------------------------------
-- VRF
--------------------------------------------------------------------------------

encodeVerKeyVRF :: C.VRFAlgorithm v => C.VerKeyVRF v -> Encoding
encodeVerKeyVRF = fromPlainEncoding . encodeFixedSized
{-# INLINE encodeVerKeyVRF #-}

decodeVerKeyVRF :: C.VRFAlgorithm v => Decoder s (C.VerKeyVRF v)
decodeVerKeyVRF = fromPlainDecoder decodeFixedSized
{-# INLINE decodeVerKeyVRF #-}

encodeSignKeyVRF :: C.VRFAlgorithm v => C.SignKeyVRF v -> Encoding
encodeSignKeyVRF = fromPlainEncoding . encodeFixedSized
{-# INLINE encodeSignKeyVRF #-}

decodeSignKeyVRF :: C.VRFAlgorithm v => Decoder s (C.SignKeyVRF v)
decodeSignKeyVRF = fromPlainDecoder decodeFixedSized
{-# INLINE decodeSignKeyVRF #-}

encodeCertVRF :: C.VRFAlgorithm v => C.CertVRF v -> Encoding
encodeCertVRF = fromPlainEncoding . encodeFixedSized
{-# INLINE encodeCertVRF #-}

decodeCertVRF :: C.VRFAlgorithm v => Decoder s (C.CertVRF v)
decodeCertVRF = fromPlainDecoder decodeFixedSized
{-# INLINE decodeCertVRF #-}

--------------------------------------------------------------------------------
-- Leios
--------------------------------------------------------------------------------

encodeLeiosCert :: C.LeiosCert -> Encoding
encodeLeiosCert = fromPlainEncoding . C.encodeLeiosCert
{-# INLINE encodeLeiosCert #-}

decodeLeiosCert :: Decoder s C.LeiosCert
decodeLeiosCert = fromPlainDecoder C.decodeLeiosCert
{-# INLINE decodeLeiosCert #-}
