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
) where

import qualified Cardano.Crypto.DSIGN.Class as C
import qualified Cardano.Crypto.KES.Class as C
import qualified Cardano.Crypto.VRF.Class as C
import Cardano.Ledger.Binary.Decoding.Decoder (Decoder, decodeFixedSized)
import Cardano.Ledger.Binary.Encoding.Encoder (Encoding, encodeFixedSized)

--------------------------------------------------------------------------------
-- DSIGN
--------------------------------------------------------------------------------

encodeVerKeyDSIGN :: C.DSIGNAlgorithm v => C.VerKeyDSIGN v -> Encoding
encodeVerKeyDSIGN = encodeFixedSized
{-# INLINE encodeVerKeyDSIGN #-}
{-# DEPRECATED encodeVerKeyDSIGN "Use encodeFixedSized instead" #-}

decodeVerKeyDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.VerKeyDSIGN v)
decodeVerKeyDSIGN = decodeFixedSized
{-# INLINE decodeVerKeyDSIGN #-}
{-# DEPRECATED decodeVerKeyDSIGN "Use decodeFixedSized instead" #-}

encodeSignKeyDSIGN :: C.DSIGNAlgorithm v => C.SignKeyDSIGN v -> Encoding
encodeSignKeyDSIGN = encodeFixedSized
{-# INLINE encodeSignKeyDSIGN #-}
{-# DEPRECATED encodeSignKeyDSIGN "Use encodeFixedSized instead" #-}

decodeSignKeyDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SignKeyDSIGN v)
decodeSignKeyDSIGN = decodeFixedSized
{-# INLINE decodeSignKeyDSIGN #-}
{-# DEPRECATED decodeSignKeyDSIGN "Use decodeFixedSized instead" #-}

encodeSigDSIGN :: C.DSIGNAlgorithm v => C.SigDSIGN v -> Encoding
encodeSigDSIGN = encodeFixedSized
{-# INLINE encodeSigDSIGN #-}
{-# DEPRECATED encodeSigDSIGN "Use encodeFixedSized instead" #-}

decodeSigDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SigDSIGN v)
decodeSigDSIGN = decodeFixedSized
{-# INLINE decodeSigDSIGN #-}
{-# DEPRECATED decodeSigDSIGN "Use decodeFixedSized instead" #-}

encodeSignedDSIGN :: C.DSIGNAlgorithm v => C.SignedDSIGN v a -> Encoding
encodeSignedDSIGN = encodeFixedSized
{-# INLINE encodeSignedDSIGN #-}
{-# DEPRECATED encodeSignedDSIGN "Use encodeFixedSized instead" #-}

decodeSignedDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SignedDSIGN v a)
decodeSignedDSIGN = decodeFixedSized
{-# INLINE decodeSignedDSIGN #-}
{-# DEPRECATED decodeSignedDSIGN "Use decodeFixedSized instead" #-}

--------------------------------------------------------------------------------
-- KES
--------------------------------------------------------------------------------

encodeVerKeyKES :: C.KESAlgorithm v => C.VerKeyKES v -> Encoding
encodeVerKeyKES = encodeFixedSized
{-# INLINE encodeVerKeyKES #-}
{-# DEPRECATED encodeVerKeyKES "Use encodeFixedSized instead" #-}

decodeVerKeyKES :: C.KESAlgorithm v => Decoder s (C.VerKeyKES v)
decodeVerKeyKES = decodeFixedSized
{-# INLINE decodeVerKeyKES #-}
{-# DEPRECATED decodeVerKeyKES "Use decodeFixedSized instead" #-}

encodeSigKES :: C.KESAlgorithm v => C.SigKES v -> Encoding
encodeSigKES = encodeFixedSized
{-# INLINE encodeSigKES #-}
{-# DEPRECATED encodeSigKES "Use encodeFixedSized instead" #-}

decodeSigKES :: C.KESAlgorithm v => Decoder s (C.SigKES v)
decodeSigKES = decodeFixedSized
{-# INLINE decodeSigKES #-}
{-# DEPRECATED decodeSigKES "Use decodeFixedSized instead" #-}

encodeSignedKES :: C.KESAlgorithm v => C.SignedKES v a -> Encoding
encodeSignedKES = encodeFixedSized
{-# INLINE encodeSignedKES #-}
{-# DEPRECATED encodeSignedKES "Use encodeFixedSized instead" #-}

decodeSignedKES :: C.KESAlgorithm v => Decoder s (C.SignedKES v a)
decodeSignedKES = decodeFixedSized
{-# INLINE decodeSignedKES #-}
{-# DEPRECATED decodeSignedKES "Use decodeFixedSized instead" #-}

--------------------------------------------------------------------------------
-- VRF
--------------------------------------------------------------------------------

encodeVerKeyVRF :: C.VRFAlgorithm v => C.VerKeyVRF v -> Encoding
encodeVerKeyVRF = encodeFixedSized
{-# INLINE encodeVerKeyVRF #-}
{-# DEPRECATED encodeVerKeyVRF "Use encodeFixedSized instead" #-}

decodeVerKeyVRF :: C.VRFAlgorithm v => Decoder s (C.VerKeyVRF v)
decodeVerKeyVRF = decodeFixedSized
{-# INLINE decodeVerKeyVRF #-}
{-# DEPRECATED decodeVerKeyVRF "Use decodeFixedSized instead" #-}

encodeSignKeyVRF :: C.VRFAlgorithm v => C.SignKeyVRF v -> Encoding
encodeSignKeyVRF = encodeFixedSized
{-# INLINE encodeSignKeyVRF #-}
{-# DEPRECATED encodeSignKeyVRF "Use encodeFixedSized instead" #-}

decodeSignKeyVRF :: C.VRFAlgorithm v => Decoder s (C.SignKeyVRF v)
decodeSignKeyVRF = decodeFixedSized
{-# INLINE decodeSignKeyVRF #-}
{-# DEPRECATED decodeSignKeyVRF "Use decodeFixedSized instead" #-}

encodeCertVRF :: C.VRFAlgorithm v => C.CertVRF v -> Encoding
encodeCertVRF = encodeFixedSized
{-# INLINE encodeCertVRF #-}
{-# DEPRECATED encodeCertVRF "Use encodeFixedSized instead" #-}

decodeCertVRF :: C.VRFAlgorithm v => Decoder s (C.CertVRF v)
decodeCertVRF = decodeFixedSized
{-# INLINE decodeCertVRF #-}
{-# DEPRECATED decodeCertVRF "Use decodeFixedSized instead" #-}
