{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Cardano.Base.Proxy (Proxy (..))
import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import qualified Cardano.Crypto.DSIGN.Class as C
import Cardano.Crypto.KES (KESAlgorithm (..))
import qualified Cardano.Crypto.KES.Class as C
import Cardano.Crypto.VRF (VRFAlgorithm (..))
import qualified Cardano.Crypto.VRF.Class as C
import qualified Cardano.Crypto.VRF.Praos as Praos
import Cardano.Ledger.Binary.Decoding.Decoder (
  Decoder,
  DecoderError (DecoderErrorSizeMismatch),
  cborError,
  decodeBytes,
  decodeBytesDefOrIndef,
  ifDecoderVersionAtLeast,
 )
import Cardano.Ledger.Binary.Encoding.Encoder (Encoding, fromPlainEncoding)
import Cardano.Ledger.Binary.Version (natVersion)
import qualified Codec.CBOR.Encoding as C
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (KnownNat, Nat, natVal)

--------------------------------------------------------------------------------
-- DSIGN
--------------------------------------------------------------------------------

class (Typeable a, KnownNat (Size a)) => FixedSizeBytes a where
  type Size a :: Nat
  rawEncode :: a -> C.Encoding
  rawDeserialise :: BS.ByteString -> Maybe a

instance DSIGNAlgorithm v => FixedSizeBytes (VerKeyDSIGN v) where
  type Size (VerKeyDSIGN v) = VerKeySizeDSIGN v
  rawEncode = C.encodeVerKeyDSIGN
  rawDeserialise = C.rawDeserialiseVerKeyDSIGN
  {-# INLINE rawDeserialise #-}

instance DSIGNAlgorithm v => FixedSizeBytes (SignKeyDSIGN v) where
  type Size (SignKeyDSIGN v) = SignKeySizeDSIGN v
  rawEncode = C.encodeSignKeyDSIGN
  rawDeserialise = C.rawDeserialiseSignKeyDSIGN
  {-# INLINE rawDeserialise #-}

instance DSIGNAlgorithm v => FixedSizeBytes (SigDSIGN v) where
  type Size (SigDSIGN v) = SigSizeDSIGN v
  rawEncode = C.encodeSigDSIGN
  rawDeserialise = C.rawDeserialiseSigDSIGN
  {-# INLINE rawDeserialise #-}

instance KESAlgorithm v => FixedSizeBytes (VerKeyKES v) where
  type Size (VerKeyKES v) = VerKeySizeKES v
  rawEncode = C.encodeVerKeyKES
  rawDeserialise = C.rawDeserialiseVerKeyKES
  {-# INLINE rawDeserialise #-}

instance KESAlgorithm v => FixedSizeBytes (SigKES v) where
  type Size (SigKES v) = SigSizeKES v
  rawEncode = C.encodeSigKES
  rawDeserialise = C.rawDeserialiseSigKES
  {-# INLINE rawDeserialise #-}

type family VerKeySizeVRF v :: Nat where
  VerKeySizeVRF Praos.PraosVRF = 32

instance (VRFAlgorithm v, KnownNat (VerKeySizeVRF v)) => FixedSizeBytes (VerKeyVRF v) where
  type Size (VerKeyVRF v) = VerKeySizeVRF v
  rawEncode = C.encodeVerKeyVRF
  rawDeserialise = C.rawDeserialiseVerKeyVRF
  {-# INLINE rawDeserialise #-}

type family SignKeySizeVRF v :: Nat where
  SignKeySizeVRF Praos.PraosVRF = 64

instance
  ( KnownNat (SignKeySizeVRF v)
  , Typeable v
  , VRFAlgorithm v
  ) =>
  FixedSizeBytes (SignKeyVRF v)
  where
  type Size (SignKeyVRF v) = SignKeySizeVRF v
  rawEncode = C.encodeSignKeyVRF
  rawDeserialise = C.rawDeserialiseSignKeyVRF
  {-# INLINE rawDeserialise #-}

type family CertSizeVRF v :: Nat where
  CertSizeVRF Praos.PraosVRF = 80

instance (KnownNat (CertSizeVRF v), Typeable v, VRFAlgorithm v) => FixedSizeBytes (CertVRF v) where
  type Size (CertVRF v) = CertSizeVRF v
  rawEncode = C.encodeCertVRF
  rawDeserialise = C.rawDeserialiseCertVRF

decodeFixedSizeBytes ::
  forall a v s.
  FixedSizeBytes (a v) =>
  Decoder s (a v)
decodeFixedSizeBytes = do
  let p = Proxy @(a v)
  bs <- decodeBytesVersioned
  case rawDeserialise bs of
    Just vk -> pure vk
    Nothing ->
      cborError $
        DecoderErrorSizeMismatch
          (T.pack . show $ typeRep p)
          (fromIntegral . natVal $ Proxy @(Size (a v)))
          (BS.length bs)
  where
    decodeBytesVersioned =
      ifDecoderVersionAtLeast
        (natVersion @12)
        decodeBytesDefOrIndef
        decodeBytes
{-# INLINE decodeFixedSizeBytes #-}

encodeFixedSizeBytes :: FixedSizeBytes a => a -> Encoding
encodeFixedSizeBytes = fromPlainEncoding . rawEncode

encodeVerKeyDSIGN :: C.DSIGNAlgorithm v => C.VerKeyDSIGN v -> Encoding
encodeVerKeyDSIGN = encodeFixedSizeBytes
{-# INLINE encodeVerKeyDSIGN #-}

decodeVerKeyDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.VerKeyDSIGN v)
decodeVerKeyDSIGN = decodeFixedSizeBytes
{-# INLINE decodeVerKeyDSIGN #-}

encodeSignKeyDSIGN :: C.DSIGNAlgorithm v => C.SignKeyDSIGN v -> Encoding
encodeSignKeyDSIGN = fromPlainEncoding . C.encodeSignKeyDSIGN
{-# INLINE encodeSignKeyDSIGN #-}

decodeSignKeyDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SignKeyDSIGN v)
decodeSignKeyDSIGN = decodeFixedSizeBytes
{-# INLINE decodeSignKeyDSIGN #-}

encodeSigDSIGN :: C.DSIGNAlgorithm v => C.SigDSIGN v -> Encoding
encodeSigDSIGN = fromPlainEncoding . C.encodeSigDSIGN
{-# INLINE encodeSigDSIGN #-}

decodeSigDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SigDSIGN v)
decodeSigDSIGN = decodeFixedSizeBytes
{-# INLINE decodeSigDSIGN #-}

encodeSignedDSIGN :: C.DSIGNAlgorithm v => C.SignedDSIGN v a -> Encoding
encodeSignedDSIGN = fromPlainEncoding . C.encodeSignedDSIGN
{-# INLINE encodeSignedDSIGN #-}

decodeSignedDSIGN :: C.DSIGNAlgorithm v => Decoder s (C.SignedDSIGN v a)
decodeSignedDSIGN = C.SignedDSIGN <$> decodeFixedSizeBytes
{-# INLINE decodeSignedDSIGN #-}

--------------------------------------------------------------------------------
-- KES
--------------------------------------------------------------------------------

encodeVerKeyKES :: C.KESAlgorithm v => C.VerKeyKES v -> Encoding
encodeVerKeyKES = fromPlainEncoding . C.encodeVerKeyKES
{-# INLINE encodeVerKeyKES #-}

decodeVerKeyKES :: C.KESAlgorithm v => Decoder s (C.VerKeyKES v)
decodeVerKeyKES = decodeFixedSizeBytes
{-# INLINE decodeVerKeyKES #-}

encodeSigKES :: C.KESAlgorithm v => C.SigKES v -> Encoding
encodeSigKES = fromPlainEncoding . C.encodeSigKES
{-# INLINE encodeSigKES #-}

decodeSigKES :: C.KESAlgorithm v => Decoder s (C.SigKES v)
decodeSigKES = decodeFixedSizeBytes
{-# INLINE decodeSigKES #-}

encodeSignedKES :: C.KESAlgorithm v => C.SignedKES v a -> Encoding
encodeSignedKES = fromPlainEncoding . C.encodeSignedKES
{-# INLINE encodeSignedKES #-}

decodeSignedKES :: C.KESAlgorithm v => Decoder s (C.SignedKES v a)
decodeSignedKES = C.SignedKES <$> decodeFixedSizeBytes
{-# INLINE decodeSignedKES #-}

--------------------------------------------------------------------------------
-- VRF
--------------------------------------------------------------------------------

encodeVerKeyVRF :: C.VRFAlgorithm v => C.VerKeyVRF v -> Encoding
encodeVerKeyVRF = fromPlainEncoding . C.encodeVerKeyVRF
{-# INLINE encodeVerKeyVRF #-}

decodeVerKeyVRF :: (C.VRFAlgorithm v, KnownNat (VerKeySizeVRF v)) => Decoder s (C.VerKeyVRF v)
decodeVerKeyVRF = decodeFixedSizeBytes
{-# INLINE decodeVerKeyVRF #-}

encodeSignKeyVRF :: C.VRFAlgorithm v => C.SignKeyVRF v -> Encoding
encodeSignKeyVRF = fromPlainEncoding . C.encodeSignKeyVRF
{-# INLINE encodeSignKeyVRF #-}

decodeSignKeyVRF :: (KnownNat (SignKeySizeVRF v), VRFAlgorithm v) => Decoder s (C.SignKeyVRF v)
decodeSignKeyVRF = decodeFixedSizeBytes
{-# INLINE decodeSignKeyVRF #-}

encodeCertVRF :: C.VRFAlgorithm v => C.CertVRF v -> Encoding
encodeCertVRF = fromPlainEncoding . C.encodeCertVRF
{-# INLINE encodeCertVRF #-}

decodeCertVRF :: (KnownNat (CertSizeVRF v), VRFAlgorithm v) => Decoder s (C.CertVRF v)
decodeCertVRF = decodeFixedSizeBytes
{-# INLINE decodeCertVRF #-}
