{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Binary.FixedSizeCodec (FixedSizeCodec (..)) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..))
import Cardano.Crypto.KES (KESAlgorithm (..))
import Cardano.Crypto.VRF (VRFAlgorithm (..))
import qualified Data.ByteString as BS
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownNat, Nat)

class (Typeable a, KnownNat (FixedSize a)) => FixedSizeCodec a where
  type FixedSize a :: Nat
  rawEncodeFixedSized :: a -> BS.ByteString
  rawDecodeFixedSized :: MonadFail m => BS.ByteString -> m a

liftDecodeResult :: MonadFail m => Maybe a -> m a
liftDecodeResult = maybe (fail "Failed to decode") pure

instance VRFAlgorithm v => FixedSizeCodec (VerKeyVRF v) where
  type FixedSize (VerKeyVRF v) = VerKeySizeVRF v
  rawEncodeFixedSized = rawSerialiseVerKeyVRF
  rawDecodeFixedSized = liftDecodeResult . rawDeserialiseVerKeyVRF
  {-# INLINE rawDecodeFixedSized #-}

instance VRFAlgorithm v => FixedSizeCodec (SignKeyVRF v) where
  type FixedSize (SignKeyVRF v) = SignKeySizeVRF v
  rawEncodeFixedSized = rawSerialiseSignKeyVRF
  rawDecodeFixedSized = liftDecodeResult . rawDeserialiseSignKeyVRF
  {-# INLINE rawDecodeFixedSized #-}

instance VRFAlgorithm v => FixedSizeCodec (CertVRF v) where
  type FixedSize (CertVRF v) = CertSizeVRF v
  rawEncodeFixedSized = rawSerialiseCertVRF
  rawDecodeFixedSized = liftDecodeResult . rawDeserialiseCertVRF
  {-# INLINE rawDecodeFixedSized #-}

instance DSIGNAlgorithm v => FixedSizeCodec (VerKeyDSIGN v) where
  type FixedSize (VerKeyDSIGN v) = VerKeySizeDSIGN v
  rawEncodeFixedSized = rawSerialiseVerKeyDSIGN
  rawDecodeFixedSized = liftDecodeResult . rawDeserialiseVerKeyDSIGN
  {-# INLINE rawDecodeFixedSized #-}

instance DSIGNAlgorithm v => FixedSizeCodec (SignKeyDSIGN v) where
  type FixedSize (SignKeyDSIGN v) = SignKeySizeDSIGN v
  rawEncodeFixedSized = rawSerialiseSignKeyDSIGN
  rawDecodeFixedSized = liftDecodeResult . rawDeserialiseSignKeyDSIGN
  {-# INLINE rawDecodeFixedSized #-}

instance DSIGNAlgorithm v => FixedSizeCodec (SigDSIGN v) where
  type FixedSize (SigDSIGN v) = SigSizeDSIGN v
  rawEncodeFixedSized = rawSerialiseSigDSIGN
  rawDecodeFixedSized = liftDecodeResult . rawDeserialiseSigDSIGN
  {-# INLINE rawDecodeFixedSized #-}

instance KESAlgorithm v => FixedSizeCodec (VerKeyKES v) where
  type FixedSize (VerKeyKES v) = VerKeySizeKES v
  rawEncodeFixedSized = rawSerialiseVerKeyKES
  rawDecodeFixedSized = liftDecodeResult . rawDeserialiseVerKeyKES
  {-# INLINE rawDecodeFixedSized #-}

instance KESAlgorithm v => FixedSizeCodec (SigKES v) where
  type FixedSize (SigKES v) = SigSizeKES v

  rawEncodeFixedSized = rawSerialiseSigKES
  rawDecodeFixedSized = liftDecodeResult . rawDeserialiseSigKES
  {-# INLINE rawDecodeFixedSized #-}
