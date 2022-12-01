{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Binary.Arbitrary (genVersion) where

import Cardano.Crypto.DSIGN.Class hiding (Signable)
import Cardano.Crypto.Util
import Cardano.Crypto.VRF.Class
import Cardano.Ledger.Binary.Version
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..), WithOrigin (..))
import Cardano.Slotting.Time (SystemStart (..))
import Codec.CBOR.ByteArray (ByteArray (..))
import Codec.CBOR.ByteArray.Sliced (SlicedByteArray (..))
import qualified Data.Foldable as F
import Data.IP (IPv4, IPv6, toIPv4w, toIPv6w)
import Data.Maybe.Strict
import qualified Data.Primitive.ByteArray as Prim (byteArrayFromListN)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.VMap as VMap
import qualified Data.Vector.Primitive as VP
import Data.Word
import GHC.Stack
import System.Random.Stateful
import Test.Cardano.Ledger.Binary.Random (QC (QC))
import Test.Crypto.Hash ()
import Test.Crypto.KES ()
import Test.Crypto.VRF ()
import Test.QuickCheck
import Test.QuickCheck.Instances ()

deriving instance Arbitrary ByteArray

instance Arbitrary SlicedByteArray where
  arbitrary = do
    NonNegative off <- arbitrary
    Positive count <- arbitrary
    NonNegative slack <- arbitrary
    let len = off + count + slack
    ba <- Prim.byteArrayFromListN len <$> (vector len :: Gen [Word8])
    pure $ SBA ba off count

instance Arbitrary IPv4 where
  arbitrary = toIPv4w <$> arbitrary

instance Arbitrary IPv6 where
  arbitrary = do
    t <- (,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    pure $ toIPv6w t

instance (VP.Prim e, Arbitrary e) => Arbitrary (VP.Vector e) where
  arbitrary = VP.fromList <$> arbitrary
  shrink = fmap VP.fromList . shrink . VP.toList

instance Arbitrary e => Arbitrary (SSeq.StrictSeq e) where
  arbitrary = SSeq.fromList <$> arbitrary
  shrink = fmap SSeq.fromList . shrink . F.toList

instance Arbitrary e => Arbitrary (StrictMaybe e) where
  arbitrary = maybeToStrictMaybe <$> arbitrary
  shrink = fmap maybeToStrictMaybe . shrink . strictMaybeToMaybe

instance
  (Ord k, VMap.Vector kv k, VMap.Vector vv v, Arbitrary k, Arbitrary v) =>
  Arbitrary (VMap.VMap kv vv k v)
  where
  arbitrary = VMap.fromMap <$> arbitrary
  shrink = fmap VMap.fromList . shrink . VMap.toList

instance DSIGNAlgorithm v => Arbitrary (VerKeyDSIGN v) where
  arbitrary = deriveVerKeyDSIGN <$> arbitrary

instance DSIGNAlgorithm v => Arbitrary (SignKeyDSIGN v) where
  arbitrary = do
    let n = sizeSignKeyDSIGN (Proxy @v) :: Word
    bs <- uniformByteStringM (fromIntegral n) QC
    maybe (error $ "Impossible: Invalid size " ++ show n) pure $ rawDeserialiseSignKeyDSIGN bs

instance DSIGNAlgorithm v => Arbitrary (SigDSIGN v) where
  arbitrary = do
    let n = sizeSigDSIGN (Proxy @v) :: Word
    bs <- uniformByteStringM (fromIntegral n) QC
    maybe (error $ "Impossible: Invalid size " ++ show n) pure $ rawDeserialiseSigDSIGN bs

instance VRFAlgorithm v => Arbitrary (OutputVRF v) where
  arbitrary =
    OutputVRF <$> uniformByteStringM (fromIntegral (sizeOutputVRF (Proxy :: Proxy v))) QC

instance
  (ContextVRF v ~ (), Signable v ~ SignableRepresentation, VRFAlgorithm v) =>
  Arbitrary (CertifiedVRF v a)
  where
  arbitrary = CertifiedVRF <$> arbitrary <*> genCertVRF
    where
      genCertVRF :: Gen (CertVRF v)
      genCertVRF = arbitrary

deriving instance Arbitrary SlotNo

instance Arbitrary t => Arbitrary (WithOrigin t) where
  arbitrary = frequency [(20, pure Origin), (80, At <$> arbitrary)]

deriving instance Arbitrary EpochNo

deriving instance Arbitrary EpochSize

deriving instance Arbitrary SystemStart

deriving instance Arbitrary BlockNo

instance Arbitrary Version where
  arbitrary = genVersion minBound maxBound

genVersion :: HasCallStack => Version -> Version -> Gen Version
genVersion minVersion maxVersion =
  genVersion64 (getVersion64 minVersion) (getVersion64 maxVersion)
  where
    genVersion64 minVersion64 maxVersion64 = do
      v64 <- choose (minVersion64, maxVersion64)
      case mkVersion64 v64 of
        Nothing -> error $ "Impossible: Invalid version generated: " ++ show v64
        Just v -> pure v
