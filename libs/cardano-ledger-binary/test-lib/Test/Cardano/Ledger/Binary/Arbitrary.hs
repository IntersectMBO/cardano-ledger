{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Binary.Arbitrary () where

import Cardano.Crypto.DSIGN.Class
import Codec.CBOR.ByteArray (ByteArray (..))
import Codec.CBOR.ByteArray.Sliced (SlicedByteArray (..))
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import Data.IP (IPv4, IPv6, toIPv4w, toIPv6w)
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict
import qualified Data.Primitive.ByteArray as Prim (byteArrayFromListN)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.VMap as VMap
import qualified Data.Vector.Primitive as VP
import Data.Word
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
    fromMaybe (error $ "Impossible: Invalid size " ++ show n)
      . rawDeserialiseSignKeyDSIGN
      . BS.pack
      <$> vectorOf (fromIntegral n) arbitrary

instance DSIGNAlgorithm v => Arbitrary (SigDSIGN v) where
  arbitrary = do
    let n = sizeSigDSIGN (Proxy @v) :: Word
    fromMaybe (error $ "Impossible: Invalid size " ++ show n)
      . rawDeserialiseSigDSIGN
      . BS.pack
      <$> vectorOf (fromIntegral n) arbitrary
