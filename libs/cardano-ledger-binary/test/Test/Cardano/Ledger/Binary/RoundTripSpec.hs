{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Binary.RoundTripSpec (spec) where

import Cardano.Ledger.Binary
import Codec.CBOR.ByteArray (ByteArray (..))
import Codec.CBOR.ByteArray.Sliced (SlicedByteArray (..))
import Control.Monad (forM_)
import Data.Fixed (Fixed (..), Nano, Pico)
import qualified Data.Foldable as F
import Data.GenValidity
import Data.GenValidity.Time.Clock ()
import Data.IP (IPv4, IPv6, toIPv4w, toIPv6w)
import Data.Int
import qualified Data.Map.Strict as Map
import qualified Data.Primitive.ByteArray as Prim (ByteArray, byteArrayFromListN)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Time.Clock
  ( NominalDiffTime,
    UTCTime (..),
    nominalDiffTimeToSeconds,
    secondsToNominalDiffTime,
  )
import qualified Data.VMap as VMap
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Numeric.Natural
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

-- | We do not handle the full precision of NominalDiffTime.
newtype NominalDiffTimeRounded = NominalDiffTimeRounded NominalDiffTime
  deriving (Show, Eq, ToCBOR, FromCBOR, Validity)

instance GenValid NominalDiffTimeRounded where
  genValid = secondsToNominalDiffTimeRounded <$> genValid
  shrinkValid = fmap secondsToNominalDiffTimeRounded . shrinkValid . nominalDiffTimeRoundedToSeconds

secondsToNominalDiffTimeRounded :: Pico -> NominalDiffTimeRounded
secondsToNominalDiffTimeRounded (MkFixed s) =
  NominalDiffTimeRounded $
    secondsToNominalDiffTime $ MkFixed (1_000_000 * (s `div` 1_000_000))

nominalDiffTimeRoundedToSeconds :: NominalDiffTimeRounded -> Pico
nominalDiffTimeRoundedToSeconds (NominalDiffTimeRounded ndt) = nominalDiffTimeToSeconds ndt

deriving instance Arbitrary ByteArray

instance Arbitrary SlicedByteArray where
  arbitrary = do
    NonNegative off <- arbitrary
    NonNegative count <- arbitrary
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

instance
  (Ord k, VMap.Vector kv k, VMap.Vector vv v, Arbitrary k, Arbitrary v) =>
  Arbitrary (VMap.VMap kv vv k v)
  where
  arbitrary = VMap.fromMap <$> arbitrary
  shrink = fmap VMap.fromList . shrink . VMap.toList

spec :: Spec
spec =
  describe "RoundTrip" $ do
    forM_ allVersions $ \version ->
      describe (show version) $ do
        roundTripSpec @() version cborTrip
        roundTripSpec @Bool version cborTrip
        roundTripValidSpec @Integer version cborTrip
        roundTripValidSpec @Natural version cborTrip
        roundTripValidSpec @Word version cborTrip
        roundTripValidSpec @Word8 version cborTrip
        roundTripValidSpec @Word16 version cborTrip
        roundTripValidSpec @Word32 version cborTrip
        roundTripValidSpec @Word64 version cborTrip
        roundTripValidSpec @Int version cborTrip
        roundTripValidSpec @Int8 version cborTrip
        roundTripValidSpec @Int16 version cborTrip
        roundTripValidSpec @Int32 version cborTrip
        roundTripValidSpec @Int64 version cborTrip
        roundTripSpec @Float version cborTrip
        roundTripSpec @Double version cborTrip
        roundTripValidSpec @Rational version cborTrip
        roundTripValidSpec @Nano version cborTrip
        roundTripValidSpec @Pico version cborTrip
        roundTripValidSpec @NominalDiffTimeRounded version cborTrip
        roundTripValidSpec @UTCTime version cborTrip
        roundTripSpec @IPv4 version cborTrip
        roundTripSpec @IPv6 version cborTrip
        roundTripSpec @[Integer] version cborTrip
        roundTripSpec @(V.Vector Integer) version cborTrip
        roundTripSpec @(VS.Vector Int16) version cborTrip
        roundTripSpec @(VP.Vector Int) version cborTrip
        roundTripSpec @(VU.Vector (Bool, Word)) version cborTrip
        roundTripSpec @(Set.Set Int) version cborTrip
        roundTripSpec @(Map.Map Integer Int) version cborTrip
        roundTripSpec @(Seq.Seq Int) version cborTrip
        roundTripSpec @(SSeq.StrictSeq Int) version cborTrip
        roundTripSpec @(VMap.VMap VMap.VB VMap.VS Integer Int) version cborTrip
        roundTripSpec @Prim.ByteArray version cborTrip
        roundTripSpec @ByteArray version cborTrip

-- roundTripSpec @SlicedByteArray version cborTrip
