{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.RoundTripSpec (spec) where

import Cardano.Ledger.Binary
import Codec.CBOR.ByteArray (ByteArray (..))
import Codec.CBOR.ByteArray.Sliced (SlicedByteArray (..))
import Control.Monad (forM_)
import Data.Fixed (Fixed (..), Nano, Pico)
import Data.IP (IPv4, IPv6)
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict
import qualified Data.Primitive.ByteArray as Prim (ByteArray)
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
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Hspec
import Test.QuickCheck

-- | We do not handle the full precision of NominalDiffTime.
newtype NominalDiffTimeRounded = NominalDiffTimeRounded NominalDiffTime
  deriving (Show, Eq, ToCBOR, FromCBOR)

instance Arbitrary NominalDiffTimeRounded where
  arbitrary = secondsToNominalDiffTimeRounded <$> arbitrary
  shrink = fmap secondsToNominalDiffTimeRounded . shrink . nominalDiffTimeRoundedToSeconds

secondsToNominalDiffTimeRounded :: Pico -> NominalDiffTimeRounded
secondsToNominalDiffTimeRounded (MkFixed s) =
  NominalDiffTimeRounded $
    secondsToNominalDiffTime $
      MkFixed (1_000_000 * (s `div` 1_000_000))

nominalDiffTimeRoundedToSeconds :: NominalDiffTimeRounded -> Pico
nominalDiffTimeRoundedToSeconds (NominalDiffTimeRounded ndt) = nominalDiffTimeToSeconds ndt

spec :: Spec
spec =
  describe "RoundTrip" $ do
    forM_ allVersions $ \version ->
      describe (show version) $ do
        roundTripSpec @() version cborTrip
        roundTripSpec @Bool version cborTrip
        roundTripSpec @Integer version cborTrip
        roundTripSpec @Natural version cborTrip
        roundTripSpec @Word version cborTrip
        roundTripSpec @Word8 version cborTrip
        roundTripSpec @Word16 version cborTrip
        roundTripSpec @Word32 version cborTrip
        roundTripSpec @Word64 version cborTrip
        roundTripSpec @Int version cborTrip
        roundTripSpec @Int8 version cborTrip
        roundTripSpec @Int16 version cborTrip
        roundTripSpec @Int32 version cborTrip
        roundTripSpec @Int64 version cborTrip
        roundTripSpec @Float version cborTrip
        roundTripSpec @Double version cborTrip
        roundTripSpec @Rational version cborTrip
        roundTripSpec @Nano version cborTrip
        roundTripSpec @Pico version cborTrip
        roundTripSpec @NominalDiffTimeRounded version cborTrip
        roundTripSpec @UTCTime version cborTrip
        roundTripSpec @IPv4 version cborTrip
        roundTripSpec @IPv6 version cborTrip
        roundTripSpec @(Maybe Integer) version cborTrip
        roundTripSpec @(StrictMaybe Integer) version cborTrip
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
        roundTripSpec @SlicedByteArray version cborTrip
        let maybeNullTrip = Trip (encodeNullMaybe toCBOR) (decodeNullMaybe fromCBOR)
        roundTripSpec @(Maybe Integer) version maybeNullTrip
