{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.RoundTripSpec (spec) where

import Cardano.Crypto.DSIGN.Class (SigDSIGN, SignKeyDSIGN, VerKeyDSIGN)
import Cardano.Crypto.DSIGN.EcdsaSecp256k1 (EcdsaSecp256k1DSIGN)
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Cardano.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import Cardano.Crypto.DSIGN.SchnorrSecp256k1 (SchnorrSecp256k1DSIGN)
import Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Crypto.Hash.Keccak256 (Keccak256)
import Cardano.Crypto.Hash.SHA256 (SHA256)
import Cardano.Crypto.Hash.SHA3_256 (SHA3_256)
import Cardano.Crypto.Hash.Short (ShortHash)
import Cardano.Crypto.KES.Class (SigKES, SignKeyKES, VerKeyKES)
import Cardano.Crypto.KES.CompactSingle (CompactSingleKES)
import Cardano.Crypto.KES.CompactSum
  ( CompactSum0KES,
    CompactSum1KES,
    CompactSum2KES,
    CompactSum3KES,
    CompactSum4KES,
    CompactSum5KES,
    CompactSum6KES,
    CompactSum7KES,
  )
import Cardano.Crypto.KES.Mock (MockKES)
import Cardano.Crypto.KES.Simple (SimpleKES)
import Cardano.Crypto.KES.Sum (Sum0KES, Sum1KES, Sum2KES, Sum3KES, Sum4KES, Sum5KES, Sum6KES, Sum7KES)
import Cardano.Crypto.VRF.Class (CertVRF, SignKeyVRF, VerKeyVRF)
import Cardano.Crypto.VRF.Mock (MockVRF)
import Cardano.Crypto.VRF.Simple (SimpleVRF)
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
        describe "Crypto" $ do
          describe "Hash" $ do
            roundTripSpec @(Hash Blake2b_224 ()) version cborTrip
            roundTripSpec @(Hash Blake2b_256 ()) version cborTrip
            roundTripSpec @(Hash SHA256 ()) version cborTrip
            roundTripSpec @(Hash SHA3_256 ()) version cborTrip
            roundTripSpec @(Hash Keccak256 ()) version cborTrip
            roundTripSpec @(Hash ShortHash ()) version cborTrip
          describe "DSIGN" $ do
            describe "Ed25519" $ do
              roundTripSpec @(SignKeyDSIGN Ed25519DSIGN) version cborTrip
              roundTripSpec @(VerKeyDSIGN Ed25519DSIGN) version cborTrip
              roundTripSpec @(SigDSIGN Ed25519DSIGN) version cborTrip
            describe "Ed448" $ do
              roundTripSpec @(SignKeyDSIGN Ed448DSIGN) version cborTrip
              roundTripSpec @(VerKeyDSIGN Ed448DSIGN) version cborTrip
              roundTripSpec @(SigDSIGN Ed448DSIGN) version cborTrip
            describe "EcdsaSecp256k1" $ do
              roundTripSpec @(SignKeyDSIGN EcdsaSecp256k1DSIGN) version cborTrip
              roundTripSpec @(VerKeyDSIGN EcdsaSecp256k1DSIGN) version cborTrip
              roundTripSpec @(SigDSIGN EcdsaSecp256k1DSIGN) version cborTrip
            describe "SchnorrSecp256k1" $ do
              roundTripSpec @(SignKeyDSIGN SchnorrSecp256k1DSIGN) version cborTrip
              roundTripSpec @(VerKeyDSIGN SchnorrSecp256k1DSIGN) version cborTrip
              roundTripSpec @(SigDSIGN SchnorrSecp256k1DSIGN) version cborTrip
            describe "Mock" $ do
              roundTripSpec @(SignKeyDSIGN MockDSIGN) version cborTrip
              roundTripSpec @(VerKeyDSIGN MockDSIGN) version cborTrip
              roundTripSpec @(SigDSIGN MockDSIGN) version cborTrip
          describe "VRF" $ do
            describe "Simple" $ do
              roundTripSpec @(SignKeyVRF SimpleVRF) version cborTrip
              roundTripSpec @(VerKeyVRF SimpleVRF) version cborTrip
              roundTripSpec @(CertVRF SimpleVRF) version cborTrip
            describe "Mock" $ do
              roundTripSpec @(SignKeyVRF MockVRF) version cborTrip
              roundTripSpec @(VerKeyVRF MockVRF) version cborTrip
              roundTripSpec @(CertVRF MockVRF) version cborTrip
          describe "KES" $ do
            describe "CompactSingle" $ do
              roundTripSpec @(SignKeyKES (CompactSingleKES Ed25519DSIGN)) version cborTrip
              roundTripSpec @(VerKeyKES (CompactSingleKES Ed25519DSIGN)) version cborTrip
              roundTripSpec @(SigKES (CompactSingleKES Ed25519DSIGN)) version cborTrip
            describe "CompactSum" $ do
              roundTripSpec @(SignKeyKES (CompactSum0KES Ed25519DSIGN)) version cborTrip
              roundTripSpec @(VerKeyKES (CompactSum0KES Ed25519DSIGN)) version cborTrip
              roundTripSpec @(SigKES (CompactSum0KES Ed25519DSIGN)) version cborTrip
              roundTripSpec @(SignKeyKES (CompactSum1KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(VerKeyKES (CompactSum1KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SigKES (CompactSum1KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SignKeyKES (CompactSum2KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(VerKeyKES (CompactSum2KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SigKES (CompactSum2KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SignKeyKES (CompactSum3KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(VerKeyKES (CompactSum3KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SigKES (CompactSum3KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SignKeyKES (CompactSum4KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(VerKeyKES (CompactSum4KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SigKES (CompactSum4KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SignKeyKES (CompactSum5KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(VerKeyKES (CompactSum5KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SigKES (CompactSum5KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SignKeyKES (CompactSum6KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(VerKeyKES (CompactSum6KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SigKES (CompactSum6KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SignKeyKES (CompactSum7KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(VerKeyKES (CompactSum7KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SigKES (CompactSum7KES Ed25519DSIGN Blake2b_256)) version cborTrip
            describe "Sum" $ do
              roundTripSpec @(SignKeyKES (Sum0KES Ed25519DSIGN)) version cborTrip
              roundTripSpec @(VerKeyKES (Sum0KES Ed25519DSIGN)) version cborTrip
              roundTripSpec @(SigKES (Sum0KES Ed25519DSIGN)) version cborTrip
              roundTripSpec @(SignKeyKES (Sum1KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(VerKeyKES (Sum1KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SigKES (Sum1KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SignKeyKES (Sum2KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(VerKeyKES (Sum2KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SigKES (Sum2KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SignKeyKES (Sum3KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(VerKeyKES (Sum3KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SigKES (Sum3KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SignKeyKES (Sum4KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(VerKeyKES (Sum4KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SigKES (Sum4KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SignKeyKES (Sum5KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(VerKeyKES (Sum5KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SigKES (Sum5KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SignKeyKES (Sum6KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(VerKeyKES (Sum6KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SigKES (Sum6KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SignKeyKES (Sum7KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(VerKeyKES (Sum7KES Ed25519DSIGN Blake2b_256)) version cborTrip
              roundTripSpec @(SigKES (Sum7KES Ed25519DSIGN Blake2b_256)) version cborTrip
            describe "Simple" $ do
              roundTripSpec @(SignKeyKES (SimpleKES Ed25519DSIGN 1)) version cborTrip
              roundTripSpec @(VerKeyKES (SimpleKES Ed25519DSIGN 1)) version cborTrip
              roundTripSpec @(SigKES (SimpleKES Ed25519DSIGN 1)) version cborTrip
              roundTripSpec @(SignKeyKES (SimpleKES Ed25519DSIGN 2)) version cborTrip
              roundTripSpec @(VerKeyKES (SimpleKES Ed25519DSIGN 2)) version cborTrip
              roundTripSpec @(SigKES (SimpleKES Ed25519DSIGN 2)) version cborTrip
              roundTripSpec @(SignKeyKES (SimpleKES Ed25519DSIGN 3)) version cborTrip
              roundTripSpec @(VerKeyKES (SimpleKES Ed25519DSIGN 3)) version cborTrip
              roundTripSpec @(SigKES (SimpleKES Ed25519DSIGN 3)) version cborTrip
              roundTripSpec @(SignKeyKES (SimpleKES Ed25519DSIGN 4)) version cborTrip
              roundTripSpec @(VerKeyKES (SimpleKES Ed25519DSIGN 4)) version cborTrip
              roundTripSpec @(SigKES (SimpleKES Ed25519DSIGN 4)) version cborTrip
              roundTripSpec @(SignKeyKES (SimpleKES Ed25519DSIGN 5)) version cborTrip
              roundTripSpec @(VerKeyKES (SimpleKES Ed25519DSIGN 5)) version cborTrip
              roundTripSpec @(SigKES (SimpleKES Ed25519DSIGN 5)) version cborTrip
              roundTripSpec @(SignKeyKES (SimpleKES Ed25519DSIGN 6)) version cborTrip
              roundTripSpec @(VerKeyKES (SimpleKES Ed25519DSIGN 6)) version cborTrip
              roundTripSpec @(SigKES (SimpleKES Ed25519DSIGN 6)) version cborTrip
              roundTripSpec @(SignKeyKES (SimpleKES Ed25519DSIGN 7)) version cborTrip
              roundTripSpec @(VerKeyKES (SimpleKES Ed25519DSIGN 7)) version cborTrip
              roundTripSpec @(SigKES (SimpleKES Ed25519DSIGN 7)) version cborTrip
            describe "Mock" $ do
              roundTripSpec @(SignKeyKES (MockKES 7)) version cborTrip
              roundTripSpec @(VerKeyKES (MockKES 7)) version cborTrip
              roundTripSpec @(SigKES (MockKES 7)) version cborTrip
