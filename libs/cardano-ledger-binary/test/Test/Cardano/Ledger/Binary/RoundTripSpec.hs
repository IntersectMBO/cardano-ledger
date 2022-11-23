{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
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
import Cardano.Crypto.VRF.Class (CertVRF, CertifiedVRF, OutputVRF, SignKeyVRF, VerKeyVRF)
import Cardano.Crypto.VRF.Mock (MockVRF)
import Cardano.Crypto.VRF.Praos (PraosVRF)
import Cardano.Crypto.VRF.Simple (SimpleVRF)
import Cardano.Ledger.Binary
import Cardano.Slotting.Block (BlockNo)
import Cardano.Slotting.Slot (EpochNo, EpochSize, SlotNo, WithOrigin)
import Cardano.Slotting.Time (SystemStart)
import Codec.CBOR.ByteArray (ByteArray (..))
import Codec.CBOR.ByteArray.Sliced (SlicedByteArray (..))
import Data.Fixed (Fixed (..), Nano, Pico)
import Data.Foldable as F
import Data.IP (IPv4, IPv6)
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Maybe.Strict
import qualified Data.Primitive.ByteArray as Prim (ByteArray)
import Data.Ratio
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Tagged (Tagged (Tagged))
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
spec = do
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
        let maybeNullTrip = mkTrip (encodeNullMaybe toCBOR) (decodeNullMaybe fromCBOR)
        roundTripSpec @(Maybe Integer) version maybeNullTrip
        describe "Slotting" $
          describe "Mock" $ do
            roundTripSpec @BlockNo version cborTrip
            roundTripSpec @SlotNo version cborTrip
            roundTripSpec @(WithOrigin EpochNo) version cborTrip
            roundTripSpec @EpochSize version cborTrip
            roundTripSpec @SystemStart version cborTrip
        describe "Crypto" $ do
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
            describe "OutputVRF" $ do
              roundTripSpec @(OutputVRF PraosVRF) version cborTrip
              roundTripSpec @(CertifiedVRF PraosVRF Bool) version cborTrip
            describe "Praos" $ do
              roundTripSpec @(SignKeyVRF PraosVRF) version cborTrip
              roundTripSpec @(VerKeyVRF PraosVRF) version cborTrip
              roundTripSpec @(CertVRF PraosVRF) version cborTrip
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
            -- below we also test some tuple roundtripping as well as KES
            describe "Simple" $ do
              roundTripSpec
                @( SignKeyKES (SimpleKES Ed25519DSIGN 1),
                   SignKeyKES (SimpleKES Ed25519DSIGN 2),
                   SignKeyKES (SimpleKES Ed25519DSIGN 3),
                   SignKeyKES (SimpleKES Ed25519DSIGN 4),
                   SignKeyKES (SimpleKES Ed25519DSIGN 5),
                   SignKeyKES (SimpleKES Ed25519DSIGN 6)
                 )
                version
                cborTrip
              roundTripSpec @(SignKeyKES (SimpleKES Ed25519DSIGN 7)) version cborTrip
              roundTripSpec
                @( VerKeyKES (SimpleKES Ed25519DSIGN 1),
                   VerKeyKES (SimpleKES Ed25519DSIGN 2),
                   VerKeyKES (SimpleKES Ed25519DSIGN 3),
                   VerKeyKES (SimpleKES Ed25519DSIGN 4),
                   VerKeyKES (SimpleKES Ed25519DSIGN 5),
                   VerKeyKES (SimpleKES Ed25519DSIGN 6),
                   VerKeyKES (SimpleKES Ed25519DSIGN 7)
                 )
                version
                cborTrip
              roundTripSpec
                @( SigKES (SimpleKES Ed25519DSIGN 1),
                   SigKES (SimpleKES Ed25519DSIGN 2),
                   SigKES (SimpleKES Ed25519DSIGN 3),
                   SigKES (SimpleKES Ed25519DSIGN 4)
                 )
                version
                cborTrip
              roundTripSpec
                @( SigKES (SimpleKES Ed25519DSIGN 5),
                   SigKES (SimpleKES Ed25519DSIGN 6),
                   SigKES (SimpleKES Ed25519DSIGN 7)
                 )
                version
                cborTrip
            describe "Mock" $ do
              roundTripSpec @(SignKeyKES (MockKES 7)) version cborTrip
              roundTripSpec @(VerKeyKES (MockKES 7)) version cborTrip
              roundTripSpec @(SigKES (MockKES 7)) version cborTrip
          describe "Hash" $ do
            roundTripSpec
              @( Hash Blake2b_224 (),
                 Hash Blake2b_256 (),
                 Hash SHA256 (),
                 Hash SHA3_256 (),
                 Hash Keccak256 (),
                 Hash ShortHash ()
               )
              version
              cborTrip
  describe "EmbedTrip" $ do
    forM_ [shelleyProtVer .. maxBound] $ \v ->
      describe (show v) $ do
        embedTripSpec v v (cborTrip @Word8 @Word16) $
          \n w -> n `shouldBe` fromIntegral w
        embedTripSpec v v (cborTrip @Word16 @Word32) $
          \n w -> n `shouldBe` fromIntegral w
        embedTripSpec v v (cborTrip @Word32 @Word64) $
          \n w -> n `shouldBe` fromIntegral w
        embedTripSpec v v (cborTrip @Word @Natural) $
          \n w -> n `shouldBe` fromIntegral w
        embedTripSpec v v (cborTrip @Int8 @Int16) $
          \n w -> n `shouldBe` fromIntegral w
        embedTripSpec v v (cborTrip @Int16 @Int32) $
          \n w -> n `shouldBe` fromIntegral w
        embedTripSpec v v (cborTrip @Int32 @Int64) $
          \n w -> n `shouldBe` fromIntegral w
        embedTripSpec v v (cborTrip @Int @Integer) $
          \n w -> n `shouldBe` fromIntegral w
        embedTripSpec v v (cborTrip @Int @(Tagged () Int)) $
          \(Tagged i') i -> i' `shouldBe` i
        embedTripSpec v v (cborTrip @(Maybe Word) @[Word]) $
          \xs mx -> listToMaybe xs `shouldBe` mx
        embedTripSpec v v (cborTrip @(StrictMaybe Word) @(Maybe Word)) $
          \m sm -> m `shouldBe` strictMaybeToMaybe sm
        embedTripSpec v v (cborTrip @(Maybe Word) @(StrictMaybe Word)) $
          \sm m -> sm `shouldBe` maybeToStrictMaybe m
        embedTripSpec v v (cborTrip @(Word, Word) @[Word]) $
          \xs (x, y) -> xs `shouldBe` [x, y]
        embedTripSpec v v (cborTrip @(Word, Word, Word) @[Word]) $
          \xs (x, y, z) -> xs `shouldBe` [x, y, z]
        embedTripSpec v v (cborTrip @(Word, Word, Word, Word) @[Word]) $
          \xs (a, b, c, d) -> xs `shouldBe` [a, b, c, d]
        embedTripSpec v v (cborTrip @(Word, Word, Word, Word, Word) @[Word]) $
          \xs (a, b, c, d, e) -> xs `shouldBe` [a, b, c, d, e]
        embedTripSpec v v (cborTrip @(Word, Word, Word, Word, Word, Word) @[Word]) $
          \xs (a, b, c, d, e, f) -> xs `shouldBe` [a, b, c, d, e, f]
        embedTripSpec v v (cborTrip @(VP.Vector Word) @[Word]) $
          \xs sxs -> xs `shouldBe` VP.toList sxs
        embedTripSpec v v (cborTrip @(Seq.Seq Word) @[Word]) $
          \xs sxs -> xs `shouldBe` F.toList sxs
        embedTripSpec v v (cborTrip @(SSeq.StrictSeq Word) @[Word]) $
          \xs sxs -> xs `shouldBe` F.toList sxs
        embedTripSpec v v (cborTrip @(Set.Set Word) @[Word]) $
          \xs sxs -> xs `shouldBe` Set.toList sxs
        embedTripSpec v v (cborTrip @(VMap.VMap VMap.VP VMap.VP Word Int) @(Map.Map Word Int)) $
          \xs sxs -> xs `shouldBe` VMap.toMap sxs
    forM_ [minBound .. natVersion @8] $ \v ->
      describe (show v) $ do
        embedTripSpec v v (cborTrip @Rational @(Integer, Integer)) $
          \(x, y) r -> (x, y) `shouldBe` (numerator r, denominator r)
