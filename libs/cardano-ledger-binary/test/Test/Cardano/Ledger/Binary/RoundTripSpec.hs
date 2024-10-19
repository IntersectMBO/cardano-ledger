{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Binary.RoundTripSpec (spec) where

import Cardano.Crypto.DSIGN.Class (SigDSIGN, SignKeyDSIGN, SignedDSIGN, VerKeyDSIGN)
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
import Cardano.Crypto.KES.Class (
  KESAlgorithm,
  ContextKES,
  SigKES,
  VerKeyKES,
  SignKeyKES,
  UnsoundPureSignKeyKES,
  SeedSizeKES,
  Signable,
  genKeyKES,
  deriveVerKeyKES,
  signKES,
  forgetSignKeyKES,
 )
import Cardano.Crypto.KES.CompactSingle (CompactSingleKES)
import Cardano.Crypto.KES.CompactSum (
  CompactSum0KES,
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
import Cardano.Crypto.KES.Sum (
  Sum0KES,
  Sum1KES,
  Sum2KES,
  Sum3KES,
  Sum4KES,
  Sum5KES,
  Sum6KES,
  Sum7KES,
 )
import Cardano.Crypto.VRF.Class (CertVRF, CertifiedVRF, OutputVRF, SignKeyVRF, VerKeyVRF)
import Cardano.Crypto.VRF.Mock (MockVRF)
import Cardano.Crypto.VRF.Praos (PraosVRF)
import Cardano.Crypto.VRF.Simple (SimpleVRF)
import Cardano.Crypto.PinnedSizedBytes (PinnedSizedBytes, psbToByteString)
import Cardano.Crypto.Libsodium
import Cardano.Crypto.Libsodium.MLockedSeed
import Cardano.Ledger.Binary
import Cardano.Slotting.Block (BlockNo)
import Cardano.Slotting.Slot (EpochNo, EpochSize, SlotNo, WithOrigin)
import Cardano.Slotting.Time (SystemStart)
import Codec.CBOR.ByteArray (ByteArray (..))
import Codec.CBOR.ByteArray.Sliced (SlicedByteArray (..))
import Control.Monad (when)
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Data.Fixed (Nano, Pico)
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
import Data.Time.Clock (UTCTime)
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

withSK :: KESAlgorithm v
       => PinnedSizedBytes (SeedSizeKES v) -> (SignKeyKES v -> IO b) -> IO b
withSK seedPSB action =
  bracket
    (fmap MLockedSeed . mlsbFromByteString . psbToByteString $ seedPSB)
    mlockedSeedFinalize
    $ \seed ->
      bracket
        (genKeyKES seed)
        forgetSignKeyKES
        action

mkVerKeyKES :: KESAlgorithm v
            => PinnedSizedBytes (SeedSizeKES v)
            -> IO (VerKeyKES v)
mkVerKeyKES seedPSB =
  withSK seedPSB deriveVerKeyKES

mkSigKES :: (KESAlgorithm v, ContextKES v ~ (), Signable v BS.ByteString)
         => (PinnedSizedBytes (SeedSizeKES v), [Word8])
         -> IO (SigKES v)
mkSigKES (seedPSB, msg) =
  withSK seedPSB $ \sk -> (signKES () 0 (BS.pack msg) sk)

spec :: Spec
spec = do
  describe "RoundTrip" $ do
    roundTripSpec @() cborTrip
    roundTripSpec @Bool cborTrip
    roundTripSpec @Integer cborTrip
    roundTripSpec @Natural cborTrip
    roundTripSpec @Word cborTrip
    roundTripSpec @Word8 cborTrip
    roundTripSpec @Word16 cborTrip
    roundTripSpec @Word32 cborTrip
    roundTripSpec @Word64 cborTrip
    roundTripSpec @Int cborTrip
    roundTripSpec @Int8 cborTrip
    roundTripSpec @Int16 cborTrip
    roundTripSpec @Int32 cborTrip
    roundTripSpec @Int64 cborTrip
    roundTripSpec @Float cborTrip
    roundTripSpec @Double cborTrip
    roundTripSpec @Rational cborTrip
    roundTripSpec @Nano cborTrip
    roundTripSpec @Pico cborTrip
    roundTripSpec @UTCTime cborTrip
    roundTripSpec @IPv4 cborTrip
    roundTripSpec @IPv6 cborTrip
    roundTripSpec @(Maybe Integer) cborTrip
    roundTripSpec @(StrictMaybe Integer) cborTrip
    roundTripSpec @[Integer] cborTrip
    roundTripSpec @(V.Vector Integer) cborTrip
    roundTripSpec @(VS.Vector Int16) cborTrip
    roundTripSpec @(VP.Vector Int) cborTrip
    roundTripSpec @(VU.Vector (Bool, Word)) cborTrip
    roundTripSpec @(Set.Set Int) cborTrip
    roundTripSpec @(Map.Map Integer Int) cborTrip
    roundTripSpec @(Seq.Seq Int) cborTrip
    roundTripSpec @(SSeq.StrictSeq Int) cborTrip
    roundTripSpec @(VMap.VMap VMap.VB VMap.VS Integer Int) cborTrip
    roundTripSpec @Prim.ByteArray cborTrip
    roundTripSpec @ByteArray cborTrip
    roundTripSpec @SlicedByteArray cborTrip
    roundTripSpec @(Maybe Integer) $
      mkTrip (encodeNullMaybe encCBOR) (decodeNullMaybe decCBOR)
    roundTripSpec @(StrictMaybe Integer) $
      mkTrip (encodeNullStrictMaybe encCBOR) (decodeNullStrictMaybe decCBOR)
    describe "Slotting" $
      describe "Mock" $ do
        roundTripSpec @BlockNo cborTrip
        roundTripSpec @SlotNo cborTrip
        roundTripSpec @(WithOrigin EpochNo) cborTrip
        roundTripSpec @EpochSize cborTrip
        roundTripSpec @SystemStart cborTrip
    describe "Crypto" $ do
      describe "DSIGN" $ do
        describe "Ed25519" $ do
          roundTripSpec @(SignKeyDSIGN Ed25519DSIGN) cborTrip
          roundTripSpec @(VerKeyDSIGN Ed25519DSIGN) cborTrip
          roundTripSpec @(SigDSIGN Ed25519DSIGN) cborTrip
          roundTripSpec @(SignedDSIGN Ed25519DSIGN ()) cborTrip
        describe "Ed448" $ do
          roundTripSpec @(SignKeyDSIGN Ed448DSIGN) cborTrip
          roundTripSpec @(VerKeyDSIGN Ed448DSIGN) cborTrip
          roundTripSpec @(SigDSIGN Ed448DSIGN) cborTrip
          roundTripSpec @(SignedDSIGN Ed448DSIGN ()) cborTrip
        describe "EcdsaSecp256k1" $ do
          roundTripSpec @(SignKeyDSIGN EcdsaSecp256k1DSIGN) cborTrip
          roundTripSpec @(VerKeyDSIGN EcdsaSecp256k1DSIGN) cborTrip
          roundTripSpec @(SigDSIGN EcdsaSecp256k1DSIGN) cborTrip
          roundTripSpec @(SignedDSIGN EcdsaSecp256k1DSIGN ()) cborTrip
        describe "SchnorrSecp256k1" $ do
          roundTripSpec @(SignKeyDSIGN SchnorrSecp256k1DSIGN) cborTrip
          roundTripSpec @(VerKeyDSIGN SchnorrSecp256k1DSIGN) cborTrip
          roundTripSpec @(SigDSIGN SchnorrSecp256k1DSIGN) cborTrip
          roundTripSpec @(SignedDSIGN SchnorrSecp256k1DSIGN ()) cborTrip
        describe "Mock" $ do
          roundTripSpec @(SignKeyDSIGN MockDSIGN) cborTrip
          roundTripSpec @(VerKeyDSIGN MockDSIGN) cborTrip
          roundTripSpec @(SigDSIGN MockDSIGN) cborTrip
          roundTripSpec @(SignedDSIGN MockDSIGN ()) cborTrip
      describe "VRF" $ do
        describe "OutputVRF" $ do
          roundTripSpec @(OutputVRF PraosVRF) cborTrip
          roundTripSpec @(CertifiedVRF PraosVRF Bool) cborTrip
        describe "Praos" $ do
          roundTripSpec @(SignKeyVRF PraosVRF) cborTrip
          roundTripSpec @(VerKeyVRF PraosVRF) cborTrip
          roundTripSpec @(CertVRF PraosVRF) cborTrip
        describe "Simple" $ do
          roundTripSpec @(SignKeyVRF SimpleVRF) cborTrip
          roundTripSpec @(VerKeyVRF SimpleVRF) cborTrip
          roundTripSpec @(CertVRF SimpleVRF) cborTrip
        describe "Mock" $ do
          roundTripSpec @(SignKeyVRF MockVRF) cborTrip
          roundTripSpec @(VerKeyVRF MockVRF) cborTrip
          roundTripSpec @(CertVRF MockVRF) cborTrip
      describe "KES" $ do
        describe "CompactSingle" $ do
          roundTripSpecIO @(VerKeyKES (CompactSingleKES Ed25519DSIGN)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (CompactSingleKES Ed25519DSIGN)) (cborTripIO mkSigKES)
          roundTripSpec @(UnsoundPureSignKeyKES (CompactSingleKES Ed25519DSIGN)) cborTrip
        describe "CompactSum" $ do
          roundTripSpecIO @(VerKeyKES (CompactSum0KES Ed25519DSIGN)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (CompactSum0KES Ed25519DSIGN)) (cborTripIO mkSigKES)
          roundTripSpecIO @(VerKeyKES (CompactSum1KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (CompactSum1KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkSigKES)
          roundTripSpecIO @(VerKeyKES (CompactSum2KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (CompactSum2KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkSigKES)
          roundTripSpecIO @(VerKeyKES (CompactSum3KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (CompactSum3KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkSigKES)
          roundTripSpecIO @(VerKeyKES (CompactSum4KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (CompactSum4KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkSigKES)
          roundTripSpecIO @(VerKeyKES (CompactSum5KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (CompactSum5KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkSigKES)
          roundTripSpecIO @(VerKeyKES (CompactSum6KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (CompactSum6KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkSigKES)
          roundTripSpecIO @(VerKeyKES (CompactSum7KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (CompactSum7KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkSigKES)
        describe "Sum" $ do
          roundTripSpecIO @(VerKeyKES (Sum0KES Ed25519DSIGN)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (Sum0KES Ed25519DSIGN)) (cborTripIO mkSigKES)
          roundTripSpecIO @(VerKeyKES (Sum1KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (Sum1KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkSigKES)
          roundTripSpecIO @(VerKeyKES (Sum2KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (Sum2KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkSigKES)
          roundTripSpecIO @(VerKeyKES (Sum3KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (Sum3KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkSigKES)
          roundTripSpecIO @(VerKeyKES (Sum4KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (Sum4KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkSigKES)
          roundTripSpecIO @(VerKeyKES (Sum5KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (Sum5KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkSigKES)
          roundTripSpecIO @(VerKeyKES (Sum6KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (Sum6KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkSigKES)
          roundTripSpecIO @(VerKeyKES (Sum7KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (Sum7KES Ed25519DSIGN Blake2b_256)) (cborTripIO mkSigKES)
        -- below we also test some tuple roundtripping as well as KES
        describe "Simple" $ do
          roundTripSpecIO
            @( VerKeyKES (SimpleKES Ed25519DSIGN 1)
             , VerKeyKES (SimpleKES Ed25519DSIGN 2)
             , VerKeyKES (SimpleKES Ed25519DSIGN 3)
             , VerKeyKES (SimpleKES Ed25519DSIGN 4)
             , VerKeyKES (SimpleKES Ed25519DSIGN 5)
             , VerKeyKES (SimpleKES Ed25519DSIGN 6)
             , VerKeyKES (SimpleKES Ed25519DSIGN 7)
             ) $
            cborTripIO $
               \(s1, s2, s3, s4, s5, s6, s7) ->
                    (,,,,,,)
                      <$> mkVerKeyKES s1
                      <*> mkVerKeyKES s2
                      <*> mkVerKeyKES s3
                      <*> mkVerKeyKES s4
                      <*> mkVerKeyKES s5
                      <*> mkVerKeyKES s6
                      <*> mkVerKeyKES s7
          roundTripSpecIO
            @( SigKES (SimpleKES Ed25519DSIGN 1)
             , SigKES (SimpleKES Ed25519DSIGN 2)
             , SigKES (SimpleKES Ed25519DSIGN 3)
             , SigKES (SimpleKES Ed25519DSIGN 4)
             ) $
            cborTripIO $
               \(s1, m1, s2, m2, s3, m3, s4, m4) ->
                    (,,,)
                      <$> mkSigKES (s1, m1)
                      <*> mkSigKES (s2, m2)
                      <*> mkSigKES (s3, m3)
                      <*> mkSigKES (s4, m4)
          roundTripSpecIO
            @( SigKES (SimpleKES Ed25519DSIGN 5)
             , SigKES (SimpleKES Ed25519DSIGN 6)
             , SigKES (SimpleKES Ed25519DSIGN 7)
             ) $
            cborTripIO $
              \(s1, m1, s2, m2, s3, m3) ->
                (,,)
                  <$> mkSigKES (s1, m1)
                  <*> mkSigKES (s2, m2)
                  <*> mkSigKES (s3, m3)
        describe "Mock" $ do
          roundTripSpecIO @(VerKeyKES (MockKES 7)) (cborTripIO mkVerKeyKES)
          roundTripSpecIO @(SigKES (MockKES 7)) (cborTripIO mkSigKES)
        describe "Hash" $ do
          roundTripSpec
            @( Hash Blake2b_224 ()
             , Hash Blake2b_256 ()
             , Hash SHA256 ()
             , Hash SHA3_256 ()
             , Hash Keccak256 ()
             , Hash ShortHash ()
             )
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
        when (v < natVersion @9) $ do
          -- Starting with version 9 Set is prefixed with tag 258, which prevents it from
          -- being deserialized into a list.
          embedTripSpec v v (cborTrip @(Set.Set Word) @[Word]) $
            \xs sxs -> xs `shouldBe` Set.toList sxs
        embedTripSpec v v (cborTrip @(VMap.VMap VMap.VP VMap.VP Word Int) @(Map.Map Word Int)) $
          \xs sxs -> xs `shouldBe` VMap.toMap sxs
    forM_ [minBound .. natVersion @8] $ \v ->
      describe (show v) $ do
        embedTripSpec v v (cborTrip @Rational @(Integer, Integer)) $
          \(x, y) r -> (x, y) `shouldBe` (numerator r, denominator r)
