{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.AddressSpec (spec) where

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Address
import Cardano.Ledger.Binary (Version, byronProtVer, decodeFull', natVersion, serialize')
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto (Crypto (ADDRHASH), StandardCrypto)
import Cardano.Ledger.Keys (
  BootstrapWitness (..),
  bootstrapWitKeyHash,
  coerceKeyRole,
  unpackByronVKey,
 )
import Control.Monad.Trans.Fail.String (errorFail)
import qualified Data.Binary.Put as B
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Either
import Data.Maybe (isNothing)
import Data.Proxy
import Data.Word
import Test.Cardano.Ledger.Binary.RoundTrip (
  cborTrip,
  roundTripCborSpec,
  roundTripRangeExpectation,
 )
import Test.Cardano.Ledger.Common hiding ((.&.))
import Test.Cardano.Ledger.Core.Address
import Test.Cardano.Ledger.Core.Arbitrary (genAddrBadPtr, genCompactAddrBadPtr)
import Test.Cardano.Ledger.Core.KeyPair (genByronVKeyAddr)

spec :: Spec
spec =
  describe "Address" $ do
    roundTripAddressSpec
    prop "rebuild the 'addr root' using a bootstrap witness" $ do
      (byronVKey, byronAddr) <- genByronVKeyAddr
      sig <- arbitrary
      let addr = BootstrapAddress byronAddr
          (shelleyVKey, chainCode) = unpackByronVKey @StandardCrypto byronVKey
          wit :: BootstrapWitness StandardCrypto
          wit =
            BootstrapWitness
              { bwKey = shelleyVKey
              , bwChainCode = chainCode
              , bwSig = sig
              , bwAttributes = serialize' byronProtVer $ Byron.addrAttributes byronAddr
              }
      pure $
        coerceKeyRole (bootstrapKeyHash @StandardCrypto addr)
          === bootstrapWitKeyHash wit

roundTripAddressSpec :: Spec
roundTripAddressSpec = do
  describe "CompactAddr" $ do
    roundTripCborSpec @(CompactAddr StandardCrypto)
    prop "compactAddr/decompactAddr round trip" $
      forAll genAddrBadPtr $
        propCompactAddrRoundTrip @StandardCrypto
    prop "Compact address binary representation" $
      forAll genAddrBadPtr $
        propCompactSerializationAgree @StandardCrypto
    prop "Ensure Addr failures on incorrect binary data" $
      propDecompactErrors @StandardCrypto
    prop "Ensure RewardAcnt failures on incorrect binary data" $
      propDeserializeRewardAccountErrors @StandardCrypto
    prop "RoundTrip-invalid" $
      forAll genCompactAddrBadPtr $
        roundTripRangeExpectation @(CompactAddr StandardCrypto)
          cborTrip
          (natVersion @2)
          (natVersion @6)
    prop "Decompact addr with junk" $
      propDecompactAddrWithJunk @StandardCrypto
    prop "Same as old decompactor" $ propSameAsOldDecompactAddr @StandardCrypto
    it "fail on extraneous bytes" $
      decodeAddr @StandardCrypto addressWithExtraneousBytes `shouldBe` Nothing
  describe "Addr" $ do
    roundTripCborSpec @(Addr StandardCrypto)
    prop "RoundTrip-invalid" $
      forAll genAddrBadPtr $
        roundTripRangeExpectation @(Addr StandardCrypto) cborTrip (natVersion @2) (natVersion @6)
    prop "Deserializing an address matches old implementation" $
      propValidateNewDeserialize @StandardCrypto
  describe "RewardAcnt" $ do
    roundTripCborSpec @(RewardAccount StandardCrypto)

propSameAsOldDecompactAddr :: forall c. Crypto c => CompactAddr c -> Expectation
propSameAsOldDecompactAddr cAddr = do
  addr `shouldBe` decompactAddrOld @c cAddr
  addr `shouldBe` decompactAddrOldLazy @c cAddr
  where
    addr = decompactAddr @c cAddr

propDecompactAddrWithJunk ::
  forall c.
  (HasCallStack, Crypto c) =>
  Addr c ->
  BS.ByteString ->
  Expectation
propDecompactAddrWithJunk addr junk = do
  -- Add garbage to the end of serialized non-Byron address
  bs <- case addr of
    AddrBootstrap _ -> pure $ serialiseAddr addr
    _ -> do
      let bs = serialiseAddr addr <> junk
      -- ensure we fail decoding of compact addresses with junk at the end
      when (BS.length junk > 0) $ do
        forM_ [natVersion @7 .. maxBound] $ \version -> do
          let cbor = serialize' version bs
          forM_ (decodeFull' version cbor) $ \(cAddr :: CompactAddr c) ->
            expectationFailure $
              unlines
                [ "Decoding with version: " ++ show version
                , "unexpectedly was able to parse an address with junk at the end: "
                , show cbor
                , "as: "
                , show cAddr
                ]
      pure bs
  -- Ensure we drop off the junk at the end all the way through Alonzo
  forM_ [minBound .. natVersion @6] $ \version -> do
    -- Encode with garbage
    let cbor = serialize' version bs
    -- Decode as compact address
    cAddr :: CompactAddr c <-
      either (error . show) pure $ decodeFull' version cbor
    -- Ensure that garbage is gone (decodeAddr will fail otherwise)
    decodeAddr (serialiseAddr (decompactAddr cAddr)) `shouldReturn` addr

propValidateNewDeserialize :: forall c. (HasCallStack, Crypto c) => Addr c -> Property
propValidateNewDeserialize addr = property $ do
  let bs = serialiseAddr addr
      deserializedOld = errorFail $ deserialiseAddrOld @c bs
      deserializedNew = errorFail $ decodeAddr @c bs
  deserializedNew `shouldBe` addr
  deserializedOld `shouldBe` deserializedNew

propCompactAddrRoundTrip :: Crypto c => Addr c -> Property
propCompactAddrRoundTrip addr =
  let compact = compactAddr addr
      decompact = decompactAddr compact
   in addr === decompact

propCompactSerializationAgree :: Addr c -> Property
propCompactSerializationAgree addr =
  let sbs = unCompactAddr $ compactAddr addr
   in sbs === SBS.toShort (serialiseAddr addr)

propDecompactErrors :: forall c. Crypto c => Addr c -> Gen Property
propDecompactErrors addr = do
  let sbs = unCompactAddr $ compactAddr addr
      hashLen = fromIntegral $ Hash.sizeHash (Proxy :: Proxy (ADDRHASH c))
      bs = SBS.fromShort sbs
      flipHeaderBit b =
        case BS.uncons bs of
          Just (h, bsTail) -> BS.cons (complementBit h b) bsTail
          Nothing -> error "Impossible: CompactAddr can't be empty"
      mingleHeader = do
        b <- elements $ case addr of
          Addr {} -> [1, 2, 3, 7]
          AddrBootstrap {} -> [0 .. 7]
        pure ("Header", flipHeaderBit b)
      mingleAddLength = do
        NonEmpty xs <- arbitrary
        pure ("Add Length", bs <> BS.pack xs)
      mingleDropLength = do
        n <- chooseInt (1, BS.length bs)
        pure ("Drop Length", BS.take (BS.length bs - n) bs)
      mingleStaking = do
        let (prefix, suffix) = BS.splitAt (1 + hashLen) bs
            genBad32 =
              putVariableLengthWord64
                <$> choose (fromIntegral (maxBound :: Word32) + 1, maxBound :: Word64)
            genBad16 =
              putVariableLengthWord64
                <$> choose (fromIntegral (maxBound :: Word16) + 1, maxBound :: Word64)
            genGood32 =
              putVariableLengthWord64 . (fromIntegral :: Word32 -> Word64) <$> arbitrary
            genGood16 =
              putVariableLengthWord64 . (fromIntegral :: Word16 -> Word64) <$> arbitrary
            serializeSuffix xs = BSL.toStrict . B.runPut . mconcat <$> sequence xs
        case addr of
          Addr _ _ StakeRefPtr {} -> do
            newSuffix <-
              oneof
                [ serializeSuffix [genBad32, genGood16, genGood16]
                , serializeSuffix [genGood32, genBad16, genGood16]
                , serializeSuffix [genGood32, genGood16, genBad16]
                , serializeSuffix [genGood32, genGood16, genGood16, genGood16]
                , -- We need to reset the first bit, to indicate that no more bytes do
                  -- follow. Besides the fact that the original suffix is retained, this
                  -- is similar to:
                  --
                  -- serializeSuffix [genGood8, genGood32, genGood16, genGood16]
                  (\x -> BS.singleton (x .&. 0b01111111) <> suffix) <$> arbitrary
                ]
            pure ("Mingle Ptr", prefix <> newSuffix)
          Addr _ _ StakeRefNull {} -> do
            NonEmpty xs <- arbitrary
            pure ("Bogus Null Ptr", prefix <> BS.pack xs)
          Addr _ _ StakeRefBase {} -> do
            xs <- arbitrary
            let xs' = if length xs == hashLen then 0 : xs else xs
            pure ("Bogus Staking", prefix <> BS.pack xs')
          AddrBootstrap {} -> pure ("Bogus Bootstrap", BS.singleton 0b10000000 <> bs)
  (mingler, badAddr) <-
    oneof
      [ mingleHeader
      , mingleAddLength
      , mingleDropLength
      , mingleStaking
      ]
  pure
    $ counterexample
      ("Mingled address with " ++ mingler ++ " was parsed: " ++ show badAddr)
    $ isLeft
    $ decodeAddrEither @c badAddr

propDeserializeRewardAccountErrors ::
  forall c. Crypto c => Version -> RewardAccount c -> Gen Property
propDeserializeRewardAccountErrors v acnt = do
  let bs = serialize' v acnt
      flipHeaderBit b =
        case BS.uncons bs of
          Just (h, bsTail) -> BS.cons (complementBit h b) bsTail
          Nothing -> error "Impossible: CompactAddr can't be empty"
      mingleHeader = do
        b <- elements [1, 2, 3, 5, 6, 7]
        pure ("Header", flipHeaderBit b)
      mingleAddLength = do
        NonEmpty xs <- arbitrary
        pure ("Add Length", bs <> BS.pack xs)
      mingleDropLength = do
        n <- chooseInt (1, BS.length bs)
        pure ("Drop Length", BS.take (BS.length bs - n) bs)
  (mingler, badAddr) <-
    oneof
      [ mingleHeader
      , mingleAddLength
      , mingleDropLength
      ]
  pure
    $ counterexample
      ("Mingled address with " ++ mingler ++ " was parsed: " ++ show badAddr)
    $ isNothing
    $ decodeRewardAccount @c badAddr

addressWithExtraneousBytes :: HasCallStack => BS.ByteString
addressWithExtraneousBytes = bs
  where
    bs = case B16.decode hs of
      Left e -> error $ show e
      Right x -> x
    hs =
      "01AA5C8B35A934ED83436ABB56CDB44878DAC627529D2DA0B59CDA794405931B9359\
      \46E9391CABDFFDED07EB727F94E9E0F23739FF85978905BD460158907C589B9F1A62"
