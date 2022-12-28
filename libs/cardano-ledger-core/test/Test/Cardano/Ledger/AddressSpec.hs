{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.AddressSpec (spec) where

import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Address
import Cardano.Ledger.Binary (Version, decodeFull', natVersion, serialize', toCBOR)
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto (Crypto (ADDRHASH), StandardCrypto)
import qualified Data.Binary.Put as B
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Either
import Data.Maybe (isNothing)
import Data.Proxy
import Data.Word
import Test.Cardano.Ledger.Binary.RoundTrip (cborTrip, mkTrip, roundTripExpectation)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Address
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.Utils (runFailError)

spec :: Spec
spec = do
  describe "CompactAddr" $ do
    prop "compactAddr/decompactAddr round trip" $
      propCompactAddrRoundTrip @StandardCrypto
    prop "Compact address binary representation" $
      propCompactSerializationAgree @StandardCrypto
    prop "Ensure Addr failures on incorrect binary data" $
      propDecompactErrors @StandardCrypto
    prop "Ensure RewardAcnt failures on incorrect binary data" $
      propDeserializeRewardAcntErrors @StandardCrypto
    prop "RoundTrip" $
      roundTripExpectation @(CompactAddr StandardCrypto) cborTrip
    prop "Decompact addr with junk" $ propDecompactAddrWithJunk @StandardCrypto
    prop "Same as old decompactor" $ propSameAsOldDecompactAddr @StandardCrypto
  describe "Addr" $ do
    prop "RoundTrip" $
      roundTripExpectation @(Addr StandardCrypto) cborTrip
    prop "RoundTrip (fromCborAddr)" $
      roundTripExpectation @(Addr StandardCrypto) (mkTrip toCBOR fromCborAddr)
    prop "Deserializing an address matches old implementation" $
      propValidateNewDeserialize @StandardCrypto
  describe "RewardAcnt" $ do
    prop "RewardAcnt" $
      roundTripExpectation @(RewardAcnt StandardCrypto) cborTrip
    prop "RewardAcnt (fromCborRewardAcnt)" $
      roundTripExpectation @(RewardAcnt StandardCrypto) (mkTrip toCBOR fromCborRewardAcnt)

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
  -- Add garbage to the end of serializaed non-Byron address
  let bs = case addr of
        AddrBootstrap _ -> serialiseAddr addr
        _ -> serialiseAddr addr <> junk
  -- Check this behavior all the way through Alonzo
  forM_ [minBound .. natVersion @6] $ \version -> do
    -- Encode with garbage
    let cbor = serialize' version bs
    -- Decode as compact address
    cAddr :: CompactAddr c <-
      either (error . show) pure $ decodeFull' version cbor
    -- Ensure that garbage is gone
    decompactAddr cAddr `shouldBe` addr

propValidateNewDeserialize :: forall c. (HasCallStack, Crypto c) => Addr c -> Property
propValidateNewDeserialize addr =
  let bs = serialiseAddr addr
      deserializedOld = runFailError $ deserialiseAddrOld @c bs
      deserializedNew = runFailError $ decodeAddr @c bs
   in deserializedNew === addr .&&. deserializedOld === deserializedNew

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
                , (\x -> BS.singleton (x .|. 0b10000000) <> suffix) <$> arbitrary
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

propDeserializeRewardAcntErrors :: forall c. Crypto c => Version -> RewardAcnt c -> Gen Property
propDeserializeRewardAcntErrors v acnt = do
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
    $ decodeRewardAcnt @c badAddr
