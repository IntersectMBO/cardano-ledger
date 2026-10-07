{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Cardano.Ledger.AddressSpec (spec) where

import Cardano.Base.Bytes (byteArrayFromByteString)
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Binary (
  DecoderError,
  Version,
  byronProtVer,
  decodeFull',
  decodeFullDecoder,
  natVersion,
  serialize',
 )
import Cardano.Ledger.Credential
import Cardano.Ledger.Hashes (ADDRHASH)
import Cardano.Ledger.Keys (
  BootstrapWitness (..),
  bootstrapWitKeyHash,
  coerceKeyRole,
  unpackByronVKey,
 )
import Cardano.Ledger.State (getScriptHash)
import Control.Monad.Trans.Fail.String (errorFail)
import Control.Monad.Trans.State.Strict (evalStateT)
import qualified Data.Aeson as Aeson
import qualified Data.Binary.Put as B
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Either
import qualified Data.Map.Strict as Map
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
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.KeyPair (genByronVKeyAddr)
import Test.QuickCheck.Classes (
  commutativeMonoidLaws,
  commutativeSemigroupLaws,
  exponentialSemigroupLaws,
  lawsCheckOne,
  monoidLaws,
  semigroupLaws,
 )

spec :: Spec
spec =
  describe "Address" $ do
    roundTripAddressSpec
    protectedAddressSpec
    prop "rebuild the 'addr root' using a bootstrap witness" $ do
      (byronVKey, byronAddr) <- genByronVKeyAddr
      sig <- arbitrary
      let addr = BootstrapAddress byronAddr
          (shelleyVKey, chainCode) = unpackByronVKey byronVKey
          wit :: BootstrapWitness
          wit =
            BootstrapWitness
              { bwKey = shelleyVKey
              , bwChainCode = chainCode
              , bwSignature = sig
              , bwAttributes =
                  byteArrayFromByteString $ serialize' byronProtVer $ Byron.addrAttributes byronAddr
              }
      pure $
        coerceKeyRole (bootstrapKeyHash addr)
          === bootstrapWitKeyHash wit

roundTripAddressSpec :: Spec
roundTripAddressSpec = do
  describe "CompactAddr" $ do
    roundTripCborSpec @CompactAddr
    prop "compactAddr/decompactAddr round trip" $
      forAll arbitrary propCompactAddrRoundTrip
    prop "Compact address binary representation" $
      forAll arbitrary propCompactSerializationAgree
    prop "Ensure Addr failures on incorrect binary data" $
      propDecompactErrors
    prop "Ensure AccountAddress failures on incorrect binary data" $
      propDeserializeAccountAddressErrors
    prop "RoundTrip-invalid" $
      forAll arbitrary $
        roundTripRangeExpectation @CompactAddr
          cborTrip
          (natVersion @2)
          (natVersion @6)
    prop "Decompact addr with junk" $
      propDecompactAddrWithJunk
    prop "Same as old decompactor" $ propSameAsOldDecompactAddr
    it "fail on extraneous bytes" $
      decodeAddr addressWithExtraneousBytes `shouldBe` Nothing
  describe "Addr" $ do
    roundTripCborSpec @Addr
    prop "RoundTrip-invalid" $
      forAll arbitrary $
        roundTripRangeExpectation @Addr cborTrip (natVersion @2) (natVersion @6)
    prop "Deserializing an address matches old implementation" $
      propValidateNewDeserialize
  describe "AccountAddress" $ do
    roundTripCborSpec @AccountAddress
  describe "Withdrawals" $ do
    it "Semigroup and Monoid" $
      lawsCheckOne
        (Proxy :: Proxy Withdrawals)
        [ semigroupLaws
        , commutativeSemigroupLaws
        , exponentialSemigroupLaws
        , monoidLaws
        , commutativeMonoidLaws
        ]
  describe "DirectDeposits" $ do
    it "Semigroup and Monoid" $
      lawsCheckOne
        (Proxy :: Proxy DirectDeposits)
        [ semigroupLaws
        , commutativeSemigroupLaws
        , exponentialSemigroupLaws
        , monoidLaws
        , commutativeMonoidLaws
        ]

propSameAsOldDecompactAddr :: CompactAddr -> Expectation
propSameAsOldDecompactAddr cAddr = do
  addr `shouldBe` decompactAddrOld cAddr
  addr `shouldBe` decompactAddrOldLazy cAddr
  where
    addr = decompactAddr cAddr

propDecompactAddrWithJunk ::
  HasCallStack =>
  Addr ->
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
          forM_ (decodeFull' version cbor) $ \(cAddr :: CompactAddr) ->
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
    cAddr :: CompactAddr <-
      either (error . show) pure $ decodeFull' version cbor
    -- Ensure that garbage is gone (decodeAddr will fail otherwise)
    decodeAddr (serialiseAddr (decompactAddr cAddr)) `shouldReturn` addr

propValidateNewDeserialize :: HasCallStack => Addr -> Property
propValidateNewDeserialize addr = property $ do
  let bs = serialiseAddr addr
      deserializedOld = errorFail $ deserialiseAddrOld bs
      deserializedNew = errorFail $ decodeAddr bs
  deserializedNew `shouldBe` addr
  deserializedOld `shouldBe` deserializedNew

propCompactAddrRoundTrip :: Addr -> Property
propCompactAddrRoundTrip addr =
  let compact = compactAddr addr
      decompact = decompactAddr compact
   in addr === decompact

propCompactSerializationAgree :: Addr -> Property
propCompactSerializationAgree addr =
  let sbs = unCompactAddr $ compactAddr addr
   in sbs === SBS.toShort (serialiseAddr addr)

propDecompactErrors :: Addr -> Gen Property
propDecompactErrors addr = do
  let sbs = unCompactAddr $ compactAddr addr
      hashLen = fromIntegral $ Hash.hashSize (Proxy :: Proxy ADDRHASH)
      bs = SBS.fromShort sbs
      flipHeaderBit b =
        case BS.uncons bs of
          Just (h, bsTail) -> BS.cons (complementBit h b) bsTail
          Nothing -> error "Impossible: CompactAddr can't be empty"
      mingleHeader = do
        b <- elements $ case addr of
          Addr {} -> [1, 2, 7]
          AddrProtected {} -> [1, 2, 7]
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
        case shelleyAddressView addr of
          Just (_, _, _, StakeRefPtr {}) -> do
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
          Just (_, _, _, StakeRefNull {}) -> do
            NonEmpty xs <- arbitrary
            pure ("Bogus Null Ptr", prefix <> BS.pack xs)
          Just (_, _, _, StakeRefBase {}) -> do
            xs <- arbitrary
            let xs' = if length xs == hashLen then 0 : xs else xs
            pure ("Bogus Staking", prefix <> BS.pack xs')
          Nothing -> pure ("Bogus Bootstrap", BS.singleton 0b10000000 <> bs)
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
    $ decodeAddrEither badAddr

propDeserializeAccountAddressErrors :: Version -> AccountAddress -> Gen Property
propDeserializeAccountAddressErrors v acnt = do
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
    $ decodeAccountAddress badAddr

addressWithExtraneousBytes :: HasCallStack => BS.ByteString
addressWithExtraneousBytes = bs
  where
    bs = case B16.decode hs of
      Left e -> error $ show e
      Right x -> x
    hs =
      "01AA5C8B35A934ED83436ABB56CDB44878DAC627529D2DA0B59CDA794405931B9359\
      \46E9391CABDFFDED07EB727F94E9E0F23739FF85978905BD460158907C589B9F1A62"

-- Fixed wire vectors keep the oracle independent of the address encoder.
protectedAddressSpec :: Spec
protectedAddressSpec = describe "Protected addresses" $ do
  let fixed header size = BS.cons header (BS.replicate size 0)
      validHeader h = h .&. 0x86 == 0 && (not (testBit h 3) || not (testBit h 6) || testBit h 5)
      payloadSize h
        | not (testBit h 6) = 56
        | testBit h 5 = 28
        | otherwise = 31
      versions = [natVersion @2, natVersion @6, natVersion @7, natVersion @9, natVersion @11]
      protectedBytes =
        [ fixed h (payloadSize h)
        | h <- [0x08, 0x09, 0x18, 0x19, 0x28, 0x29, 0x38, 0x39, 0x68, 0x69, 0x78, 0x79]
        ]
  it "classifies all 256 headers for independently constructed payloads" $
    forM_ [0 .. 255] $ \h -> do
      let bytes = fixed h (payloadSize h)
      isRight (decodeAddrEither bytes) `shouldBe` validHeader h
  it "preserves protected full and compact identity and JSON keys" $
    forM_ protectedBytes $ \bytes -> do
      addr <- either error pure (decodeAddrEither bytes)
      serialiseAddr addr `shouldBe` bytes
      decompactAddr (compactAddr addr) `shouldBe` addr
      isProtectedCompactAddr (compactAddr addr) `shouldBe` True
      fmap (\(protection, _, _, _) -> protection) (shelleyAddressView addr) `shouldBe` Just Protected
      let ordinaryBytes = BS.cons (clearBit (BS.head bytes) 3) (BS.tail bytes)
      ordinary <- either error pure (decodeAddrEither ordinaryBytes)
      addr `shouldNotBe` ordinary
      Aeson.encode addr `shouldNotBe` Aeson.encode ordinary
      Aeson.decode (Aeson.encode (Map.singleton addr (1 :: Int)))
        `shouldBe` Just (Map.singleton addr (1 :: Int))
      protectAddress ordinary `shouldBe` Right addr
  it "matches the CIP-19 payload-derived protected vectors and the historical address oracle" $ do
    -- Original CIP-19 vectors at CIPs b4a593c960f2751fef2ddc8df28bec7b22c68eb5.
    -- The CIP-160 amendment uses these payloads unchanged and adds only bit 3.
    -- Use the established old parser to supply an independent credential/family oracle.
    let originalMainnetHex =
          [ "019493315cd92eb5d8c4304e67b7e16ae36d61d34502694657811a2c8e337b62cfff6403a06a3acbc34f8c46003c69fe79a3628cefa9c47251"
          , "11c37b1b5dc0669f1d3c61a6fddb2e8fde96be87b881c60bce8e8d542f337b62cfff6403a06a3acbc34f8c46003c69fe79a3628cefa9c47251"
          , "219493315cd92eb5d8c4304e67b7e16ae36d61d34502694657811a2c8ec37b1b5dc0669f1d3c61a6fddb2e8fde96be87b881c60bce8e8d542f"
          , "31c37b1b5dc0669f1d3c61a6fddb2e8fde96be87b881c60bce8e8d542fc37b1b5dc0669f1d3c61a6fddb2e8fde96be87b881c60bce8e8d542f"
          , "619493315cd92eb5d8c4304e67b7e16ae36d61d34502694657811a2c8e"
          , "71c37b1b5dc0669f1d3c61a6fddb2e8fde96be87b881c60bce8e8d542f"
          ]
    forM_ originalMainnetHex $ \hex -> do
      mainnetBytes <- either error pure (B16.decode hex)
      forM_ [(Mainnet, BS.head mainnetBytes), (Testnet, clearBit (BS.head mainnetBytes) 0)] $ \(network, header) -> do
        let ordinaryBytes = BS.cons header (BS.tail mainnetBytes)
            protectedVectorBytes = BS.cons (setBit header 3) (BS.tail mainnetBytes)
        ordinary <- deserialiseAddrOld ordinaryBytes
        expected <- case ordinary of
          Addr n payment stake -> do
            n `shouldBe` network
            pure (AddrProtected n payment stake)
          _ -> expectationFailure "CIP-19 base/enterprise vector decoded as bootstrap" >> pure ordinary
        decodeAddrEither protectedVectorBytes `shouldBe` Right expected
        serialiseAddr expected `shouldBe` protectedVectorBytes
        BS.tail (serialiseAddr expected) `shouldBe` BS.tail ordinaryBytes
        decompactAddr (compactAddr expected) `shouldBe` expected
        decodeFull' (natVersion @12) (serialize' (natVersion @12) protectedVectorBytes)
          `shouldBe` Right expected
        decodeFullDecoder
          (natVersion @7)
          "stored CIP-19-derived address"
          fromCborStoredBothAddr
          (BSL.fromStrict (serialize' (natVersion @7) protectedVectorBytes))
          `shouldBe` Right (expected, compactAddr expected)
  it "keeps protected transaction decoding gated while permitting stored state" $
    forM_ protectedBytes $ \bytes -> do
      addr <- either error pure (decodeAddrEither bytes)
      forM_ versions $ \version -> do
        let encoded = serialize' version bytes
        (decodeFull' version encoded :: Either DecoderError Addr) `shouldSatisfy` isLeft
        decodeFullDecoder version "stored address" fromCborStoredBothAddr (BSL.fromStrict encoded)
          `shouldBe` Right (addr, compactAddr addr)
        decodeFullDecoder version "backwards address" fromCborBackwardsBothAddr (BSL.fromStrict encoded)
          `shouldSatisfy` isLeft
        decodeFullDecoder
          version
          "rigorous address"
          (fromCborRigorousBothAddr True)
          (BSL.fromStrict encoded)
          `shouldSatisfy` isLeft
      decodeFull' (natVersion @12) (serialize' (natVersion @12) bytes) `shouldBe` Right addr
  it "preserves version-12 indefinite byte-string support and old rejection" $
    forM_ [fixed 0x00 56, fixed 0x08 56] $ \bytes -> do
      addr <- either error pure (decodeAddrEither bytes)
      let encoded =
            BSL.singleton 0x5f
              <> BSL.fromStrict (serialize' (natVersion @12) (BS.take 15 bytes))
              <> BSL.fromStrict (serialize' (natVersion @12) (BS.drop 15 bytes))
              <> BSL.singleton 0xff
      decodeFullDecoder (natVersion @12) "indefinite address" fromCborBothAddr encoded
        `shouldBe` Right (addr, compactAddr addr)
      decodeFullDecoder (natVersion @12) "indefinite state address" fromCborStoredBothAddr encoded
        `shouldBe` Right (addr, compactAddr addr)
      forM_ [natVersion @2, natVersion @7, natVersion @9, natVersion @11] $ \version -> do
        decodeFullDecoder version "indefinite address" fromCborBothAddr encoded `shouldSatisfy` isLeft
        decodeFullDecoder version "indefinite state address" fromCborStoredBothAddr encoded
          `shouldSatisfy` isLeft
  it "preserves historical trailing-byte and pointer normalization policies" $ do
    let ordinaryBase = fixed 0x00 56
        baseWithJunk = ordinaryBase <> BS.pack [0xff, 0x00]
        oversizedPointer = fixed 0x40 28 <> BS.pack [0x90, 0x80, 0x80, 0x80, 0x00, 0x00, 0x00]
        normalizedPointer = fixed 0x40 31
        decodePair version bytes =
          decodeFullDecoder
            version
            "address pair"
            fromCborBothAddr
            (BSL.fromStrict (serialize' version bytes))
    baseAddr <- either error pure (decodeAddrEither ordinaryBase)
    pointerAddr <- either error pure (decodeAddrEither normalizedPointer)
    forM_ [natVersion @2, natVersion @6] $ \version -> do
      decodePair version baseWithJunk `shouldBe` Right (baseAddr, compactAddr baseAddr)
      case decodePair version oversizedPointer of
        Left err -> expectationFailure (show err)
        Right (addr, cAddr) -> do
          addr `shouldBe` pointerAddr
          cAddr `shouldBe` compactAddr pointerAddr
    forM_ [natVersion @7, natVersion @9, natVersion @12] $ \version ->
      decodePair version baseWithJunk `shouldSatisfy` isLeft
    decodePair (natVersion @7) oversizedPointer `shouldBe` Right (pointerAddr, compactAddr pointerAddr)
    forM_ [natVersion @9, natVersion @12] $ \version ->
      decodePair version oversizedPointer `shouldSatisfy` isLeft
    decodeAddrEither oversizedPointer `shouldSatisfy` isLeft
    -- The stored-state wrapper recovers both historical cases at every version.
    forM_ [natVersion @2, natVersion @7, natVersion @9, natVersion @12] $ \version -> do
      decodeFullDecoder
        version
        "stored address"
        fromCborStoredBothAddr
        (BSL.fromStrict (serialize' version baseWithJunk))
        `shouldBe` Right (baseAddr, compactAddr baseAddr)
      case decodeFullDecoder
        version
        "stored pointer"
        fromCborStoredBothAddr
        (BSL.fromStrict (serialize' version oversizedPointer)) of
        Left err -> expectationFailure (show err)
        Right (addr, cAddr) -> do
          addr `shouldBe` pointerAddr
          decompactAddr cAddr `shouldBe` pointerAddr
  it "rejects protected pointers and trailing bytes through lenient entry points" $
    forM_ [fixed 0x48 31, fixed 0x59 31, fixed 0x08 57, fixed 0x69 29] $ \bytes -> do
      decodeAddrEither bytes `shouldSatisfy` isLeft
      decodeFullDecoder
        (natVersion @2)
        "stored address"
        fromCborStoredBothAddr
        (BSL.fromStrict (serialize' (natVersion @2) bytes))
        `shouldSatisfy` isLeft
  prop "retains unsupported constructor combinations for phase-1 validation" $ \network payment pointer -> do
    let addr = AddrProtected network payment (StakeRefPtr pointer)
    decompactAddr (compactAddr addr) `shouldBe` addr
    let bytes = serialiseAddr addr
        shortBytes = SBS.toShort bytes
    decodeAddrEither bytes `shouldSatisfy` isLeft
    (decodeAddr bytes :: Maybe Addr) `shouldBe` Nothing
    (evalStateT (decodeAddrStateT shortBytes) 0 :: Maybe Addr) `shouldBe` Nothing
    (evalStateT (decodeAddrStateLenientT True True shortBytes) 0 :: Maybe Addr) `shouldBe` Nothing
    forM_ [natVersion @2, natVersion @7, natVersion @9, natVersion @12] $ \version -> do
      let encoded = BSL.fromStrict (serialize' version bytes)
      decodeFullDecoder version "address" fromCborAddr encoded `shouldSatisfy` isLeft
      decodeFullDecoder version "compact address" fromCborCompactAddr encoded `shouldSatisfy` isLeft
      decodeFullDecoder version "address pair" fromCborBothAddr encoded `shouldSatisfy` isLeft
      decodeFullDecoder version "stored address" fromCborStoredBothAddr encoded `shouldSatisfy` isLeft
      decodeFullDecoder version "backwards address" fromCborBackwardsBothAddr encoded
        `shouldSatisfy` isLeft
      forM_ [False, True] $ \lenient ->
        decodeFullDecoder version "rigorous address" (fromCborRigorousBothAddr lenient) encoded
          `shouldSatisfy` isLeft
  it "does not force credential fields to inspect network or protection" $ do
    let addr = AddrProtected Mainnet (error "payment forced") (error "stake forced")
    getNetwork addr `shouldBe` Mainnet
    fmap (\(protection, _, _, _) -> protection) (shelleyAddressView addr) `shouldBe` Just Protected
    fmap (\(_, network, _, _) -> network) (shelleyAddressView addr) `shouldBe` Just Mainnet
  prop "payment-only and stake-only inspection retain laziness" $ \payment stake -> do
    let paymentAddr = AddrProtected (error "network forced") payment (error "stake forced")
        stakeAddr = AddrProtected (error "network forced") (error "payment forced") stake
    fmap (\(_, _, pc, _) -> pc) (shelleyAddressView paymentAddr) `shouldBe` Just payment
    fmap (\(_, _, _, sr) -> sr) (shelleyAddressView stakeAddr) `shouldBe` Just stake
    getScriptHash paymentAddr `shouldBe` case payment of
      ScriptHashObj h -> Just h
      KeyHashObj _ -> Nothing
  prop "rejects protecting pointer and bootstrap addresses" $ \addr ->
    case addr of
      Addr _ _ (StakeRefPtr _) -> protectAddress addr `shouldSatisfy` isLeft
      AddrBootstrap _ -> protectAddress addr `shouldSatisfy` isLeft
      _ -> pure ()
