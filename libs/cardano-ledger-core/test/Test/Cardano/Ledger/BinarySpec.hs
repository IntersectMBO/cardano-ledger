{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.BinarySpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Hashes (EraIndependentData, SafeHash, ScriptHash)
import Cardano.Ledger.Keys
import Cardano.Ledger.Metadata (Metadatum)
import Cardano.Ledger.TxIn
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import Data.Word (Word32, Word64)
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Binary (decoderEquivalenceSpec)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (E))
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.Binary.Annotator ()

spec :: Spec
spec = do
  describe "RoundTrip" $ do
    roundTripCborSpec @Coin
    roundTripCborSpec @(CompactForm Coin)
    prop "Encode CompactCoin - Decode Coin" $
      roundTripExpectation @Coin (mkTrip (encCBOR . compactCoinOrError) decCBOR)
    prop "Encode Coin - Decode CompactCoin" $
      roundTripExpectation @Coin (mkTrip encCBOR (fromCompact <$> decCBOR))
    roundTripCborSpec @ProtVer
    prop "ProtVer/Word32" $ do
      let badProtVerGen =
            ProtVer
              <$> arbitrary
              <*> ( fromIntegral @Word64 @Natural
                      <$> choose (fromIntegral (maxBound :: Word32) + 1, fromIntegral (maxBound :: Word64) * 2)
                  )
      forAll badProtVerGen $
        roundTripCborRangeFailureExpectation (natVersion @12) maxBound
    roundTripCborSpec @Nonce
    roundTripCborSpec @Url
    roundTripCborSpec @DnsName
    roundTripCborSpec @Port
    roundTripCborSpec @ActiveSlotCoeff
    roundTripCborSpec @Network
    roundTripCborSpec @BlocksMade
    roundTripCborSpec @TxIx
    roundTripCborSpec @CertIx
    roundTripCborSpec @Anchor
    roundTripAnnCborSpec @BootstrapWitness
    roundTripCborSpec @BootstrapWitness
    roundTripCborSpec @TxId
    roundTripCborSpec @GenDelegPair
    roundTripCborSpec @GenDelegs
    roundTripCborSpec @DRepState
    roundTripCborSpec @DRep
    roundTripCborSpec @ScriptHash
    roundTripCborSpec @(SafeHash EraIndependentData)

  describe "Metadatum" $ do
    prop "Accepts bytes up to 64 bytes" $
      forAll (choose (0, 64)) $ \n ->
        decodeFull @Metadatum shelleyProtVer (toLazyByteString $ toCBOR $ E (BS.replicate n 0))
          `shouldSatisfy` isRight
    prop "Rejects bytes exceeding 64 bytes" $
      forAll (choose (65, 1000)) $ \n ->
        decodeFull @Metadatum shelleyProtVer (toLazyByteString $ toCBOR $ E (BS.replicate n 0))
          `shouldSatisfy` isLeft
    prop "Accepts text up to 64 bytes" $
      forAll (choose (0, 64)) $ \n ->
        decodeFull @Metadatum shelleyProtVer (toLazyByteString $ toCBOR $ E (T.replicate n "a"))
          `shouldSatisfy` isRight
    prop "Rejects text exceeding 64 bytes" $
      forAll (choose (65, 1000)) $ \n ->
        decodeFull @Metadatum shelleyProtVer (toLazyByteString $ toCBOR $ E (T.replicate n "a"))
          `shouldSatisfy` isLeft

  describe "DecCBOR instances equivalence" $ do
    decoderEquivalenceSpec @BootstrapWitness minBound maxBound
    decoderEquivalenceSpec @(WitVKey Witness) minBound maxBound
    decoderEquivalenceSpec @PV1.Data minBound maxBound
