{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.BinarySpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Cardano.Ledger.Crypto
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Hashes (EraIndependentData, ScriptHash)
import Cardano.Ledger.Keys
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.TxIn
import Cardano.Ledger.UMap (RDPair)
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

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
    roundTripCborSpec @Nonce
    roundTripCborSpec @Url
    roundTripCborSpec @DnsName
    roundTripCborSpec @Port
    roundTripCborSpec @ActiveSlotCoeff
    roundTripCborSpec @Network
    roundTripCborSpec @(BlocksMade StandardCrypto)
    roundTripCborSpec @TxIx
    roundTripCborSpec @CertIx
    roundTripCborSpec @(Anchor StandardCrypto)
    roundTripAnnCborSpec @(BootstrapWitness StandardCrypto)
    roundTripCborSpec @(TxId StandardCrypto)
    roundTripCborSpec @(GenDelegPair StandardCrypto)
    roundTripCborSpec @(GenDelegs StandardCrypto)
    roundTripCborSpec @(DRepState StandardCrypto)
    roundTripCborSpec @(DRep StandardCrypto)
    roundTripCborSpec @RDPair
    roundTripCborSpec @(ScriptHash StandardCrypto)
    roundTripCborSpec @(SafeHash StandardCrypto EraIndependentData)
