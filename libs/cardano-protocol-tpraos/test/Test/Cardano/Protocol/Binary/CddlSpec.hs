{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Protocol.Binary.CddlSpec (spec) where

import Cardano.Ledger.Binary.Group (CBORGroup)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley
import Cardano.Protocol.Crypto
import Cardano.Protocol.TPraos.BHeader (BHBody, BHeader)
import Cardano.Protocol.TPraos.OCert (OCert)
import Codec.CBOR.Cuddle.Huddle (Huddle)
import qualified Data.ByteString.Lazy as BSL
import Test.Cardano.Ledger.Binary.Cddl (
  beforeAllCddlFile,
  cddlDecoderEquivalenceSpec,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Binary.Cuddle
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.Binary.Cddl (readShelleyCddlFiles)
import Test.Cardano.Ledger.Shelley.CDDL (shelleyCDDL)

spec :: Spec
spec =
  describe "CDDL" $ do
    let n = 3
    specForEra @ShelleyEra readShelleyCddlFiles shelleyCDDL n

specForEra :: forall era. Era era => IO [BSL.ByteString] -> Huddle -> Int -> Spec
specForEra readCddlFiles cddlFile n = do
  describe (eraName @era) $ do
    let v = eraProtVerLow @era
    describe "Ruby-based" $
      beforeAllCddlFile n readCddlFiles $ do
        cddlRoundTripAnnCborSpec @(BHeader StandardCrypto) v "header"
        cddlRoundTripCborSpec @(BHeader StandardCrypto) v "header"
        cddlRoundTripCborSpec @(BHBody StandardCrypto) v "header_body"
        cddlRoundTripCborSpec @(CBORGroup (OCert StandardCrypto)) v "[ operational_cert ]"
        describe "DecCBOR instances equivalence via CDDL" $ do
          cddlDecoderEquivalenceSpec @(BHeader StandardCrypto) v "header"
    describe "Huddle" $ specWithHuddle cddlFile 100 $ do
      huddleRoundTripAnnCborSpec @(BHeader StandardCrypto) v "header"
      huddleRoundTripCborSpec @(BHeader StandardCrypto) v "header"
      huddleRoundTripCborSpec @(BHBody StandardCrypto) v "header_body"
      xdescribe "[ operational_cert ] doesn't roundtrip with huddle" $
        huddleRoundTripCborSpec @(CBORGroup (OCert StandardCrypto)) v "[ operational_cert ]"
      describe "DecCBOR instances equivalence via CDDL - Huddle" $ do
        huddleDecoderEquivalenceSpec @(BHeader StandardCrypto) v "header"
