{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Protocol.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Allegra.HuddleSpec (allegraCDDL)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.HuddleSpec (alonzoCDDL)
import Cardano.Ledger.Binary.Group (CBORGroup)
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.HuddleSpec (maryCDDL)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.HuddleSpec (shelleyCDDL)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.BHeader (BHBody, BHeader)
import Cardano.Protocol.TPraos.OCert (OCert)
import Codec.CBOR.Cuddle.Huddle (Huddle)
import qualified Data.ByteString.Lazy as BSL
import Test.Cardano.Ledger.Allegra.Binary.Cddl (readAllegraCddlFiles)
import Test.Cardano.Ledger.Alonzo.Binary.Cddl (readAlonzoCddlFiles)
import Test.Cardano.Ledger.Binary.Cddl (beforeAllCddlFile, cddlRoundTripCborSpec)
import Test.Cardano.Ledger.Binary.Cuddle
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Binary.Cddl (readMaryCddlFiles)
import Test.Cardano.Ledger.Shelley.Binary.Cddl (readShelleyCddlFiles)
import Test.Cardano.Protocol.Binary.Annotator ()
import Test.Cardano.Protocol.Binary.Cddl (
  cddlBlockSpec,
  huddleBlockSpec,
 )

spec :: Spec
spec =
  describe "CDDL" $ do
    let n = 3
    specForEra @ShelleyEra readShelleyCddlFiles shelleyCDDL n
    specForEra @AllegraEra readAllegraCddlFiles allegraCDDL n
    specForEra @MaryEra readMaryCddlFiles maryCDDL n
    specForEra @AlonzoEra readAlonzoCddlFiles alonzoCDDL n

specForEra ::
  forall era.
  (Era era, AtMostEra "Alonzo" era) =>
  IO [BSL.ByteString] ->
  Huddle ->
  Int ->
  Spec
specForEra readCddlFiles cddlFiles n = do
  describe (eraName @era) $ do
    describe "Ruby-based" $
      beforeAllCddlFile n readCddlFiles $ do
        cddlBlockSpec @era @StandardCrypto @BHeader @BHBody
        cddlRoundTripCborSpec @(CBORGroup (OCert StandardCrypto))
          (eraProtVerLow @era)
          "[ operational_cert ]"

    describe "Huddle" $
      specWithHuddle cddlFiles 100 $ do
        huddleBlockSpec @era @StandardCrypto @BHeader @BHBody
        xdescribe "Cannot generate a CBOR term corresponding to a group with cuddle" $
          huddleRoundTripCborSpec @(CBORGroup (OCert StandardCrypto))
            (eraProtVerLow @era)
            "[ operational_cert ]"
  where
    _atMostAlonzo = atMostEra @"Alonzo" @era
