{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Protocol.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Binary.Group (CBORGroup)
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.BHeader (BHBody, BHeader)
import Cardano.Protocol.TPraos.OCert (OCert)
import qualified Data.ByteString.Lazy as BSL
import Test.Cardano.Ledger.Allegra.Binary.Cddl (readAllegraCddlFiles)
import Test.Cardano.Ledger.Alonzo.Binary.Cddl (readAlonzoCddlFiles)
import Test.Cardano.Ledger.Binary.Cddl (beforeAllCddlFile, cddlRoundTripCborSpec)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Binary.Cddl (readMaryCddlFiles)
import Test.Cardano.Ledger.Shelley.Binary.Cddl (readShelleyCddlFiles)
import Test.Cardano.Protocol.Binary.Cddl (cddlEraSpec)

spec :: Spec
spec =
  describe "CDDL" $ do
    let n = 3
    specForEra @ShelleyEra readShelleyCddlFiles n
    specForEra @AllegraEra readAllegraCddlFiles n
    specForEra @MaryEra readMaryCddlFiles n
    specForEra @AlonzoEra readAlonzoCddlFiles n

specForEra ::
  forall era.
  (Era era, AtMostEra AlonzoEra era) =>
  IO [BSL.ByteString] ->
  Int ->
  Spec
specForEra readCddlFiles n = do
  describe (eraName @era) $ do
    -- TODO: add Huddle round trip tests
    describe "Ruby-based" $
      beforeAllCddlFile n readCddlFiles $ do
        cddlBlockSpec @era @StandardCrypto @BHeader @BHBody
        cddlRoundTripCborSpec @(CBORGroup (OCert StandardCrypto))
          (eraProtVerLow @era)
          "[ operational_cert ]"
