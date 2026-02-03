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
import Test.Cardano.Ledger.Binary.Cuddle
import Test.Cardano.Ledger.Common
import Test.Cardano.Protocol.Binary.Annotator ()
import Test.Cardano.Protocol.Binary.Cddl (huddleBlockSpec)

spec :: Spec
spec =
  describe "CDDL" $ do
    specForEra @ShelleyEra shelleyCDDL
    specForEra @AllegraEra allegraCDDL
    specForEra @MaryEra maryCDDL
    specForEra @AlonzoEra alonzoCDDL

specForEra ::
  forall era.
  (Era era, AtMostEra "Alonzo" era) =>
  Huddle ->
  Spec
specForEra cddlFiles = do
  describe (eraName @era) $ do
    describe "Huddle" $
      specWithHuddle cddlFiles $ do
        huddleBlockSpec @era @StandardCrypto @BHeader @BHBody
        xdescribe "Cannot generate a CBOR term corresponding to a group with cuddle" $
          huddleRoundTripCborSpec @(CBORGroup (OCert StandardCrypto))
            (eraProtVerLow @era)
            "[ operational_cert ]"
  where
    _atMostAlonzo = atMostEra @"Alonzo" @era
