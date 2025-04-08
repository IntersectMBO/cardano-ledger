{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.Binary.CddlSpec (spec) where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
import Test.Cardano.Ledger.Binary.Cddl (
  beforeAllCddlFile,
  cddlDecoderEquivalenceSpec,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Binary.Cuddle
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Binary.Annotator ()
import Test.Cardano.Ledger.Mary.Binary.Cddl (readMaryCddlFiles)
import Test.Cardano.Ledger.Mary.CDDL (maryCDDL)

spec :: Spec
spec =
  describe "CDDL" $ do
    let v = eraProtVerLow @MaryEra
    describe "Ruby-based" $ beforeAllCddlFile 3 readMaryCddlFiles $ do
      cddlRoundTripCborSpec @(Value MaryEra) v "value"
      cddlRoundTripAnnCborSpec @(TxBody MaryEra) v "transaction_body"
      cddlRoundTripCborSpec @(TxBody MaryEra) v "transaction_body"
      cddlRoundTripAnnCborSpec @(Script MaryEra) v "native_script"
      cddlRoundTripCborSpec @(Script MaryEra) v "native_script"
      cddlRoundTripAnnCborSpec @(TxAuxData MaryEra) v "auxiliary_data"
      cddlRoundTripCborSpec @(TxAuxData MaryEra) v "auxiliary_data"
      describe "DecCBOR instances equivalence via CDDL" $ do
        cddlDecoderEquivalenceSpec @(TxBody MaryEra) v "transaction_body"
        cddlDecoderEquivalenceSpec @(Script MaryEra) v "native_script"
        cddlDecoderEquivalenceSpec @(TxAuxData MaryEra) v "auxiliary_data"
    describe "Huddle" $ specWithHuddle maryCDDL 100 $ do
      huddleRoundTripCborSpec @(Value MaryEra) v "value"
      huddleRoundTripAnnCborSpec @(TxBody MaryEra) v "transaction_body"
      huddleRoundTripCborSpec @(TxBody MaryEra) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData MaryEra) v "auxiliary_data"
      huddleRoundTripCborSpec @(TxAuxData MaryEra) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Script MaryEra) v "native_script"
      huddleRoundTripCborSpec @(Script MaryEra) v "native_script"
      describe "DecCBOR instances equivalence via CDDL" $ do
        huddleDecoderEquivalenceSpec @(TxBody MaryEra) v "transaction_body"
        huddleDecoderEquivalenceSpec @(Script MaryEra) v "native_script"
        huddleDecoderEquivalenceSpec @(TxAuxData MaryEra) v "auxiliary_data"
