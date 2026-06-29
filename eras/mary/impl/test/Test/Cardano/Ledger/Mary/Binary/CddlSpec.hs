{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.Binary.CddlSpec (spec) where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.HuddleSpec (maryCDDL)
import Test.Cardano.Ledger.Binary.Cuddle
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Binary.Annotator ()

spec :: Spec
spec =
  describe "CDDL" $ do
    let v = eraProtVerLow @MaryEra
    specWithHuddle maryCDDL 100 $ do
      huddleRoundTripCborSpec @(Value MaryEra) v "value"
      huddleRoundTripAnnCborSpec @(TxBody TopTx MaryEra) v "transaction_body"
      huddleRoundTripCborSpec @(TxBody TopTx MaryEra) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData MaryEra) v "auxiliary_data"
      huddleRoundTripCborSpec @(TxAuxData MaryEra) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Script MaryEra) v "native_script"
      huddleRoundTripCborSpec @(Script MaryEra) v "native_script"
      describe "DecCBOR instances equivalence via CDDL" $ do
        huddleDecoderEquivalenceSpec @(TxBody TopTx MaryEra) v "transaction_body"
        huddleDecoderEquivalenceSpec @(Script MaryEra) v "native_script"
        huddleDecoderEquivalenceSpec @(TxAuxData MaryEra) v "auxiliary_data"
