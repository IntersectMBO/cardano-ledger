{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Allegra.HuddleSpec (allegraCDDL)
import Cardano.Ledger.Core
import Test.Cardano.Ledger.Allegra.Binary.Annotator ()
import Test.Cardano.Ledger.Binary.Cuddle
import Test.Cardano.Ledger.Common

spec :: Spec
spec =
  describe "CDDL" $ do
    let v = eraProtVerLow @AllegraEra
    specWithHuddle allegraCDDL 100 $ do
      huddleRoundTripCborSpec @(Value AllegraEra) v "coin"
      huddleRoundTripAnnCborSpec @(TxBody TopTx AllegraEra) v "transaction_body"
      huddleRoundTripCborSpec @(TxBody TopTx AllegraEra) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData AllegraEra) v "auxiliary_data"
      huddleRoundTripCborSpec @(TxAuxData AllegraEra) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Script AllegraEra) v "native_script"
      huddleRoundTripCborSpec @(Script AllegraEra) v "native_script"
