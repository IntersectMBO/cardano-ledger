{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Core
import Test.Cardano.Ledger.Allegra.Binary.Cddl (readAllegraCddlFiles)
import Test.Cardano.Ledger.Allegra.CDDL (allegraCDDL)
import Test.Cardano.Ledger.Binary.Cddl (
  beforeAllCddlFile,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Binary.Cuddle
import Test.Cardano.Ledger.Common

spec :: Spec
spec =
  describe "CDDL" $ do
    let v = eraProtVerLow @Allegra
    describe "Ruby-based" $ beforeAllCddlFile 3 readAllegraCddlFiles $ do
      cddlRoundTripCborSpec @(Value Allegra) v "coin"
      cddlRoundTripAnnCborSpec @(TxBody Allegra) v "transaction_body"
      cddlRoundTripAnnCborSpec @(Script Allegra) v "native_script"
      cddlRoundTripAnnCborSpec @(TxAuxData Allegra) v "auxiliary_data"
    describe "Huddle" $ specWithHuddle allegraCDDL 100 $ do
      huddleRoundTripCborSpec @(Value Allegra) v "coin"
      huddleRoundTripAnnCborSpec @(TxBody Allegra) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData Allegra) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Script Allegra) v "native_script"
