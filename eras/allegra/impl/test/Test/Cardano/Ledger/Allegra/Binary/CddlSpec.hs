{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Core
import Test.Cardano.Ledger.Allegra.Binary.Cddl (readAllegraCddlFiles)
import Test.Cardano.Ledger.Binary.Cddl (
  beforeAllCddlFile,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Common

spec :: Spec
spec =
  describe "CDDL" $ beforeAllCddlFile 3 readAllegraCddlFiles $ do
    let v = eraProtVerLow @Allegra
    cddlRoundTripCborSpec @(Value Allegra) v "coin"
    cddlRoundTripAnnCborSpec @(TxBody Allegra) v "transaction_body"
    cddlRoundTripAnnCborSpec @(Script Allegra) v "native_script"
    cddlRoundTripAnnCborSpec @(TxAuxData Allegra) v "auxiliary_data"
