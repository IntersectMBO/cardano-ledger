{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.Binary.CddlSpec (spec) where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary (Mary)
import Test.Cardano.Ledger.Binary.Cddl (
  beforeAllCddlFile,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Binary.Cddl (readMaryCddlFiles)

spec :: Spec
spec =
  describe "CDDL" $ beforeAllCddlFile 3 readMaryCddlFiles $ do
    let v = eraProtVerLow @Mary
    cddlRoundTripCborSpec @(Value Mary) v "value"
    cddlRoundTripAnnCborSpec @(TxBody Mary) v "transaction_body"
    cddlRoundTripAnnCborSpec @(Script Mary) v "native_script"
    cddlRoundTripAnnCborSpec @(TxAuxData Mary) v "auxiliary_data"
