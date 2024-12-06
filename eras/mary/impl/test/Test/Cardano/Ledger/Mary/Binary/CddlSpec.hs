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
import Test.Cardano.Ledger.Binary.Cuddle
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Binary.Cddl (readMaryCddlFiles)
import Test.Cardano.Ledger.Mary.CDDL (maryCDDL)

spec :: Spec
spec =
  describe "CDDL" $ do
    let v = eraProtVerLow @Mary
    describe "Ruby-based" $ beforeAllCddlFile 3 readMaryCddlFiles $ do
      cddlRoundTripCborSpec @(Value Mary) v "value"
      cddlRoundTripAnnCborSpec @(TxBody Mary) v "transaction_body"
      cddlRoundTripAnnCborSpec @(Script Mary) v "native_script"
      cddlRoundTripAnnCborSpec @(TxAuxData Mary) v "auxiliary_data"
    describe "Huddle" $ specWithHuddle maryCDDL 100 $ do
      huddleRoundTripCborSpec @(Value Mary) v "value"
      huddleRoundTripAnnCborSpec @(TxBody Mary) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData Mary) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Script Mary) v "native_script"
