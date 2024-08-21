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
import qualified Test.Cardano.Ledger.Mary.CDDL as MaryCDDL

spec :: Spec
spec = do
  describe "CDDL" $ beforeAllCddlFile 3 readMaryCddlFiles $ do
    let v = eraProtVerLow @Mary
    cddlRoundTripCborSpec @(Value Mary) v "value"
    cddlRoundTripAnnCborSpec @(TxBody Mary) v "transaction_body"
    cddlRoundTripAnnCborSpec @(Script Mary) v "native_script"
    cddlRoundTripAnnCborSpec @(TxAuxData Mary) v "auxiliary_data"
  newSpec

newSpec :: Spec
newSpec = describe "Huddle" $ specWithHuddle MaryCDDL.cddl 100 $ do
  let v = eraProtVerHigh @Mary
  huddleRoundTripCborSpec @(Value Mary) v "value"
  huddleRoundTripAnnCborSpec @(TxBody Mary) v "transaction_body"
  huddleRoundTripAnnCborSpec @(TxAuxData Mary) v "auxiliary_data"
  huddleRoundTripAnnCborSpec @(Script Mary) v "native_script"
