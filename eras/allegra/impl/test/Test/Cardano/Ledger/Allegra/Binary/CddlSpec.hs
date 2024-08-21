{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Core
import Test.Cardano.Ledger.Allegra.Binary.Cddl (readAllegraCddlFiles)
import qualified Test.Cardano.Ledger.Allegra.CDDL as AllegraCDDL
import Test.Cardano.Ledger.Binary.Cddl (
  beforeAllCddlFile,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Binary.Cuddle
import Test.Cardano.Ledger.Common

spec :: Spec
spec = do
  describe "CDDL" $ beforeAllCddlFile 3 readAllegraCddlFiles $ do
    let v = eraProtVerLow @Allegra
    cddlRoundTripCborSpec @(Value Allegra) v "coin"
    cddlRoundTripAnnCborSpec @(TxBody Allegra) v "transaction_body"
    cddlRoundTripAnnCborSpec @(Script Allegra) v "native_script"
    cddlRoundTripAnnCborSpec @(TxAuxData Allegra) v "auxiliary_data"
  newSpec

newSpec :: Spec
newSpec = describe "Huddle" $ specWithHuddle AllegraCDDL.cddl 100 $ do
  let v = eraProtVerHigh @Allegra
  huddleRoundTripCborSpec @(Value Allegra) v "coin"
  huddleRoundTripAnnCborSpec @(TxBody Allegra) v "transaction_body"
  huddleRoundTripAnnCborSpec @(TxAuxData Allegra) v "auxiliary_data"
  huddleRoundTripAnnCborSpec @(Script Allegra) v "native_script"
