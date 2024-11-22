{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra (AllegraEra)
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
    let v = eraProtVerLow @AllegraEra
    cddlRoundTripCborSpec @(Value AllegraEra) v "coin"
    cddlRoundTripAnnCborSpec @(TxBody AllegraEra) v "transaction_body"
    cddlRoundTripAnnCborSpec @(Script AllegraEra) v "native_script"
    cddlRoundTripAnnCborSpec @(TxAuxData AllegraEra) v "auxiliary_data"
  newSpec

newSpec :: Spec
newSpec = describe "Huddle" $ specWithHuddle AllegraCDDL.cddl 100 $ do
  let v = eraProtVerHigh @AllegraEra
  huddleRoundTripCborSpec @(Value AllegraEra) v "coin"
  huddleRoundTripAnnCborSpec @(TxBody AllegraEra) v "transaction_body"
  huddleRoundTripAnnCborSpec @(TxAuxData AllegraEra) v "auxiliary_data"
  huddleRoundTripAnnCborSpec @(Script AllegraEra) v "native_script"
