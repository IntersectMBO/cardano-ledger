{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.Binary.CddlSpec (spec) where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
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
    let v = eraProtVerLow @MaryEra
    cddlRoundTripCborSpec @(Value MaryEra) v "value"
    cddlRoundTripAnnCborSpec @(TxBody MaryEra) v "transaction_body"
    cddlRoundTripAnnCborSpec @(Script MaryEra) v "native_script"
    cddlRoundTripAnnCborSpec @(TxAuxData MaryEra) v "auxiliary_data"
  newSpec

newSpec :: Spec
newSpec = describe "Huddle" $ specWithHuddle MaryCDDL.cddl 100 $ do
  let v = eraProtVerHigh @MaryEra
  huddleRoundTripCborSpec @(Value MaryEra) v "value"
  huddleRoundTripAnnCborSpec @(TxBody MaryEra) v "transaction_body"
  huddleRoundTripAnnCborSpec @(TxAuxData MaryEra) v "auxiliary_data"
  huddleRoundTripAnnCborSpec @(Script MaryEra) v "native_script"
