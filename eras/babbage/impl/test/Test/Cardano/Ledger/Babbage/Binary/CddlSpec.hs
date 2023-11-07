{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.Data (Data, Datum)
import Test.Cardano.Ledger.Babbage.Binary.Cddl (readBabbageCddlFiles)
import Test.Cardano.Ledger.Binary.Cddl (
  beforeAllCddlFile,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Common

spec :: Spec
spec =
  describe "CDDL" $ beforeAllCddlFile 3 readBabbageCddlFiles $ do
    let v = eraProtVerHigh @Babbage
    cddlRoundTripCborSpec @(Value Babbage) v "coin"
    cddlRoundTripAnnCborSpec @(TxBody Babbage) v "transaction_body"
    cddlRoundTripAnnCborSpec @(TxAuxData Babbage) v "auxiliary_data"
    cddlRoundTripAnnCborSpec @(Timelock Babbage) v "native_script"
    cddlRoundTripAnnCborSpec @(Data Babbage) v "plutus_data"
    cddlRoundTripCborSpec @(TxOut Babbage) v "transaction_output"
    cddlRoundTripAnnCborSpec @(Script Babbage) v "script"
    cddlRoundTripCborSpec @(Datum Babbage) v "datum_option"
    cddlRoundTripAnnCborSpec @(TxWits Babbage) v "transaction_witness_set"
    cddlRoundTripCborSpec @(PParamsUpdate Babbage) v "protocol_param_update"
    cddlRoundTripCborSpec @CostModels v "costmdls"
    cddlRoundTripAnnCborSpec @(Redeemers Babbage) v "[* redeemer]"
    cddlRoundTripAnnCborSpec @(Tx Babbage) v "transaction"
