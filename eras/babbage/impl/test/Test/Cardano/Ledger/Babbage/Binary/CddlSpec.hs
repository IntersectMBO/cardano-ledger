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
import Test.Cardano.Ledger.Babbage.CDDL (babbageCDDL)
import Test.Cardano.Ledger.Binary.Cddl (
  beforeAllCddlFile,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Binary.Cuddle (
  huddleRoundTripAnnCborSpec,
  huddleRoundTripCborSpec,
  specWithHuddle,
 )
import Test.Cardano.Ledger.Common

spec :: Spec
spec =
  describe "CDDL" $ do
    let v = eraProtVerHigh @Babbage
    describe "Ruby-based" $ beforeAllCddlFile 3 readBabbageCddlFiles $ do
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
      cddlRoundTripCborSpec @CostModels v "cost_models"
      cddlRoundTripAnnCborSpec @(Redeemers Babbage) v "redeemers"
      cddlRoundTripAnnCborSpec @(Tx Babbage) v "transaction"
    describe "Huddle" $ specWithHuddle babbageCDDL 100 $ do
      huddleRoundTripCborSpec @(Value Babbage) v "coin"
      huddleRoundTripAnnCborSpec @(TxBody Babbage) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData Babbage) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Timelock Babbage) v "native_script"
      huddleRoundTripAnnCborSpec @(Data Babbage) v "plutus_data"
      huddleRoundTripCborSpec @(TxOut Babbage) v "transaction_output"
      huddleRoundTripAnnCborSpec @(Script Babbage) v "script"
      huddleRoundTripCborSpec @(Datum Babbage) v "datum_option"
      huddleRoundTripAnnCborSpec @(TxWits Babbage) v "transaction_witness_set"
      huddleRoundTripCborSpec @(PParamsUpdate Babbage) v "protocol_param_update"
      huddleRoundTripCborSpec @CostModels v "cost_models"
      huddleRoundTripAnnCborSpec @(Redeemers Babbage) v "redeemers"
      huddleRoundTripAnnCborSpec @(Tx Babbage) v "transaction"
