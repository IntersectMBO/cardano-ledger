{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Babbage (BabbageEra)
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
    let v = eraProtVerHigh @BabbageEra
    describe "Ruby-based" $ beforeAllCddlFile 3 readBabbageCddlFiles $ do
      cddlRoundTripCborSpec @(Value BabbageEra) v "coin"
      cddlRoundTripAnnCborSpec @(TxBody BabbageEra) v "transaction_body"
      cddlRoundTripAnnCborSpec @(TxAuxData BabbageEra) v "auxiliary_data"
      cddlRoundTripAnnCborSpec @(Timelock BabbageEra) v "native_script"
      cddlRoundTripAnnCborSpec @(Data BabbageEra) v "plutus_data"
      cddlRoundTripCborSpec @(TxOut BabbageEra) v "transaction_output"
      cddlRoundTripAnnCborSpec @(Script BabbageEra) v "script"
      cddlRoundTripCborSpec @(Datum BabbageEra) v "datum_option"
      cddlRoundTripAnnCborSpec @(TxWits BabbageEra) v "transaction_witness_set"
      cddlRoundTripCborSpec @(PParamsUpdate BabbageEra) v "protocol_param_update"
      cddlRoundTripCborSpec @CostModels v "cost_models"
      cddlRoundTripAnnCborSpec @(Redeemers BabbageEra) v "redeemers"
      cddlRoundTripAnnCborSpec @(Tx BabbageEra) v "transaction"
    describe "Huddle" $ specWithHuddle babbageCDDL 100 $ do
      huddleRoundTripCborSpec @(Value BabbageEra) v "coin"
      huddleRoundTripAnnCborSpec @(TxBody BabbageEra) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData BabbageEra) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Timelock BabbageEra) v "native_script"
      huddleRoundTripAnnCborSpec @(Data BabbageEra) v "plutus_data"
      huddleRoundTripCborSpec @(TxOut BabbageEra) v "transaction_output"
      huddleRoundTripAnnCborSpec @(Script BabbageEra) v "script"
      huddleRoundTripCborSpec @(Datum BabbageEra) v "datum_option"
      huddleRoundTripAnnCborSpec @(TxWits BabbageEra) v "transaction_witness_set"
      huddleRoundTripCborSpec @(PParamsUpdate BabbageEra) v "protocol_param_update"
      huddleRoundTripCborSpec @CostModels v "cost_models"
      huddleRoundTripAnnCborSpec @(Redeemers BabbageEra) v "redeemers"
      huddleRoundTripAnnCborSpec @(Tx BabbageEra) v "transaction"
