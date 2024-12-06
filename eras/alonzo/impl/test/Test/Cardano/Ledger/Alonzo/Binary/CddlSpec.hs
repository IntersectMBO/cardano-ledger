{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits, Redeemers)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.Data (Data)
import Test.Cardano.Ledger.Alonzo.Binary.Cddl (readAlonzoCddlFiles)
import Test.Cardano.Ledger.Alonzo.CDDL (alonzoCDDL)
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
    let v = eraProtVerHigh @Alonzo
    describe "Ruby-based" $ beforeAllCddlFile 3 readAlonzoCddlFiles $ do
      cddlRoundTripCborSpec @(Value Alonzo) v "coin"
      cddlRoundTripAnnCborSpec @(TxBody Alonzo) v "transaction_body"
      cddlRoundTripAnnCborSpec @(TxAuxData Alonzo) v "auxiliary_data"
      cddlRoundTripAnnCborSpec @(Timelock Alonzo) v "native_script"
      cddlRoundTripAnnCborSpec @(Data Alonzo) v "plutus_data"
      cddlRoundTripCborSpec @(TxOut Alonzo) v "transaction_output"
      cddlRoundTripAnnCborSpec @(AlonzoTxWits Alonzo) v "transaction_witness_set"
      cddlRoundTripCborSpec @(PParamsUpdate Alonzo) v "protocol_param_update"
      cddlRoundTripAnnCborSpec @(Redeemers Alonzo) v "redeemers"
      cddlRoundTripAnnCborSpec @(Tx Alonzo) v "transaction"
      cddlRoundTripCborSpec @CostModels v "cost_models"
    describe "Huddle" $ specWithHuddle alonzoCDDL 100 $ do
      huddleRoundTripCborSpec @(Value Alonzo) v "coin"
      huddleRoundTripAnnCborSpec @(TxBody Alonzo) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData Alonzo) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Timelock Alonzo) v "native_script"
      huddleRoundTripAnnCborSpec @(Data Alonzo) v "plutus_data"
      huddleRoundTripCborSpec @(TxOut Alonzo) v "transaction_output"
      huddleRoundTripAnnCborSpec @(AlonzoTxWits Alonzo) v "transaction_witness_set"
      huddleRoundTripCborSpec @(PParamsUpdate Alonzo) v "protocol_param_update"
      huddleRoundTripAnnCborSpec @(Redeemers Alonzo) v "redeemers"
      huddleRoundTripAnnCborSpec @(Tx Alonzo) v "transaction"
      huddleRoundTripCborSpec @CostModels v "cost_models"
