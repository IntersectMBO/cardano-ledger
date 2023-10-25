{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.Scripts.Data (Data)
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits, Redeemers)
import Cardano.Ledger.Core
import Test.Cardano.Ledger.Alonzo.Binary.Cddl (readAlonzoCddlFiles)
import Test.Cardano.Ledger.Binary.Cddl (
  beforeAllCddlFile,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Common

spec :: Spec
spec =
  describe "CDDL" $ beforeAllCddlFile 3 readAlonzoCddlFiles $ do
    let v = eraProtVerHigh @Alonzo
    cddlRoundTripCborSpec @(Value Alonzo) v "coin"
    cddlRoundTripAnnCborSpec @(TxBody Alonzo) v "transaction_body"
    cddlRoundTripAnnCborSpec @(TxAuxData Alonzo) v "auxiliary_data"
    cddlRoundTripAnnCborSpec @(Timelock Alonzo) v "native_script"
    cddlRoundTripAnnCborSpec @(Data Alonzo) v "plutus_data"
    cddlRoundTripCborSpec @(TxOut Alonzo) v "transaction_output"
    cddlRoundTripAnnCborSpec @(AlonzoTxWits Alonzo) v "transaction_witness_set"
    cddlRoundTripCborSpec @(PParamsUpdate Alonzo) v "protocol_param_update"
    cddlRoundTripAnnCborSpec @(Redeemers Alonzo) v "[* redeemer]"
    cddlRoundTripAnnCborSpec @(Tx Alonzo) v "transaction"
    cddlRoundTripCborSpec @CostModels v "costmdls"
