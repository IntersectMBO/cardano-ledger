{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits, Redeemers)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.Data (Data)
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
    let v = eraProtVerHigh @AlonzoEra
    cddlRoundTripCborSpec @(Value AlonzoEra) v "coin"
    cddlRoundTripAnnCborSpec @(TxBody AlonzoEra) v "transaction_body"
    cddlRoundTripAnnCborSpec @(TxAuxData AlonzoEra) v "auxiliary_data"
    cddlRoundTripAnnCborSpec @(Timelock AlonzoEra) v "native_script"
    cddlRoundTripAnnCborSpec @(Data AlonzoEra) v "plutus_data"
    cddlRoundTripCborSpec @(TxOut AlonzoEra) v "transaction_output"
    cddlRoundTripAnnCborSpec @(AlonzoTxWits AlonzoEra) v "transaction_witness_set"
    cddlRoundTripCborSpec @(PParamsUpdate AlonzoEra) v "protocol_param_update"
    cddlRoundTripAnnCborSpec @(Redeemers AlonzoEra) v "[* redeemer]"
    cddlRoundTripAnnCborSpec @(Tx AlonzoEra) v "transaction"
    cddlRoundTripCborSpec @CostModels v "costmdls"
