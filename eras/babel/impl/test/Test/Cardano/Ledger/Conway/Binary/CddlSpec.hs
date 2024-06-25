{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babel.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Babel (Babel)
import Cardano.Ledger.Babel.Governance (GovAction, ProposalProcedure, VotingProcedure)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.Data (Data, Datum)
import Test.Cardano.Ledger.Binary.Cddl (
  beforeAllCddlFile,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Babel.Binary.Cddl (readBabelCddlFiles)

spec :: Spec
spec =
  describe "CDDL" $ beforeAllCddlFile 3 readBabelCddlFiles $ do
    let v = eraProtVerHigh @Babel
    cddlRoundTripCborSpec @(Value Babel) v "positive_coin"
    cddlRoundTripCborSpec @(Value Babel) v "value"
    cddlRoundTripAnnCborSpec @(TxBody Babel) v "transaction_body"
    cddlRoundTripAnnCborSpec @(TxAuxData Babel) v "auxiliary_data"
    cddlRoundTripAnnCborSpec @(Timelock Babel) v "native_script"
    cddlRoundTripAnnCborSpec @(Data Babel) v "plutus_data"
    cddlRoundTripCborSpec @(TxOut Babel) v "transaction_output"
    cddlRoundTripAnnCborSpec @(Script Babel) v "script"
    cddlRoundTripCborSpec @(Datum Babel) v "datum_option"
    cddlRoundTripAnnCborSpec @(TxWits Babel) v "transaction_witness_set"
    cddlRoundTripCborSpec @(PParamsUpdate Babel) v "protocol_param_update"
    cddlRoundTripCborSpec @CostModels v "costmdls"
    cddlRoundTripAnnCborSpec @(Redeemers Babel) v "redeemers"
    cddlRoundTripAnnCborSpec @(Tx Babel) v "transaction"
    cddlRoundTripCborSpec @(VotingProcedure Babel) v "voting_procedure"
    cddlRoundTripCborSpec @(ProposalProcedure Babel) v "proposal_procedure"
    cddlRoundTripCborSpec @(GovAction Babel) v "gov_action"
    cddlRoundTripCborSpec @(TxCert Babel) v "certificate"
