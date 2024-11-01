{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (GovAction, ProposalProcedure, VotingProcedure)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.Data (Data, Datum)
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
import Test.Cardano.Ledger.Conway.Binary.Cddl (readConwayCddlFiles)
import Test.Cardano.Ledger.Conway.CDDL (conwayCDDL)

spec :: Spec
spec = do
  describe "CDDL" $ do
    let v = eraProtVerHigh @ConwayEra
    describe "Ruby-based" $ beforeAllCddlFile 3 readConwayCddlFiles $ do
      cddlRoundTripCborSpec @(Value ConwayEra) v "positive_coin"
      cddlRoundTripCborSpec @(Value ConwayEra) v "value"
      cddlRoundTripAnnCborSpec @(TxBody ConwayEra) v "transaction_body"
      cddlRoundTripAnnCborSpec @(TxAuxData ConwayEra) v "auxiliary_data"
      cddlRoundTripAnnCborSpec @(Timelock ConwayEra) v "native_script"
      cddlRoundTripAnnCborSpec @(Data ConwayEra) v "plutus_data"
      cddlRoundTripCborSpec @(TxOut ConwayEra) v "transaction_output"
      cddlRoundTripAnnCborSpec @(Script ConwayEra) v "script"
      cddlRoundTripCborSpec @(Datum ConwayEra) v "datum_option"
      cddlRoundTripAnnCborSpec @(TxWits ConwayEra) v "transaction_witness_set"
      cddlRoundTripCborSpec @(PParamsUpdate ConwayEra) v "protocol_param_update"
      cddlRoundTripCborSpec @CostModels v "cost_models"
      cddlRoundTripAnnCborSpec @(Redeemers ConwayEra) v "redeemers"
      cddlRoundTripAnnCborSpec @(Tx ConwayEra) v "transaction"
      cddlRoundTripCborSpec @(VotingProcedure ConwayEra) v "voting_procedure"
      cddlRoundTripCborSpec @(ProposalProcedure ConwayEra) v "proposal_procedure"
      cddlRoundTripCborSpec @(GovAction ConwayEra) v "gov_action"
      cddlRoundTripCborSpec @(TxCert ConwayEra) v "certificate"
    describe "Huddle" $ specWithHuddle conwayCDDL 100 $ do
      huddleRoundTripCborSpec @(Value ConwayEra) v "positive_coin"
      huddleRoundTripCborSpec @(Value ConwayEra) v "value"
      huddleRoundTripAnnCborSpec @(TxBody ConwayEra) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData ConwayEra) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Timelock ConwayEra) v "native_script"
      huddleRoundTripAnnCborSpec @(Data ConwayEra) v "plutus_data"
      huddleRoundTripCborSpec @(TxOut ConwayEra) v "transaction_output"
      huddleRoundTripAnnCborSpec @(Script ConwayEra) v "script"
      huddleRoundTripCborSpec @(Datum ConwayEra) v "datum_option"
      huddleRoundTripAnnCborSpec @(TxWits ConwayEra) v "transaction_witness_set"
      huddleRoundTripCborSpec @(PParamsUpdate ConwayEra) v "protocol_param_update"
      huddleRoundTripCborSpec @CostModels v "cost_models"
      huddleRoundTripAnnCborSpec @(Redeemers ConwayEra) v "redeemers"
      huddleRoundTripAnnCborSpec @(Tx ConwayEra) v "transaction"
      huddleRoundTripCborSpec @(VotingProcedure ConwayEra) v "voting_procedure"
      huddleRoundTripCborSpec @(ProposalProcedure ConwayEra) v "proposal_procedure"
      huddleRoundTripCborSpec @(GovAction ConwayEra) v "gov_action"
      huddleRoundTripCborSpec @(TxCert ConwayEra) v "certificate"
