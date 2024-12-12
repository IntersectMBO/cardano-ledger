{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Conway (Conway)
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
    let v = eraProtVerHigh @Conway
    describe "Ruby-based" $ beforeAllCddlFile 3 readConwayCddlFiles $ do
      cddlRoundTripCborSpec @(Value Conway) v "positive_coin"
      cddlRoundTripCborSpec @(Value Conway) v "value"
      cddlRoundTripAnnCborSpec @(TxBody Conway) v "transaction_body"
      cddlRoundTripAnnCborSpec @(TxAuxData Conway) v "auxiliary_data"
      cddlRoundTripAnnCborSpec @(Timelock Conway) v "native_script"
      cddlRoundTripAnnCborSpec @(Data Conway) v "plutus_data"
      cddlRoundTripCborSpec @(TxOut Conway) v "transaction_output"
      cddlRoundTripAnnCborSpec @(Script Conway) v "script"
      cddlRoundTripCborSpec @(Datum Conway) v "datum_option"
      cddlRoundTripAnnCborSpec @(TxWits Conway) v "transaction_witness_set"
      cddlRoundTripCborSpec @(PParamsUpdate Conway) v "protocol_param_update"
      cddlRoundTripCborSpec @CostModels v "cost_models"
      cddlRoundTripAnnCborSpec @(Redeemers Conway) v "redeemers"
      cddlRoundTripAnnCborSpec @(Tx Conway) v "transaction"
      cddlRoundTripCborSpec @(VotingProcedure Conway) v "voting_procedure"
      cddlRoundTripCborSpec @(ProposalProcedure Conway) v "proposal_procedure"
      cddlRoundTripCborSpec @(GovAction Conway) v "gov_action"
      cddlRoundTripCborSpec @(TxCert Conway) v "certificate"
    describe "Huddle" $ specWithHuddle conwayCDDL 100 $ do
      huddleRoundTripCborSpec @(Value Conway) v "positive_coin"
      huddleRoundTripCborSpec @(Value Conway) v "value"
      huddleRoundTripAnnCborSpec @(TxBody Conway) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData Conway) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Timelock Conway) v "native_script"
      huddleRoundTripAnnCborSpec @(Data Conway) v "plutus_data"
      huddleRoundTripCborSpec @(TxOut Conway) v "transaction_output"
      huddleRoundTripAnnCborSpec @(Script Conway) v "script"
      huddleRoundTripCborSpec @(Datum Conway) v "datum_option"
      huddleRoundTripAnnCborSpec @(TxWits Conway) v "transaction_witness_set"
      huddleRoundTripCborSpec @(PParamsUpdate Conway) v "protocol_param_update"
      huddleRoundTripCborSpec @CostModels v "cost_models"
      huddleRoundTripAnnCborSpec @(Redeemers Conway) v "redeemers"
      huddleRoundTripAnnCborSpec @(Tx Conway) v "transaction"
      huddleRoundTripCborSpec @(VotingProcedure Conway) v "voting_procedure"
      huddleRoundTripCborSpec @(ProposalProcedure Conway) v "proposal_procedure"
      huddleRoundTripCborSpec @(GovAction Conway) v "gov_action"
      huddleRoundTripCborSpec @(TxCert Conway) v "certificate"
