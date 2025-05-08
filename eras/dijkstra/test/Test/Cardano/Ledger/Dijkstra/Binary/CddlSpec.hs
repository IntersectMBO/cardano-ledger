{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Conway.Governance (GovAction, ProposalProcedure, VotingProcedure)
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Plutus.Data (Data, Datum)
import Test.Cardano.Ledger.Binary.Cddl (
  beforeAllCddlFile,
  cddlDecoderEquivalenceSpec,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Binary.Cuddle (
  huddleDecoderEquivalenceSpec,
  huddleRoundTripAnnCborSpec,
  huddleRoundTripCborSpec,
  specWithHuddle,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Binary.Annotator ()
import Test.Cardano.Ledger.Conway.Binary.Cddl (readConwayCddlFiles)
import Test.Cardano.Ledger.Conway.CDDL (conwayCDDL)

spec :: Spec
spec = do
  describe "CDDL" $ do
    let v = eraProtVerHigh @DijkstraEra
    describe "Ruby-based" $ beforeAllCddlFile 3 readConwayCddlFiles $ do
      cddlRoundTripCborSpec @(Value DijkstraEra) v "positive_coin"
      cddlRoundTripCborSpec @(Value DijkstraEra) v "value"
      cddlRoundTripAnnCborSpec @(TxBody DijkstraEra) v "transaction_body"
      cddlRoundTripCborSpec @(TxBody DijkstraEra) v "transaction_body"
      cddlRoundTripAnnCborSpec @(TxAuxData DijkstraEra) v "auxiliary_data"
      cddlRoundTripCborSpec @(TxAuxData DijkstraEra) v "auxiliary_data"
      cddlRoundTripAnnCborSpec @(Timelock DijkstraEra) v "native_script"
      cddlRoundTripCborSpec @(Timelock DijkstraEra) v "native_script"
      cddlRoundTripAnnCborSpec @(Data DijkstraEra) v "plutus_data"
      cddlRoundTripCborSpec @(Data DijkstraEra) v "plutus_data"
      cddlRoundTripCborSpec @(TxOut DijkstraEra) v "transaction_output"
      cddlRoundTripAnnCborSpec @(Script DijkstraEra) v "script"
      cddlRoundTripCborSpec @(Script DijkstraEra) v "script"
      cddlRoundTripCborSpec @(Datum DijkstraEra) v "datum_option"
      cddlRoundTripAnnCborSpec @(TxWits DijkstraEra) v "transaction_witness_set"
      cddlRoundTripCborSpec @(TxWits DijkstraEra) v "transaction_witness_set"
      cddlRoundTripCborSpec @(PParamsUpdate DijkstraEra) v "protocol_param_update"
      cddlRoundTripCborSpec @CostModels v "cost_models"
      cddlRoundTripAnnCborSpec @(Redeemers DijkstraEra) v "redeemers"
      cddlRoundTripCborSpec @(Redeemers DijkstraEra) v "redeemers"
      cddlRoundTripAnnCborSpec @(Tx DijkstraEra) v "transaction"
      cddlRoundTripCborSpec @(Tx DijkstraEra) v "transaction"
      cddlRoundTripCborSpec @(VotingProcedure DijkstraEra) v "voting_procedure"
      cddlRoundTripCborSpec @(ProposalProcedure DijkstraEra) v "proposal_procedure"
      cddlRoundTripCborSpec @(GovAction DijkstraEra) v "gov_action"
      cddlRoundTripCborSpec @(TxCert DijkstraEra) v "certificate"
      describe "DecCBOR instances equivalence via CDDL" $ do
        cddlDecoderEquivalenceSpec @(TxBody DijkstraEra) v "transaction_body"
        cddlDecoderEquivalenceSpec @(TxAuxData DijkstraEra) v "auxiliary_data"
        cddlDecoderEquivalenceSpec @(Timelock DijkstraEra) v "native_script"
        cddlDecoderEquivalenceSpec @(Data DijkstraEra) v "plutus_data"
        cddlDecoderEquivalenceSpec @(Script DijkstraEra) v "script"
        cddlDecoderEquivalenceSpec @(TxWits DijkstraEra) v "transaction_witness_set"
        cddlDecoderEquivalenceSpec @(Redeemers DijkstraEra) v "redeemers"
        cddlDecoderEquivalenceSpec @(Tx DijkstraEra) v "transaction"
    describe "Huddle" $ specWithHuddle conwayCDDL 100 $ do
      huddleRoundTripCborSpec @(Value DijkstraEra) v "positive_coin"
      huddleRoundTripCborSpec @(Value DijkstraEra) v "value"
      huddleRoundTripAnnCborSpec @(TxBody DijkstraEra) v "transaction_body"
      huddleRoundTripCborSpec @(TxBody DijkstraEra) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData DijkstraEra) v "auxiliary_data"
      huddleRoundTripCborSpec @(TxAuxData DijkstraEra) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Timelock DijkstraEra) v "native_script"
      huddleRoundTripCborSpec @(Timelock DijkstraEra) v "native_script"
      huddleRoundTripAnnCborSpec @(Data DijkstraEra) v "plutus_data"
      huddleRoundTripCborSpec @(Data DijkstraEra) v "plutus_data"
      huddleRoundTripCborSpec @(TxOut DijkstraEra) v "transaction_output"
      huddleRoundTripAnnCborSpec @(Script DijkstraEra) v "script"
      huddleRoundTripCborSpec @(Script DijkstraEra) v "script"
      huddleRoundTripCborSpec @(Datum DijkstraEra) v "datum_option"
      huddleRoundTripAnnCborSpec @(TxWits DijkstraEra) v "transaction_witness_set"
      huddleRoundTripCborSpec @(TxWits DijkstraEra) v "transaction_witness_set"
      huddleRoundTripCborSpec @(PParamsUpdate DijkstraEra) v "protocol_param_update"
      huddleRoundTripCborSpec @CostModels v "cost_models"
      huddleRoundTripAnnCborSpec @(Redeemers DijkstraEra) v "redeemers"
      huddleRoundTripCborSpec @(Redeemers DijkstraEra) v "redeemers"
      huddleRoundTripAnnCborSpec @(Tx DijkstraEra) v "transaction"
      huddleRoundTripCborSpec @(Tx DijkstraEra) v "transaction"
      huddleRoundTripCborSpec @(VotingProcedure DijkstraEra) v "voting_procedure"
      huddleRoundTripCborSpec @(ProposalProcedure DijkstraEra) v "proposal_procedure"
      huddleRoundTripCborSpec @(GovAction DijkstraEra) v "gov_action"
      huddleRoundTripCborSpec @(TxCert DijkstraEra) v "certificate"
      describe "DecCBOR instances equivalence via CDDL" $ do
        huddleDecoderEquivalenceSpec @(TxBody DijkstraEra) v "transaction_body"
        huddleDecoderEquivalenceSpec @(TxAuxData DijkstraEra) v "auxiliary_data"
        huddleDecoderEquivalenceSpec @(Timelock DijkstraEra) v "native_script"
        huddleDecoderEquivalenceSpec @(Data DijkstraEra) v "plutus_data"
        huddleDecoderEquivalenceSpec @(Script DijkstraEra) v "script"
        huddleDecoderEquivalenceSpec @(TxWits DijkstraEra) v "transaction_witness_set"
        huddleDecoderEquivalenceSpec @(Redeemers DijkstraEra) v "redeemers"
        huddleDecoderEquivalenceSpec @(Tx DijkstraEra) v "transaction"
