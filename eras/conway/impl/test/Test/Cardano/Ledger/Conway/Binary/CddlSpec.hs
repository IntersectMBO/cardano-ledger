{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (GovAction, ProposalProcedure, VotingProcedure)
import Cardano.Ledger.Conway.HuddleSpec (conwayCDDL)
import Cardano.Ledger.Core
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
  huddleRoundTripArbitraryValidate,
  huddleRoundTripCborSpec,
  specWithHuddle,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.Binary.Annotator ()
import Test.Cardano.Ledger.Conway.Binary.Cddl (readConwayCddlFiles)

spec :: Spec
spec = do
  describe "CDDL" $ do
    let v = eraProtVerHigh @ConwayEra
    describe "Ruby-based" $ beforeAllCddlFile 3 readConwayCddlFiles $ do
      cddlRoundTripCborSpec @(Value ConwayEra) v "positive_coin"
      cddlRoundTripCborSpec @(Value ConwayEra) v "value"
      cddlRoundTripAnnCborSpec @(TxBody TopTx ConwayEra) v "transaction_body"
      cddlRoundTripCborSpec @(TxBody TopTx ConwayEra) v "transaction_body"
      cddlRoundTripAnnCborSpec @(TxAuxData ConwayEra) v "auxiliary_data"
      cddlRoundTripCborSpec @(TxAuxData ConwayEra) v "auxiliary_data"
      cddlRoundTripAnnCborSpec @(Timelock ConwayEra) v "native_script"
      cddlRoundTripCborSpec @(Timelock ConwayEra) v "native_script"
      cddlRoundTripAnnCborSpec @(Data ConwayEra) v "plutus_data"
      cddlRoundTripCborSpec @(Data ConwayEra) v "plutus_data"
      cddlRoundTripCborSpec @(TxOut ConwayEra) v "transaction_output"
      cddlRoundTripAnnCborSpec @(Script ConwayEra) v "script"
      cddlRoundTripCborSpec @(Script ConwayEra) v "script"
      cddlRoundTripCborSpec @(Datum ConwayEra) v "datum_option"
      cddlRoundTripAnnCborSpec @(TxWits ConwayEra) v "transaction_witness_set"
      cddlRoundTripCborSpec @(TxWits ConwayEra) v "transaction_witness_set"
      cddlRoundTripCborSpec @(PParamsUpdate ConwayEra) v "protocol_param_update"
      cddlRoundTripCborSpec @CostModels v "cost_models"
      cddlRoundTripAnnCborSpec @(Redeemers ConwayEra) v "redeemers"
      cddlRoundTripCborSpec @(Redeemers ConwayEra) v "redeemers"
      cddlRoundTripAnnCborSpec @(Tx TopTx ConwayEra) v "transaction"
      cddlRoundTripCborSpec @(Tx TopTx ConwayEra) v "transaction"
      cddlRoundTripCborSpec @(VotingProcedure ConwayEra) v "voting_procedure"
      cddlRoundTripCborSpec @(ProposalProcedure ConwayEra) v "proposal_procedure"
      cddlRoundTripCborSpec @(GovAction ConwayEra) v "gov_action"
      cddlRoundTripCborSpec @(TxCert ConwayEra) v "certificate"
      describe "DecCBOR instances equivalence via CDDL" $ do
        cddlDecoderEquivalenceSpec @(TxBody TopTx ConwayEra) v "transaction_body"
        cddlDecoderEquivalenceSpec @(TxAuxData ConwayEra) v "auxiliary_data"
        cddlDecoderEquivalenceSpec @(Timelock ConwayEra) v "native_script"
        cddlDecoderEquivalenceSpec @(Data ConwayEra) v "plutus_data"
        cddlDecoderEquivalenceSpec @(Script ConwayEra) v "script"
        cddlDecoderEquivalenceSpec @(TxWits ConwayEra) v "transaction_witness_set"
        cddlDecoderEquivalenceSpec @(Redeemers ConwayEra) v "redeemers"
        cddlDecoderEquivalenceSpec @(Tx TopTx ConwayEra) v "transaction"
    describe "Huddle" $ specWithHuddle conwayCDDL 100 $ do
      -- Value
      huddleRoundTripCborSpec @(Value ConwayEra) v "positive_coin"
      huddleRoundTripArbitraryValidate @(Value ConwayEra) v "value"
      huddleRoundTripCborSpec @(Value ConwayEra) v "value"
      -- TxBody
      huddleRoundTripAnnCborSpec @(TxBody TopTx ConwayEra) v "transaction_body"
      -- TODO enable this once map/list expansion has been optimized in cuddle
      xdescribe "fix certs" $
        huddleRoundTripArbitraryValidate @(TxBody TopTx ConwayEra) v "transaction_body"
      huddleRoundTripCborSpec @(TxBody TopTx ConwayEra) v "transaction_body"
      -- AuxData
      huddleRoundTripAnnCborSpec @(TxAuxData ConwayEra) v "auxiliary_data"
      -- TODO fails because of plutus scripts
      xdescribe "fix plutus scripts" $
        huddleRoundTripArbitraryValidate @(TxAuxData ConwayEra) v "auxiliary_data"
      huddleRoundTripCborSpec @(TxAuxData ConwayEra) v "auxiliary_data"
      -- NativeScript
      huddleRoundTripAnnCborSpec @(Timelock ConwayEra) v "native_script"
      huddleRoundTripArbitraryValidate @(Timelock ConwayEra) v "native_script"
      huddleRoundTripCborSpec @(Timelock ConwayEra) v "native_script"
      -- Data
      huddleRoundTripAnnCborSpec @(Data ConwayEra) v "plutus_data"
      huddleRoundTripArbitraryValidate @(Data ConwayEra) v "plutus_data"
      huddleRoundTripCborSpec @(Data ConwayEra) v "plutus_data"
      -- TxOut
      huddleRoundTripCborSpec @(TxOut ConwayEra) v "transaction_output"
      -- TODO fails because of `address`
      xdescribe "fix address" $ huddleRoundTripArbitraryValidate @(TxOut ConwayEra) v "transaction_output"
      -- Script
      huddleRoundTripAnnCborSpec @(Script ConwayEra) v "script"
      -- TODO fails because of `plutus_v1_script`
      xdescribe "fix plutus_v1_script" $ huddleRoundTripArbitraryValidate @(Script ConwayEra) v "script"
      huddleRoundTripCborSpec @(Script ConwayEra) v "script"
      -- Datum
      huddleRoundTripCborSpec @(Datum ConwayEra) v "datum_option"
      -- TODO NoDatum is encoded as an empty bytestring
      xdescribe "fix NoDatum" $ huddleRoundTripArbitraryValidate @(Datum ConwayEra) v "datum_option"
      -- TxWits
      huddleRoundTripAnnCborSpec @(TxWits ConwayEra) v "transaction_witness_set"
      -- TODO fails because of plutus_v1_script
      xdescribe "fix plutus_v1_script" $
        huddleRoundTripArbitraryValidate @(TxWits ConwayEra) v "transaction_witness_set"
      huddleRoundTripCborSpec @(TxWits ConwayEra) v "transaction_witness_set"
      -- PParamsUpdate
      huddleRoundTripCborSpec @(PParamsUpdate ConwayEra) v "protocol_param_update"
      -- TODO enable this once map/list expansion has been optimized in cuddle
      xdescribe "fix unit_interval" $
        huddleRoundTripArbitraryValidate @(PParamsUpdate ConwayEra) v "protocol_param_update"
      -- CostModels
      huddleRoundTripCborSpec @CostModels v "cost_models"
      huddleRoundTripArbitraryValidate @CostModels v "cost_models"
      -- Redeemers
      huddleRoundTripAnnCborSpec @(Redeemers ConwayEra) v "redeemers"
      -- TODO arbitrary can generate empty redeemers, which is not allowed in the CDDL
      xdescribe "fix redeemers" $ huddleRoundTripArbitraryValidate @(Redeemers ConwayEra) v "redeemers"
      huddleRoundTripCborSpec @(Redeemers ConwayEra) v "redeemers"
      -- Tx
      huddleRoundTripAnnCborSpec @(Tx TopTx ConwayEra) v "transaction"
      -- TODO enable this once map/list expansion has been optimized in cuddle
      xdescribe "fix transaction_body" $
        huddleRoundTripArbitraryValidate @(Tx TopTx ConwayEra) v "transaction"
      huddleRoundTripCborSpec @(Tx TopTx ConwayEra) v "transaction"
      -- VotingProcedure
      huddleRoundTripCborSpec @(VotingProcedure ConwayEra) v "voting_procedure"
      huddleRoundTripArbitraryValidate @(VotingProcedure ConwayEra) v "voting_procedure"
      -- ProposalProcedure
      huddleRoundTripCborSpec @(ProposalProcedure ConwayEra) v "proposal_procedure"
      -- TODO This fails because of the hard-coded `reward_account` in the CDDL
      xdescribe "fix reward_account" $
        huddleRoundTripArbitraryValidate @(ProposalProcedure ConwayEra) v "proposal_procedure"
      -- GovAction
      huddleRoundTripCborSpec @(GovAction ConwayEra) v "gov_action"
      -- TODO enable this once map/list expansion has been optimized in cuddle
      xdescribe "fix protocol_param_update" $
        huddleRoundTripArbitraryValidate @(GovAction ConwayEra) v "gov_action"
      -- TxCert
      huddleRoundTripCborSpec @(TxCert ConwayEra) v "certificate"
      -- TODO this fails because of the hard-coded `unit_interval` in the CDDL
      xdescribe "fix unit_interval" $ huddleRoundTripArbitraryValidate @(TxCert ConwayEra) v "certificate"
      describe "DecCBOR instances equivalence via CDDL" $ do
        huddleDecoderEquivalenceSpec @(TxBody TopTx ConwayEra) v "transaction_body"
        huddleDecoderEquivalenceSpec @(TxAuxData ConwayEra) v "auxiliary_data"
        huddleDecoderEquivalenceSpec @(Timelock ConwayEra) v "native_script"
        huddleDecoderEquivalenceSpec @(Data ConwayEra) v "plutus_data"
        huddleDecoderEquivalenceSpec @(Script ConwayEra) v "script"
        huddleDecoderEquivalenceSpec @(TxWits ConwayEra) v "transaction_witness_set"
        huddleDecoderEquivalenceSpec @(Redeemers ConwayEra) v "redeemers"
        huddleDecoderEquivalenceSpec @(Tx TopTx ConwayEra) v "transaction"
