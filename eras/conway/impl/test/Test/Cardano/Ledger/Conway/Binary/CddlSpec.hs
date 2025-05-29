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
    let v = eraProtVerHigh @ConwayEra
    describe "Ruby-based" $ beforeAllCddlFile 3 readConwayCddlFiles $ do
      cddlRoundTripCborSpec @(Value ConwayEra) v "positive_coin"
      cddlRoundTripCborSpec @(Value ConwayEra) v "value"
      cddlRoundTripAnnCborSpec @(TxBody ConwayEra) v "transaction_body"
      cddlRoundTripCborSpec @(TxBody ConwayEra) v "transaction_body"
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
      cddlRoundTripAnnCborSpec @(Tx ConwayEra) v "transaction"
      cddlRoundTripCborSpec @(Tx ConwayEra) v "transaction"
      cddlRoundTripCborSpec @(VotingProcedure ConwayEra) v "voting_procedure"
      cddlRoundTripCborSpec @(ProposalProcedure ConwayEra) v "proposal_procedure"
      cddlRoundTripCborSpec @(GovAction ConwayEra) v "gov_action"
      cddlRoundTripCborSpec @(TxCert ConwayEra) v "certificate"
      describe "DecCBOR instances equivalence via CDDL" $ do
        cddlDecoderEquivalenceSpec @(TxBody ConwayEra) v "transaction_body"
        cddlDecoderEquivalenceSpec @(TxAuxData ConwayEra) v "auxiliary_data"
        cddlDecoderEquivalenceSpec @(Timelock ConwayEra) v "native_script"
        cddlDecoderEquivalenceSpec @(Data ConwayEra) v "plutus_data"
        cddlDecoderEquivalenceSpec @(Script ConwayEra) v "script"
        cddlDecoderEquivalenceSpec @(TxWits ConwayEra) v "transaction_witness_set"
        cddlDecoderEquivalenceSpec @(Redeemers ConwayEra) v "redeemers"
        cddlDecoderEquivalenceSpec @(Tx ConwayEra) v "transaction"
    describe "Huddle" $ specWithHuddle conwayCDDL 100 $ do
      huddleRoundTripCborSpec @(Value ConwayEra) v "positive_coin"
      huddleRoundTripCborSpec @(Value ConwayEra) v "value"
      huddleRoundTripAnnCborSpec @(TxBody ConwayEra) v "transaction_body"
      huddleRoundTripCborSpec @(TxBody ConwayEra) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData ConwayEra) v "auxiliary_data"
      huddleRoundTripCborSpec @(TxAuxData ConwayEra) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Timelock ConwayEra) v "native_script"
      huddleRoundTripCborSpec @(Timelock ConwayEra) v "native_script"
      huddleRoundTripAnnCborSpec @(Data ConwayEra) v "plutus_data"
      huddleRoundTripCborSpec @(Data ConwayEra) v "plutus_data"
      huddleRoundTripCborSpec @(TxOut ConwayEra) v "transaction_output"
      huddleRoundTripAnnCborSpec @(Script ConwayEra) v "script"
      huddleRoundTripCborSpec @(Script ConwayEra) v "script"
      huddleRoundTripCborSpec @(Datum ConwayEra) v "datum_option"
      huddleRoundTripAnnCborSpec @(TxWits ConwayEra) v "transaction_witness_set"
      huddleRoundTripCborSpec @(TxWits ConwayEra) v "transaction_witness_set"
      huddleRoundTripCborSpec @(PParamsUpdate ConwayEra) v "protocol_param_update"
      huddleRoundTripCborSpec @CostModels v "cost_models"
      huddleRoundTripAnnCborSpec @(Redeemers ConwayEra) v "redeemers"
      huddleRoundTripCborSpec @(Redeemers ConwayEra) v "redeemers"
      huddleRoundTripAnnCborSpec @(Tx ConwayEra) v "transaction"
      huddleRoundTripCborSpec @(Tx ConwayEra) v "transaction"
      huddleRoundTripCborSpec @(VotingProcedure ConwayEra) v "voting_procedure"
      huddleRoundTripCborSpec @(ProposalProcedure ConwayEra) v "proposal_procedure"
      huddleRoundTripCborSpec @(GovAction ConwayEra) v "gov_action"
      huddleRoundTripCborSpec @(TxCert ConwayEra) v "certificate"
      describe "DecCBOR instances equivalence via CDDL" $ do
        huddleDecoderEquivalenceSpec @(TxBody ConwayEra) v "transaction_body"
        huddleDecoderEquivalenceSpec @(TxAuxData ConwayEra) v "auxiliary_data"
        huddleDecoderEquivalenceSpec @(Timelock ConwayEra) v "native_script"
        huddleDecoderEquivalenceSpec @(Data ConwayEra) v "plutus_data"
        huddleDecoderEquivalenceSpec @(Script ConwayEra) v "script"
        huddleDecoderEquivalenceSpec @(TxWits ConwayEra) v "transaction_witness_set"
        huddleDecoderEquivalenceSpec @(Redeemers ConwayEra) v "redeemers"
        huddleDecoderEquivalenceSpec @(Tx ConwayEra) v "transaction"
