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
import Cardano.Ledger.Binary (DecCBOR)
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
import Test.Cardano.Ledger.Conway.Binary.Annotator (Annotator)
import Test.Cardano.Ledger.Conway.Binary.Cddl (readConwayCddlFiles)
import Test.Cardano.Ledger.Conway.CDDL (conwayCDDL)
import Test.Cardano.Ledger.Conway.ImpTest (ConwayEraImp)

spec ::
  forall era.
  ( ConwayEraImp era
  , DecCBOR (Annotator (TxBody era))
  , DecCBOR (Annotator (TxWits era))
  , DecCBOR (Annotator (Tx era))
  ) =>
  Spec
spec = do
  describe "CDDL" $ do
    let v = eraProtVerHigh @era
    describe "Ruby-based" $ beforeAllCddlFile 3 readConwayCddlFiles $ do
      cddlRoundTripCborSpec @(Value era) v "positive_coin"
      cddlRoundTripCborSpec @(Value era) v "value"
      cddlRoundTripAnnCborSpec @(TxBody era) v "transaction_body"
      cddlRoundTripCborSpec @(TxBody era) v "transaction_body"
      cddlRoundTripAnnCborSpec @(TxAuxData era) v "auxiliary_data"
      cddlRoundTripCborSpec @(TxAuxData era) v "auxiliary_data"
      cddlRoundTripAnnCborSpec @(Timelock era) v "native_script"
      cddlRoundTripCborSpec @(Timelock era) v "native_script"
      cddlRoundTripAnnCborSpec @(Data era) v "plutus_data"
      cddlRoundTripCborSpec @(Data era) v "plutus_data"
      cddlRoundTripCborSpec @(TxOut era) v "transaction_output"
      cddlRoundTripAnnCborSpec @(Script era) v "script"
      cddlRoundTripCborSpec @(Script era) v "script"
      cddlRoundTripCborSpec @(Datum era) v "datum_option"
      cddlRoundTripAnnCborSpec @(TxWits era) v "transaction_witness_set"
      cddlRoundTripCborSpec @(TxWits era) v "transaction_witness_set"
      cddlRoundTripCborSpec @(PParamsUpdate era) v "protocol_param_update"
      cddlRoundTripCborSpec @CostModels v "cost_models"
      cddlRoundTripAnnCborSpec @(Redeemers era) v "redeemers"
      cddlRoundTripCborSpec @(Redeemers era) v "redeemers"
      cddlRoundTripAnnCborSpec @(Tx era) v "transaction"
      cddlRoundTripCborSpec @(Tx era) v "transaction"
      cddlRoundTripCborSpec @(VotingProcedure era) v "voting_procedure"
      cddlRoundTripCborSpec @(ProposalProcedure era) v "proposal_procedure"
      cddlRoundTripCborSpec @(GovAction era) v "gov_action"
      cddlRoundTripCborSpec @(TxCert era) v "certificate"
      describe "DecCBOR instances equivalence via CDDL" $ do
        cddlDecoderEquivalenceSpec @(TxBody era) v "transaction_body"
        cddlDecoderEquivalenceSpec @(TxAuxData era) v "auxiliary_data"
        cddlDecoderEquivalenceSpec @(Timelock era) v "native_script"
        cddlDecoderEquivalenceSpec @(Data era) v "plutus_data"
        cddlDecoderEquivalenceSpec @(Script era) v "script"
        cddlDecoderEquivalenceSpec @(TxWits era) v "transaction_witness_set"
        cddlDecoderEquivalenceSpec @(Redeemers era) v "redeemers"
        cddlDecoderEquivalenceSpec @(Tx era) v "transaction"
    describe "Huddle" $ specWithHuddle conwayCDDL 100 $ do
      huddleRoundTripCborSpec @(Value era) v "positive_coin"
      huddleRoundTripCborSpec @(Value era) v "value"
      huddleRoundTripAnnCborSpec @(TxBody era) v "transaction_body"
      huddleRoundTripCborSpec @(TxBody era) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData era) v "auxiliary_data"
      huddleRoundTripCborSpec @(TxAuxData era) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Timelock era) v "native_script"
      huddleRoundTripCborSpec @(Timelock era) v "native_script"
      huddleRoundTripAnnCborSpec @(Data era) v "plutus_data"
      huddleRoundTripCborSpec @(Data era) v "plutus_data"
      huddleRoundTripCborSpec @(TxOut era) v "transaction_output"
      huddleRoundTripAnnCborSpec @(Script era) v "script"
      huddleRoundTripCborSpec @(Script era) v "script"
      huddleRoundTripCborSpec @(Datum era) v "datum_option"
      huddleRoundTripAnnCborSpec @(TxWits era) v "transaction_witness_set"
      huddleRoundTripCborSpec @(TxWits era) v "transaction_witness_set"
      huddleRoundTripCborSpec @(PParamsUpdate era) v "protocol_param_update"
      huddleRoundTripCborSpec @CostModels v "cost_models"
      huddleRoundTripAnnCborSpec @(Redeemers era) v "redeemers"
      huddleRoundTripCborSpec @(Redeemers era) v "redeemers"
      huddleRoundTripAnnCborSpec @(Tx era) v "transaction"
      huddleRoundTripCborSpec @(Tx era) v "transaction"
      huddleRoundTripCborSpec @(VotingProcedure era) v "voting_procedure"
      huddleRoundTripCborSpec @(ProposalProcedure era) v "proposal_procedure"
      huddleRoundTripCborSpec @(GovAction era) v "gov_action"
      huddleRoundTripCborSpec @(TxCert era) v "certificate"
      describe "DecCBOR instances equivalence via CDDL" $ do
        huddleDecoderEquivalenceSpec @(TxBody era) v "transaction_body"
        huddleDecoderEquivalenceSpec @(TxAuxData era) v "auxiliary_data"
        huddleDecoderEquivalenceSpec @(Timelock era) v "native_script"
        huddleDecoderEquivalenceSpec @(Data era) v "plutus_data"
        huddleDecoderEquivalenceSpec @(Script era) v "script"
        huddleDecoderEquivalenceSpec @(TxWits era) v "transaction_witness_set"
        huddleDecoderEquivalenceSpec @(Redeemers era) v "redeemers"
        huddleDecoderEquivalenceSpec @(Tx era) v "transaction"
