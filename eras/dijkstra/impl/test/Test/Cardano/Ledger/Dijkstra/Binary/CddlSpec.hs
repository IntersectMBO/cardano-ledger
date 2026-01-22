{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Binary.CddlSpec (spec) where

import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Conway.Governance (GovAction, ProposalProcedure, VotingProcedure)
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.HuddleSpec (dijkstraCDDL)
import Cardano.Ledger.Plutus.Data (Data, Datum)
import Test.Cardano.Ledger.Binary.Cuddle (
  huddleDecoderEquivalenceSpec,
  huddleRoundTripAnnCborSpec,
  huddleRoundTripArbitraryValidate,
  huddleRoundTripCborSpec,
  specWithHuddle,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.Binary.Annotator ()

spec :: Spec
spec = do
  describe "CDDL" $ do
    let v = eraProtVerHigh @DijkstraEra
    specWithHuddle dijkstraCDDL 100 $ do
      huddleRoundTripCborSpec @(Value DijkstraEra) v "positive_coin"
      huddleRoundTripArbitraryValidate @(Value DijkstraEra) v "value"
      describe "MultiAsset" $ do
        huddleRoundTripCborSpec @(Value DijkstraEra) v "value"
      xdescribe "fix TxBody" $ do
        huddleRoundTripAnnCborSpec @(TxBody TopTx DijkstraEra) v "transaction_body"
        huddleRoundTripCborSpec @(TxBody TopTx DijkstraEra) v "transaction_body"
      -- TODO enable this once map/list expansion has been optimized in cuddle
      xdescribe "hangs" $
        huddleRoundTripArbitraryValidate @(TxBody TopTx DijkstraEra) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData DijkstraEra) v "auxiliary_data"
      -- TODO fails because of plutus scripts
      xdescribe "fix plutus scripts" $ do
        huddleRoundTripArbitraryValidate @(TxAuxData DijkstraEra) v "auxiliary_data"
        huddleRoundTripCborSpec @(TxAuxData DijkstraEra) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(NativeScript DijkstraEra) v "native_script"
      huddleRoundTripArbitraryValidate @(NativeScript DijkstraEra) v "native_script"
      huddleRoundTripCborSpec @(NativeScript DijkstraEra) v "native_script"
      huddleRoundTripAnnCborSpec @(Data DijkstraEra) v "plutus_data"
      huddleRoundTripArbitraryValidate @(Data DijkstraEra) v "plutus_data"
      huddleRoundTripCborSpec @(Data DijkstraEra) v "plutus_data"
      xdescribe "fix TxOut" $ do
        huddleRoundTripCborSpec @(TxOut DijkstraEra) v "transaction_output"
      -- TODO fails because of `address`
      xdescribe "fix address" $
        huddleRoundTripArbitraryValidate @(TxOut DijkstraEra) v "transaction_output"
      xdescribe "fix Script" $ do
        huddleRoundTripAnnCborSpec @(Script DijkstraEra) v "script"
        huddleRoundTripCborSpec @(Script DijkstraEra) v "script"
      -- TODO fails because of `plutus_v1_script`
      xdescribe "fix plutus_v1_script" $ huddleRoundTripArbitraryValidate @(Script DijkstraEra) v "script"
      huddleRoundTripCborSpec @(Datum DijkstraEra) v "datum_option"
      -- TODO NoDatum is encoded as an empty bytestring
      xdescribe "fix NoDatum" $ huddleRoundTripArbitraryValidate @(Datum DijkstraEra) v "datum_option"
      -- TODO enable once CDDL sets no longer generate duplicate elements
      xdescribe "fix duplicates in maps" $ do
        huddleRoundTripAnnCborSpec @(TxWits DijkstraEra) v "transaction_witness_set"
        huddleRoundTripCborSpec @(TxWits DijkstraEra) v "transaction_witness_set"
      -- TODO fails because of plutus_v1_script
      xdescribe "fix plutus_v1_script" $
        huddleRoundTripArbitraryValidate @(TxWits DijkstraEra) v "transaction_witness_set"
      huddleRoundTripCborSpec @(PParamsUpdate DijkstraEra) v "protocol_param_update"
      -- TODO enable this once map/list expansion has been optimized in cuddle
      xdescribe "hangs" $ do
        huddleRoundTripArbitraryValidate @(PParamsUpdate DijkstraEra) v "protocol_param_update"
      huddleRoundTripCborSpec @CostModels v "cost_models"
      huddleRoundTripArbitraryValidate @CostModels v "cost_models"
      huddleRoundTripAnnCborSpec @(Redeemers DijkstraEra) v "redeemers"
      -- TODO arbitrary can generate empty redeemers, which is not allowed in the CDDL
      xdescribe "fix redeemers" $ huddleRoundTripArbitraryValidate @(Redeemers DijkstraEra) v "redeemers"
      huddleRoundTripCborSpec @(Redeemers DijkstraEra) v "redeemers"
      xdescribe "fix Transaction" $ do
        huddleRoundTripAnnCborSpec @(Tx TopTx DijkstraEra) v "transaction"
        huddleRoundTripCborSpec @(Tx TopTx DijkstraEra) v "transaction"
      -- TODO enable this once map/list expansion has been optimized in cuddle
      xdescribe "hangs" $ huddleRoundTripArbitraryValidate @(Tx TopTx DijkstraEra) v "transaction"
      huddleRoundTripCborSpec @(VotingProcedure DijkstraEra) v "voting_procedure"
      huddleRoundTripArbitraryValidate @(VotingProcedure DijkstraEra) v "voting_procedure"
      huddleRoundTripCborSpec @(ProposalProcedure DijkstraEra) v "proposal_procedure"
      -- TODO This fails because of the hard-coded `reward_account` in the CDDL
      xdescribe "fix reward_account" $
        huddleRoundTripArbitraryValidate @(ProposalProcedure DijkstraEra) v "proposal_procedure"
      huddleRoundTripCborSpec @(GovAction DijkstraEra) v "gov_action"
      -- TODO enable this once map/list expansion has been optimized in cuddle
      xdescribe "hangs" $ huddleRoundTripArbitraryValidate @(GovAction DijkstraEra) v "gov_action"
      describe "TxCert" $ do
        huddleRoundTripCborSpec @(TxCert DijkstraEra) v "certificate"
      -- TODO this fails because of the hard-coded `unit_interval` in the CDDL
      xdescribe "fix unit_interval" $
        huddleRoundTripArbitraryValidate @(TxCert DijkstraEra) v "certificate"
      describe "DecCBOR instances equivalence via CDDL" $ do
        huddleDecoderEquivalenceSpec @(TxBody TopTx DijkstraEra) v "transaction_body"
        xdescribe "Fix decoder equivalence of TxAuxData" $ do
          huddleDecoderEquivalenceSpec @(TxAuxData DijkstraEra) v "auxiliary_data"
        huddleDecoderEquivalenceSpec @(NativeScript DijkstraEra) v "native_script"
        huddleDecoderEquivalenceSpec @(Data DijkstraEra) v "plutus_data"
        huddleDecoderEquivalenceSpec @(Script DijkstraEra) v "script"
        huddleDecoderEquivalenceSpec @(TxWits DijkstraEra) v "transaction_witness_set"
        huddleDecoderEquivalenceSpec @(Redeemers DijkstraEra) v "redeemers"
        xdescribe "Fix decoder equivalence of Tx" $ do
          huddleDecoderEquivalenceSpec @(Tx TopTx DijkstraEra) v "transaction"
