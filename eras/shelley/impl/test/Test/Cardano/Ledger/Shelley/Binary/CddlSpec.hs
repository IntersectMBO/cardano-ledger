{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Binary.CddlSpec (spec) where

import Cardano.Ledger.Address (Addr, RewardAccount)
import Cardano.Ledger.Core
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API (
  Credential,
  MultiSig,
  ProposedPPUpdates,
  Update,
 )
import Cardano.Ledger.State (StakePoolRelay)
import Cardano.Ledger.TxIn (TxIn)
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
import Test.Cardano.Ledger.Shelley.Binary.Annotator ()
import Test.Cardano.Ledger.Shelley.Binary.Cddl (readShelleyCddlFiles)
import Test.Cardano.Ledger.Shelley.CDDL (shelleyCDDL)

spec :: Spec
spec =
  describe "CDDL" $ do
    let v = eraProtVerLow @ShelleyEra
    describe "Ruby-based" $ beforeAllCddlFile 3 readShelleyCddlFiles $ do
      cddlRoundTripAnnCborSpec @BootstrapWitness v "bootstrap_witness"
      cddlRoundTripCborSpec @BootstrapWitness v "bootstrap_witness"
      cddlRoundTripCborSpec @Addr v "address"
      cddlRoundTripCborSpec @RewardAccount v "reward_account"
      cddlRoundTripCborSpec @(Credential 'Staking) v "stake_credential"
      cddlRoundTripAnnCborSpec @(TxBody ShelleyEra) v "transaction_body"
      cddlRoundTripCborSpec @(TxBody ShelleyEra) v "transaction_body"
      cddlRoundTripCborSpec @(TxOut ShelleyEra) v "transaction_output"
      cddlRoundTripCborSpec @StakePoolRelay v "relay"
      cddlRoundTripCborSpec @(TxCert ShelleyEra) v "certificate"
      cddlRoundTripCborSpec @TxIn v "transaction_input"
      cddlRoundTripAnnCborSpec @(TxAuxData ShelleyEra) v "transaction_metadata"
      cddlRoundTripCborSpec @(TxAuxData ShelleyEra) v "transaction_metadata"
      cddlRoundTripAnnCborSpec @(MultiSig ShelleyEra) v "multisig_script"
      cddlRoundTripCborSpec @(MultiSig ShelleyEra) v "multisig_script"
      cddlRoundTripCborSpec @(Update ShelleyEra) v "update"
      cddlRoundTripCborSpec @(ProposedPPUpdates ShelleyEra) v "proposed_protocol_parameter_updates"
      cddlRoundTripCborSpec @(PParamsUpdate ShelleyEra) v "protocol_param_update"
      cddlRoundTripAnnCborSpec @(Tx ShelleyEra) v "transaction"
      cddlRoundTripCborSpec @(Tx ShelleyEra) v "transaction"
      describe "DecCBOR instances equivalence via CDDL" $ do
        cddlDecoderEquivalenceSpec @BootstrapWitness v "bootstrap_witness"
        cddlDecoderEquivalenceSpec @(TxBody ShelleyEra) v "transaction_body"
        cddlDecoderEquivalenceSpec @(TxAuxData ShelleyEra) v "transaction_metadata"
        cddlDecoderEquivalenceSpec @(MultiSig ShelleyEra) v "multisig_script"
        cddlDecoderEquivalenceSpec @(Tx ShelleyEra) v "transaction"

    describe "Huddle" $ specWithHuddle shelleyCDDL 100 $ do
      huddleRoundTripCborSpec @Addr v "address"
      -- TODO re-enable this once we've removed the hard-coded definition for `address`
      xdescribe "bad CDDL" $ huddleRoundTripArbitraryValidate @Addr v "address"
      huddleRoundTripAnnCborSpec @BootstrapWitness v "bootstrap_witness"
      huddleRoundTripArbitraryValidate @BootstrapWitness v "bootstrap_witness"
      huddleRoundTripCborSpec @BootstrapWitness v "bootstrap_witness"
      huddleRoundTripCborSpec @RewardAccount v "reward_account"
      huddleRoundTripCborSpec @(Credential 'Staking) v "stake_credential"
      huddleRoundTripAnnCborSpec @(TxBody ShelleyEra) v "transaction_body"
      huddleRoundTripCborSpec @(TxBody ShelleyEra) v "transaction_body"
      huddleRoundTripCborSpec @(TxOut ShelleyEra) v "transaction_output"
      huddleRoundTripCborSpec @StakePoolRelay v "relay"
      huddleRoundTripCborSpec @(TxCert ShelleyEra) v "certificate"
      huddleRoundTripCborSpec @TxIn v "transaction_input"
      huddleRoundTripAnnCborSpec @(TxAuxData ShelleyEra) v "transaction_metadata"
      huddleRoundTripCborSpec @(TxAuxData ShelleyEra) v "transaction_metadata"
      huddleRoundTripAnnCborSpec @(MultiSig ShelleyEra) v "multisig_script"
      huddleRoundTripCborSpec @(MultiSig ShelleyEra) v "multisig_script"
      huddleRoundTripCborSpec @(Update ShelleyEra) v "update"
      huddleRoundTripCborSpec @(ProposedPPUpdates ShelleyEra) v "proposed_protocol_parameter_updates"
      huddleRoundTripCborSpec @(PParamsUpdate ShelleyEra) v "protocol_param_update"
      huddleRoundTripAnnCborSpec @(Tx ShelleyEra) v "transaction"
      huddleRoundTripCborSpec @(Tx ShelleyEra) v "transaction"
      huddleRoundTripAnnCborSpec @(TxWits ShelleyEra) v "transaction_witness_set"
      huddleRoundTripCborSpec @(TxWits ShelleyEra) v "transaction_witness_set"
      describe "DecCBOR instances equivalence via CDDL" $ do
        huddleDecoderEquivalenceSpec @BootstrapWitness v "bootstrap_witness"
        huddleDecoderEquivalenceSpec @(TxBody ShelleyEra) v "transaction_body"
        huddleDecoderEquivalenceSpec @(TxAuxData ShelleyEra) v "transaction_metadata"
        huddleDecoderEquivalenceSpec @(MultiSig ShelleyEra) v "multisig_script"
        huddleDecoderEquivalenceSpec @(Tx ShelleyEra) v "transaction"
