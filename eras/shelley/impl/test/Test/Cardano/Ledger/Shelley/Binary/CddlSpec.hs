{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Binary.CddlSpec (spec) where

import Cardano.Ledger.Address (Addr, RewardAccount)
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.PoolParams (StakePoolRelay)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API (
  Credential,
  MultiSig,
  ProposedPPUpdates,
  Update,
 )
import Cardano.Ledger.TxIn (TxIn)
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
import Test.Cardano.Ledger.Shelley.Binary.Cddl (readShelleyCddlFiles)
import qualified Test.Cardano.Ledger.Shelley.CDDL as ShelleyCDDL

spec :: Spec
spec = do
  newSpec
  describe "CDDL" $ beforeAllCddlFile 3 readShelleyCddlFiles $ do
    let v = eraProtVerLow @ShelleyEra
    cddlRoundTripAnnCborSpec @BootstrapWitness v "bootstrap_witness"
    cddlRoundTripCborSpec @Addr v "address"
    cddlRoundTripCborSpec @RewardAccount v "reward_account"
    cddlRoundTripCborSpec @(Credential 'Staking) v "stake_credential"
    cddlRoundTripAnnCborSpec @(TxBody ShelleyEra) v "transaction_body"
    cddlRoundTripCborSpec @(TxOut ShelleyEra) v "transaction_output"
    cddlRoundTripCborSpec @StakePoolRelay v "relay"
    cddlRoundTripCborSpec @(TxCert ShelleyEra) v "certificate"
    cddlRoundTripCborSpec @TxIn v "transaction_input"
    cddlRoundTripAnnCborSpec @(TxAuxData ShelleyEra) v "transaction_metadata"
    cddlRoundTripAnnCborSpec @(MultiSig ShelleyEra) v "multisig_script"
    cddlRoundTripCborSpec @(Update ShelleyEra) v "update"
    cddlRoundTripCborSpec @(ProposedPPUpdates ShelleyEra) v "proposed_protocol_parameter_updates"
    cddlRoundTripCborSpec @(PParamsUpdate ShelleyEra) v "protocol_param_update"
    cddlRoundTripAnnCborSpec @(Tx ShelleyEra) v "transaction"

newSpec :: Spec
newSpec = describe "Huddle" $ specWithHuddle ShelleyCDDL.shelley 100 $ do
  let v = eraProtVerHigh @ShelleyEra
  huddleRoundTripCborSpec @Addr v "address"
  huddleRoundTripAnnCborSpec @BootstrapWitness v "bootstrap_witness"
  huddleRoundTripCborSpec @RewardAccount v "reward_account"
  huddleRoundTripCborSpec @(Credential 'Staking) v "stake_credential"
  huddleRoundTripAnnCborSpec @(TxBody ShelleyEra) v "transaction_body"
  huddleRoundTripCborSpec @(TxOut ShelleyEra) v "transaction_output"
  huddleRoundTripCborSpec @StakePoolRelay v "relay"
  huddleRoundTripCborSpec @(TxCert ShelleyEra) v "certificate"
  huddleRoundTripCborSpec @TxIn v "transaction_input"
  huddleRoundTripAnnCborSpec @(TxAuxData ShelleyEra) v "transaction_metadata"
  huddleRoundTripAnnCborSpec @(MultiSig ShelleyEra) v "multisig_script"
  huddleRoundTripCborSpec @(Update ShelleyEra) v "update"
  huddleRoundTripCborSpec @(ProposedPPUpdates ShelleyEra) v "proposed_protocol_parameter_updates"
  huddleRoundTripCborSpec @(PParamsUpdate ShelleyEra) v "protocol_param_update"
  huddleRoundTripAnnCborSpec @(Tx ShelleyEra) v "transaction"
  huddleRoundTripAnnCborSpec @(TxWits ShelleyEra) v "transaction_witness_set"
