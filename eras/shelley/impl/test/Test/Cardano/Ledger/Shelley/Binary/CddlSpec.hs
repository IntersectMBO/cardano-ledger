{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Binary.CddlSpec (spec) where

import Cardano.Ledger.Address (Addr, RewardAccount)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.PoolParams (StakePoolRelay)
import Cardano.Ledger.Shelley (Shelley)
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
    let v = eraProtVerLow @Shelley
    cddlRoundTripAnnCborSpec @(BootstrapWitness StandardCrypto) v "bootstrap_witness"
    cddlRoundTripCborSpec @(Addr StandardCrypto) v "address"
    cddlRoundTripCborSpec @(RewardAccount StandardCrypto) v "reward_account"
    cddlRoundTripCborSpec @(Credential 'Staking StandardCrypto) v "stake_credential"
    cddlRoundTripAnnCborSpec @(TxBody Shelley) v "transaction_body"
    cddlRoundTripCborSpec @(TxOut Shelley) v "transaction_output"
    cddlRoundTripCborSpec @StakePoolRelay v "relay"
    cddlRoundTripCborSpec @(TxCert Shelley) v "certificate"
    cddlRoundTripCborSpec @(TxIn StandardCrypto) v "transaction_input"
    cddlRoundTripAnnCborSpec @(TxAuxData Shelley) v "transaction_metadata"
    cddlRoundTripAnnCborSpec @(MultiSig Shelley) v "multisig_script"
    cddlRoundTripCborSpec @(Update Shelley) v "update"
    cddlRoundTripCborSpec @(ProposedPPUpdates Shelley) v "proposed_protocol_parameter_updates"
    cddlRoundTripCborSpec @(PParamsUpdate Shelley) v "protocol_param_update"
    cddlRoundTripAnnCborSpec @(Tx Shelley) v "transaction"

newSpec :: Spec
newSpec = describe "Huddle" $ specWithHuddle ShelleyCDDL.shelley 100 $ do
  let v = eraProtVerHigh @Shelley
  huddleRoundTripCborSpec @(Addr StandardCrypto) v "address"
  huddleRoundTripAnnCborSpec @(BootstrapWitness StandardCrypto) v "bootstrap_witness"
  huddleRoundTripCborSpec @(RewardAccount StandardCrypto) v "reward_account"
  huddleRoundTripCborSpec @(Credential 'Staking StandardCrypto) v "stake_credential"
  huddleRoundTripAnnCborSpec @(TxBody Shelley) v "transaction_body"
  huddleRoundTripCborSpec @(TxOut Shelley) v "transaction_output"
  huddleRoundTripCborSpec @StakePoolRelay v "relay"
  huddleRoundTripCborSpec @(TxCert Shelley) v "certificate"
  huddleRoundTripCborSpec @(TxIn StandardCrypto) v "transaction_input"
  huddleRoundTripAnnCborSpec @(TxAuxData Shelley) v "transaction_metadata"
  huddleRoundTripAnnCborSpec @(MultiSig Shelley) v "multisig_script"
  huddleRoundTripCborSpec @(Update Shelley) v "update"
  huddleRoundTripCborSpec @(ProposedPPUpdates Shelley) v "proposed_protocol_parameter_updates"
  huddleRoundTripCborSpec @(PParamsUpdate Shelley) v "protocol_param_update"
  huddleRoundTripAnnCborSpec @(Tx Shelley) v "transaction"
  huddleRoundTripAnnCborSpec @(TxWits Shelley) v "transaction_witness_set"
