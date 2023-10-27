{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Binary.CddlSpec (spec) where

import Cardano.Ledger.Address (Addr, RewardAcnt)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (Staking))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.API (
  Credential,
  MultiSig,
  ProposedPPUpdates,
  Update,
 )
import Cardano.Ledger.Shelley.TxBody (StakePoolRelay)
import Cardano.Ledger.TxIn (TxIn)
import Test.Cardano.Ledger.Binary.Cddl (
  beforeAllCddlFile,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.Binary.Cddl (readShelleyCddlFiles)

spec :: Spec
spec =
  describe "CDDL" $ beforeAllCddlFile 3 readShelleyCddlFiles $ do
    let v = eraProtVerLow @Shelley
    cddlRoundTripAnnCborSpec @(BootstrapWitness StandardCrypto) v "bootstrap_witness"
    cddlRoundTripCborSpec @(Addr StandardCrypto) v "address"
    cddlRoundTripCborSpec @(RewardAcnt StandardCrypto) v "reward_account"
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
