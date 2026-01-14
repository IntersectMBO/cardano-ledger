{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wwarn #-}

module Test.Cardano.Ledger.Conway.Scls (
  spec,
) where

import Cardano.Ledger.Address (CompactAddr, RewardAccount)
import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript, Prices)
import Cardano.Ledger.BaseTypes (
  Anchor,
  EpochInterval,
  EpochNo,
  NonNegativeInterval,
  Nonce,
  ProtVer,
  SlotNo,
  UnitInterval,
 )
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (
  GovActionPurpose (..),
  Vote (..),
 )
import Cardano.Ledger.Conway.SCLS
import Cardano.Ledger.Conway.SCLS.Arbitrary ()
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Hashes qualified as H
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Plutus.CostModels
import Cardano.Ledger.Plutus.Data (BinaryData)
import Cardano.Ledger.Shelley.TxOut qualified as Shelley
import Cardano.Ledger.State (
  PoolMetadata (..),
  StakePoolParams (..),
  StakePoolRelay (..),
 )
import Cardano.SCLS.Testlib
import Data.Typeable
import GHC.TypeLits
import Test.Cardano.Ledger.Common

spec :: Spec
spec = do
  describe "Cardano ledger state: namespaces tests" testAllNS
  describe "Cardano ledger state: instace tests" $ do
    isCanonical @"gov/pparams/v0" @CanonicalPoolVotingThresholds
    isCanonical @"gov/pparams/v0" @CanonicalDRepVotingThresholds
    isCanonical @"gov/pparams/v0" @CanonicalExUnits
    isCanonical @"gov/proposals/v0" @(Vote)
    isCanonical @"gov/proposals/v0" @(CanonicalPurposeId PParamUpdatePurpose)
    isCanonical @"snapshots/v0" @(RewardAccount)
    isCanonical @"snapshots/v0" @(PoolMetadata)
    isCanonical @"snapshots/v0" @(StakePoolRelay)
    isCanonical @"snapshots/v0" @(StakePoolParams)
    isCanonical @"utxo/v0" @(Shelley.ShelleyTxOut ConwayEra)
    isCanonical @"utxo/v0" @(AlonzoScript ConwayEra)
    isCanonical @"utxo/v0" @MaryValue
    isCanonical @"utxo/v0" @CompactAddr
    isCanonical @"utxo/v0" @(Timelock ConwayEra)
    isCanonical @"utxo/v0" @(BinaryData ConwayEra)
    isCanonical @"common" @(ScriptHash)
    isCanonical @"common" @(KeyHash Guard)
    isCanonical @"common" @(VRFVerKeyHash BlockIssuerVRF)
    isCanonical @"common" @(NonNegativeInterval)
    isCanonical @"common" @(UnitInterval)
    isCanonical @"common" @(Prices)
    isCanonical @"common" @(EpochInterval)
    isCanonical @"common" @(ProtVer)
    isCanonical @"common" @(Anchor)
    isCanonical @"common" @(EpochNo)
  describe "Cardano ledger state: CDDL conformance tests" $ do
    describe "gov/committee/v0" $ do
      validateType @"gov/committee/v0" @CanonicalCommitteeState "committee"
      validateType @"gov/committee/v0" @CanonicalCommitteeAuthorization "committee_authorization"
    describe "gov/pparams/v0" $ do
      validateType @"gov/pparams/v0" @CanonicalPoolVotingThresholds "pool_voting_thresholds"
      validateType @"gov/pparams/v0" @CanonicalDRepVotingThresholds "drep_voting_thresholds"
      validateType @"gov/pparams/v0" @CostModels "cost_models"
      validateType @"gov/pparams/v0" @CanonicalExUnits "ex_units"
      validateType @"gov/pparams/v0" @CanonicalPParams "gov_pparams_out"
      validateType @"gov/proposals/v0" @CanonicalPParamsUpdate "gov_params_update"
    describe "gov/constitution/v0" $ do
      validateType @"gov/constitution/v0" @CanonicalConstitution "constitution"
    describe "gov/proposals/v0" $ do
      validateType @"gov/proposals/v0" @CanonicalGovActionState "proposal"
      validateType @"gov/proposals/v0" @CanonicalProposalProcedure "proposal_procedure"
      validateType @"gov/proposals/v0" @CanonicalGovAction "gov_action"
      validateType @"gov/proposals/v0" @(CanonicalPurposeId PParamUpdatePurpose) "gov_action_id"
      validateType @"gov/proposals/v0" @(Vote) "coin"
    describe "snapshots/v0" $ do
      validateType @"snapshots/v0" @(StakePoolParams) "pool_params"
      validateType @"snapshots/v0" @(RewardAccount) "reward_account"
      validateType @"snapshots/v0" @(PoolMetadata) "pool_metadata"
      validateType @"snapshots/v0" @(StakePoolRelay) "relay"
    describe "utxo/v0" $ do
      validateType @"utxo/v0" @(Shelley.ShelleyTxOut ConwayEra) "shelley_tx_out"
      validateType @"utxo/v0" @(AlonzoScript ConwayEra) "script"
      validateType @"utxo/v0" @MaryValue "value"
      validateType @"utxo/v0" @CompactAddr "address"
      validateType @"utxo/v0" @(Timelock ConwayEra) "native_script"
    describe "common" $ do
      validateType @"gov/proposals/v0" @(Credential Guard) "credential"
      validateType @"gov/constitution/v0" @(ScriptHash) "script_hash"
      validateType @"gov/proposals/v0" @(KeyHash Guard) "addr_keyhash"
      validateType @"pool_stake/v0" @(VRFVerKeyHash StakePoolVRF) "vrf_keyhash"
      validateType @"gov/proposals/v0" @(NonNegativeInterval) "nonnegative_interval"
      validateType @"gov/pparams/v0" @(UnitInterval) "unit_interval"
      validateType @"gov/pparams/v0" @(Prices) "ex_unit_prices"
      validateType @"gov/pparams/v0" @(EpochInterval) "epoch_interval"
      validateType @"gov/pparams/v0" @(ProtVer) "protocol_version"
      validateType @"gov/committee/v0" @(Anchor) "anchor"
      validateType @"gov/proposals/v0" @(EpochNo) "epoch_no"
      validateType @"utxo/v0" @(SlotNo) "slot_no"
    describe "nonces/v0" $ do
      validateType @"nonces/v0" @(CanonicalNonce) "nonce"
      validateType @"nonces/v0" @(H.Hash H.HASH Nonce) "hash32"

isCanonical ::
  forall ns a. (KnownSymbol ns, ToCanonicalCBOR ns a, Typeable a, Arbitrary a, Show a) => Spec
isCanonical = go
  where
    typ = showsTypeRep (typeRep (Proxy @a))
    go = prop (typ " is canonical") $ propTypeIsCanonical @ns @a
