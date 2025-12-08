{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.HuddleSpec (
  module Cardano.Ledger.Allegra.HuddleSpec,
  maryCDDL,
  maryMultiasset,
  maryValueRule,
  maryMintRule,
  assetNameRule,
) where

import Cardano.Ledger.Allegra.HuddleSpec
import Cardano.Ledger.Mary (MaryEra)
import Codec.CBOR.Cuddle.Huddle
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import Prelude hiding ((/))

maryCDDL :: Huddle
maryCDDL =
  collectFrom
    [ HIRule $ huddleRule @"block" (Proxy @MaryEra)
    , HIRule $ huddleRule @"transaction" (Proxy @MaryEra)
    , HIRule $ huddleRule @"policy_id" (Proxy @MaryEra)
    , HIRule $ huddleRule @"asset_name" (Proxy @MaryEra)
    ]

maryMultiasset ::
  forall era a.
  (HuddleRule "policy_id" era, HuddleRule "asset_name" era, IsType0 a) => Proxy era -> a -> GRuleCall
maryMultiasset p =
  binding $ \x ->
    "multiasset"
      =:= mp
        [ 0
            <+ asKey (huddleRule @"policy_id" p)
            ==> mp [1 <+ asKey (huddleRule @"asset_name" p) ==> x]
        ]

maryValueRule ::
  forall era.
  (HuddleRule "policy_id" era, HuddleRule "asset_name" era) =>
  Proxy era ->
  Rule
maryValueRule p =
  "value"
    =:= huddleRule @"coin" p
    / sarr [a $ huddleRule @"coin" p, a $ maryMultiasset p VUInt]

maryMintRule ::
  forall era.
  (HuddleRule "policy_id" era, HuddleRule "asset_name" era) =>
  Proxy era ->
  Rule
maryMintRule p = "mint" =:= maryMultiasset p (huddleRule @"int64" p)

assetNameRule :: Proxy era -> Rule
assetNameRule _ = "asset_name" =:= VBytes `sized` (0 :: Word64, 32 :: Word64)

instance HuddleRule "block" MaryEra where
  huddleRule = blockRule @MaryEra

instance HuddleRule "transaction" MaryEra where
  huddleRule = transactionRule @MaryEra

instance HuddleRule "header" MaryEra where
  huddleRule = headerRule @MaryEra

instance HuddleRule "header_body" MaryEra where
  huddleRule = headerBodyRule @MaryEra

instance HuddleGroup "protocol_version" MaryEra where
  huddleGroup = shelleyProtocolVersionGroup @MaryEra

instance HuddleRule "major_protocol_version" MaryEra where
  huddleRule = majorProtocolVersionRule @MaryEra

instance HuddleRule "transaction_id" MaryEra where
  huddleRule = transactionIdRule @MaryEra

instance HuddleRule "transaction_input" MaryEra where
  huddleRule = transactionInputRule @MaryEra

instance HuddleGroup "operational_cert" MaryEra where
  huddleGroup = shelleyOperationalCertGroup @MaryEra

instance HuddleRule "vkeywitness" MaryEra where
  huddleRule = vkeywitnessRule @MaryEra

instance HuddleRule "bootstrap_witness" MaryEra where
  huddleRule = bootstrapWitnessRule @MaryEra

instance HuddleRule "transaction_witness_set" MaryEra where
  huddleRule = transactionWitnessSetRule @MaryEra

instance HuddleRule "withdrawals" MaryEra where
  huddleRule = shelleyWithdrawalsRule @MaryEra

instance HuddleRule "certificate" MaryEra where
  huddleRule = certificateRule @MaryEra

instance HuddleGroup "account_registration_cert" MaryEra where
  huddleGroup = accountRegistrationCertGroup @MaryEra

instance HuddleGroup "account_unregistration_cert" MaryEra where
  huddleGroup = accountUnregistrationCertGroup @MaryEra

instance HuddleGroup "delegation_to_stake_pool_cert" MaryEra where
  huddleGroup = delegationToStakePoolCertGroup @MaryEra

instance HuddleGroup "pool_registration_cert" MaryEra where
  huddleGroup = poolRegistrationCertGroup @MaryEra

instance HuddleGroup "pool_retirement_cert" MaryEra where
  huddleGroup = poolRetirementCertGroup @MaryEra

instance HuddleGroup "genesis_delegation_cert" MaryEra where
  huddleGroup = genesisDelegationCertGroup @MaryEra

instance HuddleGroup "move_instantaneous_rewards_cert" MaryEra where
  huddleGroup = moveInstantaneousRewardsCertGroup @MaryEra

instance HuddleRule "genesis_hash" MaryEra where
  huddleRule = genesisHashRule @MaryEra

instance HuddleRule "genesis_delegate_hash" MaryEra where
  huddleRule = genesisDelegateHashRule @MaryEra

instance HuddleRule "delta_coin" MaryEra where
  huddleRule _ = deltaCoinRule

instance HuddleRule "move_instantaneous_reward" MaryEra where
  huddleRule = moveInstantaneousRewardRule @MaryEra

instance HuddleGroup "pool_params" MaryEra where
  huddleGroup = poolParamsGroup @MaryEra

instance HuddleRule "pool_metadata" MaryEra where
  huddleRule = poolMetadataRule @MaryEra

instance HuddleRule "dns_name" MaryEra where
  huddleRule _ = dnsNameRule

instance HuddleRule "url" MaryEra where
  huddleRule _ = urlRule

instance HuddleGroup "single_host_addr" MaryEra where
  huddleGroup = singleHostAddrGroup @MaryEra

instance HuddleGroup "single_host_name" MaryEra where
  huddleGroup = singleHostNameGroup @MaryEra

instance HuddleGroup "multi_host_name" MaryEra where
  huddleGroup = multiHostNameGroup @MaryEra

instance HuddleRule "relay" MaryEra where
  huddleRule = relayRule @MaryEra

instance HuddleRule "protocol_param_update" MaryEra where
  huddleRule = protocolParamUpdateRule @MaryEra

instance HuddleRule "proposed_protocol_parameter_updates" MaryEra where
  huddleRule = proposedProtocolParameterUpdatesRule @MaryEra

instance HuddleRule "update" MaryEra where
  huddleRule = updateRule @MaryEra

instance HuddleGroup "script_pubkey" MaryEra where
  huddleGroup = scriptPubkeyGroup @MaryEra

instance HuddleGroup "script_all" MaryEra where
  huddleGroup = scriptAllGroup @MaryEra

instance HuddleGroup "script_any" MaryEra where
  huddleGroup = scriptAnyGroup @MaryEra

instance HuddleGroup "script_n_of_k" MaryEra where
  huddleGroup = scriptNOfKGroup @MaryEra

instance HuddleGroup "script_invalid_before" MaryEra where
  huddleGroup = scriptInvalidBeforeGroup @MaryEra

instance HuddleGroup "script_invalid_hereafter" MaryEra where
  huddleGroup = scriptInvalidHereafterGroup @MaryEra

instance HuddleRule "native_script" MaryEra where
  huddleRule = nativeScriptRule @MaryEra

instance HuddleRule "transaction_body" MaryEra where
  huddleRule p =
    "transaction_body"
      =:= mp
        [ idx 0 ==> untaggedSet (huddleRule @"transaction_input" p)
        , idx 1 ==> arr [0 <+ a (huddleRule @"transaction_output" p)]
        , idx 2 ==> huddleRule @"coin" p
        , opt (idx 3 ==> huddleRule @"slot" p)
        , opt (idx 4 ==> arr [0 <+ a (huddleRule @"certificate" p)])
        , opt (idx 5 ==> huddleRule @"withdrawals" p)
        , opt (idx 6 ==> huddleRule @"update" p)
        , opt (idx 7 ==> huddleRule @"auxiliary_data_hash" p)
        , opt (idx 8 ==> huddleRule @"slot" p)
        , opt (idx 9 ==> huddleRule @"mint" p)
        ]

instance HuddleRule "transaction_output" MaryEra where
  huddleRule p =
    "transaction_output"
      =:= arr
        [ a $ huddleRule @"address" p
        , "amount" ==> huddleRule @"value" p
        ]

instance HuddleRule "value" MaryEra where
  huddleRule = maryValueRule @MaryEra

instance HuddleRule "policy_id" MaryEra where
  huddleRule p = "policy_id" =:= huddleRule @"script_hash" p

instance HuddleRule "asset_name" MaryEra where
  huddleRule = assetNameRule @MaryEra

instance HuddleRule "mint" MaryEra where
  huddleRule = maryMintRule @MaryEra

instance HuddleRule "auxiliary_data" MaryEra where
  huddleRule = auxiliaryDataRule @MaryEra

instance HuddleRule "auxiliary_data_array" MaryEra where
  huddleRule = auxiliaryDataArrayRule @MaryEra

instance HuddleRule "auxiliary_scripts" MaryEra where
  huddleRule = auxiliaryScriptsRule @MaryEra
