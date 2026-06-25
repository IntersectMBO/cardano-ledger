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
  MaryEra,
  maryCDDL,
  maryMultiasset,
  maryValueRule,
  maryMintRule,
  assetNameRule,
) where

import Cardano.Ledger.Allegra.HuddleSpec
import Cardano.Ledger.Mary (MaryEra)
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
  (HuddleRule "policy_id" era, HuddleRule "asset_name" era, IsType0 a) =>
  Proxy "multiasset" ->
  Proxy era ->
  a ->
  GRuleCall
maryMultiasset pname p =
  binding $ \x ->
    pname
      =.= mp
        [ 0
            <+ asKey (huddleRule @"policy_id" p)
            ==> mp [1 <+ asKey (huddleRule @"asset_name" p) ==> x]
        ]

maryValueRule ::
  forall era.
  HuddleRule1 "multiasset" era =>
  Proxy "value" ->
  Proxy era ->
  Rule
maryValueRule pname p =
  pname
    =.= huddleRule @"coin" p
    / sarr [a $ huddleRule @"coin" p, a $ huddleRule1 @"multiasset" p VUInt]

maryMintRule ::
  forall era.
  HuddleRule1 "multiasset" era =>
  Proxy "mint" ->
  Proxy era ->
  Rule
maryMintRule pname p = pname =.= huddleRule1 @"multiasset" p (huddleRule @"int64" p)

assetNameRule :: Proxy "asset_name" -> Rule
assetNameRule pname = pname =.= VBytes `sized` (0 :: Word64, 32 :: Word64)

instance HuddleRule "block" MaryEra where
  huddleRuleNamed = blockRule

instance HuddleRule "transaction" MaryEra where
  huddleRuleNamed = transactionRule

instance HuddleRule "header" MaryEra where
  huddleRuleNamed = headerRule

instance HuddleRule "header_body" MaryEra where
  huddleRuleNamed = shelleyHeaderBodyRule

instance HuddleGroup "protocol_version" MaryEra where
  huddleGroupNamed = shelleyProtocolVersionGroup

instance HuddleRule "major_protocol_version" MaryEra where
  huddleRuleNamed = majorProtocolVersionRule

instance HuddleRule "transaction_id" MaryEra where
  huddleRuleNamed = transactionIdRule

instance HuddleRule "transaction_input" MaryEra where
  huddleRuleNamed = transactionInputRule

instance HuddleGroup "operational_cert" MaryEra where
  huddleGroupNamed = shelleyOperationalCertGroup

instance HuddleRule "vkeywitness" MaryEra where
  huddleRuleNamed = vkeywitnessRule

instance HuddleRule "bootstrap_witness" MaryEra where
  huddleRuleNamed = bootstrapWitnessRule

instance HuddleRule "transaction_witness_set" MaryEra where
  huddleRuleNamed = transactionWitnessSetRule

instance HuddleRule "withdrawals" MaryEra where
  huddleRuleNamed = shelleyWithdrawalsRule

instance HuddleRule "certificate" MaryEra where
  huddleRuleNamed = certificateRule

instance HuddleGroup "account_registration_cert" MaryEra where
  huddleGroupNamed = accountRegistrationCertGroup

instance HuddleGroup "account_unregistration_cert" MaryEra where
  huddleGroupNamed = accountUnregistrationCertGroup

instance HuddleGroup "delegation_to_stake_pool_cert" MaryEra where
  huddleGroupNamed = delegationToStakePoolCertGroup

instance HuddleGroup "pool_registration_cert" MaryEra where
  huddleGroupNamed = poolRegistrationCertGroup

instance HuddleGroup "pool_retirement_cert" MaryEra where
  huddleGroupNamed = poolRetirementCertGroup

instance HuddleGroup "genesis_delegation_cert" MaryEra where
  huddleGroupNamed = genesisDelegationCertGroup

instance HuddleGroup "move_instantaneous_rewards_cert" MaryEra where
  huddleGroupNamed = moveInstantaneousRewardsCertGroup

instance HuddleRule "genesis_hash" MaryEra where
  huddleRuleNamed = genesisHashRule

instance HuddleRule "genesis_delegate_hash" MaryEra where
  huddleRuleNamed = genesisDelegateHashRule

instance HuddleRule "delta_coin" MaryEra where
  huddleRuleNamed pname _ = deltaCoinRule pname

instance HuddleRule "move_instantaneous_reward" MaryEra where
  huddleRuleNamed = moveInstantaneousRewardRule

instance HuddleGroup "pool_params" MaryEra where
  huddleGroupNamed = poolParamsGroup

instance HuddleRule "pool_metadata" MaryEra where
  huddleRuleNamed = poolMetadataRule

instance HuddleRule "dns_name" MaryEra where
  huddleRuleNamed pname _ = dnsNameRule pname

instance HuddleRule "url" MaryEra where
  huddleRuleNamed pname _ = urlRule pname

instance HuddleGroup "single_host_addr" MaryEra where
  huddleGroupNamed = singleHostAddrGroup

instance HuddleGroup "single_host_name" MaryEra where
  huddleGroupNamed = singleHostNameGroup

instance HuddleGroup "multi_host_name" MaryEra where
  huddleGroupNamed = multiHostNameGroup

instance HuddleRule "relay" MaryEra where
  huddleRuleNamed = relayRule

instance HuddleRule "protocol_param_update" MaryEra where
  huddleRuleNamed = protocolParamUpdateRule

instance HuddleRule "proposed_protocol_parameter_updates" MaryEra where
  huddleRuleNamed = proposedProtocolParameterUpdatesRule

instance HuddleRule "update" MaryEra where
  huddleRuleNamed = updateRule

instance HuddleGroup "script_pubkey" MaryEra where
  huddleGroupNamed = scriptPubkeyGroup

instance HuddleGroup "script_all" MaryEra where
  huddleGroupNamed = scriptAllGroup

instance HuddleGroup "script_any" MaryEra where
  huddleGroupNamed = scriptAnyGroup

instance HuddleGroup "script_n_of_k" MaryEra where
  huddleGroupNamed = scriptNOfKGroup

instance HuddleGroup "script_invalid_before" MaryEra where
  huddleGroupNamed = scriptInvalidBeforeGroup

instance HuddleGroup "script_invalid_hereafter" MaryEra where
  huddleGroupNamed = scriptInvalidHereafterGroup

instance HuddleRule "native_script" MaryEra where
  huddleRuleNamed = nativeScriptRule

instance HuddleRule "transaction_body" MaryEra where
  huddleRuleNamed pname p =
    pname
      =.= mp
        [ idx 0 ==> huddleRule1 @"set" p (huddleRule @"transaction_input" p)
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
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ a $ huddleRule @"address" p
        , "amount" ==> huddleRule @"value" p
        ]

instance HuddleRule "value" MaryEra where
  huddleRuleNamed = maryValueRule

instance HuddleRule "policy_id" MaryEra where
  huddleRuleNamed pname p = pname =.= huddleRule @"script_hash" p

instance HuddleRule "asset_name" MaryEra where
  huddleRuleNamed pname _ = assetNameRule pname

instance HuddleRule "mint" MaryEra where
  huddleRuleNamed = maryMintRule

instance HuddleRule "auxiliary_data" MaryEra where
  huddleRuleNamed = auxiliaryDataRule

instance HuddleRule "auxiliary_data_array" MaryEra where
  huddleRuleNamed = auxiliaryDataArrayRule

instance HuddleRule "auxiliary_scripts" MaryEra where
  huddleRuleNamed = auxiliaryScriptsRule

instance HuddleRule1 "set" MaryEra where
  huddleRule1Named pname _ = untaggedSet pname

instance HuddleRule1 "multiasset" MaryEra where
  huddleRule1Named = maryMultiasset
