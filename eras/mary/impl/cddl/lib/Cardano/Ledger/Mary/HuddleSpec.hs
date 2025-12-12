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
import Codec.CBOR.Cuddle.Huddle
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import GHC.TypeLits (KnownSymbol)
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
  forall name era a.
  (KnownSymbol name, HuddleRule "policy_id" era, HuddleRule "asset_name" era, IsType0 a) =>
  Proxy name ->
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
  forall name era.
  (KnownSymbol name, HuddleRule "policy_id" era, HuddleRule "asset_name" era) =>
  Proxy name ->
  Proxy era ->
  Rule
maryValueRule pname p =
  pname
    =.= huddleRule @"coin" p
    / sarr [a $ huddleRule @"coin" p, a $ maryMultiasset (Proxy @"multiasset") p VUInt]

maryMintRule ::
  forall name era.
  (KnownSymbol name, HuddleRule "policy_id" era, HuddleRule "asset_name" era) =>
  Proxy name ->
  Proxy era ->
  Rule
maryMintRule pname p = pname =.= maryMultiasset (Proxy @"multiasset") p (huddleRule @"int64" p)

assetNameRule :: forall name. KnownSymbol name => Proxy name -> Rule
assetNameRule pname = pname =.= VBytes `sized` (0 :: Word64, 32 :: Word64)

instance HuddleRule "block" MaryEra where
  huddleRuleNamed pname p = blockRule pname p

instance HuddleRule "transaction" MaryEra where
  huddleRuleNamed pname p = transactionRule pname p

instance HuddleRule "header" MaryEra where
  huddleRuleNamed pname p = headerRule pname p

instance HuddleRule "header_body" MaryEra where
  huddleRuleNamed pname p = headerBodyRule pname p

instance HuddleGroup "protocol_version" MaryEra where
  huddleGroupNamed pname p = shelleyProtocolVersionGroup pname p

instance HuddleRule "major_protocol_version" MaryEra where
  huddleRuleNamed pname p = majorProtocolVersionRule pname p

instance HuddleRule "transaction_id" MaryEra where
  huddleRuleNamed pname p = transactionIdRule pname p

instance HuddleRule "transaction_input" MaryEra where
  huddleRuleNamed pname p = transactionInputRule pname p

instance HuddleGroup "operational_cert" MaryEra where
  huddleGroupNamed pname p = shelleyOperationalCertGroup pname p

instance HuddleRule "vkeywitness" MaryEra where
  huddleRuleNamed pname p = vkeywitnessRule pname p

instance HuddleRule "bootstrap_witness" MaryEra where
  huddleRuleNamed pname p = bootstrapWitnessRule pname p

instance HuddleRule "transaction_witness_set" MaryEra where
  huddleRuleNamed pname p = transactionWitnessSetRule pname p

instance HuddleRule "withdrawals" MaryEra where
  huddleRuleNamed pname p = shelleyWithdrawalsRule pname p

instance HuddleRule "certificate" MaryEra where
  huddleRuleNamed pname p = certificateRule pname p

instance HuddleGroup "account_registration_cert" MaryEra where
  huddleGroupNamed pname p = accountRegistrationCertGroup pname p

instance HuddleGroup "account_unregistration_cert" MaryEra where
  huddleGroupNamed pname p = accountUnregistrationCertGroup pname p

instance HuddleGroup "delegation_to_stake_pool_cert" MaryEra where
  huddleGroupNamed pname p = delegationToStakePoolCertGroup pname p

instance HuddleGroup "pool_registration_cert" MaryEra where
  huddleGroupNamed pname p = poolRegistrationCertGroup pname p

instance HuddleGroup "pool_retirement_cert" MaryEra where
  huddleGroupNamed pname p = poolRetirementCertGroup pname p

instance HuddleGroup "genesis_delegation_cert" MaryEra where
  huddleGroupNamed pname p = genesisDelegationCertGroup pname p

instance HuddleGroup "move_instantaneous_rewards_cert" MaryEra where
  huddleGroupNamed pname p = moveInstantaneousRewardsCertGroup pname p

instance HuddleRule "genesis_hash" MaryEra where
  huddleRuleNamed pname p = genesisHashRule pname p

instance HuddleRule "genesis_delegate_hash" MaryEra where
  huddleRuleNamed pname p = genesisDelegateHashRule pname p

instance HuddleRule "delta_coin" MaryEra where
  huddleRuleNamed pname _ = deltaCoinRule pname

instance HuddleRule "move_instantaneous_reward" MaryEra where
  huddleRuleNamed pname p = moveInstantaneousRewardRule pname p

instance HuddleGroup "pool_params" MaryEra where
  huddleGroupNamed pname p = poolParamsGroup pname p

instance HuddleRule "pool_metadata" MaryEra where
  huddleRuleNamed pname p = poolMetadataRule pname p

instance HuddleRule "dns_name" MaryEra where
  huddleRuleNamed pname _ = dnsNameRule pname

instance HuddleRule "url" MaryEra where
  huddleRuleNamed pname _ = urlRule pname

instance HuddleGroup "single_host_addr" MaryEra where
  huddleGroupNamed pname p = singleHostAddrGroup pname p

instance HuddleGroup "single_host_name" MaryEra where
  huddleGroupNamed pname p = singleHostNameGroup pname p

instance HuddleGroup "multi_host_name" MaryEra where
  huddleGroupNamed pname p = multiHostNameGroup pname p

instance HuddleRule "relay" MaryEra where
  huddleRuleNamed pname p = relayRule pname p

instance HuddleRule "protocol_param_update" MaryEra where
  huddleRuleNamed pname p = protocolParamUpdateRule pname p

instance HuddleRule "proposed_protocol_parameter_updates" MaryEra where
  huddleRuleNamed pname p = proposedProtocolParameterUpdatesRule pname p

instance HuddleRule "update" MaryEra where
  huddleRuleNamed pname p = updateRule pname p

instance HuddleGroup "script_pubkey" MaryEra where
  huddleGroupNamed pname p = scriptPubkeyGroup pname p

instance HuddleGroup "script_all" MaryEra where
  huddleGroupNamed pname p = scriptAllGroup pname p

instance HuddleGroup "script_any" MaryEra where
  huddleGroupNamed pname p = scriptAnyGroup pname p

instance HuddleGroup "script_n_of_k" MaryEra where
  huddleGroupNamed pname p = scriptNOfKGroup pname p

instance HuddleGroup "script_invalid_before" MaryEra where
  huddleGroupNamed pname p = scriptInvalidBeforeGroup pname p

instance HuddleGroup "script_invalid_hereafter" MaryEra where
  huddleGroupNamed pname p = scriptInvalidHereafterGroup pname p

instance HuddleRule "native_script" MaryEra where
  huddleRuleNamed pname p = nativeScriptRule pname p

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
  huddleRuleNamed pname p = maryValueRule pname p

instance HuddleRule "policy_id" MaryEra where
  huddleRuleNamed pname p = pname =.= huddleRule @"script_hash" p

instance HuddleRule "asset_name" MaryEra where
  huddleRuleNamed pname _ = assetNameRule pname

instance HuddleRule "mint" MaryEra where
  huddleRuleNamed pname p = maryMintRule pname p

instance HuddleRule "auxiliary_data" MaryEra where
  huddleRuleNamed pname p = auxiliaryDataRule pname p

instance HuddleRule "auxiliary_data_array" MaryEra where
  huddleRuleNamed pname p = auxiliaryDataArrayRule pname p

instance HuddleRule "auxiliary_scripts" MaryEra where
  huddleRuleNamed pname p = auxiliaryScriptsRule pname p

instance HuddleRule1 "set" MaryEra where
  huddleRule1Named _ _ = huddleRule1 @"set" (Proxy @ShelleyEra)

instance HuddleRule1 "nonempty_set" MaryEra where
  huddleRule1Named _ _ = huddleRule1 @"nonempty_set" (Proxy @ShelleyEra)

instance HuddleRule1 "nonempty_oset" MaryEra where
  huddleRule1Named _ _ = huddleRule1 @"nonempty_oset" (Proxy @ShelleyEra)
