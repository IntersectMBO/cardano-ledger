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

module Cardano.Ledger.Shelley.HuddleSpec (
  module Cardano.Ledger.Huddle,
  module Cardano.Ledger.Core.HuddleSpec,
  shelleyCDDL,
  shelleyProtocolVersionGroup,
  headerRule,
  proposedProtocolParameterUpdatesRule,
  updateRule,
  protocolParamUpdateRule,
  headerBodyRule,
  transactionWitnessSetRule,
  vkeywitnessRule,
  bootstrapWitnessRule,
  shelleyOperationalCertGroup,
  genesisHashRule,
  scriptPubkeyGroup,
  scriptAllGroup,
  scriptAnyGroup,
  transactionIdRule,
  transactionInputRule,
  transactionOutputRule,
  withdrawalsRule,
  dnsNameRule,
  urlRule,
  poolMetadataRule,
  singleHostAddrGroup,
  singleHostNameGroup,
  multiHostNameGroup,
  relayRule,
  poolParamsGroup,
  poolRegistrationCertGroup,
  poolRetirementCertGroup,
  genesisDelegateHashRule,
  genesisDelegationCertGroup,
  deltaCoinRule,
  moveInstantaneousRewardRule,
  moveInstantaneousRewardsCertGroup,
  accountRegistrationCertGroup,
  accountUnregistrationCertGroup,
  delegationToStakePoolCertGroup,
  certificateRule,
  untaggedSet,
) where

import Cardano.Ledger.Core.HuddleSpec (majorProtocolVersionRule)
import Cardano.Ledger.Huddle
import Cardano.Ledger.Shelley (ShelleyEra)
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import Text.Heredoc
import Prelude hiding ((/))

shelleyCDDL :: Huddle
shelleyCDDL =
  collectFrom
    [ HIRule $ huddleRule @"block" (Proxy @ShelleyEra)
    , HIRule $ huddleRule @"transaction" (Proxy @ShelleyEra)
    , HIRule $ huddleRule @"signkey_kes" (Proxy @ShelleyEra)
    ]

shelleyProtocolVersionGroup ::
  forall era. HuddleRule "major_protocol_version" era => Proxy era -> Named Group
shelleyProtocolVersionGroup p = "protocol_version" =:~ grp [a $ huddleRule @"major_protocol_version" p, a VUInt]

headerRule :: forall era. HuddleRule "header_body" era => Proxy era -> Rule
headerRule p =
  "header"
    =:= arr [a $ huddleRule @"header_body" p, "body_signature" ==> huddleRule @"kes_signature" p]

proposedProtocolParameterUpdatesRule ::
  forall era.
  (HuddleRule "genesis_hash" era, HuddleRule "protocol_param_update" era) => Proxy era -> Rule
proposedProtocolParameterUpdatesRule p =
  "proposed_protocol_parameter_updates"
    =:= mp [0 <+ asKey (huddleRule @"genesis_hash" p) ==> huddleRule @"protocol_param_update" p]

updateRule ::
  forall era. HuddleRule "proposed_protocol_parameter_updates" era => Proxy era -> Rule
updateRule p =
  "update"
    =:= arr [a $ huddleRule @"proposed_protocol_parameter_updates" p, a $ huddleRule @"epoch" p]

protocolParamUpdateRule ::
  forall era. HuddleGroup "protocol_version" era => Proxy era -> Rule
protocolParamUpdateRule p =
  "protocol_param_update"
    =:= mp
      [ opt (idx 0 ==> VUInt) //- "minfee A"
      , opt (idx 1 ==> VUInt) //- "minfee B"
      , opt (idx 2 ==> VUInt) //- "max block body size"
      , opt (idx 3 ==> VUInt) //- "max transaction size"
      , opt (idx 4 ==> VUInt `sized` (2 :: Word64)) //- "max block header size"
      , opt (idx 5 ==> huddleRule @"coin" p) //- "key deposit"
      , opt (idx 6 ==> huddleRule @"coin" p) //- "pool deposit"
      , opt (idx 7 ==> huddleRule @"epoch_interval" p) //- "maximum epoch"
      , opt (idx 8 ==> VUInt `sized` (2 :: Word64)) //- "n_opt: desired number of stake pools"
      , opt (idx 9 ==> huddleRule @"nonnegative_interval" p) //- "pool pledge influence"
      , opt (idx 10 ==> huddleRule @"unit_interval" p) //- "expansion rate"
      , opt (idx 11 ==> huddleRule @"unit_interval" p) //- "treasury growth rate"
      , opt (idx 12 ==> huddleRule @"unit_interval" p) //- "decentralization constant"
      , opt (idx 13 ==> huddleRule @"nonce" p) //- "extra entropy"
      , opt (idx 14 ==> arr [a $ huddleGroup @"protocol_version" p]) //- "protocol version"
      , opt (idx 15 ==> huddleRule @"coin" p) //- "min utxo value"
      , opt (idx 16 ==> huddleRule @"coin" p) //- "min pool cost"
      ]

headerBodyRule ::
  forall era.
  ( HuddleGroup "operational_cert" era
  , HuddleGroup "protocol_version" era
  ) =>
  Proxy era ->
  Rule
headerBodyRule p =
  "header_body"
    =:= arr
      [ "block_number" ==> huddleRule @"block_number" p
      , "slot" ==> huddleRule @"slot" p
      , "prev_hash" ==> huddleRule @"hash32" p / VNil
      , "issuer_vkey" ==> huddleRule @"vkey" p
      , "vrf_vkey" ==> huddleRule @"vrf_vkey" p
      , "nonce_vrf" ==> huddleRule @"vrf_cert" p
      , "leader_vrf" ==> huddleRule @"vrf_cert" p
      , "block_body_size" ==> VUInt `sized` (4 :: Word64)
      , "block_body_hash" ==> huddleRule @"hash32" p
      , a (huddleGroup @"operational_cert" p)
      , a (huddleGroup @"protocol_version" p)
      ]

transactionWitnessSetRule ::
  forall era.
  ( HuddleRule "vkeywitness" era
  , HuddleRule "native_script" era
  , HuddleRule "bootstrap_witness" era
  ) =>
  Proxy era ->
  Rule
transactionWitnessSetRule p =
  "transaction_witness_set"
    =:= mp
      [ opt $ idx 0 ==> arr [0 <+ a (huddleRule @"vkeywitness" p)]
      , opt $ idx 1 ==> arr [0 <+ a (huddleRule @"native_script" p)]
      , opt $ idx 2 ==> arr [0 <+ a (huddleRule @"bootstrap_witness" p)]
      ]

vkeywitnessRule :: forall era. Era era => Proxy era -> Rule
vkeywitnessRule p =
  "vkeywitness"
    =:= arr [a $ huddleRule @"vkey" p, a $ huddleRule @"signature" p]

bootstrapWitnessRule :: forall era. Era era => Proxy era -> Rule
bootstrapWitnessRule p =
  "bootstrap_witness"
    =:= arr
      [ "public_key" ==> huddleRule @"vkey" p
      , "signature" ==> huddleRule @"signature" p
      , "chain_code" ==> VBytes `sized` (32 :: Word64)
      , "attributes" ==> VBytes
      ]

shelleyOperationalCertGroup :: forall era. Era era => Proxy era -> Named Group
shelleyOperationalCertGroup p =
  "operational_cert"
    =:~ grp
      [ "hot_vkey" ==> huddleRule @"kes_vkey" p
      , "sequence_number" ==> huddleRule @"sequence_number" p
      , "kes_period" ==> huddleRule @"kes_period" p
      , "sigma" ==> huddleRule @"signature" p
      ]

genesisHashRule :: forall era. Era era => Proxy era -> Rule
genesisHashRule p = "genesis_hash" =:= huddleRule @"hash28" p

scriptPubkeyGroup :: forall era. Era era => Proxy era -> Named Group
scriptPubkeyGroup p = "script_pubkey" =:~ grp [0, a $ huddleRule @"addr_keyhash" p]

scriptAllGroup :: forall era. HuddleRule "native_script" era => Proxy era -> Named Group
scriptAllGroup p = "script_all" =:~ grp [1, a $ arr [0 <+ a (huddleRule @"native_script" p)]]

scriptAnyGroup :: forall era. HuddleRule "native_script" era => Proxy era -> Named Group
scriptAnyGroup p = "script_any" =:~ grp [2, a $ arr [0 <+ a (huddleRule @"native_script" p)]]

transactionIdRule :: forall era. Era era => Proxy era -> Rule
transactionIdRule p = "transaction_id" =:= huddleRule @"hash32" p

transactionInputRule :: forall era. HuddleRule "transaction_id" era => Proxy era -> Rule
transactionInputRule p =
  "transaction_input"
    =:= arr
      [ "id" ==> huddleRule @"transaction_id" p
      , "index" ==> VUInt `sized` (2 :: Word64)
      ]

transactionOutputRule :: forall era. Era era => Proxy era -> Rule
transactionOutputRule p =
  "transaction_output"
    =:= arr [a $ huddleRule @"address" p, "amount" ==> huddleRule @"coin" p]

withdrawalsRule :: forall era. Era era => Proxy era -> Rule
withdrawalsRule p =
  "withdrawals"
    =:= mp [0 <+ asKey (huddleRule @"reward_account" p) ==> huddleRule @"coin" p]

dnsNameRule :: Rule
dnsNameRule = "dns_name" =:= VText `sized` (0 :: Word64, 64 :: Word64)

urlRule :: Rule
urlRule = "url" =:= VText `sized` (0 :: Word64, 64 :: Word64)

poolMetadataRule :: forall era. HuddleRule "url" era => Proxy era -> Rule
poolMetadataRule p =
  "pool_metadata"
    =:= arr [a $ huddleRule @"url" p, a VBytes]

singleHostAddrGroup :: forall era. Era era => Proxy era -> Named Group
singleHostAddrGroup p =
  "single_host_addr"
    =:~ grp
      [ 0
      , a $ huddleRule @"port" p / VNil
      , a $ huddleRule @"ipv4" p / VNil
      , a $ huddleRule @"ipv6" p / VNil
      ]

singleHostNameGroup :: forall era. HuddleRule "dns_name" era => Proxy era -> Named Group
singleHostNameGroup p =
  comment
    "dns_name: An A or AAAA DNS record"
    $ "single_host_name"
      =:~ grp
        [ 1
        , a $ huddleRule @"port" p / VNil
        , a $ huddleRule @"dns_name" p
        ]

multiHostNameGroup :: forall era. HuddleRule "dns_name" era => Proxy era -> Named Group
multiHostNameGroup p =
  comment
    "dns_name: An SRV DNS record"
    $ "multi_host_name"
      =:~ grp [2, a $ huddleRule @"dns_name" p]

relayRule ::
  forall era.
  ( HuddleGroup "single_host_addr" era
  , HuddleGroup "single_host_name" era
  , HuddleGroup "multi_host_name" era
  ) =>
  Proxy era ->
  Rule
relayRule p =
  "relay"
    =:= arr [a $ huddleGroup @"single_host_addr" p]
    / arr [a $ huddleGroup @"single_host_name" p]
    / arr [a $ huddleGroup @"multi_host_name" p]

poolParamsGroup ::
  forall era.
  ( HuddleRule "relay" era
  , HuddleRule "pool_metadata" era
  ) =>
  Proxy era ->
  Named Group
poolParamsGroup p =
  comment
    "Pool parameters for stake pool registration"
    $ "pool_params"
      =:~ grp
        [ "operator" ==> huddleRule @"pool_keyhash" p
        , "vrf_keyhash" ==> huddleRule @"vrf_keyhash" p
        , "pledge" ==> huddleRule @"coin" p
        , "cost" ==> huddleRule @"coin" p
        , "margin" ==> huddleRule @"unit_interval" p
        , "reward_account" ==> huddleRule @"reward_account" p
        , "pool_owners" ==> untaggedSet (huddleRule @"addr_keyhash" p)
        , "relays" ==> arr [0 <+ a (huddleRule @"relay" p)]
        , "pool_metadata" ==> huddleRule @"pool_metadata" p / VNil
        ]

poolRegistrationCertGroup :: forall era. HuddleGroup "pool_params" era => Proxy era -> Named Group
poolRegistrationCertGroup p = "pool_registration_cert" =:~ grp [3, a $ huddleGroup @"pool_params" p]

poolRetirementCertGroup :: forall era. Era era => Proxy era -> Named Group
poolRetirementCertGroup p =
  "pool_retirement_cert"
    =:~ grp [4, a $ huddleRule @"pool_keyhash" p, a $ huddleRule @"epoch" p]

genesisDelegateHashRule :: forall era. Era era => Proxy era -> Rule
genesisDelegateHashRule p = "genesis_delegate_hash" =:= huddleRule @"hash28" p

genesisDelegationCertGroup ::
  forall era.
  ( HuddleRule "genesis_hash" era
  , HuddleRule "genesis_delegate_hash" era
  ) =>
  Proxy era ->
  Named Group
genesisDelegationCertGroup p =
  "genesis_delegation_cert"
    =:~ grp
      [ 5
      , a $ huddleRule @"genesis_hash" p
      , a $ huddleRule @"genesis_delegate_hash" p
      , a $ huddleRule @"vrf_keyhash" p
      ]

deltaCoinRule :: Rule
deltaCoinRule =
  comment
    "This too has been introduced in Shelley as a backport from Alonzo."
    $ "delta_coin" =:= VInt

moveInstantaneousRewardRule :: forall era. HuddleRule "delta_coin" era => Proxy era -> Rule
moveInstantaneousRewardRule p =
  comment
    [str|The first field determines where the funds are drawn from.
        |  0 denotes the reserves,
        |  1 denotes the treasury.
        |If the second field is a map, funds are moved to stake credentials.
        |Otherwise, the funds are given to the other accounting pot.
        |NOTE:
        |  This has been safely backported to Shelley from Alonzo.
        |]
    $ "move_instantaneous_reward"
      =:= arr
        [ a (int 0 / int 1)
        , a
            ( smp
                [0 <+ asKey (huddleRule @"stake_credential" p) ==> huddleRule @"delta_coin" p]
                / huddleRule @"coin" p
            )
        ]

moveInstantaneousRewardsCertGroup ::
  forall era. HuddleRule "move_instantaneous_reward" era => Proxy era -> Named Group
moveInstantaneousRewardsCertGroup p =
  "move_instantaneous_rewards_cert"
    =:~ grp [6, a $ huddleRule @"move_instantaneous_reward" p]

accountRegistrationCertGroup :: forall era. Era era => Proxy era -> Named Group
accountRegistrationCertGroup p =
  comment
    "This certificate will be deprecated in a future era"
    $ "account_registration_cert"
      =:~ grp [0, a $ huddleRule @"stake_credential" p]

accountUnregistrationCertGroup :: forall era. Era era => Proxy era -> Named Group
accountUnregistrationCertGroup p =
  comment
    "This certificate will be deprecated in a future era"
    $ "account_unregistration_cert"
      =:~ grp [1, a $ huddleRule @"stake_credential" p]

delegationToStakePoolCertGroup :: forall era. Era era => Proxy era -> Named Group
delegationToStakePoolCertGroup p =
  "delegation_to_stake_pool_cert"
    =:~ grp [2, a $ huddleRule @"stake_credential" p, a $ huddleRule @"pool_keyhash" p]

certificateRule ::
  forall era.
  ( HuddleGroup "account_registration_cert" era
  , HuddleGroup "account_unregistration_cert" era
  , HuddleGroup "delegation_to_stake_pool_cert" era
  , HuddleGroup "pool_registration_cert" era
  , HuddleGroup "pool_retirement_cert" era
  , HuddleGroup "genesis_delegation_cert" era
  , HuddleGroup "move_instantaneous_rewards_cert" era
  ) =>
  Proxy era ->
  Rule
certificateRule p =
  "certificate"
    =:= arr [a $ huddleGroup @"account_registration_cert" p]
    / arr [a $ huddleGroup @"account_unregistration_cert" p]
    / arr [a $ huddleGroup @"delegation_to_stake_pool_cert" p]
    / arr [a $ huddleGroup @"pool_registration_cert" p]
    / arr [a $ huddleGroup @"pool_retirement_cert" p]
    / arr [a $ huddleGroup @"genesis_delegation_cert" p]
    / arr [a $ huddleGroup @"move_instantaneous_rewards_cert" p]

untaggedSet :: IsType0 a => a -> GRuleCall
untaggedSet = binding $ \x -> "set" =:= arr [0 <+ a x]

instance HuddleRule "dns_name" ShelleyEra where
  huddleRule _ = dnsNameRule

instance HuddleRule "url" ShelleyEra where
  huddleRule _ = urlRule

instance HuddleRule "pool_metadata" ShelleyEra where
  huddleRule = poolMetadataRule @ShelleyEra

instance HuddleGroup "single_host_addr" ShelleyEra where
  huddleGroup = singleHostAddrGroup @ShelleyEra

instance HuddleGroup "single_host_name" ShelleyEra where
  huddleGroup = singleHostNameGroup @ShelleyEra

instance HuddleGroup "multi_host_name" ShelleyEra where
  huddleGroup = multiHostNameGroup @ShelleyEra

instance HuddleRule "relay" ShelleyEra where
  huddleRule = relayRule @ShelleyEra

instance HuddleGroup "pool_params" ShelleyEra where
  huddleGroup = poolParamsGroup @ShelleyEra

instance HuddleGroup "pool_registration_cert" ShelleyEra where
  huddleGroup = poolRegistrationCertGroup @ShelleyEra

instance HuddleGroup "pool_retirement_cert" ShelleyEra where
  huddleGroup = poolRetirementCertGroup @ShelleyEra

instance HuddleRule "genesis_hash" ShelleyEra where
  huddleRule = genesisHashRule @ShelleyEra

instance HuddleRule "genesis_delegate_hash" ShelleyEra where
  huddleRule = genesisDelegateHashRule @ShelleyEra

instance HuddleGroup "genesis_delegation_cert" ShelleyEra where
  huddleGroup = genesisDelegationCertGroup @ShelleyEra

instance HuddleRule "delta_coin" ShelleyEra where
  huddleRule _ = deltaCoinRule

instance HuddleRule "move_instantaneous_reward" ShelleyEra where
  huddleRule = moveInstantaneousRewardRule @ShelleyEra

instance HuddleGroup "move_instantaneous_rewards_cert" ShelleyEra where
  huddleGroup = moveInstantaneousRewardsCertGroup @ShelleyEra

instance HuddleGroup "account_registration_cert" ShelleyEra where
  huddleGroup = accountRegistrationCertGroup @ShelleyEra

instance HuddleGroup "account_unregistration_cert" ShelleyEra where
  huddleGroup = accountUnregistrationCertGroup @ShelleyEra

instance HuddleGroup "delegation_to_stake_pool_cert" ShelleyEra where
  huddleGroup = delegationToStakePoolCertGroup @ShelleyEra

instance HuddleRule "certificate" ShelleyEra where
  huddleRule = certificateRule @ShelleyEra

instance HuddleRule "withdrawals" ShelleyEra where
  huddleRule = withdrawalsRule @ShelleyEra

instance HuddleRule "major_protocol_version" ShelleyEra where
  huddleRule = majorProtocolVersionRule @ShelleyEra

instance HuddleGroup "protocol_version" ShelleyEra where
  huddleGroup = shelleyProtocolVersionGroup @ShelleyEra

instance HuddleRule "protocol_param_update" ShelleyEra where
  huddleRule = protocolParamUpdateRule @ShelleyEra

instance HuddleRule "proposed_protocol_parameter_updates" ShelleyEra where
  huddleRule = proposedProtocolParameterUpdatesRule @ShelleyEra

instance HuddleRule "update" ShelleyEra where
  huddleRule = updateRule @ShelleyEra

instance HuddleGroup "operational_cert" ShelleyEra where
  huddleGroup = shelleyOperationalCertGroup @ShelleyEra

instance HuddleRule "header_body" ShelleyEra where
  huddleRule = headerBodyRule @ShelleyEra

instance HuddleRule "header" ShelleyEra where
  huddleRule = headerRule @ShelleyEra

instance HuddleRule "transaction_id" ShelleyEra where
  huddleRule = transactionIdRule @ShelleyEra

instance HuddleRule "transaction_input" ShelleyEra where
  huddleRule = transactionInputRule @ShelleyEra

instance HuddleRule "transaction_output" ShelleyEra where
  huddleRule = transactionOutputRule @ShelleyEra

instance HuddleGroup "script_pubkey" ShelleyEra where
  huddleGroup = scriptPubkeyGroup @ShelleyEra

instance HuddleGroup "script_all" ShelleyEra where
  huddleGroup = scriptAllGroup @ShelleyEra

instance HuddleGroup "script_any" ShelleyEra where
  huddleGroup = scriptAnyGroup @ShelleyEra

instance HuddleGroup "script_n_of_k" ShelleyEra where
  huddleGroup p =
    "script_n_of_k"
      =:~ grp [3, "n" ==> VUInt, a $ arr [0 <+ a (huddleRule @"native_script" p)]]

instance HuddleRule "native_script" ShelleyEra where
  huddleRule p =
    comment
      [str|Native scripts support 4 operations:
          |  - Signature verification (script_pubkey)
          |  - Conjunctions (script_all)
          |  - Disjunctions (script_any)
          |  - M-of-N thresholds (script_n_of_k)
          |
          |Note: Shelley uses VUInt for the threshold in script_n_of_k.
          |]
      $ "native_script"
        =:= arr [a $ huddleGroup @"script_pubkey" p]
        / arr [a $ huddleGroup @"script_all" p]
        / arr [a $ huddleGroup @"script_any" p]
        / arr [a $ huddleGroup @"script_n_of_k" p]

instance HuddleRule "vkeywitness" ShelleyEra where
  huddleRule = vkeywitnessRule @ShelleyEra

instance HuddleRule "bootstrap_witness" ShelleyEra where
  huddleRule = bootstrapWitnessRule @ShelleyEra

instance HuddleRule "transaction_witness_set" ShelleyEra where
  huddleRule = transactionWitnessSetRule @ShelleyEra

instance HuddleRule "transaction_body" ShelleyEra where
  huddleRule p =
    "transaction_body"
      =:= mp
        [ idx 0 ==> untaggedSet (huddleRule @"transaction_input" p)
        , idx 1 ==> arr [0 <+ a (huddleRule @"transaction_output" p)]
        , idx 2 ==> huddleRule @"coin" p
        , idx 3 ==> huddleRule @"slot" p
        , opt (idx 4 ==> arr [0 <+ a (huddleRule @"certificate" p)])
        , opt (idx 5 ==> huddleRule @"withdrawals" p)
        , opt (idx 6 ==> huddleRule @"update" p)
        , opt (idx 7 ==> huddleRule @"auxiliary_data_hash" p)
        ]

instance HuddleRule "transaction" ShelleyEra where
  huddleRule p =
    "transaction"
      =:= arr
        [ a $ huddleRule @"transaction_body" p
        , a $ huddleRule @"transaction_witness_set" p
        , a $ huddleRule @"metadata" p / VNil
        ]

instance HuddleRule "block" ShelleyEra where
  huddleRule p =
    "block"
      =:= arr
        [ a $ huddleRule @"header" p
        , "transaction_bodies" ==> arr [0 <+ a (huddleRule @"transaction_body" p)]
        , "transaction_witness_sets" ==> arr [0 <+ a (huddleRule @"transaction_witness_set" p)]
        , "transaction_metadata_set"
            ==> mp
              [ 0 <+ asKey (huddleRule @"transaction_index" p) ==> huddleRule @"metadata" p
              ]
        ]
