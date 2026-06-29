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
  ShelleyEra,
  shelleyCDDL,
  shelleyProtocolVersionGroup,
  headerRule,
  proposedProtocolParameterUpdatesRule,
  updateRule,
  protocolParamUpdateRule,
  shelleyHeaderBodyRule,
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
  shelleyWithdrawalsRule,
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
  forall era.
  HuddleRule "major_protocol_version" era => Proxy "protocol_version" -> Proxy era -> GroupDef
shelleyProtocolVersionGroup pname p = pname =.~ grp [a $ huddleRule @"major_protocol_version" p, a VUInt]

headerRule ::
  forall era. HuddleRule "header_body" era => Proxy "header" -> Proxy era -> Rule
headerRule pname p =
  pname
    =.= arr [a $ huddleRule @"header_body" p, "body_signature" ==> huddleRule @"kes_signature" p]

proposedProtocolParameterUpdatesRule ::
  forall era.
  (HuddleRule "genesis_hash" era, HuddleRule "protocol_param_update" era) =>
  Proxy "proposed_protocol_parameter_updates" -> Proxy era -> Rule
proposedProtocolParameterUpdatesRule pname p =
  pname
    =.= mp [0 <+ asKey (huddleRule @"genesis_hash" p) ==> huddleRule @"protocol_param_update" p]

updateRule ::
  forall era.
  HuddleRule "proposed_protocol_parameter_updates" era =>
  Proxy "update" -> Proxy era -> Rule
updateRule pname p =
  pname
    =.= arr [a $ huddleRule @"proposed_protocol_parameter_updates" p, a $ huddleRule @"epoch" p]

protocolParamUpdateRule ::
  forall era.
  HuddleGroup "protocol_version" era => Proxy "protocol_param_update" -> Proxy era -> Rule
protocolParamUpdateRule pname p =
  pname
    =.= mp
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

shelleyHeaderBodyRule ::
  forall era.
  ( HuddleGroup "operational_cert" era
  , HuddleGroup "protocol_version" era
  ) =>
  Proxy "header_body" ->
  Proxy era ->
  Rule
shelleyHeaderBodyRule pname p =
  pname
    =.= arr
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
  Proxy "transaction_witness_set" ->
  Proxy era ->
  Rule
transactionWitnessSetRule pname p =
  pname
    =.= mp
      [ opt $ idx 0 ==> arr [0 <+ a (huddleRule @"vkeywitness" p)]
      , opt $ idx 1 ==> arr [0 <+ a (huddleRule @"native_script" p)]
      , opt $ idx 2 ==> arr [0 <+ a (huddleRule @"bootstrap_witness" p)]
      ]

vkeywitnessRule :: forall era. Era era => Proxy "vkeywitness" -> Proxy era -> Rule
vkeywitnessRule pname p =
  pname
    =.= arr [a $ huddleRule @"vkey" p, a $ huddleRule @"signature" p]

bootstrapWitnessRule ::
  forall era. Era era => Proxy "bootstrap_witness" -> Proxy era -> Rule
bootstrapWitnessRule pname p =
  pname
    =.= arr
      [ "public_key" ==> huddleRule @"vkey" p
      , "signature" ==> huddleRule @"signature" p
      , "chain_code" ==> VBytes `sized` (32 :: Word64)
      , "attributes" ==> VBytes
      ]

shelleyOperationalCertGroup ::
  forall era. Era era => Proxy "operational_cert" -> Proxy era -> GroupDef
shelleyOperationalCertGroup pname p =
  pname
    =.~ grp
      [ "hot_vkey" ==> huddleRule @"kes_vkey" p
      , "sequence_number" ==> huddleRule @"sequence_number" p
      , "kes_period" ==> huddleRule @"kes_period" p
      , "sigma" ==> huddleRule @"signature" p
      ]

genesisHashRule :: forall era. Era era => Proxy "genesis_hash" -> Proxy era -> Rule
genesisHashRule pname p = pname =.= huddleRule @"hash28" p

scriptPubkeyGroup ::
  forall era. Era era => Proxy "script_pubkey" -> Proxy era -> GroupDef
scriptPubkeyGroup pname p = pname =.~ grp [0, a $ huddleRule @"addr_keyhash" p]

scriptAllGroup ::
  forall era.
  HuddleRule "native_script" era => Proxy "script_all" -> Proxy era -> GroupDef
scriptAllGroup pname p = pname =.~ grp [1, a $ arr [0 <+ a (huddleRule @"native_script" p)]]

scriptAnyGroup ::
  forall era.
  HuddleRule "native_script" era => Proxy "script_any" -> Proxy era -> GroupDef
scriptAnyGroup pname p = pname =.~ grp [2, a $ arr [0 <+ a (huddleRule @"native_script" p)]]

transactionIdRule :: forall era. Era era => Proxy "transaction_id" -> Proxy era -> Rule
transactionIdRule pname p = pname =.= huddleRule @"hash32" p

transactionInputRule ::
  forall era.
  HuddleRule "transaction_id" era => Proxy "transaction_input" -> Proxy era -> Rule
transactionInputRule pname p =
  pname
    =.= arr
      [ "id" ==> huddleRule @"transaction_id" p
      , "index" ==> VUInt `sized` (2 :: Word64)
      ]

transactionOutputRule ::
  forall era. Era era => Proxy "transaction_output" -> Proxy era -> Rule
transactionOutputRule pname p =
  pname
    =.= arr [a $ huddleRule @"address" p, "amount" ==> huddleRule @"coin" p]

shelleyWithdrawalsRule ::
  forall era. Era era => Proxy "withdrawals" -> Proxy era -> Rule
shelleyWithdrawalsRule pname p =
  pname
    =.= mp [0 <+ asKey (huddleRule @"reward_account" p) ==> huddleRule @"coin" p]

dnsNameRule :: Proxy "dns_name" -> Rule
dnsNameRule pname = pname =.= VText `sized` (0 :: Word64, 64 :: Word64)

urlRule :: Proxy "url" -> Rule
urlRule pname = pname =.= VText `sized` (0 :: Word64, 64 :: Word64)

poolMetadataRule ::
  forall era. HuddleRule "url" era => Proxy "pool_metadata" -> Proxy era -> Rule
poolMetadataRule pname p =
  pname
    =.= arr [a $ huddleRule @"url" p, a VBytes]

singleHostAddrGroup ::
  forall era. Era era => Proxy "single_host_addr" -> Proxy era -> GroupDef
singleHostAddrGroup pname p =
  pname
    =.~ grp
      [ 0
      , a $ huddleRule @"port" p / VNil
      , a $ huddleRule @"ipv4" p / VNil
      , a $ huddleRule @"ipv6" p / VNil
      ]

singleHostNameGroup ::
  forall era.
  HuddleRule "dns_name" era => Proxy "single_host_name" -> Proxy era -> GroupDef
singleHostNameGroup pname p =
  comment
    "dns_name: An A or AAAA DNS record"
    $ pname
      =.~ grp
        [ 1
        , a $ huddleRule @"port" p / VNil
        , a $ huddleRule @"dns_name" p
        ]

multiHostNameGroup ::
  forall era.
  HuddleRule "dns_name" era => Proxy "multi_host_name" -> Proxy era -> GroupDef
multiHostNameGroup pname p =
  comment
    "dns_name: An SRV DNS record"
    $ pname
      =.~ grp [2, a $ huddleRule @"dns_name" p]

relayRule ::
  forall era.
  ( HuddleGroup "single_host_addr" era
  , HuddleGroup "single_host_name" era
  , HuddleGroup "multi_host_name" era
  ) =>
  Proxy "relay" ->
  Proxy era ->
  Rule
relayRule pname p =
  pname
    =.= arr [a $ huddleGroup @"single_host_addr" p]
    / arr [a $ huddleGroup @"single_host_name" p]
    / arr [a $ huddleGroup @"multi_host_name" p]

poolParamsGroup ::
  forall era.
  ( HuddleRule "relay" era
  , HuddleRule "pool_metadata" era
  , HuddleRule1 "set" era
  ) =>
  Proxy "pool_params" ->
  Proxy era ->
  GroupDef
poolParamsGroup pname p =
  comment
    "Pool parameters for stake pool registration"
    $ pname
      =.~ grp
        [ "operator" ==> huddleRule @"pool_keyhash" p
        , "vrf_keyhash" ==> huddleRule @"vrf_keyhash" p
        , "pledge" ==> huddleRule @"coin" p
        , "cost" ==> huddleRule @"coin" p
        , "margin" ==> huddleRule @"unit_interval" p
        , "reward_account" ==> huddleRule @"reward_account" p
        , "pool_owners" ==> huddleRule1 @"set" p (huddleRule @"addr_keyhash" p)
        , "relays" ==> arr [0 <+ a (huddleRule @"relay" p)]
        , "pool_metadata" ==> huddleRule @"pool_metadata" p / VNil
        ]

poolRegistrationCertGroup ::
  forall era.
  HuddleGroup "pool_params" era => Proxy "pool_registration_cert" -> Proxy era -> GroupDef
poolRegistrationCertGroup pname p = pname =.~ grp [3, a $ huddleGroup @"pool_params" p]

poolRetirementCertGroup ::
  forall era. Era era => Proxy "pool_retirement_cert" -> Proxy era -> GroupDef
poolRetirementCertGroup pname p =
  pname
    =.~ grp [4, a $ huddleRule @"pool_keyhash" p, a $ huddleRule @"epoch" p]

genesisDelegateHashRule ::
  forall era. Era era => Proxy "genesis_delegate_hash" -> Proxy era -> Rule
genesisDelegateHashRule pname p = pname =.= huddleRule @"hash28" p

genesisDelegationCertGroup ::
  forall era.
  ( HuddleRule "genesis_hash" era
  , HuddleRule "genesis_delegate_hash" era
  ) =>
  Proxy "genesis_delegation_cert" ->
  Proxy era ->
  GroupDef
genesisDelegationCertGroup pname p =
  pname
    =.~ grp
      [ 5
      , a $ huddleRule @"genesis_hash" p
      , a $ huddleRule @"genesis_delegate_hash" p
      , a $ huddleRule @"vrf_keyhash" p
      ]

deltaCoinRule :: Proxy "delta_coin" -> Rule
deltaCoinRule pname =
  comment
    "This too has been introduced in Shelley as a backport from Alonzo."
    $ pname =.= VInt

moveInstantaneousRewardRule ::
  forall era. HuddleRule "delta_coin" era => Proxy "move_instantaneous_reward" -> Proxy era -> Rule
moveInstantaneousRewardRule pname p =
  comment
    [str|The first field determines where the funds are drawn from.
        |  0 denotes the reserves,
        |  1 denotes the treasury.
        |If the second field is a map, funds are moved to stake credentials.
        |Otherwise, the funds are given to the other accounting pot.
        |NOTE:
        |  This has been safely backported to Shelley from Alonzo.
        |]
    $ pname
      =.= arr
        [ a (int 0 / int 1)
        , a
            ( smp
                [0 <+ asKey (huddleRule @"stake_credential" p) ==> huddleRule @"delta_coin" p]
                / huddleRule @"coin" p
            )
        ]

moveInstantaneousRewardsCertGroup ::
  forall era.
  HuddleRule "move_instantaneous_reward" era =>
  Proxy "move_instantaneous_rewards_cert" ->
  Proxy era ->
  GroupDef
moveInstantaneousRewardsCertGroup pname p =
  pname
    =.~ grp [6, a $ huddleRule @"move_instantaneous_reward" p]

accountRegistrationCertGroup ::
  forall era. Era era => Proxy "account_registration_cert" -> Proxy era -> GroupDef
accountRegistrationCertGroup pname p =
  comment
    "This certificate will be deprecated in a future era"
    $ pname
      =.~ grp [0, a $ huddleRule @"stake_credential" p]

accountUnregistrationCertGroup ::
  forall era. Era era => Proxy "account_unregistration_cert" -> Proxy era -> GroupDef
accountUnregistrationCertGroup pname p =
  comment
    "This certificate will be deprecated in a future era"
    $ pname
      =.~ grp [1, a $ huddleRule @"stake_credential" p]

delegationToStakePoolCertGroup ::
  forall era. Era era => Proxy "delegation_to_stake_pool_cert" -> Proxy era -> GroupDef
delegationToStakePoolCertGroup pname p =
  pname
    =.~ grp [2, a $ huddleRule @"stake_credential" p, a $ huddleRule @"pool_keyhash" p]

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
  Proxy "certificate" ->
  Proxy era ->
  Rule
certificateRule pname p =
  pname
    =.= arr [a $ huddleGroup @"account_registration_cert" p]
    / arr [a $ huddleGroup @"account_unregistration_cert" p]
    / arr [a $ huddleGroup @"delegation_to_stake_pool_cert" p]
    / arr [a $ huddleGroup @"pool_registration_cert" p]
    / arr [a $ huddleGroup @"pool_retirement_cert" p]
    / arr [a $ huddleGroup @"genesis_delegation_cert" p]
    / arr [a $ huddleGroup @"move_instantaneous_rewards_cert" p]

untaggedSet :: IsType0 a => Proxy "set" -> a -> GRuleCall
untaggedSet pname = binding $ \x -> pname =.= arr [0 <+ a x]

instance HuddleRule "dns_name" ShelleyEra where
  huddleRuleNamed pname _ = dnsNameRule pname

instance HuddleRule "url" ShelleyEra where
  huddleRuleNamed pname _ = urlRule pname

instance HuddleRule "pool_metadata" ShelleyEra where
  huddleRuleNamed = poolMetadataRule

instance HuddleGroup "single_host_addr" ShelleyEra where
  huddleGroupNamed = singleHostAddrGroup

instance HuddleGroup "single_host_name" ShelleyEra where
  huddleGroupNamed = singleHostNameGroup

instance HuddleGroup "multi_host_name" ShelleyEra where
  huddleGroupNamed = multiHostNameGroup

instance HuddleRule "relay" ShelleyEra where
  huddleRuleNamed = relayRule

instance HuddleGroup "pool_params" ShelleyEra where
  huddleGroupNamed = poolParamsGroup

instance HuddleGroup "pool_registration_cert" ShelleyEra where
  huddleGroupNamed = poolRegistrationCertGroup

instance HuddleGroup "pool_retirement_cert" ShelleyEra where
  huddleGroupNamed = poolRetirementCertGroup

instance HuddleRule "genesis_hash" ShelleyEra where
  huddleRuleNamed = genesisHashRule

instance HuddleRule "genesis_delegate_hash" ShelleyEra where
  huddleRuleNamed = genesisDelegateHashRule

instance HuddleGroup "genesis_delegation_cert" ShelleyEra where
  huddleGroupNamed = genesisDelegationCertGroup

instance HuddleRule "delta_coin" ShelleyEra where
  huddleRuleNamed pname _ = deltaCoinRule pname

instance HuddleRule "move_instantaneous_reward" ShelleyEra where
  huddleRuleNamed = moveInstantaneousRewardRule

instance HuddleGroup "move_instantaneous_rewards_cert" ShelleyEra where
  huddleGroupNamed = moveInstantaneousRewardsCertGroup

instance HuddleGroup "account_registration_cert" ShelleyEra where
  huddleGroupNamed = accountRegistrationCertGroup

instance HuddleGroup "account_unregistration_cert" ShelleyEra where
  huddleGroupNamed = accountUnregistrationCertGroup

instance HuddleGroup "delegation_to_stake_pool_cert" ShelleyEra where
  huddleGroupNamed = delegationToStakePoolCertGroup

instance HuddleRule "certificate" ShelleyEra where
  huddleRuleNamed = certificateRule

instance HuddleRule "withdrawals" ShelleyEra where
  huddleRuleNamed = shelleyWithdrawalsRule

instance HuddleRule "major_protocol_version" ShelleyEra where
  huddleRuleNamed = majorProtocolVersionRule

instance HuddleGroup "protocol_version" ShelleyEra where
  huddleGroupNamed = shelleyProtocolVersionGroup

instance HuddleRule "protocol_param_update" ShelleyEra where
  huddleRuleNamed = protocolParamUpdateRule

instance HuddleRule "proposed_protocol_parameter_updates" ShelleyEra where
  huddleRuleNamed = proposedProtocolParameterUpdatesRule

instance HuddleRule "update" ShelleyEra where
  huddleRuleNamed = updateRule

instance HuddleGroup "operational_cert" ShelleyEra where
  huddleGroupNamed = shelleyOperationalCertGroup

instance HuddleRule "header_body" ShelleyEra where
  huddleRuleNamed = shelleyHeaderBodyRule

instance HuddleRule "header" ShelleyEra where
  huddleRuleNamed = headerRule

instance HuddleRule "transaction_id" ShelleyEra where
  huddleRuleNamed = transactionIdRule

instance HuddleRule "transaction_input" ShelleyEra where
  huddleRuleNamed = transactionInputRule

instance HuddleRule "transaction_output" ShelleyEra where
  huddleRuleNamed = transactionOutputRule

instance HuddleGroup "script_pubkey" ShelleyEra where
  huddleGroupNamed = scriptPubkeyGroup

instance HuddleGroup "script_all" ShelleyEra where
  huddleGroupNamed = scriptAllGroup

instance HuddleGroup "script_any" ShelleyEra where
  huddleGroupNamed = scriptAnyGroup

instance HuddleGroup "script_n_of_k" ShelleyEra where
  huddleGroupNamed pname p =
    pname
      =.~ grp [3, "n" ==> VUInt, a $ arr [0 <+ a (huddleRule @"native_script" p)]]

instance HuddleRule "native_script" ShelleyEra where
  huddleRuleNamed pname p =
    comment
      [str|Native scripts support 4 operations:
          |  - Signature verification (script_pubkey)
          |  - Conjunctions (script_all)
          |  - Disjunctions (script_any)
          |  - M-of-N thresholds (script_n_of_k)
          |
          |Note: Shelley uses VUInt for the threshold in script_n_of_k.
          |]
      $ pname
        =.= arr [a $ huddleGroup @"script_pubkey" p]
        / arr [a $ huddleGroup @"script_all" p]
        / arr [a $ huddleGroup @"script_any" p]
        / arr [a $ huddleGroup @"script_n_of_k" p]

instance HuddleRule "vkeywitness" ShelleyEra where
  huddleRuleNamed = vkeywitnessRule

instance HuddleRule "bootstrap_witness" ShelleyEra where
  huddleRuleNamed = bootstrapWitnessRule

instance HuddleRule "transaction_witness_set" ShelleyEra where
  huddleRuleNamed = transactionWitnessSetRule

instance HuddleRule "transaction_body" ShelleyEra where
  huddleRuleNamed pname p =
    pname
      =.= mp
        [ idx 0 ==> huddleRule1 @"set" p (huddleRule @"transaction_input" p)
        , idx 1 ==> arr [0 <+ a (huddleRule @"transaction_output" p)]
        , idx 2 ==> huddleRule @"coin" p
        , idx 3 ==> huddleRule @"slot" p
        , opt (idx 4 ==> arr [0 <+ a (huddleRule @"certificate" p)])
        , opt (idx 5 ==> huddleRule @"withdrawals" p)
        , opt (idx 6 ==> huddleRule @"update" p)
        , opt (idx 7 ==> huddleRule @"auxiliary_data_hash" p)
        ]

instance HuddleRule "transaction" ShelleyEra where
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ a $ huddleRule @"transaction_body" p
        , a $ huddleRule @"transaction_witness_set" p
        , a $ huddleRule @"metadata" p / VNil
        ]

instance HuddleRule "block" ShelleyEra where
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ a $ huddleRule @"header" p
        , "transaction_bodies" ==> arr [0 <+ a (huddleRule @"transaction_body" p)]
        , "transaction_witness_sets" ==> arr [0 <+ a (huddleRule @"transaction_witness_set" p)]
        , "transaction_metadata_set"
            ==> mp
              [ 0 <+ asKey (huddleRule @"transaction_index" p) ==> huddleRule @"metadata" p
              ]
        ]

instance HuddleRule1 "set" ShelleyEra where
  huddleRule1Named pname _ = untaggedSet pname
