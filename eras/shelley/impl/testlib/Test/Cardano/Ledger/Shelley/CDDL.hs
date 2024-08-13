{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Evaluate" #-}
module Test.Cardano.Ledger.Shelley.CDDL where

import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Word (Word64)
import GHC.Num (Integer)
import Test.Cardano.Ledger.Core.Binary.CDDL

block :: Rule
block =
  "block"
    =:= arr
      [ a header,
        "transaction_bodies" ==> arr [0 <+ a transaction_body],
        "transaction_witness_sets"
          ==> arr [0 <+ a transaction_witness_set],
        "transaction_metadata_set"
          ==> mp [0 <+ asKey transaction_index ==> transaction_metadata]
      ]

transaction :: Rule
transaction =
  "transaction"
    =:= arr
      [ a transaction_body,
        a transaction_witness_set,
        a (transaction_metadata / VNil)
      ]

transaction_index :: Rule
transaction_index = "transaction_index" =:= VUInt `sized` (2 :: Word64)

header :: Rule
header =
  "header"
    =:= arr
      [ a header_body,
        "body_signature" ==> kes_signature
      ]

header_body :: Rule
header_body =
  "header_body"
    =:= arr
      [ "block_number" ==> VUInt,
        "slot" ==> VUInt,
        "prev_hash" ==> (hash32 / VNil),
        "issuer_vkey" ==> vkey,
        "vrf_vkey" ==> vrf_vkey,
        "nonce_vrf" ==> vrf_cert,
        "leader_vrf" ==> vrf_cert,
        "block_body_size" ==> (VUInt `sized` (4 :: Word64)),
        "block_body_hash" ==> hash32,
        a operational_cert,
        a protocol_version
      ]

operational_cert :: Rule
operational_cert =
  "operational_cert"
    =:= arr
      [ "hot_vkey" ==> kes_vkey,
        "sequence_number" ==> VUInt,
        "kes_period" ==> VUInt,
        "sigma" ==> signature
      ]

-- TODO Replace with the following once
-- https://github.com/input-output-hk/cuddle/issues/29 is addressed in cuddle.
--
-- next_major_protocol_version :: Rule
-- next_major_protocol_version = "next_major_protocol_version" =:= (10 :: Integer)
next_major_protocol_version :: Integer
next_major_protocol_version = 3

major_protocol_version :: Rule
major_protocol_version = "major_protocol_version" =:= (1 :: Integer) ... next_major_protocol_version

protocol_version :: Rule
protocol_version = "protocol_version" =:= arr [a major_protocol_version, a VUInt]

transaction_body :: Rule
transaction_body =
  "transaction_body"
    =:= mp
      [ idx 0 ==> set transaction_input,
        idx 1 ==> arr [0 <+ a transaction_output],
        idx 2 ==> coin,
        opt (idx 3 ==> VUInt),
        opt (idx 4 ==> arr [0 <+ a certificate]),
        opt (idx 5 ==> withdrawals),
        opt (idx 6 ==> update),
        opt (idx 7 ==> metadata_hash)
      ]

transaction_input :: Rule
transaction_input =
  "transaction_input"
    =:= arr
      [ "transaction_id" ==> hash32,
        "index" ==> VUInt
      ]

transaction_output :: Rule
transaction_output =
  "transaction_output"
    =:= arr
      [ a address,
        "amount" ==> coin
      ]

certificate :: Rule
certificate =
  "certificate"
    =:= arr [a stake_registration]
    / arr [a stake_deregistration]
    / arr [a stake_delegation]
    / arr [a pool_registration]
    / arr [a pool_retirement]
    / arr [a genesis_key_delegation]
    / arr [a move_instantaneous_rewards_cert]

stake_registration :: Named Group
stake_registration =
  comment "This will be deprecated in a future era" $
    "stake_registration" =:~ grp [0, a stake_credential]

stake_deregistration :: Named Group
stake_deregistration =
  comment "This will be deprecated in a future era" $
    "stake_deregistration" =:~ grp [1, a stake_credential]

stake_delegation :: Named Group
stake_delegation =
  "stake_delegation"
    =:~ grp [2, a stake_credential, a pool_keyhash]

-- POOL
pool_registration :: Named Group
pool_registration = "pool_registration" =:~ grp [3, a pool_params]

pool_retirement :: Named Group
pool_retirement = "pool_retirement" =:~ grp [4, a pool_keyhash, a epoch]

genesis_key_delegation :: Named Group
genesis_key_delegation =
  "genesis_key_delegation"
    =:~ grp [5, a genesishash, a genesis_delegate_hash, a vrf_keyhash]

move_instantaneous_rewards_cert :: Named Group
move_instantaneous_rewards_cert =
  "move_instantaneous_rewards_cert"
    =:~ grp [6, a move_instantaneous_reward]

move_instantaneous_reward :: Rule
move_instantaneous_reward =
  "move_instantaneous_reward"
    =:= arr [a (int 0 / int 1), a $ mp [0 <+ asKey stake_credential ==> coin]]

stake_credential :: Rule
stake_credential =
  "stake_credential"
    =:= arr
      [0, a addr_keyhash]
    / arr [1, a scripthash]

pool_params :: Named Group
pool_params =
  "pool_params"
    =:~ grp
      [ "operator" ==> pool_keyhash,
        "vrf_keyhash" ==> vrf_keyhash,
        "pledge" ==> coin,
        "cost" ==> coin,
        "margin" ==> unit_interval,
        "reward_account" ==> reward_account,
        "pool_owners" ==> set addr_keyhash,
        "relays" ==> arr [0 <+ a relay],
        "pool_metadata" ==> (pool_metadata / VNil)
      ]

port :: Rule
port = "port" =:= VUInt `le` 65535

ipv4 :: Rule
ipv4 = "ipv4" =:= VBytes `sized` (4 :: Word64)

ipv6 :: Rule
ipv6 = "ipv6" =:= VBytes `sized` (16 :: Word64)

dns_name :: Rule
dns_name = "dns_name" =:= VText `sized` (0 :: Word64, 64 :: Word64)

single_host_addr :: Named Group
single_host_addr =
  "single_host_addr"
    =:~ grp
      [ 0,
        port / VNil,
        ipv4 / VNil,
        ipv6 / VNil
      ]

single_host_name :: Named Group
single_host_name =
  "single_host_name"
    =:~ grp
      [ 1,
        port / VNil,
        a dns_name -- An A or AAAA DNS record
      ]

multi_host_name :: Named Group
multi_host_name =
  "multi_host_name"
    =:~ grp
      [ 2,
        a dns_name -- A SRV DNS record
      ]

relay :: Rule
relay =
  "relay"
    =:= arr [a single_host_addr]
    / arr [a single_host_name]
    / arr [a multi_host_name]

pool_metadata :: Rule
pool_metadata = "pool_metadata" =:= arr [a url, a pool_metadata_hash]

url :: Rule
url = "url" =:= VText `sized` (0 :: Word64, 64 :: Word64)

withdrawals :: Rule
withdrawals = "withdrawals" =:= mp [1 <+ asKey reward_account ==> coin]

update :: Rule
update = "update" =:= arr [ a proposed_protocol_parameter_updates, a epoch]

proposed_protocol_parameter_updates :: Rule
proposed_protocol_parameter_updates = "proposed_protocol_parameter_updates"
  =:= mp [0 <+ asKey genesishash ==> protocol_param_update]

protocol_param_update :: Rule
protocol_param_update =
  "protocol_param_update"
    =:= mp
      [ opt (idx 0 ==> coin), -- minfee A
        opt (idx 1 ==> coin), -- minfee B
        opt (idx 2 ==> (VUInt `sized` (4 :: Word64))), -- max block body size
        opt (idx 3 ==> (VUInt `sized` (4 :: Word64))), -- max transaction size
        opt (idx 4 ==> (VUInt `sized` (2 :: Word64))), -- max block header size
        opt (idx 5 ==> coin), -- key deposit
        opt (idx 6 ==> coin), -- pool deposit
        opt (idx 7 ==> epoch), -- maximum epoch
        opt (idx 8 ==> (VUInt `sized` (2 :: Word64))), -- n_opt: desired number of stake pools
        opt (idx 9 ==> nonnegative_interval), -- pool pledge influence
        opt (idx 10 ==> unit_interval), -- expansion rate
        opt (idx 11 ==> unit_interval), -- treasury growth rate
        opt (idx 12 ==> unit_interval), -- decentralisation constant
        opt (idx 13 ==> nonce), -- extra entropy
        opt (idx 14 ==> arr [a protocol_version]), -- protocol version
        opt (idx 15 ==> coin) -- min utxo value
      ]

transaction_witness_set :: Rule
transaction_witness_set =
  "transaction_witness_set"
    =:= mp
      [ opt $ idx 0 ==> arr [ 0 <+ a vkeywitness],
        opt $ idx 1 ==> arr [ 0 <+ a multisig_script],
        opt $ idx 2 ==> arr [ 0 <+ a bootstrap_witness]
      ]

transaction_metadatum :: Rule
transaction_metadatum =
  "transaction_metadatum"
    =:= smp [0 <+ asKey transaction_metadatum ==> transaction_metadatum]
    / sarr [0 <+ a transaction_metadatum]
    / VInt
    / (VBytes `sized` (0 :: Word64, 64 :: Word64))
    / (VText `sized` (0 :: Word64, 64 :: Word64))

transaction_metadatum_label :: Rule
transaction_metadatum_label = "transaction_metadatum_label" =:= VUInt

transaction_metadata :: Rule
transaction_metadata =
  "transaction_metadata"
    =:= mp
      [ 0
          <+ asKey transaction_metadatum_label
          ==> transaction_metadatum
      ]

vkeywitness :: Rule
vkeywitness = "vkeywitness" =:= arr [a vkey, a signature]

bootstrap_witness :: Rule
bootstrap_witness =
  "bootstrap_witness"
    =:= arr
      [ "public_key" ==> vkey,
        "signature" ==> signature,
        "chain_code" ==> (VBytes `sized` (32 :: Word64)),
        "attributes" ==> VBytes
      ]

multisig_script :: Rule
multisig_script =
  "multisig_script"
    =:= arr [a script_pubkey]
    / arr [a script_all]
    / arr [a script_any]
    / arr [a script_n_of_k]

script_pubkey :: Named Group
script_pubkey = "script_pubkey" =:~ grp [0, a addr_keyhash]

script_all :: Named Group
script_all = "script_all" =:~ grp [1, a (arr [0 <+ a multisig_script])]

script_any :: Named Group
script_any = "script_any" =:~ grp [2, a (arr [0 <+ a multisig_script])]

script_n_of_k :: Named Group
script_n_of_k =
  "script_n_of_k"
    =:~ grp [3, "n" ==> VUInt, a (arr [0 <+ a multisig_script])]

epoch :: Rule
epoch = "epoch" =:= VUInt

genesis_delegate_hash :: Rule
genesis_delegate_hash = "genesis_delegate_hash" =:= hash28

genesishash :: Rule
genesishash = "genesishash" =:= hash28

scripthash :: Rule
scripthash = "scripthash" =:= hash28

metadata_hash :: Rule
metadata_hash = "metadata_hash" =:= hash32

pool_metadata_hash :: Rule
pool_metadata_hash = "pool_metadata_hash" =:= hash32

nonce :: Rule
nonce = "nonce" =:=
  arr [ a (int 0 / int 1), a (VBytes `sized` (32 :: Word64))]
