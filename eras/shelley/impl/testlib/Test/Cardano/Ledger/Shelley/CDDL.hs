{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Evaluate" -}

module Test.Cardano.Ledger.Shelley.CDDL (
  module Test.Cardano.Ledger.Core.Binary.CDDL,
  shelleyCDDL,
  shelley_auxiliary_data,
  shelley_transaction_output,
  shelley_certificate,
  shelley_withdrawals,
  shelley_epoch,
  shelley_protocol_param_updates,
  shelley_header,
  shelley_nonce,
  shelley_genesis_hash,
  shelley_operational_cert,
  shelley_stake_credential,
  shelley_stake_delegation,
  shelley_stake_registration,
  shelley_stake_deregistration,
) where

import Cardano.Ledger.Core (Era)
import Cardano.Ledger.Shelley (ShelleyEra)
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Word (Word64)
import Test.Cardano.Ledger.Core.Binary.CDDL
import Text.Heredoc
import Prelude hiding ((/))

shelleyCDDL :: Huddle
shelleyCDDL = collectFrom [HIRule $ block @ShelleyEra, HIRule $ transaction @ShelleyEra, HIRule signkey_kes]

block :: forall era. Era era => Rule
block =
  "block"
    =:= arr
      [ a $ shelley_header @era
      , "transaction_bodies" ==> arr [0 <+ a (transaction_body @era)]
      , "transaction_witness_sets" ==> arr [0 <+ a transaction_witness_set]
      , "transaction_metadata_set" ==> mp [0 <+ asKey transaction_ix ==> shelley_auxiliary_data]
      ]

transaction :: forall era. Era era => Rule
transaction =
  "transaction"
    =:= arr
      [ a $ transaction_body @era
      , a transaction_witness_set
      , a (shelley_auxiliary_data / VNil)
      ]

shelley_header :: forall era. Era era => Rule
shelley_header = "header" =:= arr [a $ header_body @era, "body_signature" ==> kes_signature]

header_body :: forall era. Era era => Rule
header_body =
  "header_body"
    =:= arr
      [ "block_number" ==> VUInt
      , "slot" ==> VUInt
      , "prev_hash" ==> (bytes32 / VNil)
      , "issuer_vkey" ==> vkey
      , "vrf_vkey" ==> vrf_vkey
      , "nonce_vrf" ==> vrf_cert
      , "leader_vrf" ==> vrf_cert
      , "block_body_size" ==> (VUInt `sized` (4 :: Word64))
      , "block_body_hash" ==> bytes32
      , a shelley_operational_cert
      , a $ protocol_version @era
      ]

shelley_operational_cert :: Named Group
shelley_operational_cert =
  "operational_cert"
    =:~ grp
      [ "hot_vkey" ==> kes_vkey
      , "sequence_number" ==> VUInt
      , "kes_period" ==> VUInt
      , "sigma" ==> signature
      ]

transaction_body :: forall era. Era era => Rule
transaction_body =
  "transaction_body"
    =:= mp
      [ idx 0 ==> untagged_set transaction_input
      , idx 1 ==> arr [0 <+ a shelley_transaction_output]
      , idx 2 ==> coin
      , idx 3 ==> VUInt
      , opt (idx 4 ==> arr [0 <+ a shelley_certificate])
      , opt (idx 5 ==> shelley_withdrawals)
      , opt (idx 6 ==> arr [a $ shelley_protocol_param_updates @era, a shelley_epoch])
      , opt (idx 7 ==> auxiliary_data_hash)
      ]

shelley_transaction_output :: Rule
shelley_transaction_output = "shelley_transaction_output" =:= arr [a address, "amount" ==> coin]

shelley_certificate :: Rule
shelley_certificate =
  "certificate"
    =:= arr [a shelley_stake_registration]
    / arr [a shelley_stake_deregistration]
    / arr [a shelley_stake_delegation]
    / arr [a pool_registration]
    / arr [a pool_retirement]
    / arr [a genesis_key_delegation]
    / arr [a move_instantaneous_rewards_cert]

shelley_stake_registration :: Named Group
shelley_stake_registration =
  comment "This will be deprecated in a future era" $
    "stake_registration" =:~ grp [0, a shelley_stake_credential]

shelley_stake_deregistration :: Named Group
shelley_stake_deregistration =
  comment "This will be deprecated in a future era" $
    "stake_deregistration" =:~ grp [1, a shelley_stake_credential]

shelley_stake_delegation :: Named Group
shelley_stake_delegation = "stake_delegation" =:~ grp [2, a shelley_stake_credential, a pool_keyhash]

pool_registration :: Named Group
pool_registration = "pool_registration" =:~ grp [3, a pool_params]

pool_retirement :: Named Group
pool_retirement = "pool_retirement" =:~ grp [4, a pool_keyhash, a shelley_epoch]

genesis_key_delegation :: Named Group
genesis_key_delegation =
  "genesis_key_delegation"
    =:~ grp [5, a shelley_genesis_hash, a genesis_delegate_hash, a vrf_keyhash]

move_instantaneous_rewards_cert :: Named Group
move_instantaneous_rewards_cert =
  "move_instantaneous_rewards_cert"
    =:~ grp [6, a move_instantaneous_reward]

move_instantaneous_reward :: Rule
move_instantaneous_reward =
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
        , a (smp [0 <+ asKey shelley_stake_credential ==> delta_coin] / coin)
        ]

delta_coin :: Rule
delta_coin =
  comment
    [str|This too has been introduced in Shelley as a backport from Alonzo.
        |]
    $ "delta_coin" =:= VInt

shelley_stake_credential :: Rule
shelley_stake_credential = "stake_credential" =:= credential

pool_params :: Named Group
pool_params =
  "pool_params"
    =:~ grp
      [ "operator" ==> pool_keyhash
      , "vrf_keyhash" ==> vrf_keyhash
      , "pledge" ==> coin
      , "cost" ==> coin
      , "margin" ==> unit_interval
      , "reward_account" ==> reward_account
      , "pool_owners" ==> untagged_set addr_keyhash
      , "relays" ==> arr [0 <+ a relay]
      , "pool_metadata" ==> (shelley_pool_metadata / VNil)
      ]

dns_name :: Rule
dns_name = shelley_dns_name

shelley_dns_name :: Rule
shelley_dns_name = "dns_name" =:= VText `sized` (0 :: Word64, 64 :: Word64)

single_host_name :: Named Group
single_host_name =
  comment
    [str|dns_name: An A or AAAA DNS record
        |]
    $ "single_host_name" =:~ grp [1, a $ port / VNil, a dns_name]

multi_host_name :: Named Group
multi_host_name =
  comment
    [str|dns_name: An SRV DNS record
        |]
    $ "multi_host_name" =:~ grp [2, a dns_name]

relay :: Rule
relay =
  "relay"
    =:= arr [a single_host_addr]
    / arr [a single_host_name]
    / arr [a multi_host_name]

shelley_pool_metadata :: Rule
shelley_pool_metadata = "pool_metadata" =:= arr [a url, a VBytes]

url :: Rule
url = "url" =:= VText `sized` (0 :: Word64, 64 :: Word64)

shelley_withdrawals :: Rule
shelley_withdrawals = "withdrawals" =:= mp [0 <+ asKey reward_account ==> coin]

shelley_protocol_param_updates :: forall era. Era era => Rule
shelley_protocol_param_updates =
  "protocol_param_updates"
    =:= mp [0 <+ asKey shelley_genesis_hash ==> protocol_param_update @era]

protocol_param_update :: forall era. Era era => Rule
protocol_param_update =
  "protocol_param_update"
    =:= mp
      [ opt (idx 0 ==> VUInt) //- "minfee A"
      , opt (idx 1 ==> VUInt) //- "minfee B"
      , opt (idx 2 ==> VUInt) //- "max block body size"
      , opt (idx 3 ==> VUInt) //- "max transaction size"
      , opt (idx 4 ==> (VUInt `sized` (2 :: Word64))) //- "max block header size"
      , opt (idx 5 ==> coin) //- "key deposit"
      , opt (idx 6 ==> coin) //- "pool deposit"
      , opt (idx 7 ==> shelley_epoch) //- "maximum epoch"
      , opt (idx 8 ==> VUInt `sized` (2 :: Word64)) //- "n_opt: desired number of stake pools"
      , opt (idx 9 ==> nonnegative_interval) //- "pool pledge influence"
      , opt (idx 10 ==> unit_interval) //- "expansion rate"
      , opt (idx 11 ==> unit_interval) //- "treasury growth rate"
      , opt (idx 12 ==> unit_interval) //- "decentralization constant"
      , opt (idx 13 ==> shelley_nonce) //- "extra entropy"
      , opt (idx 14 ==> arr [a $ protocol_version @era]) //- "protocol version"
      , opt (idx 15 ==> coin) //- "min utxo value"
      , opt (idx 16 ==> coin) //- "min pool cost"
      ]

transaction_witness_set :: Rule
transaction_witness_set =
  "transaction_witness_set"
    =:= mp
      [ opt $ idx 0 ==> arr [0 <+ a vkey_witness]
      , opt $ idx 1 ==> arr [0 <+ a shelley_native_script]
      , opt $ idx 2 ==> arr [0 <+ a bootstrap_witness]
      ]

shelley_transaction_metadatum_label :: Rule
shelley_transaction_metadatum_label = "transaction_metadatum_label" =:= VUInt

shelley_auxiliary_data :: Rule
shelley_auxiliary_data =
  "shelley_auxiliary_data"
    =:= mp [0 <+ asKey shelley_transaction_metadatum_label ==> transaction_metadatum]

shelley_native_script :: Rule
shelley_native_script =
  "native_script"
    =:= arr [a script_pubkey]
    / arr [a script_all]
    / arr [a script_any]
    / arr [a script_n_of_k]

script_pubkey :: Named Group
script_pubkey = "script_pubkey" =:~ grp [0, a addr_keyhash]

script_all :: Named Group
script_all = "script_all" =:~ grp [1, a (arr [0 <+ a shelley_native_script])]

script_any :: Named Group
script_any = "script_any" =:~ grp [2, a (arr [0 <+ a shelley_native_script])]

script_n_of_k :: Named Group
script_n_of_k = "script_n_of_k" =:~ grp [3, "n" ==> VUInt, a (arr [0 <+ a shelley_native_script])]

shelley_epoch :: Rule
shelley_epoch = "epoch" =:= VUInt

genesis_delegate_hash :: Rule
genesis_delegate_hash = "genesis_delegate_hash" =:= bytes28

shelley_genesis_hash :: Rule
shelley_genesis_hash = "genesis_hash" =:= bytes28

shelley_nonce :: Rule
shelley_nonce = "nonce" =:= arr [0] / arr [1, a (VBytes `sized` (32 :: Word64))]
