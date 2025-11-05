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

  -- * Certificates and withdrawals
  certificate,
  account_registration_cert,
  account_unregistration_cert,
  delegation_to_stake_pool_cert,
  withdrawals,
  genesis_hash,

  -- * Transaction
  transaction_input,
  transaction_id,
  transaction_output,

  -- * Block and header
  header,
  operational_cert,
  major_protocol_version,
  protocol_version,

  -- * Update
  update,

  -- * Witnesses
  vkeywitness,
  bootstrap_witness,
) where

import Cardano.Ledger.BaseTypes (getVersion)
import Cardano.Ledger.Core (ByronEra, Era, eraProtVerHigh, eraProtVerLow)
import Cardano.Ledger.Shelley (ShelleyEra)
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Word (Word64)
import Test.Cardano.Ledger.Core.Binary.CDDL
import Text.Heredoc
import Prelude hiding ((/))

shelleyCDDL :: Huddle
shelleyCDDL =
  collectFrom $
    [HIRule $ block @ShelleyEra, HIRule $ transaction @ShelleyEra, HIRule signkey_kes]
      <> shelleyPoolRules

pool_registration_cert :: Named Group
pool_retirement_cert :: Named Group
shelleyPoolRules :: [HuddleItem]
(pool_registration_cert, pool_retirement_cert, shelleyPoolRules) = mkPoolRules dns_name64 url64

block :: forall era. Era era => Rule
block =
  "block"
    =:= arr
      [ a $ header @era
      , "transaction_bodies" ==> arr [0 <+ a (transaction_body @era)]
      , "transaction_witness_sets" ==> arr [0 <+ a transaction_witness_set]
      , "transaction_metadata_set" ==> mp [0 <+ asKey transaction_index ==> metadata]
      ]

transaction :: forall era. Era era => Rule
transaction =
  "transaction"
    =:= arr
      [ a $ transaction_body @era
      , a transaction_witness_set
      , a (metadata / VNil)
      ]

header :: forall era. Era era => Rule
header = "header" =:= arr [a $ header_body @era, "body_signature" ==> kes_signature]

header_body :: forall era. Era era => Rule
header_body =
  "header_body"
    =:= arr
      [ "block_number" ==> block_number
      , "slot" ==> slot
      , "prev_hash" ==> (hash32 / VNil)
      , "issuer_vkey" ==> vkey
      , "vrf_vkey" ==> vrf_vkey
      , "nonce_vrf" ==> vrf_cert
      , "leader_vrf" ==> vrf_cert
      , "block_body_size" ==> (VUInt `sized` (4 :: Word64))
      , "block_body_hash" ==> hash32
      , a operational_cert
      , a $ protocol_version @era
      ]

operational_cert :: Named Group
operational_cert =
  "operational_cert"
    =:~ grp
      [ "hot_vkey" ==> kes_vkey
      , "sequence_number" ==> sequence_number
      , "kes_period" ==> kes_period
      , "sigma" ==> signature
      ]

major_protocol_version :: forall era. Era era => Rule
major_protocol_version =
  "major_protocol_version"
    =:= (getVersion @Integer (eraProtVerLow @ByronEra) ... succ (getVersion @Integer (eraProtVerHigh @era)))

protocol_version :: forall era. Era era => Named Group
protocol_version = "protocol_version" =:~ grp [a $ major_protocol_version @era, a VUInt]

transaction_body :: forall era. Era era => Rule
transaction_body =
  "transaction_body"
    =:= mp
      [ idx 0 ==> untagged_set transaction_input
      , idx 1 ==> arr [0 <+ a transaction_output]
      , idx 2 ==> coin
      , idx 3 ==> slot
      , opt (idx 4 ==> arr [0 <+ a certificate])
      , opt (idx 5 ==> withdrawals)
      , opt (idx 6 ==> update @era)
      , opt (idx 7 ==> auxiliary_data_hash)
      ]

transaction_input :: Rule
transaction_input =
  "transaction_input"
    =:= arr
      [ "id" ==> transaction_id
      , "index" ==> VUInt `sized` (2 :: Word64)
      ]

transaction_id :: Rule
transaction_id =
  "transaction_id" =:= hash32

transaction_output :: Rule
transaction_output = "transaction_output" =:= arr [a address, "amount" ==> coin]

account_registration_cert :: Named Group
account_registration_cert =
  comment "This certificate will be deprecated in a future era" $
    "account_registration_cert" =:~ grp [0, a stake_credential]

account_unregistration_cert :: Named Group
account_unregistration_cert =
  comment "This certificate will be deprecated in a future era" $
    "account_unregistration_cert" =:~ grp [1, a stake_credential]

delegation_to_stake_pool_cert :: Named Group
delegation_to_stake_pool_cert = "delegation_to_stake_pool_cert" =:~ grp [2, a stake_credential, a pool_keyhash]

-- Legacy certificates (removed in Conway)
genesis_delegation_cert :: Named Group
genesis_delegation_cert =
  "genesis_delegation_cert"
    =:~ grp [5, a genesis_hash, a genesis_delegate_hash, a vrf_keyhash]

genesis_hash :: Rule
genesis_hash = "genesis_hash" =:= hash28

genesis_delegate_hash :: Rule
genesis_delegate_hash = "genesis_delegate_hash" =:= hash28

move_instantaneous_rewards_cert :: Named Group
move_instantaneous_rewards_cert =
  "move_instantaneous_rewards_cert" =:~ grp [6, a move_instantaneous_reward]

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
        , a (smp [0 <+ asKey stake_credential ==> delta_coin] / coin)
        ]

delta_coin :: Rule
delta_coin =
  comment
    [str|This too has been introduced in Shelley as a backport from Alonzo.
        |]
    $ "delta_coin" =:= VInt

certificate :: Rule
certificate =
  "certificate"
    =:= arr [a account_registration_cert]
    / arr [a account_unregistration_cert]
    / arr [a delegation_to_stake_pool_cert]
    / arr [a pool_registration_cert]
    / arr [a pool_retirement_cert]
    / arr [a genesis_delegation_cert]
    / arr [a move_instantaneous_rewards_cert]

withdrawals :: Rule
withdrawals = "withdrawals" =:= mp [0 <+ asKey reward_account ==> coin]

update :: forall era. Era era => Rule
update = "update" =:= arr [a $ proposed_protocol_parameter_updates @era, a epoch]

proposed_protocol_parameter_updates :: forall era. Era era => Rule
proposed_protocol_parameter_updates =
  "proposed_protocol_parameter_updates"
    =:= mp [0 <+ asKey genesis_hash ==> protocol_param_update @era]

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
      , opt (idx 7 ==> epoch_interval) //- "maximum epoch"
      , opt (idx 8 ==> VUInt `sized` (2 :: Word64)) //- "n_opt: desired number of stake pools"
      , opt (idx 9 ==> nonnegative_interval) //- "pool pledge influence"
      , opt (idx 10 ==> unit_interval) //- "expansion rate"
      , opt (idx 11 ==> unit_interval) //- "treasury growth rate"
      , opt (idx 12 ==> unit_interval) //- "decentralization constant"
      , opt (idx 13 ==> nonce) //- "extra entropy"
      , opt (idx 14 ==> arr [a $ protocol_version @era]) //- "protocol version"
      , opt (idx 15 ==> coin) //- "min utxo value"
      , opt (idx 16 ==> coin) //- "min pool cost"
      ]

transaction_witness_set :: Rule
transaction_witness_set =
  "transaction_witness_set"
    =:= mp
      [ opt $ idx 0 ==> arr [0 <+ a vkeywitness]
      , opt $ idx 1 ==> arr [0 <+ a shelley_native_script]
      , opt $ idx 2 ==> arr [0 <+ a bootstrap_witness]
      ]

vkeywitness :: Rule
vkeywitness = "vkeywitness" =:= arr [a vkey, a signature]

bootstrap_witness :: Rule
bootstrap_witness =
  "bootstrap_witness"
    =:= arr
      [ "public_key" ==> vkey
      , "signature" ==> signature
      , "chain_code" ==> (VBytes `sized` (32 :: Word64))
      , "attributes" ==> VBytes
      ]

shelley_native_script :: Rule
shelley_native_script =
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
      =:= arr [a script_pubkey]
      / arr [a script_all]
      / arr [a script_any]
      / arr [a script_n_of_k]

script_pubkey :: Named Group
script_pubkey = mkScriptPubkey

script_all :: Named Group
script_all = mkScriptAll shelley_native_script

script_any :: Named Group
script_any = mkScriptAny shelley_native_script

script_n_of_k :: Named Group
script_n_of_k = mkScriptNOfK VUInt shelley_native_script
