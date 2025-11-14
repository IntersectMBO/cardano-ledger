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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.CDDL (
  shelleyCDDL,
  mkHeader,
  mkProposedProtocolParameterUpdates,
  mkUpdate,
  mkProtocolParamUpdate,
  mkHeaderBody,
  mkTransactionWitnessSet,
  untagged_set,
) where

import Cardano.Ledger.BaseTypes (getVersion)
import Cardano.Ledger.CDDL
import Cardano.Ledger.Core (ByronEra, Era, eraProtVerHigh, eraProtVerLow)
import Cardano.Ledger.Core.CDDL ()
import Cardano.Ledger.Shelley (ShelleyEra)
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Word (Word64)
import Text.Heredoc
import Prelude hiding ((/))

shelleyCDDL :: Huddle
shelleyCDDL =
  collectFrom
    [ huddleItem @"block" @ShelleyEra
    , huddleItem @"transaction" @ShelleyEra
    , huddleItem @"signkey_kes" @ShelleyEra
    ]

mkHeader :: forall era. HasCDDL "header_body" era => HuddleItem
mkHeader =
  HIRule $
    "header"
      =:= arr [a $ huddleRule @"header_body" @era, "body_signature" ==> huddleRule @"kes_signature" @era]

mkProposedProtocolParameterUpdates :: forall era. HasCDDL "protocol_param_update" era => HuddleItem
mkProposedProtocolParameterUpdates =
  HIRule $
    "proposed_protocol_parameter_updates"
      =:= mp [0 <+ asKey (huddleRule @"genesis_hash" @era) ==> huddleRule @"protocol_param_update" @era]

mkUpdate :: forall era. HasCDDL "proposed_protocol_parameter_updates" era => HuddleItem
mkUpdate =
  HIRule $
    "update"
      =:= arr [a $ huddleRule @"proposed_protocol_parameter_updates" @era, a $ huddleRule @"epoch" @era]

mkProtocolParamUpdate :: forall era. HasCDDL "protocol_version" era => HuddleItem
mkProtocolParamUpdate =
  HIRule $
    "protocol_param_update"
      =:= mp
        [ opt (idx 0 ==> VUInt) //- "minfee A"
        , opt (idx 1 ==> VUInt) //- "minfee B"
        , opt (idx 2 ==> VUInt) //- "max block body size"
        , opt (idx 3 ==> VUInt) //- "max transaction size"
        , opt (idx 4 ==> VUInt `sized` (2 :: Word64)) //- "max block header size"
        , opt (idx 5 ==> huddleRule @"coin" @era) //- "key deposit"
        , opt (idx 6 ==> huddleRule @"coin" @era) //- "pool deposit"
        , opt (idx 7 ==> huddleRule @"epoch_interval" @era) //- "maximum epoch"
        , opt (idx 8 ==> VUInt `sized` (2 :: Word64)) //- "n_opt: desired number of stake pools"
        , opt (idx 9 ==> huddleRule @"nonnegative_interval" @era) //- "pool pledge influence"
        , opt (idx 10 ==> huddleRule @"unit_interval" @era) //- "expansion rate"
        , opt (idx 11 ==> huddleRule @"unit_interval" @era) //- "treasury growth rate"
        , opt (idx 12 ==> huddleRule @"unit_interval" @era) //- "decentralization constant"
        , opt (idx 13 ==> huddleRule @"nonce" @era) //- "extra entropy"
        , opt (idx 14 ==> arr [a $ huddleGroup @"protocol_version" @era]) //- "protocol version"
        , opt (idx 15 ==> huddleRule @"coin" @era) //- "min utxo value"
        , opt (idx 16 ==> huddleRule @"coin" @era) //- "min pool cost"
        ]

mkHeaderBody ::
  forall era operationalCertEra.
  (HasCDDL "operational_cert" operationalCertEra, HasCDDL "protocol_version" era) => HuddleItem
mkHeaderBody =
  HIRule $
    "header_body"
      =:= arr
        [ "block_number" ==> huddleRule @"block_number" @era
        , "slot" ==> huddleRule @"slot" @era
        , "prev_hash" ==> (huddleRule @"hash32" @era / VNil)
        , "issuer_vkey" ==> huddleRule @"vkey" @era
        , "vrf_vkey" ==> huddleRule @"vrf_vkey" @era
        , "nonce_vrf" ==> huddleRule @"vrf_cert" @era
        , "leader_vrf" ==> huddleRule @"vrf_cert" @era
        , "block_body_size" ==> VUInt `sized` (4 :: Word64)
        , "block_body_hash" ==> huddleRule @"hash32" @era
        , a $ huddleGroup @"operational_cert" @operationalCertEra
        , a $ huddleGroup @"protocol_version" @era
        ]

mkTransactionWitnessSet ::
  forall nativeScriptEra. HasCDDL "native_script" nativeScriptEra => HuddleItem
mkTransactionWitnessSet =
  HIRule $
    "transaction_witness_set"
      =:= mp
        [ opt $ idx 0 ==> arr [0 <+ a (huddleRule @"vkeywitness" @ShelleyEra)]
        , opt $ idx 1 ==> arr [0 <+ a (huddleRule @"native_script" @nativeScriptEra)]
        , opt $ idx 2 ==> arr [0 <+ a (huddleRule @"bootstrap_witness" @ShelleyEra)]
        ]

instance HasCDDL "dns_name" ShelleyEra where
  huddleItem = HIRule $ "dns_name" =:= VText `sized` (0 :: Word64, 64 :: Word64)

instance HasCDDL "url" ShelleyEra where
  huddleItem = HIRule $ "url" =:= VText `sized` (0 :: Word64, 64 :: Word64)

instance HasCDDL "pool_metadata" ShelleyEra where
  huddleItem =
    HIRule $
      "pool_metadata"
        =:= arr [a $ huddleRule @"url" @ShelleyEra, a VBytes]

instance HasCDDL "single_host_addr" ShelleyEra where
  huddleItem =
    HIGroup $
      "single_host_addr"
        =:~ grp
          [ 0
          , a $ huddleRule @"port" @ShelleyEra / VNil
          , a $ huddleRule @"ipv4" @ShelleyEra / VNil
          , a $ huddleRule @"ipv6" @ShelleyEra / VNil
          ]

instance HasCDDL "single_host_name" ShelleyEra where
  huddleItem =
    HIGroup
      $ comment
        "dns_name: An A or AAAA DNS record"
      $ "single_host_name"
        =:~ grp
          [ 1
          , a $ huddleRule @"port" @ShelleyEra / VNil
          , a $ huddleRule @"dns_name" @ShelleyEra
          ]

instance HasCDDL "multi_host_name" ShelleyEra where
  huddleItem =
    HIGroup
      $ comment
        "dns_name: An SRV DNS record"
      $ "multi_host_name"
        =:~ grp [2, a $ huddleRule @"dns_name" @ShelleyEra]

instance HasCDDL "relay" ShelleyEra where
  huddleItem =
    HIRule $
      "relay"
        =:= arr [a $ huddleGroup @"single_host_addr" @ShelleyEra]
        / arr [a $ huddleGroup @"single_host_name" @ShelleyEra]
        / arr [a $ huddleGroup @"multi_host_name" @ShelleyEra]

instance HasCDDL "pool_params" ShelleyEra where
  huddleItem =
    HIGroup
      $ comment
        "Pool parameters for stake pool registration"
      $ "pool_params"
        =:~ grp
          [ "operator" ==> huddleRule @"pool_keyhash" @ShelleyEra
          , "vrf_keyhash" ==> huddleRule @"vrf_keyhash" @ShelleyEra
          , "pledge" ==> huddleRule @"coin" @ShelleyEra
          , "cost" ==> huddleRule @"coin" @ShelleyEra
          , "margin" ==> huddleRule @"unit_interval" @ShelleyEra
          , "reward_account" ==> huddleRule @"reward_account" @ShelleyEra
          , "pool_owners" ==> untagged_set (huddleRule @"addr_keyhash" @ShelleyEra)
          , "relays" ==> arr [0 <+ a (huddleRule @"relay" @ShelleyEra)]
          , "pool_metadata" ==> huddleRule @"pool_metadata" @ShelleyEra / VNil
          ]

untagged_set :: IsType0 a => a -> GRuleCall
untagged_set = binding $ \x -> "set" =:= arr [0 <+ a x]

instance HasCDDL "pool_registration_cert" ShelleyEra where
  huddleItem = HIGroup $ "pool_registration_cert" =:~ grp [3, a $ huddleGroup @"pool_params" @ShelleyEra]

instance HasCDDL "pool_retirement_cert" ShelleyEra where
  huddleItem =
    HIGroup $
      "pool_retirement_cert"
        =:~ grp [4, a $ huddleRule @"pool_keyhash" @ShelleyEra, a $ huddleRule @"epoch" @ShelleyEra]

instance Era era => HasCDDL "genesis_hash" era where
  huddleItem = HIRule $ "genesis_hash" =:= huddleRule @"hash28" @era

instance Era era => HasCDDL "genesis_delegate_hash" era where
  huddleItem = HIRule $ "genesis_delegate_hash" =:= huddleRule @"hash28" @era

instance HasCDDL "genesis_delegation_cert" ShelleyEra where
  huddleItem =
    HIGroup $
      "genesis_delegation_cert"
        =:~ grp
          [ 5
          , a $ huddleRule @"genesis_hash" @ShelleyEra
          , a $ huddleRule @"genesis_delegate_hash" @ShelleyEra
          , a $ huddleRule @"vrf_keyhash" @ShelleyEra
          ]

instance HasCDDL "delta_coin" ShelleyEra where
  huddleItem =
    HIRule $
      comment
        "This too has been introduced in Shelley as a backport from Alonzo."
        ("delta_coin" =:= VInt)

instance HasCDDL "move_instantaneous_reward" ShelleyEra where
  huddleItem =
    HIRule
      $ comment
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
                  [0 <+ asKey (huddleRule @"stake_credential" @ShelleyEra) ==> huddleRule @"delta_coin" @ShelleyEra]
                  / huddleRule @"coin" @ShelleyEra
              )
          ]

instance HasCDDL "move_instantaneous_rewards_cert" ShelleyEra where
  huddleItem =
    HIGroup $
      "move_instantaneous_rewards_cert"
        =:~ grp [6, a $ huddleRule @"move_instantaneous_reward" @ShelleyEra]

instance HasCDDL "account_registration_cert" ShelleyEra where
  huddleItem =
    HIGroup
      $ comment
        "This certificate will be deprecated in a future era"
      $ "account_registration_cert"
        =:~ grp [0, a $ huddleRule @"stake_credential" @ShelleyEra]

instance HasCDDL "account_unregistration_cert" ShelleyEra where
  huddleItem =
    HIGroup
      $ comment
        "This certificate will be deprecated in a future era"
      $ "account_unregistration_cert"
        =:~ grp [1, a $ huddleRule @"stake_credential" @ShelleyEra]

instance HasCDDL "delegation_to_stake_pool_cert" ShelleyEra where
  huddleItem =
    HIGroup $
      "delegation_to_stake_pool_cert"
        =:~ grp [2, a $ huddleRule @"stake_credential" @ShelleyEra, a $ huddleRule @"pool_keyhash" @ShelleyEra]

instance HasCDDL "certificate" ShelleyEra where
  huddleItem =
    HIRule $
      "certificate"
        =:= arr [a $ huddleGroup @"account_registration_cert" @ShelleyEra]
        / arr [a $ huddleGroup @"account_unregistration_cert" @ShelleyEra]
        / arr [a $ huddleGroup @"delegation_to_stake_pool_cert" @ShelleyEra]
        / arr [a $ huddleGroup @"pool_registration_cert" @ShelleyEra]
        / arr [a $ huddleGroup @"pool_retirement_cert" @ShelleyEra]
        / arr [a $ huddleGroup @"genesis_delegation_cert" @ShelleyEra]
        / arr [a $ huddleGroup @"move_instantaneous_rewards_cert" @ShelleyEra]

instance HasCDDL "withdrawals" ShelleyEra where
  huddleItem =
    HIRule $
      "withdrawals"
        =:= mp [0 <+ asKey (huddleRule @"reward_account" @ShelleyEra) ==> huddleRule @"coin" @ShelleyEra]

instance HasCDDL "major_protocol_version" ShelleyEra where
  huddleItem =
    HIRule $
      "major_protocol_version"
        =:= getVersion @Integer (eraProtVerLow @ByronEra)
        ... succ (getVersion @Integer (eraProtVerHigh @ShelleyEra))

instance HasCDDL "protocol_version" ShelleyEra where
  huddleItem =
    HIGroup $ "protocol_version" =:~ grp [a $ huddleRule @"major_protocol_version" @ShelleyEra, a VUInt]

instance HasCDDL "protocol_param_update" ShelleyEra where
  huddleItem = mkProtocolParamUpdate @ShelleyEra

instance HasCDDL "proposed_protocol_parameter_updates" ShelleyEra where
  huddleItem = mkProposedProtocolParameterUpdates @ShelleyEra

instance HasCDDL "update" ShelleyEra where
  huddleItem = mkUpdate @ShelleyEra

instance HasCDDL "operational_cert" ShelleyEra where
  huddleItem =
    HIGroup $
      "operational_cert"
        =:~ grp
          [ "hot_vkey" ==> huddleRule @"kes_vkey" @ShelleyEra
          , "sequence_number" ==> huddleRule @"sequence_number" @ShelleyEra
          , "kes_period" ==> huddleRule @"kes_period" @ShelleyEra
          , "sigma" ==> huddleRule @"signature" @ShelleyEra
          ]

instance HasCDDL "header_body" ShelleyEra where
  huddleItem = mkHeaderBody @ShelleyEra @ShelleyEra

instance HasCDDL "header" ShelleyEra where
  huddleItem = mkHeader @ShelleyEra

instance HasCDDL "transaction_id" ShelleyEra where
  huddleItem = HIRule $ "transaction_id" =:= huddleRule @"hash32" @ShelleyEra

instance HasCDDL "transaction_input" ShelleyEra where
  huddleItem =
    HIRule $
      "transaction_input"
        =:= arr
          [ "id" ==> huddleRule @"transaction_id" @ShelleyEra
          , "index" ==> VUInt `sized` (2 :: Word64)
          ]

instance HasCDDL "transaction_output" ShelleyEra where
  huddleItem =
    HIRule $
      "transaction_output"
        =:= arr [a $ huddleRule @"address" @ShelleyEra, "amount" ==> huddleRule @"coin" @ShelleyEra]

instance HasCDDL "script_pubkey" ShelleyEra where
  huddleItem = HIGroup $ "script_pubkey" =:~ grp [0, a $ huddleRule @"addr_keyhash" @ShelleyEra]

instance HasCDDL "script_all" ShelleyEra where
  huddleItem = HIGroup $ "script_all" =:~ grp [1, a $ arr [0 <+ a (huddleRule @"native_script" @ShelleyEra)]]

instance HasCDDL "script_any" ShelleyEra where
  huddleItem = HIGroup $ "script_any" =:~ grp [2, a $ arr [0 <+ a (huddleRule @"native_script" @ShelleyEra)]]

instance HasCDDL "script_n_of_k" ShelleyEra where
  huddleItem =
    HIGroup $
      "script_n_of_k"
        =:~ grp [3, "n" ==> VUInt, a $ arr [0 <+ a (huddleRule @"native_script" @ShelleyEra)]]

instance HasCDDL "native_script" ShelleyEra where
  huddleItem =
    HIRule
      $ comment
        [str|Native scripts support 4 operations:
            |  - Signature verification (script_pubkey)
            |  - Conjunctions (script_all)
            |  - Disjunctions (script_any)
            |  - M-of-N thresholds (script_n_of_k)
            |
            |Note: Shelley uses VUInt for the threshold in script_n_of_k.
            |]
      $ "native_script"
        =:= arr [a $ huddleGroup @"script_pubkey" @ShelleyEra]
        / arr [a $ huddleGroup @"script_all" @ShelleyEra]
        / arr [a $ huddleGroup @"script_any" @ShelleyEra]
        / arr [a $ huddleGroup @"script_n_of_k" @ShelleyEra]

instance HasCDDL "vkeywitness" ShelleyEra where
  huddleItem =
    HIRule $
      "vkeywitness" =:= arr [a $ huddleRule @"vkey" @ShelleyEra, a $ huddleRule @"signature" @ShelleyEra]

instance HasCDDL "bootstrap_witness" ShelleyEra where
  huddleItem =
    HIRule $
      "bootstrap_witness"
        =:= arr
          [ "public_key" ==> huddleRule @"vkey" @ShelleyEra
          , "signature" ==> huddleRule @"signature" @ShelleyEra
          , "chain_code" ==> VBytes `sized` (32 :: Word64)
          , "attributes" ==> VBytes
          ]

instance HasCDDL "transaction_witness_set" ShelleyEra where
  huddleItem = mkTransactionWitnessSet @ShelleyEra

instance HasCDDL "transaction_body" ShelleyEra where
  huddleItem =
    HIRule $
      "transaction_body"
        =:= mp
          [ idx 0 ==> untagged_set (huddleRule @"transaction_input" @ShelleyEra)
          , idx 1 ==> arr [0 <+ a (huddleRule @"transaction_output" @ShelleyEra)]
          , idx 2 ==> huddleRule @"coin" @ShelleyEra
          , idx 3 ==> huddleRule @"slot" @ShelleyEra
          , opt (idx 4 ==> arr [0 <+ a (huddleRule @"certificate" @ShelleyEra)])
          , opt (idx 5 ==> huddleRule @"withdrawals" @ShelleyEra)
          , opt (idx 6 ==> huddleRule @"update" @ShelleyEra)
          , opt (idx 7 ==> huddleRule @"auxiliary_data_hash" @ShelleyEra)
          ]

instance HasCDDL "transaction" ShelleyEra where
  huddleItem =
    HIRule $
      "transaction"
        =:= arr
          [ a $ huddleRule @"transaction_body" @ShelleyEra
          , a $ huddleRule @"transaction_witness_set" @ShelleyEra
          , a $ huddleRule @"metadata" @ShelleyEra / VNil
          ]

instance HasCDDL "block" ShelleyEra where
  huddleItem =
    HIRule $
      "block"
        =:= arr
          [ a $ huddleRule @"header" @ShelleyEra
          , "transaction_bodies" ==> arr [0 <+ a (huddleRule @"transaction_body" @ShelleyEra)]
          , "transaction_witness_sets" ==> arr [0 <+ a (huddleRule @"transaction_witness_set" @ShelleyEra)]
          , "transaction_metadata_set"
              ==> mp [0 <+ asKey (huddleRule @"transaction_index" @ShelleyEra) ==> huddleRule @"metadata" @ShelleyEra]
          ]
