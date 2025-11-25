{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.HuddleSpec (
  module Cardano.Ledger.Babbage.HuddleSpec,
  conwayCDDL,
  maybeTaggedSet,
  maybeTaggedNonemptySet,
  maybeTaggedOset,
  maybeTaggedNonemptyOset,
) where

import Cardano.Ledger.Babbage.HuddleSpec hiding (babbageScript)
import Cardano.Ledger.Conway (ConwayEra)
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Word (Word64)
import Text.Heredoc
import Prelude hiding ((/))

conwayCDDL :: Huddle
conwayCDDL =
  collectFrom
    [ HIRule $ huddleRule @"block" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"transaction" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"kes_signature" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"language" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"potential_languages" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"signkey_kes" (Proxy @ConwayEra)
    , HIRule $ huddleRule @"certificate" (Proxy @ConwayEra)
    ]

-- ===================================================================
-- LAYER 1: Primitives (no dependencies on other Conway rules)
-- ===================================================================

instance HuddleRule "min_int64" ConwayEra where
  huddleRule _ = minInt64Rule

instance HuddleRule "max_int64" ConwayEra where
  huddleRule _ = maxInt64Rule

instance HuddleRule "int64" ConwayEra where
  huddleRule = int64Rule @ConwayEra

instance HuddleRule "bounded_bytes" ConwayEra where
  huddleRule _ = boundedBytesRule

instance HuddleRule "distinct_bytes" ConwayEra where
  huddleRule _ = distinctBytesRule

instance HuddleRule "big_uint" ConwayEra where
  huddleRule = bigUintRule

instance HuddleRule "big_nint" ConwayEra where
  huddleRule = bigNintRule

instance HuddleRule "big_int" ConwayEra where
  huddleRule = bigIntRule

instance HuddleRule "network_id" ConwayEra where
  huddleRule _ = networkIdRule

instance HuddleRule "dns_name" ConwayEra where
  huddleRule _ = "dns_name" =:= VText `sized` (0 :: Word64, 128 :: Word64)

instance HuddleRule "url" ConwayEra where
  huddleRule _ = "url" =:= VText `sized` (0 :: Word64, 128 :: Word64)

instance HuddleRule "major_protocol_version" ConwayEra where
  huddleRule _ = "major_protocol_version" =:= (0 :: Integer) ... (12 :: Integer)

instance HuddleRule "genesis_hash" ConwayEra where
  huddleRule = genesisHashRule @ConwayEra

instance HuddleRule "genesis_delegate_hash" ConwayEra where
  huddleRule = genesisDelegateHashRule @ConwayEra

instance HuddleRule "transaction_id" ConwayEra where
  huddleRule = transactionIdRule @ConwayEra

instance HuddleRule "vkeywitness" ConwayEra where
  huddleRule = vkeywitnessRule @ConwayEra

instance HuddleRule "bootstrap_witness" ConwayEra where
  huddleRule = bootstrapWitnessRule @ConwayEra

instance HuddleRule "ex_units" ConwayEra where
  huddleRule _ = exUnitsRule

instance HuddleRule "positive_interval" ConwayEra where
  huddleRule = positiveIntervalRule

instance HuddleRule "vote" ConwayEra where
  huddleRule _ = "vote" =:= int 0 / int 1 / int 2

instance HuddleRule "asset_name" ConwayEra where
  huddleRule _ = "asset_name" =:= VBytes `sized` (0 :: Word64, 32 :: Word64)

instance HuddleRule "plutus_data" ConwayEra where
  huddleRule = plutusDataRule

-- ===================================================================
-- LAYER 2: Basic structures (depend on Layer 1)
-- ===================================================================

instance HuddleRule "drep_credential" ConwayEra where
  huddleRule p = "drep_credential" =:= huddleRule @"credential" p

instance HuddleRule "committee_cold_credential" ConwayEra where
  huddleRule p = "committee_cold_credential" =:= huddleRule @"credential" p

instance HuddleRule "committee_hot_credential" ConwayEra where
  huddleRule p = "committee_hot_credential" =:= huddleRule @"credential" p

instance HuddleRule "anchor" ConwayEra where
  huddleRule p =
    "anchor"
      =:= arr
        [ "anchor_url" ==> huddleRule @"url" p
        , "anchor_data_hash" ==> huddleRule @"hash32" p
        ]

instance HuddleRule "drep" ConwayEra where
  huddleRule p =
    "drep"
      =:= arr [0, a (huddleRule @"addr_keyhash" p)]
      / arr [1, a (huddleRule @"script_hash" p)]
      / arr [2]
      / arr [3]

instance HuddleRule "voter" ConwayEra where
  huddleRule p =
    "voter"
      =:= arr [0, a (huddleRule @"addr_keyhash" p)]
      / arr [1, a (huddleRule @"script_hash" p)]
      / arr [2, a (huddleRule @"addr_keyhash" p)]
      / arr [3, a (huddleRule @"script_hash" p)]
      / arr [4, a (huddleRule @"addr_keyhash" p)]

instance HuddleRule "gov_action_id" ConwayEra where
  huddleRule p =
    "gov_action_id"
      =:= arr
        [ "transaction_id" ==> huddleRule @"transaction_id" p
        , "gov_action_index" ==> (VUInt `sized` (2 :: Word64))
        ]

instance HuddleRule "operational_cert" ConwayEra where
  huddleRule p =
    "operational_cert"
      =:= arr
        [ "hot_vkey" ==> huddleRule @"kes_vkey" p
        , "sequence_number" ==> huddleRule @"sequence_number" p
        , "kes_period" ==> huddleRule @"kes_period" p
        , "sigma" ==> huddleRule @"signature" p
        ]

instance HuddleRule "protocol_version" ConwayEra where
  huddleRule p =
    "protocol_version"
      =:= arr [a (huddleRule @"major_protocol_version" p), a VUInt]

instance HuddleRule "plutus_v1_script" ConwayEra where
  huddleRule = plutusV1ScriptRule

instance HuddleRule "plutus_v2_script" ConwayEra where
  huddleRule p = "plutus_v2_script" =:= huddleRule @"distinct_bytes" p

instance HuddleRule "plutus_v3_script" ConwayEra where
  huddleRule p = "plutus_v3_script" =:= huddleRule @"distinct_bytes" p

instance HuddleRule "negative_int64" ConwayEra where
  huddleRule p =
    "negative_int64"
      =:= huddleRule @"min_int64" p ... (-1 :: Integer)

instance HuddleRule "positive_int64" ConwayEra where
  huddleRule p =
    "positive_int64"
      =:= (1 :: Integer) ... huddleRule @"max_int64" p

instance HuddleRule "nonzero_int64" ConwayEra where
  huddleRule p =
    "nonzero_int64"
      =:= huddleRule @"negative_int64" p
      / huddleRule @"positive_int64" p

instance HuddleRule "policy_id" ConwayEra where
  huddleRule p = "policy_id" =:= huddleRule @"script_hash" p

instance HuddleRule "policy_hash" ConwayEra where
  huddleRule p = "policy_hash" =:= huddleRule @"script_hash" p

-- ===================================================================
-- LAYER 3: Script components
-- ===================================================================

instance HuddleGroup "script_pubkey" ConwayEra where
  huddleGroup = scriptPubkeyGroup @ConwayEra

instance HuddleGroup "script_all" ConwayEra where
  huddleGroup = scriptAllGroup @ConwayEra

instance HuddleGroup "script_any" ConwayEra where
  huddleGroup = scriptAnyGroup @ConwayEra

instance HuddleGroup "script_n_of_k" ConwayEra where
  huddleGroup = scriptNOfKGroup @ConwayEra

instance HuddleGroup "script_invalid_before" ConwayEra where
  huddleGroup = scriptInvalidBeforeGroup @ConwayEra

instance HuddleGroup "script_invalid_hereafter" ConwayEra where
  huddleGroup = scriptInvalidHereafterGroup @ConwayEra

instance HuddleRule "native_script" ConwayEra where
  huddleRule = nativeScriptRule @ConwayEra

-- ===================================================================
-- LAYER 4: Pool infrastructure
-- ===================================================================

instance HuddleGroup "single_host_addr" ConwayEra where
  huddleGroup = singleHostAddrGroup @ConwayEra

instance HuddleGroup "single_host_name" ConwayEra where
  huddleGroup = singleHostNameGroup @ConwayEra

instance HuddleGroup "multi_host_name" ConwayEra where
  huddleGroup = multiHostNameGroup @ConwayEra

instance HuddleRule "relay" ConwayEra where
  huddleRule = relayRule @ConwayEra

instance HuddleRule "pool_metadata" ConwayEra where
  huddleRule = poolMetadataRule @ConwayEra

instance HuddleGroup "pool_params" ConwayEra where
  huddleGroup = poolParamsGroup @ConwayEra

-- ===================================================================
-- LAYER 5: Certificates
-- ===================================================================

instance HuddleGroup "account_registration_cert" ConwayEra where
  huddleGroup = accountRegistrationCertGroup @ConwayEra

instance HuddleGroup "account_unregistration_cert" ConwayEra where
  huddleGroup = accountUnregistrationCertGroup @ConwayEra

instance HuddleGroup "delegation_to_stake_pool_cert" ConwayEra where
  huddleGroup = delegationToStakePoolCertGroup @ConwayEra

instance HuddleGroup "pool_registration_cert" ConwayEra where
  huddleGroup = poolRegistrationCertGroup @ConwayEra

instance HuddleGroup "pool_retirement_cert" ConwayEra where
  huddleGroup = poolRetirementCertGroup @ConwayEra

instance HuddleGroup "account_registration_deposit_cert" ConwayEra where
  huddleGroup p =
    "account_registration_deposit_cert"
      =:~ grp [7, a (huddleRule @"stake_credential" p), a (huddleRule @"coin" p)]

instance HuddleGroup "account_unregistration_deposit_cert" ConwayEra where
  huddleGroup p =
    "account_unregistration_deposit_cert"
      =:~ grp [8, a (huddleRule @"stake_credential" p), a (huddleRule @"coin" p)]

instance HuddleGroup "delegation_to_drep_cert" ConwayEra where
  huddleGroup p =
    "delegation_to_drep_cert"
      =:~ grp [9, a (huddleRule @"stake_credential" p), a (huddleRule @"drep" p)]

instance HuddleGroup "delegation_to_stake_pool_and_drep_cert" ConwayEra where
  huddleGroup p =
    "delegation_to_stake_pool_and_drep_cert"
      =:~ grp
        [ 10
        , a (huddleRule @"stake_credential" p)
        , a (huddleRule @"pool_keyhash" p)
        , a (huddleRule @"drep" p)
        ]

instance HuddleGroup "account_registration_delegation_to_stake_pool_cert" ConwayEra where
  huddleGroup p =
    "account_registration_delegation_to_stake_pool_cert"
      =:~ grp
        [ 11
        , a (huddleRule @"stake_credential" p)
        , a (huddleRule @"pool_keyhash" p)
        , a (huddleRule @"coin" p)
        ]

instance HuddleGroup "account_registration_delegation_to_drep_cert" ConwayEra where
  huddleGroup p =
    "account_registration_delegation_to_drep_cert"
      =:~ grp
        [ 12
        , a (huddleRule @"stake_credential" p)
        , a (huddleRule @"drep" p)
        , a (huddleRule @"coin" p)
        ]

instance HuddleGroup "account_registration_delegation_to_stake_pool_and_drep_cert" ConwayEra where
  huddleGroup p =
    "account_registration_delegation_to_stake_pool_and_drep_cert"
      =:~ grp
        [ 13
        , a (huddleRule @"stake_credential" p)
        , a (huddleRule @"pool_keyhash" p)
        , a (huddleRule @"drep" p)
        , a (huddleRule @"coin" p)
        ]

instance HuddleGroup "committee_authorization_cert" ConwayEra where
  huddleGroup p =
    "committee_authorization_cert"
      =:~ grp
        [ 14
        , a (huddleRule @"committee_cold_credential" p)
        , a (huddleRule @"committee_hot_credential" p)
        ]

instance HuddleGroup "committee_resignation_cert" ConwayEra where
  huddleGroup p =
    "committee_resignation_cert"
      =:~ grp [15, a (huddleRule @"committee_cold_credential" p), a (huddleRule @"anchor" p / VNil)]

instance HuddleGroup "drep_registration_cert" ConwayEra where
  huddleGroup p =
    "drep_registration_cert"
      =:~ grp [16, a (huddleRule @"drep_credential" p), a (huddleRule @"coin" p), a (huddleRule @"anchor" p / VNil)]

instance HuddleGroup "drep_unregistration_cert" ConwayEra where
  huddleGroup p =
    "drep_unregistration_cert"
      =:~ grp [17, a (huddleRule @"drep_credential" p), a (huddleRule @"coin" p)]

instance HuddleGroup "drep_update_cert" ConwayEra where
  huddleGroup p =
    "drep_update_cert"
      =:~ grp [18, a (huddleRule @"drep_credential" p), a (huddleRule @"anchor" p / VNil)]

instance HuddleRule "certificate" ConwayEra where
  huddleRule p =
    "certificate"
      =:= arr [a $ huddleGroup @"account_registration_cert" p]
      / arr [a $ huddleGroup @"account_unregistration_cert" p]
      / arr [a $ huddleGroup @"delegation_to_stake_pool_cert" p]
      / arr [a $ huddleGroup @"pool_registration_cert" p]
      / arr [a $ huddleGroup @"pool_retirement_cert" p]
      / arr [a $ huddleGroup @"account_registration_deposit_cert" p]
      / arr [a $ huddleGroup @"account_unregistration_deposit_cert" p]
      / arr [a $ huddleGroup @"delegation_to_drep_cert" p]
      / arr [a $ huddleGroup @"delegation_to_stake_pool_and_drep_cert" p]
      / arr [a $ huddleGroup @"account_registration_delegation_to_stake_pool_cert" p]
      / arr [a $ huddleGroup @"account_registration_delegation_to_drep_cert" p]
      / arr [a $ huddleGroup @"account_registration_delegation_to_stake_pool_and_drep_cert" p]
      / arr [a $ huddleGroup @"committee_authorization_cert" p]
      / arr [a $ huddleGroup @"committee_resignation_cert" p]
      / arr [a $ huddleGroup @"drep_registration_cert" p]
      / arr [a $ huddleGroup @"drep_unregistration_cert" p]
      / arr [a $ huddleGroup @"drep_update_cert" p]

instance HuddleRule "certificates" ConwayEra where
  huddleRule p =
    "certificates"
      =:= maybeTaggedNonemptyOset (huddleRule @"certificate" p)

-- ===================================================================
-- LAYER 6: Governance actions and voting
-- ===================================================================

instance HuddleRule "voting_procedure" ConwayEra where
  huddleRule p =
    "voting_procedure"
      =:= arr [a (huddleRule @"vote" p), a (huddleRule @"anchor" p / VNil)]

instance HuddleRule "voting_procedures" ConwayEra where
  huddleRule p =
    "voting_procedures"
      =:= mp
        [ 1
            <+ asKey (huddleRule @"voter" p)
            ==> mp [1 <+ asKey (huddleRule @"gov_action_id" p) ==> huddleRule @"voting_procedure" p]
        ]

instance HuddleRule "constitution" ConwayEra where
  huddleRule p =
    "constitution"
      =:= arr
        [ a (huddleRule @"anchor" p)
        , a (huddleRule @"script_hash" p / VNil)
        ]

instance HuddleGroup "parameter_change_action" ConwayEra where
  huddleGroup p =
    "parameter_change_action"
      =:~ grp
        [ 0
        , a $ huddleRule @"gov_action_id" p / VNil
        , a $ huddleRule @"protocol_param_update" p
        , a $ huddleRule @"policy_hash" p / VNil
        ]

instance HuddleGroup "hard_fork_initiation_action" ConwayEra where
  huddleGroup p =
    "hard_fork_initiation_action"
      =:~ grp [1, a $ huddleRule @"gov_action_id" p / VNil, a $ huddleRule @"protocol_version" p]

instance HuddleGroup "treasury_withdrawals_action" ConwayEra where
  huddleGroup p =
    "treasury_withdrawals_action"
      =:~ grp
        [ 2
        , a $ huddleRule @"withdrawals" p
        , a $ huddleRule @"policy_hash" p / VNil
        ]

instance HuddleGroup "no_confidence" ConwayEra where
  huddleGroup p =
    "no_confidence"
      =:~ grp [3, a $ huddleRule @"gov_action_id" p / VNil]

instance HuddleGroup "update_committee" ConwayEra where
  huddleGroup p =
    "update_committee"
      =:~ grp
        [ 4
        , a $ huddleRule @"gov_action_id" p / VNil
        , a $ maybeTaggedSet (huddleRule @"committee_cold_credential" p)
        , a
            $ mp
              [ 0
                  <+ asKey (huddleRule @"committee_cold_credential" p)
                  ==> huddleRule @"epoch" p
              ]
        , a $ huddleRule @"unit_interval" p
        ]

instance HuddleGroup "new_constitution" ConwayEra where
  huddleGroup p =
    "new_constitution"
      =:~ grp
        [ 5
        , a $ huddleRule @"gov_action_id" p / VNil
        , a $ huddleRule @"constitution" p
        ]

instance HuddleGroup "info_action" ConwayEra where
  huddleGroup _ = "info_action" =:~ grp [6]

instance HuddleRule "gov_action" ConwayEra where
  huddleRule p =
    "gov_action"
      =:= arr [a (huddleGroup @"parameter_change_action" p)]
      / arr [a (huddleGroup @"hard_fork_initiation_action" p)]
      / arr [a (huddleGroup @"treasury_withdrawals_action" p)]
      / arr [a (huddleGroup @"no_confidence" p)]
      / arr [a (huddleGroup @"update_committee" p)]
      / arr [a (huddleGroup @"new_constitution" p)]
      / arr [a (huddleGroup @"info_action" p)]

instance HuddleRule "proposal_procedure" ConwayEra where
  huddleRule p =
    "proposal_procedure"
      =:= arr
        [ "deposit" ==> huddleRule @"coin" p
        , a (huddleRule @"reward_account" p)
        , a (huddleRule @"gov_action" p)
        , a (huddleRule @"anchor" p)
        ]

instance HuddleRule "proposal_procedures" ConwayEra where
  huddleRule p =
    "proposal_procedures"
      =:= maybeTaggedNonemptyOset (huddleRule @"proposal_procedure" p)

-- ===================================================================
-- LAYER 7: Transaction inputs/outputs and value
-- ===================================================================

instance HuddleRule "transaction_input" ConwayEra where
  huddleRule p =
    "transaction_input"
      =:= arr
        [ "transaction_id" ==> huddleRule @"transaction_id" p
        , "index" ==> (VUInt `sized` (2 :: Word64))
        ]

instance HuddleRule "required_signers" ConwayEra where
  huddleRule p =
    "required_signers"
      =:= maybeTaggedNonemptySet (huddleRule @"addr_keyhash" p)

conwayMultiasset :: IsType0 a => Proxy ConwayEra -> a -> GRuleCall
conwayMultiasset _ =
  binding $ \x ->
    "multiasset"
      =:= mp
        [ 0
            <+ asKey (VBytes `sized` (28 :: Word64))
            ==> mp [1 <+ asKey (VBytes `sized` (0 :: Word64, 32 :: Word64)) ==> x]
        ]

instance HuddleRule "value" ConwayEra where
  huddleRule p =
    "value"
      =:= huddleRule @"coin" p
      / sarr [a $ huddleRule @"coin" p, a $ conwayMultiasset p (huddleRule @"positive_coin" p)]

instance HuddleRule "mint" ConwayEra where
  huddleRule p =
    "mint"
      =:= mp
        [ 1
            <+ asKey (huddleRule @"policy_id" p)
            ==> mp [1 <+ asKey (huddleRule @"asset_name" p) ==> huddleRule @"nonzero_int64" p]
        ]

instance HuddleRule "withdrawals" ConwayEra where
  huddleRule p =
    "withdrawals"
      =:= mp
        [ 0
            <+ asKey (huddleRule @"reward_account" p)
            ==> huddleRule @"coin" p
        ]

instance HuddleRule "data" ConwayEra where
  huddleRule p =
    "data" =:= tag 24 (VBytes `cbor` huddleRule @"plutus_data" p)

instance HuddleRule "datum_option" ConwayEra where
  huddleRule p =
    "datum_option"
      =:= arr [0, a (huddleRule @"hash32" p)]
      / arr [1, a (huddleRule @"data" p)]

instance HuddleRule "shelley_transaction_output" ConwayEra where
  huddleRule p =
    "shelley_transaction_output"
      =:= arr
        [ a (huddleRule @"address" p)
        , "amount" ==> huddleRule @"value" p
        , opt $ a (huddleRule @"hash32" p) //- "datum_hash"
        ]

instance HuddleRule "transaction_output" ConwayEra where
  huddleRule p =
    comment
      [str|Both of the Alonzo and Babbage style TxOut formats are equally valid
          |and can be used interchangeably
          |]
      $ "transaction_output"
        =:= huddleRule @"shelley_transaction_output" p
        / babbageTransactionOutput p (huddleRule @"script" p)

-- ===================================================================
-- LAYER 8: Scripts, execution, and redeemers
-- ===================================================================

instance HuddleRule "script" ConwayEra where
  huddleRule p =
    comment
      [str|Conway supports four script types:
          |  0: Native scripts (timelock) - unchanged from Allegra
          |  1: Plutus V1 scripts
          |  2: Plutus V2 scripts
          |  3: Plutus V3 scripts
          |]
      $ "script"
        =:= arr [0, a (huddleRule @"native_script" p)]
        / arr [1, a (huddleRule @"plutus_v1_script" p)]
        / arr [2, a (huddleRule @"plutus_v2_script" p)]
        / arr [3, a (huddleRule @"plutus_v3_script" p)]

instance HuddleRule "language" ConwayEra where
  huddleRule _ =
    comment
      [str|0: Plutus v1
          |1: Plutus v2
          |2: Plutus v3
          |]
      $ "language" =:= int 0 / int 1 / int 2

instance HuddleRule "potential_languages" ConwayEra where
  huddleRule _ = "potential_languages" =:= (0 :: Integer) ... (255 :: Integer)

instance HuddleRule "cost_models" ConwayEra where
  huddleRule p =
    comment
      [str|The format for cost_models is flexible enough to allow adding
          |Plutus built-ins and language versions in the future.
          |
          |Plutus v1: only 166 integers are used, but more are accepted (and ignored)
          |Plutus v2: only 175 integers are used, but more are accepted (and ignored)
          |Plutus v3: only 223 integers are used, but more are accepted (and ignored)
          |
          |Any 8-bit unsigned number can be used as a key.
          |]
      $ "cost_models"
        =:= mp
          [ opt $ idx 0 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , opt $ idx 1 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , opt $ idx 2 ==> arr [0 <+ a (huddleRule @"int64" p)]
          , 0 <+ asKey ((3 :: Integer) ... (255 :: Integer)) ==> arr [0 <+ a (huddleRule @"int64" p)]
          ]

instance HuddleRule "redeemer_tag" ConwayEra where
  huddleRule _ =
    comment
      [str|0: spend
          |1: mint
          |2: cert
          |3: reward
          |4: voting
          |5: proposing
          |]
      $ "redeemer_tag"
        =:= int 0
        / int 1
        / int 2
        / int 3
        / int 4
        / int 5

instance HuddleRule "redeemer" ConwayEra where
  huddleRule p =
    "redeemer"
      =:= arr
        [ "tag" ==> huddleRule @"redeemer_tag" p
        , "index" ==> (VUInt `sized` (4 :: Word64))
        , "data" ==> huddleRule @"plutus_data" p
        , "ex_units" ==> huddleRule @"ex_units" p
        ]

instance HuddleRule "redeemers" ConwayEra where
  huddleRule p =
    comment
      [str|Flat Array support is included for backwards compatibility and
          |will be removed in the next era. It is recommended for tools to
          |adopt using a Map instead of Array going forward.
          |]
      $ "redeemers"
        =:= sarr [1 <+ a (huddleRule @"redeemer" p)]
        / smp
          [ 1
              <+ asKey
                ( arr
                    [ "tag" ==> huddleRule @"redeemer_tag" p
                    , "index" ==> (VUInt `sized` (4 :: Word64))
                    ]
                )
              ==> arr
                [ "data" ==> huddleRule @"plutus_data" p
                , "ex_units" ==> huddleRule @"ex_units" p
                ]
          ]

instance HuddleRule "script_data_hash" ConwayEra where
  huddleRule p =
    comment
      [str|This is a hash of data which may affect evaluation of a script.
          |This data consists of:
          |  - The redeemers from the transaction_witness_set (the value of field 5).
          |  - The datums from the transaction_witness_set (the value of field 4).
          |  - The value in the cost_models map corresponding to the script's language
          |    (in field 18 of protocol_param_update.)
          |(In the future it may contain additional protocol parameters.)
          |
          |Since this data does not exist in contiguous form inside a transaction, it needs
          |to be independently constructed by each recipient.
          |
          |The bytestring which is hashed is the concatenation of three things:
          |  redeemers || datums || language views
          |The redeemers are exactly the data present in the transaction witness set.
          |Similarly for the datums, if present. If no datums are provided, the middle
          |field is omitted (i.e. it is the empty/null bytestring).
          |
          |language views CDDL:
          |{ * language => script_integrity_data }
          |
          |This must be encoded canonically, using the same scheme as in
          |RFC7049 section 3.9:
          | - Maps, strings, and bytestrings must use a definite-length encoding
          | - Integers must be as small as possible.
          | - The expressions for map length, string length, and bytestring length
          |   must be as short as possible.
          | - The keys in the map must be sorted as follows:
          |    -  If two keys have different lengths, the shorter one sorts earlier.
          |    -  If two keys have the same length, the one with the lower value
          |       in (byte-wise) lexical order sorts earlier.
          |
          |For PlutusV1 (language id 0), the language view is the following:
          |  - the value of cost_models map at key 0 (in other words, the script_integrity_data)
          |    is encoded as an indefinite length list and the result is encoded as a bytestring.
          |    (our apologies)
          |    For example, the script_integrity_data corresponding to the all zero costmodel for V1
          |    would be encoded as (in hex):
          |    58a89f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ff
          |  - the language ID tag is also encoded twice. first as a uint then as
          |    a bytestring. (our apologies)
          |    Concretely, this means that the language version for V1 is encoded as
          |    4100 in hex.
          |For PlutusV2 (language id 1), the language view is the following:
          |  - the value of cost_models map at key 1 is encoded as an definite length list.
          |    For example, the script_integrity_data corresponding to the all zero costmodel for V2
          |    would be encoded as (in hex):
          |    98af0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
          |  - the language ID tag is encoded as expected.
          |    Concretely, this means that the language version for V2 is encoded as
          |    01 in hex.
          |For PlutusV3 (language id 2), the language view is the following:
          |  - the value of cost_models map at key 2 is encoded as a definite length list.
          |
          |Note that each Plutus language represented inside a transaction must have
          |a cost model in the cost_models protocol parameter in order to execute,
          |regardless of what the script integrity data is.
          |
          |Finally, note that in the case that a transaction includes datums but does not
          |include the redeemers field, the script data format becomes (in hex):
          |[ A0 | datums | A0 ]
          |corresponding to a CBOR empty map and an empty map for language view.
          |This empty redeeemer case has changed from the previous eras, since default
          |representation for redeemers has been changed to a map. Also whenever redeemers are
          |supplied either as a map or as an array they must contain at least one element,
          |therefore there is no way to override this behavior by providing a custom
          |representation for empty redeemers.
          |]
      $ scriptDataHashRule p

-- ===================================================================
-- LAYER 9: Transaction body and witness set
-- ===================================================================

instance HuddleRule "transaction_body" ConwayEra where
  huddleRule p =
    "transaction_body"
      =:= mp
        [ idx 0 ==> maybeTaggedSet (huddleRule @"transaction_input" p)
        , idx 1 ==> arr [0 <+ a (huddleRule @"transaction_output" p)]
        , idx 2 ==> huddleRule @"coin" p //- "fee"
        , opt (idx 3 ==> huddleRule @"slot" p) //- "time to live"
        , opt (idx 4 ==> huddleRule @"certificates" p)
        , opt (idx 5 ==> huddleRule @"withdrawals" p)
        , opt (idx 7 ==> huddleRule @"auxiliary_data_hash" p)
        , opt (idx 8 ==> huddleRule @"slot" p) //- "validity interval start"
        , opt (idx 9 ==> huddleRule @"mint" p)
        , opt (idx 11 ==> huddleRule @"script_data_hash" p)
        , opt (idx 13 ==> maybeTaggedNonemptySet (huddleRule @"transaction_input" p)) //- "collateral"
        , opt (idx 14 ==> huddleRule @"required_signers" p)
        , opt (idx 15 ==> huddleRule @"network_id" p)
        , opt (idx 16 ==> huddleRule @"transaction_output" p) //- "collateral return"
        , opt (idx 17 ==> huddleRule @"coin" p) //- "total collateral"
        , opt (idx 18 ==> maybeTaggedNonemptySet (huddleRule @"transaction_input" p)) //- "reference inputs"
        , opt (idx 19 ==> huddleRule @"voting_procedures" p)
        , opt (idx 20 ==> huddleRule @"proposal_procedures" p)
        , opt (idx 21 ==> huddleRule @"coin" p) //- "current treasury value"
        , opt (idx 22 ==> huddleRule @"positive_coin" p) //- "donation"
        ]

instance HuddleRule "transaction_witness_set" ConwayEra where
  huddleRule p =
    "transaction_witness_set"
      =:= mp
        [ opt $ idx 0 ==> maybeTaggedNonemptySet (huddleRule @"vkeywitness" p)
        , opt $ idx 1 ==> maybeTaggedNonemptySet (huddleRule @"native_script" p)
        , opt $ idx 2 ==> maybeTaggedNonemptySet (huddleRule @"bootstrap_witness" p)
        , opt $ idx 3 ==> maybeTaggedNonemptySet (huddleRule @"plutus_v1_script" p)
        , opt $ idx 4 ==> maybeTaggedNonemptySet (huddleRule @"plutus_data" p)
        , opt $ idx 5 ==> huddleRule @"redeemers" p
        , opt $ idx 6 ==> maybeTaggedNonemptySet (huddleRule @"plutus_v2_script" p)
        , opt $ idx 7 ==> maybeTaggedNonemptySet (huddleRule @"plutus_v3_script" p)
        ]

-- ===================================================================
-- LAYER 10: Transaction
-- ===================================================================

instance HuddleRule "transaction" ConwayEra where
  huddleRule p =
    "transaction"
      =:= arr
        [ a $ huddleRule @"transaction_body" p
        , a $ huddleRule @"transaction_witness_set" p
        , a VBool
        , a (huddleRule @"auxiliary_data" p / VNil)
        ]

-- ===================================================================
-- LAYER 11: Protocol parameters
-- ===================================================================

instance HuddleRule "ex_unit_prices" ConwayEra where
  huddleRule p =
    "ex_unit_prices"
      =:= arr
        [ "mem_price" ==> huddleRule @"nonnegative_interval" p
        , "step_price" ==> huddleRule @"nonnegative_interval" p
        ]

instance HuddleRule "pool_voting_thresholds" ConwayEra where
  huddleRule p =
    "pool_voting_thresholds"
      =:= arr
        [ a (huddleRule @"unit_interval" p) //- "motion no confidence"
        , a (huddleRule @"unit_interval" p) //- "committee normal"
        , a (huddleRule @"unit_interval" p) //- "committee no confidence"
        , a (huddleRule @"unit_interval" p) //- "hard fork initiation"
        , a (huddleRule @"unit_interval" p) //- "security relevant parameter voting threshold"
        ]

instance HuddleRule "drep_voting_thresholds" ConwayEra where
  huddleRule p =
    "drep_voting_thresholds"
      =:= arr
        [ a (huddleRule @"unit_interval" p) //- "motion no confidence"
        , a (huddleRule @"unit_interval" p) //- "committee normal"
        , a (huddleRule @"unit_interval" p) //- "committee no confidence"
        , a (huddleRule @"unit_interval" p) //- "update constitution"
        , a (huddleRule @"unit_interval" p) //- "hard fork initiation"
        , a (huddleRule @"unit_interval" p) //- "PP network group"
        , a (huddleRule @"unit_interval" p) //- "PP economic group"
        , a (huddleRule @"unit_interval" p) //- "PP technical group"
        , a (huddleRule @"unit_interval" p) //- "PP governance group"
        , a (huddleRule @"unit_interval" p) //- "treasury withdrawal"
        ]

instance HuddleRule "protocol_param_update" ConwayEra where
  huddleRule p =
    "protocol_param_update"
      =:= mp
        [ opt (idx 0 ==> VUInt) //- "minfeeA"
        , opt (idx 1 ==> VUInt) //- "minfeeB"
        , opt (idx 2 ==> (VUInt `sized` (4 :: Word64))) //- "max block body size"
        , opt (idx 3 ==> (VUInt `sized` (4 :: Word64))) //- "max transaction size"
        , opt (idx 4 ==> (VUInt `sized` (2 :: Word64))) //- "max block header size"
        , opt (idx 5 ==> huddleRule @"coin" p) //- "key deposit"
        , opt (idx 6 ==> huddleRule @"coin" p) //- "pool deposit"
        , opt (idx 7 ==> huddleRule @"epoch_interval" p) //- "maximum epoch"
        , opt (idx 8 ==> VUInt `sized` (2 :: Word64)) //- "n_opt: desired number of stake pools"
        , opt (idx 9 ==> huddleRule @"nonnegative_interval" p) //- "pool pledge influence"
        , opt (idx 10 ==> huddleRule @"unit_interval" p) //- "expansion rate"
        , opt (idx 11 ==> huddleRule @"unit_interval" p) //- "treasury growth rate"
        , opt (idx 16 ==> huddleRule @"coin" p) //- "min pool cost"
        , opt (idx 17 ==> huddleRule @"coin" p) //- "ada per utxo byte"
        , opt (idx 18 ==> huddleRule @"cost_models" p) //- "cost models for script languages"
        , opt (idx 19 ==> huddleRule @"ex_unit_prices" p) //- "execution costs"
        , opt (idx 20 ==> huddleRule @"ex_units" p) //- "max tx ex units"
        , opt (idx 21 ==> huddleRule @"ex_units" p) //- "max block ex units"
        , opt (idx 22 ==> (VUInt `sized` (4 :: Word64))) //- "max value size"
        , opt (idx 23 ==> (VUInt `sized` (2 :: Word64))) //- "collateral percentage"
        , opt (idx 24 ==> (VUInt `sized` (2 :: Word64))) //- "max collateral inputs"
        , opt (idx 25 ==> huddleRule @"pool_voting_thresholds" p) //- "pool voting thresholds"
        , opt (idx 26 ==> huddleRule @"drep_voting_thresholds" p) //- "drep voting thresholds"
        , opt (idx 27 ==> (VUInt `sized` (2 :: Word64))) //- "min committee size"
        , opt (idx 28 ==> huddleRule @"epoch_interval" p) //- "committee term limit"
        , opt (idx 29 ==> huddleRule @"epoch_interval" p) //- "goveranance action validity period"
        , opt (idx 30 ==> huddleRule @"coin" p) //- "governance action deposit"
        , opt (idx 31 ==> huddleRule @"coin" p) //- "drep deposit"
        , opt (idx 32 ==> huddleRule @"epoch_interval" p) //- "drep inactivity period"
        , opt (idx 33 ==> huddleRule @"nonnegative_interval" p) //- "minfee refscriptcoinsperbyte"
        ]

instance HuddleRule "proposed_protocol_parameter_updates" ConwayEra where
  huddleRule = proposedProtocolParameterUpdatesRule @ConwayEra

instance HuddleRule "update" ConwayEra where
  huddleRule = updateRule @ConwayEra

-- ===================================================================
-- LAYER 12: Headers and blocks
-- ===================================================================

instance HuddleRule "header_body" ConwayEra where
  huddleRule p =
    "header_body"
      =:= arr
        [ "block_number" ==> huddleRule @"block_number" p
        , "slot" ==> huddleRule @"slot" p
        , "prev_hash" ==> (huddleRule @"hash32" p / VNil)
        , "issuer_vkey" ==> huddleRule @"vkey" p
        , "vrf_vkey" ==> huddleRule @"vrf_vkey" p
        , "vrf_result" ==> huddleRule @"vrf_cert" p
        , "block_body_size" ==> (VUInt `sized` (4 :: Word64)) //- "merkle triple root"
        , "block_body_hash" ==> huddleRule @"hash32" p
        , a $ huddleRule @"operational_cert" p
        , a $ huddleRule @"protocol_version" p
        ]

instance HuddleRule "header" ConwayEra where
  huddleRule p =
    "header"
      =:= arr
        [ a $ huddleRule @"header_body" p
        , "body_signature" ==> huddleRule @"kes_signature" p
        ]

instance HuddleRule "block" ConwayEra where
  huddleRule p =
    comment
      [str|Valid blocks must also satisfy the following two constraints:
          |  1) the length of transaction_bodies and transaction_witness_sets must be
          |     the same
          |  2) every transaction_index must be strictly smaller than the length of
          |     transaction_bodies
          |]
      $ "block"
        =:= arr
          [ a $ huddleRule @"header" p
          , "transaction_bodies" ==> arr [0 <+ a (huddleRule @"transaction_body" p)]
          , "transaction_witness_sets" ==> arr [0 <+ a (huddleRule @"transaction_witness_set" p)]
          , "auxiliary_data_set"
              ==> mp
                [ 0
                    <+ asKey (huddleRule @"transaction_index" p)
                    ==> huddleRule @"auxiliary_data" p
                ]
          , "invalid_transactions" ==> arr [0 <+ a (huddleRule @"transaction_index" p)]
          ]

-- ===================================================================
-- LAYER 13: Auxiliary data
-- ===================================================================

instance HuddleRule "auxiliary_scripts" ConwayEra where
  huddleRule p =
    "auxiliary_scripts" =:= arr [0 <+ a (huddleRule @"native_script" p)]

instance HuddleRule "auxiliary_data_map" ConwayEra where
  huddleRule p =
    "auxiliary_data_map"
      =:= tag
        259
        ( mp
            [ opt (idx 0 ==> huddleRule @"metadata" p)
            , opt (idx 1 ==> arr [0 <+ a (huddleRule @"native_script" p)])
            , opt (idx 2 ==> arr [0 <+ a (huddleRule @"plutus_v1_script" p)])
            , opt (idx 3 ==> arr [0 <+ a (huddleRule @"plutus_v2_script" p)])
            , opt (idx 4 ==> arr [0 <+ a (huddleRule @"plutus_v3_script" p)])
            ]
        )

instance HuddleRule "auxiliary_data_array" ConwayEra where
  huddleRule = auxiliaryDataArrayRule @ConwayEra

instance HuddleRule "auxiliary_data" ConwayEra where
  huddleRule p =
    comment
      [str|auxiliary_data supports three serialization formats:
          |  1. metadata (raw) - Supported since Shelley
          |  2. auxiliary_data_array - Array format, introduced in Allegra
          |  3. auxiliary_data_map - Tagged map format, introduced in Alonzo
          |     Conway adds plutus_v3_script support at index 4
          |]
      $ "auxiliary_data"
        =:= huddleRule @"metadata" p
        / huddleRule @"auxiliary_data_array" p
        / huddleRule @"auxiliary_data_map" p

-- ===================================================================
-- Helper functions (at end of file)
-- ===================================================================

mkMaybeTaggedSet :: IsType0 a => T.Text -> Word64 -> a -> GRuleCall
mkMaybeTaggedSet label n = binding $ \x -> label =:= tag 258 (arr [n <+ a x]) / sarr [n <+ a x]

maybeTaggedSet :: IsType0 a => a -> GRuleCall
maybeTaggedSet = mkMaybeTaggedSet "set" 0

maybeTaggedNonemptySet :: IsType0 a => a -> GRuleCall
maybeTaggedNonemptySet = mkMaybeTaggedSet "nonempty_set" 1

maybeTaggedOset :: IsType0 a => a -> GRuleCall
maybeTaggedOset = mkMaybeTaggedSet "oset" 0

maybeTaggedNonemptyOset :: IsType0 a => a -> GRuleCall
maybeTaggedNonemptyOset = mkMaybeTaggedSet "nonempty_oset" 1
