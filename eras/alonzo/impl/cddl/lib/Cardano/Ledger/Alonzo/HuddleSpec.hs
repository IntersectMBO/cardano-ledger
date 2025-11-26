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

module Cardano.Ledger.Alonzo.HuddleSpec (
  module Cardano.Ledger.Mary.HuddleSpec,
  alonzoCDDL,
  constr,
  exUnitsRule,
  networkIdRule,
  positiveIntervalRule,
  bigUintRule,
  bigNintRule,
  bigIntRule,
  scriptDataHashRule,
  boundedBytesRule,
  distinctBytesRule,
  plutusV1ScriptRule,
  plutusDataRule,
  alonzoTransactionOutputRule,
  alonzoRedeemer,
  alonzoRedeemerTag,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Mary.HuddleSpec
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import Text.Heredoc
import Prelude hiding ((/))

alonzoCDDL :: Huddle
alonzoCDDL =
  collectFrom
    [ HIRule $ huddleRule @"block" (Proxy @AlonzoEra)
    , HIRule $ huddleRule @"transaction" (Proxy @AlonzoEra)
    , HIRule $ huddleRule @"kes_signature" (Proxy @AlonzoEra)
    , HIRule $ huddleRule @"language" (Proxy @AlonzoEra)
    , HIRule $ huddleRule @"signkey_kes" (Proxy @AlonzoEra)
    ]

exUnitsRule :: Rule
exUnitsRule = "ex_units" =:= arr ["mem" ==> VUInt, "steps" ==> VUInt]

networkIdRule :: Rule
networkIdRule = "network_id" =:= int 0 / int 1

positiveIntervalRule :: forall era. Era era => Proxy era -> Rule
positiveIntervalRule p =
  "positive_interval"
    =:= tag 30 (arr [a (huddleRule @"positive_int" p), a (huddleRule @"positive_int" p)])

bigUintRule :: forall era. HuddleRule "bounded_bytes" era => Proxy era -> Rule
bigUintRule p = "big_uint" =:= tag 2 (huddleRule @"bounded_bytes" p)

bigNintRule :: forall era. HuddleRule "bounded_bytes" era => Proxy era -> Rule
bigNintRule p = "big_nint" =:= tag 3 (huddleRule @"bounded_bytes" p)

bigIntRule :: forall era. HuddleRule "bounded_bytes" era => Proxy era -> Rule
bigIntRule p = "big_int" =:= VInt / bigUintRule p / bigNintRule p

scriptDataHashRule :: forall era. Era era => Proxy era -> Rule
scriptDataHashRule p = "script_data_hash" =:= huddleRule @"hash32" p

boundedBytesRule :: Rule
boundedBytesRule =
  comment
    [str|The real bounded_bytes does not have this limit. it instead has
        |a different limit which cannot be expressed in CDDL.
        |
        |The limit is as follows:
        | - bytes with a definite-length encoding are limited to size 0..64
        | - for bytes with an indefinite-length CBOR encoding, each chunk is
        |   limited to size 0..64
        | ( reminder: in CBOR, the indefinite-length encoding of
        | bytestrings consists of a token #2.31 followed by a sequence
        | of definite-length encoded bytestrings and a stop code )
        |]
    $ "bounded_bytes" =:= VBytes `sized` (0 :: Word64, 64 :: Word64)

distinctBytesRule :: Rule
distinctBytesRule =
  comment
    [str|A type for distinct values.
        |The type parameter must support .size, for example: bytes or uint
        |]
    $ "distinct_bytes"
      =:= (VBytes `sized` (8 :: Word64))
      / (VBytes `sized` (16 :: Word64))
      / (VBytes `sized` (20 :: Word64))
      / (VBytes `sized` (24 :: Word64))
      / (VBytes `sized` (30 :: Word64))
      / (VBytes `sized` (32 :: Word64))

plutusV1ScriptRule :: forall era. HuddleRule "distinct_bytes" era => Proxy era -> Rule
plutusV1ScriptRule p =
  comment
    [str|Alonzo introduces Plutus smart contracts.
        |Plutus V1 scripts are opaque bytestrings.
        |]
    $ "plutus_v1_script" =:= huddleRule @"distinct_bytes" p

plutusDataRule ::
  forall era.
  (HuddleRule "plutus_data" era, HuddleRule "bounded_bytes" era, HuddleRule "big_int" era) =>
  Proxy era -> Rule
plutusDataRule p =
  "plutus_data"
    =:= constr (huddleRule @"plutus_data" p)
    / smp [0 <+ asKey (huddleRule @"plutus_data" p) ==> huddleRule @"plutus_data" p]
    / sarr [0 <+ a (huddleRule @"plutus_data" p)]
    / huddleRule @"big_int" p
    / huddleRule @"bounded_bytes" p

alonzoTransactionOutputRule ::
  forall era.
  HuddleRule "value" era =>
  Proxy era ->
  Rule
alonzoTransactionOutputRule p =
  "transaction_output"
    =:= arr
      [ a (huddleRule @"address" p)
      , "amount" ==> huddleRule @"value" p
      , opt ("datum_hash" ==> huddleRule @"hash32" p)
      ]

instance HuddleGroup "operational_cert" AlonzoEra where
  huddleGroup = shelleyOperationalCertGroup @AlonzoEra

instance HuddleRule "transaction_id" AlonzoEra where
  huddleRule = transactionIdRule @AlonzoEra

instance HuddleRule "transaction_input" AlonzoEra where
  huddleRule = transactionInputRule @AlonzoEra

instance HuddleRule "certificate" AlonzoEra where
  huddleRule = certificateRule @AlonzoEra

instance HuddleGroup "account_registration_cert" AlonzoEra where
  huddleGroup = accountRegistrationCertGroup @AlonzoEra

instance HuddleGroup "account_unregistration_cert" AlonzoEra where
  huddleGroup = accountUnregistrationCertGroup @AlonzoEra

instance HuddleGroup "delegation_to_stake_pool_cert" AlonzoEra where
  huddleGroup = delegationToStakePoolCertGroup @AlonzoEra

instance HuddleGroup "pool_registration_cert" AlonzoEra where
  huddleGroup = poolRegistrationCertGroup @AlonzoEra

instance HuddleGroup "pool_retirement_cert" AlonzoEra where
  huddleGroup = poolRetirementCertGroup @AlonzoEra

instance HuddleGroup "genesis_delegation_cert" AlonzoEra where
  huddleGroup = genesisDelegationCertGroup @AlonzoEra

instance HuddleGroup "move_instantaneous_rewards_cert" AlonzoEra where
  huddleGroup = moveInstantaneousRewardsCertGroup @AlonzoEra

instance HuddleRule "withdrawals" AlonzoEra where
  huddleRule = shelleyWithdrawalsRule @AlonzoEra

instance HuddleRule "genesis_hash" AlonzoEra where
  huddleRule = genesisHashRule @AlonzoEra

instance HuddleRule "genesis_delegate_hash" AlonzoEra where
  huddleRule = genesisDelegateHashRule @AlonzoEra

instance HuddleGroup "pool_params" AlonzoEra where
  huddleGroup = poolParamsGroup @AlonzoEra

instance HuddleRule "pool_metadata" AlonzoEra where
  huddleRule = poolMetadataRule @AlonzoEra

instance HuddleRule "dns_name" AlonzoEra where
  huddleRule _ = dnsNameRule

instance HuddleRule "url" AlonzoEra where
  huddleRule _ = urlRule

instance HuddleGroup "single_host_addr" AlonzoEra where
  huddleGroup = singleHostAddrGroup @AlonzoEra

instance HuddleGroup "single_host_name" AlonzoEra where
  huddleGroup = singleHostNameGroup @AlonzoEra

instance HuddleGroup "multi_host_name" AlonzoEra where
  huddleGroup = multiHostNameGroup @AlonzoEra

instance HuddleRule "relay" AlonzoEra where
  huddleRule = relayRule @AlonzoEra

instance HuddleRule "move_instantaneous_reward" AlonzoEra where
  huddleRule = moveInstantaneousRewardRule @AlonzoEra

instance HuddleRule "delta_coin" AlonzoEra where
  huddleRule _ = deltaCoinRule

instance HuddleRule "vkeywitness" AlonzoEra where
  huddleRule = vkeywitnessRule @AlonzoEra

instance HuddleRule "bootstrap_witness" AlonzoEra where
  huddleRule = bootstrapWitnessRule @AlonzoEra

instance HuddleRule "auxiliary_scripts" AlonzoEra where
  huddleRule = auxiliaryScriptsRule @AlonzoEra

instance HuddleRule "auxiliary_data_array" AlonzoEra where
  huddleRule = auxiliaryDataArrayRule @AlonzoEra

instance HuddleRule "int64" AlonzoEra where
  huddleRule = int64Rule @AlonzoEra

instance HuddleRule "min_int64" AlonzoEra where
  huddleRule _ = minInt64Rule

instance HuddleRule "max_int64" AlonzoEra where
  huddleRule _ = maxInt64Rule

instance HuddleGroup "script_pubkey" AlonzoEra where
  huddleGroup = scriptPubkeyGroup @AlonzoEra

instance HuddleGroup "script_all" AlonzoEra where
  huddleGroup = scriptAllGroup @AlonzoEra

instance HuddleGroup "script_any" AlonzoEra where
  huddleGroup = scriptAnyGroup @AlonzoEra

instance HuddleGroup "script_n_of_k" AlonzoEra where
  huddleGroup = scriptNOfKGroup @AlonzoEra

instance HuddleGroup "script_invalid_before" AlonzoEra where
  huddleGroup = scriptInvalidBeforeGroup @AlonzoEra

instance HuddleGroup "script_invalid_hereafter" AlonzoEra where
  huddleGroup = scriptInvalidHereafterGroup @AlonzoEra

instance HuddleRule "policy_id" AlonzoEra where
  huddleRule p = "policy_id" =:= huddleRule @"script_hash" p

instance HuddleRule "asset_name" AlonzoEra where
  huddleRule _ = "asset_name" =:= VBytes `sized` (0 :: Word64, 32 :: Word64)

instance HuddleRule "native_script" AlonzoEra where
  huddleRule = nativeScriptRule @AlonzoEra

instance HuddleRule "value" AlonzoEra where
  huddleRule = maryValueRule @AlonzoEra

instance HuddleRule "mint" AlonzoEra where
  huddleRule = maryMintRule @AlonzoEra

instance HuddleRule "block" AlonzoEra where
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
          , "invalid_transactions" ==> arr [0 <+ a (huddleRule @"transaction_index" p)] //- "new"
          ]

instance HuddleRule "header" AlonzoEra where
  huddleRule p =
    "header"
      =:= arr
        [ a $ huddleRule @"header_body" p
        , "body_signature" ==> huddleRule @"kes_signature" p
        ]

instance HuddleRule "header_body" AlonzoEra where
  huddleRule p =
    "header_body"
      =:= arr
        [ "block_number" ==> huddleRule @"block_number" p
        , "slot" ==> huddleRule @"slot" p
        , "prev_hash" ==> (huddleRule @"hash32" p / VNil)
        , "issuer_vkey" ==> huddleRule @"vkey" p
        , "vrf_vkey" ==> huddleRule @"vrf_vkey" p
        , "nonce_vrf" ==> huddleRule @"vrf_cert" p
        , "leader_vrf" ==> huddleRule @"vrf_cert" p
        , "block_body_size" ==> VUInt
        , "block_body_hash" ==> huddleRule @"hash32" p //- "merkle triple root"
        , a $ huddleGroup @"operational_cert" p
        , a $ huddleGroup @"protocol_version" p
        ]

instance HuddleGroup "protocol_version" AlonzoEra where
  huddleGroup = shelleyProtocolVersionGroup @AlonzoEra

instance HuddleRule "major_protocol_version" AlonzoEra where
  huddleRule = majorProtocolVersionRule @AlonzoEra

instance HuddleRule "transaction" AlonzoEra where
  huddleRule p =
    "transaction"
      =:= arr
        [ a $ huddleRule @"transaction_body" p
        , a $ huddleRule @"transaction_witness_set" p
        , a VBool
        , a (huddleRule @"auxiliary_data" p / VNil)
        ]

instance HuddleRule "transaction_body" AlonzoEra where
  huddleRule p =
    "transaction_body"
      =:= mp
        [ idx 0 ==> untaggedSet (huddleRule @"transaction_input" p)
        , idx 1 ==> arr [0 <+ a (huddleRule @"transaction_output" p)]
        , idx 2 ==> huddleRule @"coin" p //- "fee"
        , opt (idx 3 ==> huddleRule @"slot" p) //- "time to live"
        , opt (idx 4 ==> arr [0 <+ a (huddleRule @"certificate" p)])
        , opt (idx 5 ==> huddleRule @"withdrawals" p)
        , opt (idx 6 ==> huddleRule @"update" p)
        , opt (idx 7 ==> huddleRule @"auxiliary_data_hash" p)
        , opt (idx 8 ==> huddleRule @"slot" p) //- "validity interval start"
        , opt (idx 9 ==> huddleRule @"mint" p)
        , opt (idx 11 ==> huddleRule @"script_data_hash" p) //- "new"
        , opt (idx 13 ==> untaggedSet (huddleRule @"transaction_input" p)) //- "collateral"
        , opt (idx 14 ==> huddleRule @"required_signers" p) //- "new"
        , opt (idx 15 ==> huddleRule @"network_id" p) //- "new"
        ]

instance HuddleRule "transaction_output" AlonzoEra where
  huddleRule = alonzoTransactionOutputRule @AlonzoEra

instance HuddleRule "update" AlonzoEra where
  huddleRule p =
    "update"
      =:= arr
        [ a (huddleRule @"proposed_protocol_parameter_updates" p)
        , a (huddleRule @"epoch" p)
        ]

instance HuddleRule "proposed_protocol_parameter_updates" AlonzoEra where
  huddleRule p =
    "proposed_protocol_parameter_updates"
      =:= mp
        [ 0
            <+ asKey (huddleRule @"genesis_hash" p)
            ==> huddleRule @"protocol_param_update" p
        ]

instance HuddleRule "protocol_param_update" AlonzoEra where
  huddleRule p =
    "protocol_param_update"
      =:= mp
        [ opt (idx 0 ==> VUInt) //- "minfee A"
        , opt (idx 1 ==> VUInt) //- "minfee B"
        , opt (idx 2 ==> VUInt `sized` (4 :: Word64)) //- "max block body size"
        , opt (idx 3 ==> VUInt `sized` (4 :: Word64)) //- "max transaction size"
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
        , opt (idx 14 ==> arr [a (huddleGroup @"protocol_version" p)]) //- "protocol version"
        , opt (idx 16 ==> huddleRule @"coin" p) //- "min pool cost"
        , opt (idx 17 ==> huddleRule @"coin" p) //- "ada per utxo byte"
        , opt (idx 18 ==> huddleRule @"cost_models" p) //- "cost models for script languages"
        , opt (idx 19 ==> huddleRule @"ex_unit_prices" p) //- "execution costs"
        , opt (idx 20 ==> huddleRule @"ex_units" p) //- "max tx ex units"
        , opt (idx 21 ==> huddleRule @"ex_units" p) //- "max block ex units"
        , opt (idx 22 ==> VUInt) //- "max value size"
        , opt (idx 23 ==> VUInt) //- "collateral percentage"
        , opt (idx 24 ==> VUInt) //- "max collateral inputs"
        ]

instance HuddleRule "transaction_witness_set" AlonzoEra where
  huddleRule p =
    "transaction_witness_set"
      =:= mp
        [ opt $ idx 0 ==> arr [0 <+ a (huddleRule @"vkeywitness" p)]
        , opt $ idx 1 ==> arr [0 <+ a (huddleRule @"native_script" p)]
        , opt $ idx 2 ==> arr [0 <+ a (huddleRule @"bootstrap_witness" p)]
        , opt $ idx 3 ==> arr [0 <+ a (huddleRule @"plutus_v1_script" p)] //- "new"
        , opt $ idx 4 ==> arr [0 <+ a (huddleRule @"plutus_data" p)] //- "new"
        , opt $ idx 5 ==> huddleRule @"redeemers" p //- "new"
        ]

instance HuddleRule "auxiliary_data" AlonzoEra where
  huddleRule p =
    comment
      [str|auxiliary_data supports three serialization formats:
          |  1. metadata (raw) - Supported since Shelley
          |  2. auxiliary_data_array - Array format, introduced in Allegra
          |  3. auxiliary_data_map - Tagged map format, introduced in Alonzo
          |]
      $ "auxiliary_data"
        =:= huddleRule @"metadata" p
        / huddleRule @"auxiliary_data_array" p
        / huddleRule @"auxiliary_data_map" p

instance HuddleRule "auxiliary_data_map" AlonzoEra where
  huddleRule p =
    "auxiliary_data_map"
      =:= tag
        259
        ( mp
            [ opt (idx 0 ==> huddleRule @"metadata" p)
            , opt (idx 1 ==> arr [0 <+ a (huddleRule @"native_script" p)])
            , opt (idx 2 ==> arr [0 <+ a (huddleRule @"plutus_v1_script" p)])
            ]
        )

instance HuddleRule "script_data_hash" AlonzoEra where
  huddleRule p =
    comment
      [str|This is a hash of data which may affect evaluation of a script.
          |
          |This data consists of:
          |  - The redeemers from the transaction_witness_set (the value of field 5).
          |  - The datums from the transaction_witness_set (the value of field 4).
          |  - The value in the cost_models map corresponding to the script's language
          |    (in field 18 of protocol_param_update.)
          |(In the future it may contain additional protocol parameters.)
          |
          |Since this data does not exist in contiguous form inside a
          |transaction, it needs to be independently constructed by each
          |recipient.
          |
          |The bytestring which is hashed is the concatenation of three things:
          |  redeemers || datums || language views
          |
          |The redeemers are exactly the data present in the transaction
          |witness set. Similarly for the datums, if present. If no datums
          |are provided, the middle field is omitted (i.e. it is the
          |empty/null bytestring).
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
          |  - the value of cost_models map at key 0 is encoded as an indefinite length
          |    list and the result is encoded as a bytestring. (our apologies)
          |  - the language ID tag is also encoded twice. first as a uint then as
          |    a bytestring. (our apologies)
          |
          |Note that each Plutus language represented inside a transaction
          |must have a cost model in the cost_models protocol parameter in
          |order to execute, regardless of what the script integrity data
          |is. In the Alonzo era, this means cost_models must have a key 0
          |for Plutus V1.
          |
          |Finally, note that in the case that a transaction includes
          |datums but does not include any redeemers, the script data
          |format becomes (in hex):
          |  [ 80 | datums | A0 ]
          |
          |corresponding to a CBOR empty list and an empty map (our
          |apologies).
          |]
      $ scriptDataHashRule p

instance HuddleRule "required_signers" AlonzoEra where
  huddleRule p = "required_signers" =:= untaggedSet (huddleRule @"addr_keyhash" p)

instance HuddleRule "network_id" AlonzoEra where
  huddleRule _ = networkIdRule

instance HuddleRule "plutus_v1_script" AlonzoEra where
  huddleRule = plutusV1ScriptRule

instance HuddleRule "distinct_bytes" AlonzoEra where
  huddleRule _ = distinctBytesRule

instance HuddleRule "bounded_bytes" AlonzoEra where
  huddleRule _ = boundedBytesRule

instance HuddleRule "big_uint" AlonzoEra where
  huddleRule = bigUintRule

instance HuddleRule "big_nint" AlonzoEra where
  huddleRule = bigNintRule

instance HuddleRule "big_int" AlonzoEra where
  huddleRule = bigIntRule

instance HuddleRule "plutus_data" AlonzoEra where
  huddleRule = plutusDataRule

constr :: IsType0 a => a -> GRuleCall
constr =
  binding $ \x ->
    "constr"
      =:= tag 121 (arr [0 <+ a x])
      / tag 122 (arr [0 <+ a x])
      / tag 123 (arr [0 <+ a x])
      / tag 124 (arr [0 <+ a x])
      / tag 125 (arr [0 <+ a x])
      / tag 126 (arr [0 <+ a x])
      / tag 127 (arr [0 <+ a x])
      / tag 102 (arr [a VUInt, a $ arr [0 <+ a x]])

instance HuddleRule "redeemers" AlonzoEra where
  huddleRule p = "redeemers" =:= arr [0 <+ a (huddleRule @"redeemer" p)]

alonzoRedeemer ::
  forall era.
  ( HuddleRule "redeemer_tag" era
  , HuddleRule "plutus_data" era
  , HuddleRule "ex_units" era
  ) =>
  Proxy era ->
  Rule
alonzoRedeemer p =
  "redeemer"
    =:= arr
      [ "tag" ==> huddleRule @"redeemer_tag" p
      , "index" ==> VUInt
      , "data" ==> huddleRule @"plutus_data" p
      , "ex_units" ==> huddleRule @"ex_units" p
      ]

instance HuddleRule "redeemer" AlonzoEra where
  huddleRule = alonzoRedeemer @AlonzoEra

alonzoRedeemerTag :: Rule
alonzoRedeemerTag =
  comment
    [str|0: spend
        |1: mint
        |2: cert
        |3: reward
        |]
    $ "redeemer_tag" =:= (0 :: Integer) ... (3 :: Integer)

instance HuddleRule "redeemer_tag" AlonzoEra where
  huddleRule _ = alonzoRedeemerTag

instance HuddleRule "ex_units" AlonzoEra where
  huddleRule _ = exUnitsRule

instance HuddleRule "ex_unit_prices" AlonzoEra where
  huddleRule p =
    "ex_unit_prices"
      =:= arr
        [ "mem_price" ==> huddleRule @"positive_interval" p
        , "step_price" ==> huddleRule @"positive_interval" p
        ]

instance HuddleRule "positive_interval" AlonzoEra where
  huddleRule = positiveIntervalRule

instance HuddleRule "language" AlonzoEra where
  huddleRule _ =
    comment
      [str|NOTE: NEW
          |  This is an enumeration. for now there's only one value. Plutus V1
          |]
      $ "language" =:= int 0

instance HuddleRule "cost_models" AlonzoEra where
  huddleRule p =
    "cost_models"
      =:= mp [0 <+ asKey (huddleRule @"language" p) ==> huddleRule @"cost_model" p]

instance HuddleRule "cost_model" AlonzoEra where
  huddleRule p =
    comment
      [str|NOTE: NEW
          |  The keys to the cost model map are not present in the serialization.
          |  The values in the serialization are assumed to be ordered
          |  lexicographically by their correpsonding key value.
          |  See Plutus' `ParamName` for parameter ordering
          |]
      $ "cost_model" =:= arr [166 <+ a (huddleRule @"int64" p) +> 166]
