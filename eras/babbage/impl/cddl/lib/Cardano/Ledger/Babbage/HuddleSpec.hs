{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.HuddleSpec (
  module Cardano.Ledger.Alonzo.HuddleSpec,
  babbageCDDL,
  babbageOperationalCertRule,
  babbageProtocolVersionRule,
  babbageTransactionOutput,
  babbageScript,
  alonzoTransactionOutputRule,
  dataRule,
  datumOptionRule,
) where

import Cardano.Ledger.Alonzo.HuddleSpec hiding (
  shelleyOperationalCertGroup,
  shelleyProtocolVersionGroup,
 )
import Cardano.Ledger.Babbage (BabbageEra)
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import Text.Heredoc
import Prelude hiding ((/))

babbageCDDL :: Huddle
babbageCDDL =
  collectFrom
    [ HIRule $ huddleRule @"block" (Proxy @BabbageEra)
    , HIRule $ huddleRule @"transaction" (Proxy @BabbageEra)
    , HIRule $ huddleRule @"kes_signature" (Proxy @BabbageEra)
    , HIRule $ huddleRule @"language" (Proxy @BabbageEra)
    , HIRule $ huddleRule @"signkey_kes" (Proxy @BabbageEra)
    ]

-- | Babbage changed protocol_version from Named Group to Rule to match actual block
-- serialization. See 'header_body' instance for full explanation.
-- Ref: PR #3762, Issue #3559
babbageProtocolVersionRule ::
  forall era. HuddleRule "major_protocol_version" era => Proxy era -> Rule
babbageProtocolVersionRule p =
  "protocol_version" =:= arr [a $ huddleRule @"major_protocol_version" p, a VUInt]

-- | Babbage changed operational_cert from Named Group to Rule to match actual block
-- serialization. See 'header_body' instance for full explanation.
-- Ref: PR #3762, Issue #3559
babbageOperationalCertRule :: forall era. Era era => Proxy era -> Rule
babbageOperationalCertRule p =
  "operational_cert"
    =:= arr
      [ "hot_vkey" ==> huddleRule @"kes_vkey" p
      , "sequence_number" ==> huddleRule @"sequence_number" p
      , "kes_period" ==> huddleRule @"kes_period" p
      , "sigma" ==> huddleRule @"signature" p
      ]

alonzoTransactionOutputRule ::
  forall era.
  (HuddleRule "address" era, HuddleRule "value" era, HuddleRule "hash32" era) =>
  Proxy era -> Rule
alonzoTransactionOutputRule p =
  "alonzo_transaction_output"
    =:= arr
      [ a (huddleRule @"address" p)
      , "amount" ==> huddleRule @"value" p
      , opt ("datum_hash" ==> huddleRule @"hash32" p)
      ]

dataRule :: forall era. HuddleRule "plutus_data" era => Proxy era -> Rule
dataRule p = "data" =:= tag 24 (VBytes `cbor` huddleRule @"plutus_data" p)

datumOptionRule ::
  forall era.
  (HuddleRule "hash32" era, HuddleRule "data" era) =>
  Proxy era -> Rule
datumOptionRule p =
  "datum_option"
    =:= arr [0, a (huddleRule @"hash32" p)]
    / arr [1, a (huddleRule @"data" p)]

instance HuddleGroup "account_registration_cert" BabbageEra where
  huddleGroup = accountRegistrationCertGroup @BabbageEra

instance HuddleGroup "account_unregistration_cert" BabbageEra where
  huddleGroup = accountUnregistrationCertGroup @BabbageEra

instance HuddleGroup "delegation_to_stake_pool_cert" BabbageEra where
  huddleGroup = delegationToStakePoolCertGroup @BabbageEra

instance HuddleGroup "pool_registration_cert" BabbageEra where
  huddleGroup = poolRegistrationCertGroup @BabbageEra

instance HuddleGroup "pool_retirement_cert" BabbageEra where
  huddleGroup = poolRetirementCertGroup @BabbageEra

instance HuddleGroup "genesis_delegation_cert" BabbageEra where
  huddleGroup = genesisDelegationCertGroup @BabbageEra

instance HuddleGroup "move_instantaneous_rewards_cert" BabbageEra where
  huddleGroup = moveInstantaneousRewardsCertGroup @BabbageEra

instance HuddleRule "certificate" BabbageEra where
  huddleRule = certificateRule @BabbageEra

instance HuddleRule "withdrawals" BabbageEra where
  huddleRule = shelleyWithdrawalsRule @BabbageEra

instance HuddleRule "genesis_hash" BabbageEra where
  huddleRule = genesisHashRule @BabbageEra

instance HuddleRule "genesis_delegate_hash" BabbageEra where
  huddleRule = genesisDelegateHashRule @BabbageEra

instance HuddleGroup "pool_params" BabbageEra where
  huddleGroup = poolParamsGroup @BabbageEra

instance HuddleRule "pool_metadata" BabbageEra where
  huddleRule = poolMetadataRule @BabbageEra

instance HuddleRule "dns_name" BabbageEra where
  huddleRule _ = dnsNameRule

instance HuddleRule "url" BabbageEra where
  huddleRule _ = urlRule

instance HuddleGroup "single_host_addr" BabbageEra where
  huddleGroup = singleHostAddrGroup @BabbageEra

instance HuddleGroup "single_host_name" BabbageEra where
  huddleGroup = singleHostNameGroup @BabbageEra

instance HuddleGroup "multi_host_name" BabbageEra where
  huddleGroup = multiHostNameGroup @BabbageEra

instance HuddleRule "relay" BabbageEra where
  huddleRule = relayRule @BabbageEra

instance HuddleRule "move_instantaneous_reward" BabbageEra where
  huddleRule = moveInstantaneousRewardRule @BabbageEra

instance HuddleRule "delta_coin" BabbageEra where
  huddleRule _ = deltaCoinRule

instance HuddleRule "transaction_id" BabbageEra where
  huddleRule = transactionIdRule @BabbageEra

instance HuddleRule "transaction_input" BabbageEra where
  huddleRule = transactionInputRule @BabbageEra

instance HuddleRule "vkeywitness" BabbageEra where
  huddleRule = vkeywitnessRule @BabbageEra

instance HuddleRule "bootstrap_witness" BabbageEra where
  huddleRule = bootstrapWitnessRule @BabbageEra

instance HuddleRule "policy_id" BabbageEra where
  huddleRule p = "policy_id" =:= huddleRule @"script_hash" p

instance HuddleRule "asset_name" BabbageEra where
  huddleRule _ = "asset_name" =:= VBytes `sized` (0 :: Word64, 32 :: Word64)

instance HuddleRule "value" BabbageEra where
  huddleRule = maryValueRule @BabbageEra

instance HuddleRule "mint" BabbageEra where
  huddleRule = maryMintRule @BabbageEra

instance HuddleRule "proposed_protocol_parameter_updates" BabbageEra where
  huddleRule = proposedProtocolParameterUpdatesRule @BabbageEra

instance HuddleRule "update" BabbageEra where
  huddleRule = updateRule @BabbageEra

instance HuddleRule "required_signers" BabbageEra where
  huddleRule = requiredSignersRule @BabbageEra

instance HuddleRule "network_id" BabbageEra where
  huddleRule _ = networkIdRule

instance HuddleRule "bounded_bytes" BabbageEra where
  huddleRule _ = boundedBytesRule

instance HuddleRule "big_uint" BabbageEra where
  huddleRule = bigUintRule

instance HuddleRule "big_nint" BabbageEra where
  huddleRule = bigNintRule

instance HuddleRule "big_int" BabbageEra where
  huddleRule = bigIntRule

instance HuddleRule "distinct_bytes" BabbageEra where
  huddleRule _ = distinctBytesRule

instance HuddleRule "redeemers" BabbageEra where
  huddleRule p = "redeemers" =:= arr [0 <+ a (huddleRule @"redeemer" p)]

instance HuddleRule "redeemer" BabbageEra where
  huddleRule p =
    comment
      [str|NEW
          |]
      $ alonzoRedeemer p

instance HuddleRule "redeemer_tag" BabbageEra where
  huddleRule _ = alonzoRedeemerTag

instance HuddleRule "ex_units" BabbageEra where
  huddleRule _ = exUnitsRule

instance HuddleRule "ex_unit_prices" BabbageEra where
  huddleRule = exUnitPricesRule @BabbageEra

instance HuddleRule "positive_interval" BabbageEra where
  huddleRule = positiveIntervalRule

instance HuddleRule "operational_cert" BabbageEra where
  huddleRule = babbageOperationalCertRule @BabbageEra

instance HuddleRule "protocol_version" BabbageEra where
  huddleRule = babbageProtocolVersionRule @BabbageEra

instance HuddleRule "major_protocol_version" BabbageEra where
  huddleRule = majorProtocolVersionRule @BabbageEra

instance HuddleRule "block" BabbageEra where
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

instance HuddleRule "header" BabbageEra where
  huddleRule p =
    "header"
      =:= arr
        [ a $ huddleRule @"header_body" p
        , "body_signature" ==> huddleRule @"kes_signature" p
        ]

-- IMPORTANT: Babbage changed operational_cert and protocol_version from Named Group
-- (grp) to Rule (arr) to match actual block serialization.
--
-- Semantic difference:
--   * Named Group (grp): Fields are inlined directly into parent array.
--     -> header_body becomes a 14-element flat array
--   * Rule (arr): Fields are nested as separate sub-arrays.
--     -> header_body becomes a 10-element array with nested structures
--
-- Pre-Babbage eras (Shelley through Alonzo) used Named Group, but actual Babbage+
-- blocks serialize with Rule (nested arrays). This change corrects the CDDL spec to
-- match the actual CBOR serialization.
--
-- See 'babbageProtocolVersionRule' and 'operational_cert' instance for details.
-- References: PR #3762, Issue #3559
instance HuddleRule "header_body" BabbageEra where
  huddleRule p =
    "header_body"
      =:= arr
        [ "block_number" ==> huddleRule @"block_number" p
        , "slot" ==> huddleRule @"slot" p
        , "prev_hash" ==> (huddleRule @"hash32" p / VNil)
        , "issuer_vkey" ==> huddleRule @"vkey" p
        , "vrf_vkey" ==> huddleRule @"vrf_vkey" p
        , "vrf_result" ==> huddleRule @"vrf_cert" p //- "replaces nonce_vrf and leader_vrf"
        , "block_body_size" ==> VUInt
        , "block_body_hash" ==> huddleRule @"hash32" p //- "merkle triple root"
        , a $ huddleRule @"operational_cert" p
        , a $ huddleRule @"protocol_version" p
        ]

instance HuddleRule "transaction" BabbageEra where
  huddleRule p =
    "transaction"
      =:= arr
        [ a $ huddleRule @"transaction_body" p
        , a $ huddleRule @"transaction_witness_set" p
        , a VBool
        , a (huddleRule @"auxiliary_data" p / VNil)
        ]

instance HuddleRule "transaction_body" BabbageEra where
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
        , opt (idx 11 ==> huddleRule @"script_data_hash" p)
        , opt (idx 13 ==> untaggedSet (huddleRule @"transaction_input" p)) //- "collateral"
        , opt (idx 14 ==> huddleRule @"required_signers" p)
        , opt (idx 15 ==> huddleRule @"network_id" p)
        , opt (idx 16 ==> huddleRule @"transaction_output" p) //- "collateral return"
        , opt (idx 17 ==> huddleRule @"coin" p) //- "total collateral"
        , opt (idx 18 ==> untaggedSet (huddleRule @"transaction_input" p)) //- "reference inputs"
        ]

instance HuddleRule "script_data_hash" BabbageEra where
  huddleRule p =
    comment
      [str|This is a hash of data which may affect evaluation of a script.
          |This data consists of:
          |  - The redeemers from the transaction_witness_set (the value of field 5).
          |  - The datums from the transaction_witness_set (the value of field 4).
          |  - The value in the costmdls map corresponding to the script's language
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
          |  - the value of costmdls map at key 0 (in other words, the script_integrity_data)
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
          |  - the value of costmdls map at key 1 is encoded as an definite length list.
          |    For example, the script_integrity_data corresponding to the all zero costmodel for V2
          |    would be encoded as (in hex):
          |    98af0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
          |  - the language ID tag is encoded as expected.
          |    Concretely, this means that the language version for V2 is encoded as
          |    01 in hex.
          |
          |Note that each Plutus language represented inside a transaction must have
          |a cost model in the costmdls protocol parameter in order to execute,
          |regardless of what the script integrity data is.
          |
          |Finally, note that in the case that a transaction includes datums but does not
          |include the redeemers field, the script data format becomes (in hex):
          |[ 80 | datums | A0 ]
          |corresponding to a CBOR empty list and an empty map.
          |Note that a transaction might include the redeemers field and set it to the
          |empty map, in which case the user supplied encoding of the empty map is used.
          |]
      $ scriptDataHashRule p

instance HuddleRule "transaction_output" BabbageEra where
  huddleRule p =
    comment
      [str|Both of the Alonzo and Babbage style TxOut formats are equally valid
          |and can be used interchangeably.
          |]
      $ "transaction_output"
        =:= huddleRule @"alonzo_transaction_output" p
        / babbageTransactionOutput p (huddleRule @"script" p)

instance HuddleRule "alonzo_transaction_output" BabbageEra where
  huddleRule = alonzoTransactionOutputRule @BabbageEra

babbageTransactionOutput ::
  forall era.
  (HuddleRule "address" era, HuddleRule "value" era, HuddleRule "datum_option" era) =>
  Proxy era -> Rule -> Rule
babbageTransactionOutput p script =
  "babbage_transaction_output"
    =:= mp
      [ idx 0 ==> huddleRule @"address" p
      , idx 1 ==> huddleRule @"value" p
      , opt $ idx 2 ==> huddleRule @"datum_option" p //- "new"
      , opt $ idx 3 ==> ("script_ref" =:= tag 24 (VBytes `cbor` script)) //- "new"
      ]

instance HuddleRule "datum_option" BabbageEra where
  huddleRule = datumOptionRule @BabbageEra

instance HuddleRule "data" BabbageEra where
  huddleRule = dataRule @BabbageEra

instance HuddleRule "transaction_witness_set" BabbageEra where
  huddleRule p =
    "transaction_witness_set"
      =:= mp
        [ opt $ idx 0 ==> arr [0 <+ a (huddleRule @"vkeywitness" p)]
        , opt $ idx 1 ==> arr [0 <+ a (huddleRule @"native_script" p)]
        , opt $ idx 2 ==> arr [0 <+ a (huddleRule @"bootstrap_witness" p)]
        , opt $ idx 3 ==> arr [0 <+ a (huddleRule @"plutus_v1_script" p)]
        , opt $ idx 4 ==> arr [0 <+ a (huddleRule @"plutus_data" p)]
        , opt $ idx 5 ==> huddleRule @"redeemers" p
        , opt $ idx 6 ==> arr [0 <+ a (huddleRule @"plutus_v2_script" p)]
        ]

instance HuddleRule "native_script" BabbageEra where
  huddleRule = nativeScriptRule @BabbageEra

instance HuddleGroup "script_pubkey" BabbageEra where
  huddleGroup = scriptPubkeyGroup @BabbageEra

instance HuddleGroup "script_all" BabbageEra where
  huddleGroup = scriptAllGroup @BabbageEra

instance HuddleGroup "script_any" BabbageEra where
  huddleGroup = scriptAnyGroup @BabbageEra

instance HuddleGroup "script_n_of_k" BabbageEra where
  huddleGroup = scriptNOfKGroup @BabbageEra

instance HuddleGroup "script_invalid_before" BabbageEra where
  huddleGroup = scriptInvalidBeforeGroup @BabbageEra

instance HuddleGroup "script_invalid_hereafter" BabbageEra where
  huddleGroup = scriptInvalidHereafterGroup @BabbageEra

instance (Era era, HuddleRule "distinct_bytes" era) => HuddleRule "plutus_v2_script" era where
  huddleRule p =
    comment
      [str|Babbage introduces Plutus V2 with improved cost model
          |and additional builtins.
          |]
      $ "plutus_v2_script" =:= huddleRule @"distinct_bytes" p

instance HuddleRule "script" BabbageEra where
  huddleRule = babbageScript

babbageScript ::
  forall era.
  ( HuddleRule "native_script" era
  , HuddleRule "plutus_v1_script" era
  , HuddleRule "plutus_v2_script" era
  ) =>
  Proxy era -> Rule
babbageScript p =
  comment
    [str|Babbage supports three script types:
        |  0: Native scripts (timelock)
        |  1: Plutus V1 scripts
        |  2: Plutus V2 scripts
        |]
    $ "script"
      =:= arr [0, a (huddleRule @"native_script" p)]
      / arr [1, a (huddleRule @"plutus_v1_script" p)]
      / arr [2, a (huddleRule @"plutus_v2_script" p)]

instance HuddleRule "language" BabbageEra where
  huddleRule _ =
    comment
      [str|0: Plutus v1
          |1: Plutus v2
          |]
      $ "language" =:= (0 :: Integer) ... (1 :: Integer)

instance HuddleRule "cost_models" BabbageEra where
  huddleRule p =
    "cost_models"
      =:= mp
        [ opt $ idx 0 ==> arr [166 <+ a (huddleRule @"int64" p) +> 166]
        , opt $ idx 1 ==> arr [175 <+ a (huddleRule @"int64" p) +> 175]
        ]

instance HuddleRule "protocol_param_update" BabbageEra where
  huddleRule p =
    "protocol_param_update"
      =:= mp
        [ opt (idx 0 ==> VUInt) //- "minfee A"
        , opt (idx 1 ==> VUInt) //- "minfee B"
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
        , opt (idx 14 ==> huddleRule @"protocol_version" p) //- "protocol version"
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

instance HuddleRule "auxiliary_data" BabbageEra where
  huddleRule p =
    comment
      [str|auxiliary_data supports three serialization formats:
          |  1. metadata (raw) - Supported since Shelley
          |  2. auxiliary_data_array - Array format, introduced in Allegra
          |  3. auxiliary_data_map - Tagged map format, introduced in Alonzo
          |     Babbage adds plutus_v2_script support at index 3
          |]
      $ "auxiliary_data"
        =:= huddleRule @"metadata" p
        / huddleRule @"auxiliary_data_array" p
        / huddleRule @"auxiliary_data_map" p

instance HuddleRule "auxiliary_data_array" BabbageEra where
  huddleRule = auxiliaryDataArrayRule @BabbageEra

instance HuddleRule "auxiliary_scripts" BabbageEra where
  huddleRule p =
    "auxiliary_scripts" =:= arr [0 <+ a (huddleRule @"native_script" p)]

instance HuddleRule "auxiliary_data_map" BabbageEra where
  huddleRule p =
    "auxiliary_data_map"
      =:= tag
        259
        ( mp
            [ opt (idx 0 ==> huddleRule @"metadata" p)
            , opt (idx 1 ==> arr [0 <+ a (huddleRule @"native_script" p)])
            , opt (idx 2 ==> arr [0 <+ a (huddleRule @"plutus_v1_script" p)])
            , opt (idx 3 ==> arr [0 <+ a (huddleRule @"plutus_v2_script" p)])
            ]
        )
