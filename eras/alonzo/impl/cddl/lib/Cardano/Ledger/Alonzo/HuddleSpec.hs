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

module Cardano.Ledger.Alonzo.HuddleSpec (
  module Cardano.Ledger.Mary.HuddleSpec,
  AlonzoEra,
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
  alonzoRedeemer,
  alonzoRedeemerTag,
  exUnitPricesRule,
  requiredSignersRule,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Mary.HuddleSpec
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import GHC.TypeLits (KnownSymbol)
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

exUnitsRule :: forall name. KnownSymbol name => Proxy name -> Rule
exUnitsRule pname = pname =.= arr ["mem" ==> VUInt, "steps" ==> VUInt]

networkIdRule :: forall name. KnownSymbol name => Proxy name -> Rule
networkIdRule pname = pname =.= int 0 / int 1

positiveIntervalRule ::
  forall name era. (KnownSymbol name, Era era) => Proxy name -> Proxy era -> Rule
positiveIntervalRule pname p =
  pname
    =.= tag 30 (arr [a (huddleRule @"positive_int" p), a (huddleRule @"positive_int" p)])

bigUintRule ::
  forall name era.
  (KnownSymbol name, HuddleRule "bounded_bytes" era) => Proxy name -> Proxy era -> Rule
bigUintRule pname p = pname =.= tag 2 (huddleRule @"bounded_bytes" p)

bigNintRule ::
  forall name era.
  (KnownSymbol name, HuddleRule "bounded_bytes" era) => Proxy name -> Proxy era -> Rule
bigNintRule pname p = pname =.= tag 3 (huddleRule @"bounded_bytes" p)

bigIntRule ::
  forall name era.
  ( KnownSymbol name
  , HuddleRule "big_uint" era
  , HuddleRule "big_nint" era
  ) =>
  Proxy name ->
  Proxy era ->
  Rule
bigIntRule pname p = pname =.= VInt / huddleRule @"big_uint" p / huddleRule @"big_nint" p

scriptDataHashRule ::
  forall name era. (KnownSymbol name, Era era) => Proxy name -> Proxy era -> Rule
scriptDataHashRule pname p = pname =.= huddleRule @"hash32" p

boundedBytesRule :: forall name. KnownSymbol name => Proxy name -> Rule
boundedBytesRule pname =
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
    $ pname =.= VBytes `sized` (0 :: Word64, 64 :: Word64)

distinctBytesRule :: forall name. KnownSymbol name => Proxy name -> Rule
distinctBytesRule pname =
  comment
    [str|A type for distinct values.
        |The type parameter must support .size, for example: bytes or uint
        |]
    $ pname
      =.= (VBytes `sized` (8 :: Word64))
      / (VBytes `sized` (16 :: Word64))
      / (VBytes `sized` (20 :: Word64))
      / (VBytes `sized` (24 :: Word64))
      / (VBytes `sized` (30 :: Word64))
      / (VBytes `sized` (32 :: Word64))

exUnitPricesRule ::
  forall name era.
  (KnownSymbol name, HuddleRule "positive_interval" era) => Proxy name -> Proxy era -> Rule
exUnitPricesRule pname p =
  pname
    =.= arr
      [ "mem_price" ==> huddleRule @"positive_interval" p
      , "step_price" ==> huddleRule @"positive_interval" p
      ]

requiredSignersRule ::
  forall name era.
  (KnownSymbol name, HuddleRule "addr_keyhash" era, HuddleRule1 "set" era) =>
  Proxy name -> Proxy era -> Rule
requiredSignersRule pname p = pname =.= huddleRule1 @"set" p (huddleRule @"addr_keyhash" p)

constr :: (KnownSymbol name, IsType0 a) => Proxy name -> a -> GRuleCall
constr pname =
  binding $ \x ->
    pname
      =.= tag 121 (arr [0 <+ a x])
      / tag 122 (arr [0 <+ a x])
      / tag 123 (arr [0 <+ a x])
      / tag 124 (arr [0 <+ a x])
      / tag 125 (arr [0 <+ a x])
      / tag 126 (arr [0 <+ a x])
      / tag 127 (arr [0 <+ a x])
      / tag 102 (arr [a VUInt, a $ arr [0 <+ a x]])

instance HuddleGroup "operational_cert" AlonzoEra where
  huddleGroupNamed = shelleyOperationalCertGroup

instance HuddleRule "transaction_id" AlonzoEra where
  huddleRuleNamed = transactionIdRule

instance HuddleRule "transaction_input" AlonzoEra where
  huddleRuleNamed = transactionInputRule

instance HuddleRule "certificate" AlonzoEra where
  huddleRuleNamed = certificateRule

instance HuddleGroup "account_registration_cert" AlonzoEra where
  huddleGroupNamed = accountRegistrationCertGroup

instance HuddleGroup "account_unregistration_cert" AlonzoEra where
  huddleGroupNamed = accountUnregistrationCertGroup

instance HuddleGroup "delegation_to_stake_pool_cert" AlonzoEra where
  huddleGroupNamed = delegationToStakePoolCertGroup

instance HuddleGroup "pool_registration_cert" AlonzoEra where
  huddleGroupNamed = poolRegistrationCertGroup

instance HuddleGroup "pool_retirement_cert" AlonzoEra where
  huddleGroupNamed = poolRetirementCertGroup

instance HuddleGroup "genesis_delegation_cert" AlonzoEra where
  huddleGroupNamed = genesisDelegationCertGroup

instance HuddleGroup "move_instantaneous_rewards_cert" AlonzoEra where
  huddleGroupNamed = moveInstantaneousRewardsCertGroup

instance HuddleRule "withdrawals" AlonzoEra where
  huddleRuleNamed = shelleyWithdrawalsRule

instance HuddleRule "genesis_hash" AlonzoEra where
  huddleRuleNamed = genesisHashRule

instance HuddleRule "genesis_delegate_hash" AlonzoEra where
  huddleRuleNamed = genesisDelegateHashRule

instance HuddleGroup "pool_params" AlonzoEra where
  huddleGroupNamed = poolParamsGroup

instance HuddleRule "pool_metadata" AlonzoEra where
  huddleRuleNamed = poolMetadataRule

instance HuddleRule "dns_name" AlonzoEra where
  huddleRuleNamed pname _ = dnsNameRule pname

instance HuddleRule "url" AlonzoEra where
  huddleRuleNamed pname _ = urlRule pname

instance HuddleGroup "single_host_addr" AlonzoEra where
  huddleGroupNamed = singleHostAddrGroup

instance HuddleGroup "single_host_name" AlonzoEra where
  huddleGroupNamed = singleHostNameGroup

instance HuddleGroup "multi_host_name" AlonzoEra where
  huddleGroupNamed = multiHostNameGroup

instance HuddleRule "relay" AlonzoEra where
  huddleRuleNamed = relayRule

instance HuddleRule "move_instantaneous_reward" AlonzoEra where
  huddleRuleNamed = moveInstantaneousRewardRule

instance HuddleRule "delta_coin" AlonzoEra where
  huddleRuleNamed pname _ = deltaCoinRule pname

instance HuddleRule "vkeywitness" AlonzoEra where
  huddleRuleNamed = vkeywitnessRule

instance HuddleRule "bootstrap_witness" AlonzoEra where
  huddleRuleNamed = bootstrapWitnessRule

instance HuddleRule "auxiliary_scripts" AlonzoEra where
  huddleRuleNamed = auxiliaryScriptsRule

instance HuddleRule "auxiliary_data_array" AlonzoEra where
  huddleRuleNamed = auxiliaryDataArrayRule

instance HuddleGroup "script_pubkey" AlonzoEra where
  huddleGroupNamed = scriptPubkeyGroup

instance HuddleGroup "script_all" AlonzoEra where
  huddleGroupNamed = scriptAllGroup

instance HuddleGroup "script_any" AlonzoEra where
  huddleGroupNamed = scriptAnyGroup

instance HuddleGroup "script_n_of_k" AlonzoEra where
  huddleGroupNamed = scriptNOfKGroup

instance HuddleGroup "script_invalid_before" AlonzoEra where
  huddleGroupNamed = scriptInvalidBeforeGroup

instance HuddleGroup "script_invalid_hereafter" AlonzoEra where
  huddleGroupNamed = scriptInvalidHereafterGroup

instance HuddleRule "policy_id" AlonzoEra where
  huddleRuleNamed pname p = pname =.= huddleRule @"script_hash" p

instance HuddleRule "asset_name" AlonzoEra where
  huddleRuleNamed pname _ = pname =.= VBytes `sized` (0 :: Word64, 32 :: Word64)

instance HuddleRule "native_script" AlonzoEra where
  huddleRuleNamed = nativeScriptRule

instance HuddleRule "value" AlonzoEra where
  huddleRuleNamed = maryValueRule

instance HuddleRule "mint" AlonzoEra where
  huddleRuleNamed = maryMintRule

instance HuddleRule "block" AlonzoEra where
  huddleRuleNamed pname p =
    comment
      [str|Valid blocks must also satisfy the following two constraints:
          |  1) the length of transaction_bodies and transaction_witness_sets must be
          |     the same
          |  2) every transaction_index must be strictly smaller than the length of
          |     transaction_bodies
          |]
      $ pname
        =.= arr
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
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ a $ huddleRule @"header_body" p
        , "body_signature" ==> huddleRule @"kes_signature" p
        ]

instance HuddleRule "header_body" AlonzoEra where
  huddleRuleNamed pname p =
    pname
      =.= arr
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
  huddleGroupNamed = shelleyProtocolVersionGroup

instance HuddleRule "major_protocol_version" AlonzoEra where
  huddleRuleNamed = majorProtocolVersionRule

instance HuddleRule "transaction" AlonzoEra where
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ a $ huddleRule @"transaction_body" p
        , a $ huddleRule @"transaction_witness_set" p
        , a VBool
        , a (huddleRule @"auxiliary_data" p / VNil)
        ]

instance HuddleRule "transaction_body" AlonzoEra where
  huddleRuleNamed pname p =
    pname
      =.= mp
        [ idx 0 ==> huddleRule1 @"set" p (huddleRule @"transaction_input" p)
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
        , opt (idx 13 ==> huddleRule1 @"set" p (huddleRule @"transaction_input" p)) //- "collateral"
        , opt (idx 14 ==> huddleRule @"required_signers" p) //- "new"
        , opt (idx 15 ==> huddleRule @"network_id" p) //- "new"
        ]

instance HuddleRule "transaction_output" AlonzoEra where
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ a (huddleRule @"address" p)
        , "amount" ==> huddleRule @"value" p
        , opt ("datum_hash" ==> huddleRule @"hash32" p)
        ]

instance HuddleRule "update" AlonzoEra where
  huddleRuleNamed pname p =
    pname
      =.= arr
        [ a (huddleRule @"proposed_protocol_parameter_updates" p)
        , a (huddleRule @"epoch" p)
        ]

instance HuddleRule "proposed_protocol_parameter_updates" AlonzoEra where
  huddleRuleNamed pname p =
    pname
      =.= mp
        [ 0
            <+ asKey (huddleRule @"genesis_hash" p)
            ==> huddleRule @"protocol_param_update" p
        ]

instance HuddleRule "protocol_param_update" AlonzoEra where
  huddleRuleNamed pname p =
    pname
      =.= mp
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
  huddleRuleNamed pname p =
    pname
      =.= mp
        [ opt $ idx 0 ==> arr [0 <+ a (huddleRule @"vkeywitness" p)]
        , opt $ idx 1 ==> arr [0 <+ a (huddleRule @"native_script" p)]
        , opt $ idx 2 ==> arr [0 <+ a (huddleRule @"bootstrap_witness" p)]
        , opt $ idx 3 ==> arr [0 <+ a (huddleRule @"plutus_v1_script" p)] //- "new"
        , opt $ idx 4 ==> arr [0 <+ a (huddleRule @"plutus_data" p)] //- "new"
        , opt $ idx 5 ==> huddleRule @"redeemers" p //- "new"
        ]

instance HuddleRule "auxiliary_data" AlonzoEra where
  huddleRuleNamed pname p =
    comment
      [str|auxiliary_data supports three serialization formats:
          |  1. metadata (raw) - Supported since Shelley
          |  2. auxiliary_data_array - Array format, introduced in Allegra
          |  3. auxiliary_data_map - Tagged map format, introduced in Alonzo
          |]
      $ pname
        =.= huddleRule @"metadata" p
        / huddleRule @"auxiliary_data_array" p
        / huddleRule @"auxiliary_data_map" p

instance HuddleRule "auxiliary_data_map" AlonzoEra where
  huddleRuleNamed pname p =
    pname
      =.= tag
        259
        ( mp
            [ opt (idx 0 ==> huddleRule @"metadata" p)
            , opt (idx 1 ==> arr [0 <+ a (huddleRule @"native_script" p)])
            , opt (idx 2 ==> arr [0 <+ a (huddleRule @"plutus_v1_script" p)])
            ]
        )

instance HuddleRule "script_data_hash" AlonzoEra where
  huddleRuleNamed pname p =
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
      $ scriptDataHashRule pname p

instance HuddleRule "required_signers" AlonzoEra where
  huddleRuleNamed = requiredSignersRule

instance HuddleRule "network_id" AlonzoEra where
  huddleRuleNamed pname _ = networkIdRule pname

instance (Era era, HuddleRule "distinct_bytes" era) => HuddleRule "plutus_v1_script" era where
  huddleRuleNamed pname p =
    comment
      [str|Alonzo introduces Plutus smart contracts.
          |Plutus V1 scripts are opaque bytestrings.
          |]
      $ pname =.= huddleRule @"distinct_bytes" p

instance HuddleRule "distinct_bytes" AlonzoEra where
  huddleRuleNamed pname _ = distinctBytesRule pname

instance HuddleRule "bounded_bytes" AlonzoEra where
  huddleRuleNamed pname _ = boundedBytesRule pname

instance HuddleRule "big_uint" AlonzoEra where
  huddleRuleNamed = bigUintRule

instance HuddleRule "big_nint" AlonzoEra where
  huddleRuleNamed = bigNintRule

instance HuddleRule "big_int" AlonzoEra where
  huddleRuleNamed = bigIntRule

instance
  (Era era, HuddleRule "big_int" era, HuddleRule "bounded_bytes" era, HuddleRule1 "constr" era) =>
  HuddleRule "plutus_data" era
  where
  huddleRuleNamed pname p =
    pname
      =.= huddleRule1 @"constr" p (huddleRule @"plutus_data" p)
      / smp [0 <+ asKey (huddleRule @"plutus_data" p) ==> huddleRule @"plutus_data" p]
      / sarr [0 <+ a (huddleRule @"plutus_data" p)]
      / huddleRule @"big_int" p
      / huddleRule @"bounded_bytes" p

instance Era era => HuddleRule1 "constr" era where
  huddleRule1Named pname _ = constr pname

instance HuddleRule "redeemers" AlonzoEra where
  huddleRuleNamed pname p = pname =.= arr [0 <+ a (huddleRule @"redeemer" p)]

alonzoRedeemer ::
  forall name era.
  ( KnownSymbol name
  , HuddleRule "redeemer_tag" era
  , HuddleRule "plutus_data" era
  , HuddleRule "ex_units" era
  ) =>
  Proxy name ->
  Proxy era ->
  Rule
alonzoRedeemer pname p =
  pname
    =.= arr
      [ "tag" ==> huddleRule @"redeemer_tag" p
      , "index" ==> VUInt
      , "data" ==> huddleRule @"plutus_data" p
      , "ex_units" ==> huddleRule @"ex_units" p
      ]

instance HuddleRule "redeemer" AlonzoEra where
  huddleRuleNamed = alonzoRedeemer

alonzoRedeemerTag :: forall name. KnownSymbol name => Proxy name -> Rule
alonzoRedeemerTag pname =
  comment
    [str|0: spend
        |1: mint
        |2: cert
        |3: reward
        |]
    $ pname =.= (0 :: Integer) ... (3 :: Integer)

instance HuddleRule "redeemer_tag" AlonzoEra where
  huddleRuleNamed pname _ = alonzoRedeemerTag pname

instance HuddleRule "ex_units" AlonzoEra where
  huddleRuleNamed pname _ = exUnitsRule pname

instance HuddleRule "ex_unit_prices" AlonzoEra where
  huddleRuleNamed = exUnitPricesRule

instance HuddleRule "positive_interval" AlonzoEra where
  huddleRuleNamed = positiveIntervalRule

instance HuddleRule "language" AlonzoEra where
  huddleRuleNamed pname _ =
    comment
      [str|NOTE: NEW
          |  This is an enumeration. for now there's only one value. Plutus V1
          |]
      $ pname =.= int 0

instance HuddleRule "cost_models" AlonzoEra where
  huddleRuleNamed pname p =
    pname
      =.= mp [0 <+ asKey (huddleRule @"language" p) ==> huddleRule @"cost_model" p]

instance HuddleRule "cost_model" AlonzoEra where
  huddleRuleNamed pname p =
    comment
      [str|NOTE: NEW
          |  The keys to the cost model map are not present in the serialization.
          |  The values in the serialization are assumed to be ordered
          |  lexicographically by their correpsonding key value.
          |  See Plutus' `ParamName` for parameter ordering
          |]
      $ pname =.= arr [166 <+ a (huddleRule @"int64" p) +> 166]

instance HuddleRule1 "set" AlonzoEra where
  huddleRule1Named pname _ = untaggedSet pname

instance HuddleRule1 "multiasset" AlonzoEra where
  huddleRule1Named = maryMultiasset
