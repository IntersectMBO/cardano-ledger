{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Evaluate" -}

module Test.Cardano.Ledger.Conway.CDDL (
  module Test.Cardano.Ledger.Babbage.CDDL,
  conwayCDDL,
  conway_dns_name,
  conway_url,
  conway_anchor,
  conway_pool_metadata,
  conway_pool_params,
  conway_ex_unit_prices,
  conway_value,
  conway_potential_languages,
  conway_pool_registration,
  conway_pool_retirement,
  conway_epoch,
  conway_reg_cert,
  conway_unreg_cert,
  conway_vote_deleg_cert,
  conway_stake_vote_deleg_cert,
  conway_stake_reg_deleg_cert,
  conway_vote_reg_deleg_cert,
  conway_stake_vote_reg_deleg_cert,
  conway_auth_committee_hot_cert,
  conway_resign_committee_cold_cert,
  conway_reg_drep_cert,
  conway_unreg_drep_cert,
  conway_update_drep_cert,
  conway_block_no,
  conway_slot_no,
  conway_invalid_before,
  conway_invalid_hereafter,
  conway_mint,
  conway_voting_procedures,
  conway_hard_fork_initiation_action,
  conway_treasury_withdrawals_action,
  conway_no_confidence,
  conway_update_committee,
  conway_new_constitution,
  conway_info_action,
  conway_gov_action_id,
  conway_policy_hash,
  conway_epoch_interval,
  conway_pool_voting_thresholds,
  conway_drep_voting_thresholds,
  conway_plutus_v1_script,
  conway_plutus_v2_script,
  conway_plutus_v3_script,
  conway_script_n_of_k,
  conway_auxiliary_data,
  conway_shelley_ma_auxiliary_data,
  conway_alonzo_auxiliary_data,
) where

import Cardano.Ledger.Conway (ConwayEra)
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Word (Word64)
import GHC.Num (Integer)
import Test.Cardano.Ledger.Babbage.CDDL
import Text.Heredoc

conwayCDDL :: Huddle
conwayCDDL =
  collectFromInit
    [ HIRule block
    , HIRule transaction
    , HIRule kes_signature
    , HIRule language
    , HIRule conway_potential_languages
    , HIRule signkey_kes
    , -- Certificates
      HIRule certificate
    , HIGroup shelley_stake_registration
    , HIGroup shelley_stake_deregistration
    , HIGroup shelley_stake_delegation
    , HIGroup conway_pool_registration
    , HIGroup conway_pool_retirement
    , HIGroup conway_reg_cert
    , HIGroup conway_unreg_cert
    , HIGroup conway_vote_deleg_cert
    , HIGroup conway_stake_vote_deleg_cert
    , HIGroup conway_stake_reg_deleg_cert
    , HIGroup conway_vote_reg_deleg_cert
    , HIGroup conway_stake_vote_reg_deleg_cert
    , HIGroup conway_auth_committee_hot_cert
    , HIGroup conway_resign_committee_cold_cert
    , HIGroup conway_reg_drep_cert
    , HIGroup conway_unreg_drep_cert
    , HIGroup conway_update_drep_cert
    ]

block :: Rule
block =
  comment
    [str|Valid blocks must also satisfy the following two constraints:
        |1. the length of transaction_bodies and transaction_witness_sets
        |   must be the same
        |2. every transaction_index must be strictly smaller than the
        |   length of transaction_bodies
        |]
    $ "block"
      =:= arr
        [ a header
        , "transaction_bodies" ==> arr [0 <+ a transaction_body]
        , "transaction_witness_sets"
            ==> arr [0 <+ a transaction_witness_set]
        , "auxiliary_data_set"
            ==> mp [0 <+ asKey transaction_ix ==> auxiliary_data]
        , "invalid_transactions" ==> arr [0 <+ a transaction_ix]
        ]

transaction :: Rule
transaction =
  "transaction"
    =:= arr
      [ a transaction_body
      , a transaction_witness_set
      , a VBool
      , a (auxiliary_data / VNil)
      ]

header :: Rule
header =
  "header"
    =:= arr
      [ a header_body
      , "body_signature" ==> kes_signature
      ]

header_body :: Rule
header_body =
  "header_body"
    =:= arr
      [ "block_number" ==> conway_block_no
      , "slot" ==> conway_slot_no
      , "prev_hash" ==> (bytes32 / VNil)
      , "issuer_vkey" ==> vkey
      , "vrf_vkey" ==> vrf_vkey
      , "vrf_result" ==> vrf_cert
      , "block_body_size" ==> (VUInt `sized` (4 :: Word64))
      , "block_body_hash" ==> bytes32
      , a babbage_operational_cert
      , a (protocol_version @ConwayEra)
      ]

transaction_body :: Rule
transaction_body =
  "transaction_body"
    =:= mp
      [ idx 0 ==> tagged_set transaction_input
      , idx 1 ==> arr [0 <+ a conway_transaction_output]
      , idx 2 ==> coin
      , opt (idx 3 ==> conway_slot_no)
      , opt (idx 4 ==> certificates)
      , opt (idx 5 ==> withdrawals)
      , opt (idx 7 ==> auxiliary_data_hash)
      , opt (idx 8 ==> conway_slot_no) -- Validity interval start
      , opt (idx 9 ==> conway_mint)
      , opt (idx 11 ==> script_data_hash)
      , opt (idx 13 ==> tagged_nonempty_set transaction_input)
      , opt (idx 14 ==> required_signers)
      , opt (idx 15 ==> alonzo_network_id)
      , opt (idx 16 ==> conway_transaction_output)
      , opt (idx 17 ==> coin)
      , opt (idx 18 ==> tagged_nonempty_set transaction_input)
      , opt (idx 19 ==> conway_voting_procedures)
      , opt (idx 20 ==> proposal_procedures)
      , opt (idx 21 ==> coin)
      , opt (idx 22 ==> positive_coin)
      ]

conway_voting_procedures :: Rule
conway_voting_procedures =
  "voting_procedures"
    =:= mp [1 <+ asKey voter ==> mp [1 <+ asKey conway_gov_action_id ==> voting_procedure]]

voting_procedure :: Rule
voting_procedure = "voting_procedure" =:= arr [a vote, a (conway_anchor / VNil)]

proposal_procedure :: Rule
proposal_procedure =
  "proposal_procedure"
    =:= arr
      [ "deposit" ==> coin
      , a reward_account
      , a gov_action
      , a conway_anchor
      ]

proposal_procedures :: Rule
proposal_procedures = "proposal_procedures" =:= tagged_nonempty_oset proposal_procedure

certificates :: Rule
certificates = "certificates" =:= tagged_nonempty_oset certificate

gov_action :: Rule
gov_action =
  "gov_action"
    =:= arr [a parameter_change_action]
    / arr [a conway_hard_fork_initiation_action]
    / arr [a conway_treasury_withdrawals_action]
    / arr [a conway_no_confidence]
    / arr [a conway_update_committee]
    / arr [a conway_new_constitution]
    / arr [a conway_info_action]

conway_policy_hash :: Rule
conway_policy_hash = "policy_hash" =:= script_hash

parameter_change_action :: Named Group
parameter_change_action =
  "parameter_change_action"
    =:~ grp
      [ 0
      , a $ conway_gov_action_id / VNil
      , a protocol_param_update
      , a $ conway_policy_hash / VNil
      ]

conway_hard_fork_initiation_action :: Named Group
conway_hard_fork_initiation_action =
  "hard_fork_initiation_action"
    =:~ grp [1, a $ conway_gov_action_id / VNil, a (protocol_version @ConwayEra)]

conway_treasury_withdrawals_action :: Named Group
conway_treasury_withdrawals_action =
  "treasury_withdrawals_action"
    =:~ grp [2, a (mp [0 <+ asKey reward_account ==> coin]), a $ conway_policy_hash / VNil]

conway_no_confidence :: Named Group
conway_no_confidence = "no_confidence" =:~ grp [3, a $ conway_gov_action_id / VNil]

conway_update_committee :: Named Group
conway_update_committee =
  "update_committee"
    =:~ grp
      [ 4
      , a $ conway_gov_action_id / VNil
      , a (tagged_set conway_committee_cold_credential)
      , a (mp [0 <+ asKey conway_committee_cold_credential ==> conway_epoch])
      , a unit_interval
      ]

conway_new_constitution :: Named Group
conway_new_constitution =
  "new_constitution"
    =:~ grp [5, a $ conway_gov_action_id / VNil, a constitution]

constitution :: Rule
constitution =
  "constitution"
    =:= arr
      [ a conway_anchor
      , a (script_hash / VNil)
      ]

conway_info_action :: Rule
conway_info_action = "info_action" =:= int 6

voter :: Rule
voter =
  "voter"
    =:= (arr [0, a addr_keyhash] //- "constitutional committee hot key hash")
    / (arr [1, a script_hash] //- "consitutional committee script hash")
    / (arr [2, a addr_keyhash] //- "drep keyhash")
    / (arr [3, a script_hash] //- "drep script hash")
    / (arr [4, a addr_keyhash] //- "staking pool key hash")

conway_anchor :: Rule
conway_anchor =
  "anchor"
    =:= arr
      [ "anchor_url" ==> conway_url
      , "anchor_data_hash" ==> bytes32
      ]

vote :: Rule
vote = "vote" =:= (0 :: Integer) ... (2 :: Integer)

conway_gov_action_id :: Rule
conway_gov_action_id =
  "gov_action_id"
    =:= arr
      [ "transaction_id" ==> transaction_id
      , "gov_action_index" ==> (VUInt `sized` (2 :: Word64))
      ]

required_signers :: Rule
required_signers = "required_signers" =:= tagged_nonempty_set addr_keyhash

conway_transaction_output :: Rule
conway_transaction_output =
  comment
    [str|Both of the Alonzo and Babbage style TxOut formats are equally valid
        |and can be used interchangeably
        |]
    $ "conway_transaction_output"
      =:= mkAlonzoTransactionOutput conway_value
      / mkBabbageTransactionOutput conway_value conway_script

script_data_hash :: Rule
script_data_hash =
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
    $ "script_data_hash" =:= bytes32

certificate :: Rule
certificate =
  "certificate"
    =:= arr [a shelley_stake_registration]
    / arr [a shelley_stake_deregistration]
    / arr [a shelley_stake_delegation]
    / arr [a conway_pool_registration]
    / arr [a conway_pool_retirement]
    / arr [a conway_reg_cert]
    / arr [a conway_unreg_cert]
    / arr [a conway_vote_deleg_cert]
    / arr [a conway_stake_vote_deleg_cert]
    / arr [a conway_stake_reg_deleg_cert]
    / arr [a conway_vote_reg_deleg_cert]
    / arr [a conway_stake_vote_reg_deleg_cert]
    / arr [a conway_auth_committee_hot_cert]
    / arr [a conway_resign_committee_cold_cert]
    / arr [a conway_reg_drep_cert]
    / arr [a conway_unreg_drep_cert]
    / arr [a conway_update_drep_cert]

-- POOL
conway_pool_registration :: Named Group
conway_pool_registration = "pool_registration" =:~ grp [3, a conway_pool_params]

conway_pool_retirement :: Named Group
conway_pool_retirement = "pool_retirement" =:~ grp [4, a pool_keyhash, a conway_epoch]

-- numbers 5 and 6 used to be the Genesis and MIR certificates respectively,
-- which were deprecated in Conway

-- DELEG
conway_reg_cert :: Named Group
conway_reg_cert = "reg_cert" =:~ grp [7, a shelley_stake_credential, a coin]

conway_unreg_cert :: Named Group
conway_unreg_cert = "unreg_cert" =:~ grp [8, a shelley_stake_credential, a coin]

conway_vote_deleg_cert :: Named Group
conway_vote_deleg_cert = "vote_deleg_cert" =:~ grp [9, a shelley_stake_credential, a conway_drep]

conway_stake_vote_deleg_cert :: Named Group
conway_stake_vote_deleg_cert =
  "stake_vote_deleg_cert"
    =:~ grp [10, a shelley_stake_credential, a pool_keyhash, a conway_drep]

conway_stake_reg_deleg_cert :: Named Group
conway_stake_reg_deleg_cert =
  "stake_reg_deleg_cert"
    =:~ grp [11, a shelley_stake_credential, a pool_keyhash, a coin]

conway_vote_reg_deleg_cert :: Named Group
conway_vote_reg_deleg_cert =
  "vote_reg_deleg_cert"
    =:~ grp [12, a shelley_stake_credential, a conway_drep, a coin]

conway_stake_vote_reg_deleg_cert :: Named Group
conway_stake_vote_reg_deleg_cert =
  "stake_vote_reg_deleg_cert"
    =:~ grp [13, a shelley_stake_credential, a pool_keyhash, a conway_drep, a coin]

-- GOVCERT
conway_auth_committee_hot_cert :: Named Group
conway_auth_committee_hot_cert =
  "auth_committee_hot_cert"
    =:~ grp [14, a conway_committee_cold_credential, a conway_committee_hot_credential]

conway_resign_committee_cold_cert :: Named Group
conway_resign_committee_cold_cert =
  "resign_committee_cold_cert"
    =:~ grp [15, a conway_committee_cold_credential, a $ conway_anchor / VNil]

conway_reg_drep_cert :: Named Group
conway_reg_drep_cert = "reg_drep_cert" =:~ grp [16, a conway_drep_credential, a coin, a $ conway_anchor / VNil]

conway_unreg_drep_cert :: Named Group
conway_unreg_drep_cert = "unreg_drep_cert" =:~ grp [17, a conway_drep_credential, a coin]

conway_update_drep_cert :: Named Group
conway_update_drep_cert = "update_drep_cert" =:~ grp [18, a conway_drep_credential, a $ conway_anchor / VNil]

conway_drep :: Rule
conway_drep =
  "drep"
    =:= arr [0, a addr_keyhash]
    / arr [1, a script_hash]
    / arr [2] -- always abstain
    / arr [3] -- always no confidence

conway_drep_credential :: Rule
conway_drep_credential = "drep_credential" =:= credential

conway_committee_cold_credential :: Rule
conway_committee_cold_credential = "committee_cold_credential" =:= credential

conway_committee_hot_credential :: Rule
conway_committee_hot_credential = "committee_hot_credential" =:= credential

conway_pool_params :: Named Group
conway_pool_params =
  comment
    [str|        pool_keyhash: operator
        |                coin: pledge
        |                coin: cost
        |       unit_interval: margin
        |   set<addr_keyhash>: pool_owners
        |]
    $ "pool_params"
      =:~ grp
        [ "operator" ==> pool_keyhash
        , "vrf_keyhash" ==> vrf_keyhash
        , "pledge" ==> coin
        , "cost" ==> coin
        , "margin" ==> unit_interval
        , "reward_account" ==> reward_account
        , "pool_owners" ==> tagged_set addr_keyhash
        , "relays" ==> arr [0 <+ a relay]
        , "pool_metadata" ==> (conway_pool_metadata / VNil)
        ]

conway_dns_name :: Rule
conway_dns_name = "dns_name" =:= VText `sized` (0 :: Word64, 128 :: Word64)

single_host_name :: Named Group
single_host_name =
  comment
    [str|dns_name: An A or AAAA DNS record
        |]
    $ "single_host_name" =:~ grp [1, a $ port / VNil, a conway_dns_name]

multi_host_name :: Named Group
multi_host_name =
  comment
    [str|dns_name: An SRV DNS record
        |]
    $ "multi_host_name"
      =:~ grp [2, a conway_dns_name]

relay :: Rule
relay =
  "relay"
    =:= arr [a single_host_addr]
    / arr [a single_host_name]
    / arr [a multi_host_name]

conway_url :: Rule
conway_url = "url" =:= VText `sized` (0 :: Word64, 128 :: Word64)

conway_pool_metadata :: Rule
conway_pool_metadata = "pool_metadata" =:= arr [a conway_url, a VBytes]

withdrawals :: Rule
withdrawals = "withdrawals" =:= mp [1 <+ asKey reward_account ==> coin]

protocol_param_update :: Rule
protocol_param_update =
  "protocol_param_update"
    =:= mp
      [ opt (idx 0 ==> coin) //- "minfeeA"
      , opt (idx 1 ==> coin) //- "minfeeB"
      , opt (idx 2 ==> (VUInt `sized` (4 :: Word64))) //- "max block body size"
      , opt (idx 3 ==> (VUInt `sized` (4 :: Word64))) //- "max transaction size"
      , opt (idx 4 ==> (VUInt `sized` (2 :: Word64))) //- "max block header size"
      , opt (idx 5 ==> coin) //- "key deposit"
      , opt (idx 6 ==> coin) //- "pool deposit"
      , opt (idx 7 ==> conway_epoch_interval) //- "maximum epoch"
      , opt (idx 8 ==> (VUInt `sized` (2 :: Word64))) //- "n_opt: desired number of stake pools"
      , opt (idx 9 ==> nonnegative_interval) //- "pool pledge influence"
      , opt (idx 10 ==> unit_interval) //- "expansion rate"
      , opt (idx 11 ==> unit_interval) //- "treasury growth rate"
      , opt (idx 16 ==> coin) //- "min pool cost"
      , opt (idx 17 ==> coin) //- "ada per utxo byte"
      , opt (idx 18 ==> cost_models) //- "cost models for script languages"
      , opt (idx 19 ==> conway_ex_unit_prices) //- "execution costs"
      , opt (idx 20 ==> alonzo_ex_units) //- "max tx ex units"
      , opt (idx 21 ==> alonzo_ex_units) //- "max block ex units"
      , opt (idx 22 ==> (VUInt `sized` (4 :: Word64))) //- "max value size"
      , opt (idx 23 ==> (VUInt `sized` (2 :: Word64))) //- "collateral percentage"
      , opt (idx 24 ==> (VUInt `sized` (2 :: Word64))) //- "max collateral inputs"
      , opt (idx 25 ==> conway_pool_voting_thresholds) //- "pool voting thresholds"
      , opt (idx 26 ==> conway_drep_voting_thresholds) //- "drep voting thresholds"
      , opt (idx 27 ==> (VUInt `sized` (2 :: Word64))) //- "min committee size"
      , opt (idx 28 ==> conway_epoch_interval) //- "committee term limit"
      , opt (idx 29 ==> conway_epoch_interval) //- "goveranance action validity period"
      , opt (idx 30 ==> coin) //- "governance action deposit"
      , opt (idx 31 ==> coin) //- "drep deposit"
      , opt (idx 32 ==> conway_epoch_interval) //- "drep inactivity period"
      , opt (idx 33 ==> nonnegative_interval) //- "minfee refscriptcoinsperbyte"
      ]

conway_pool_voting_thresholds :: Rule
conway_pool_voting_thresholds =
  "pool_voting_thresholds"
    =:= arr
      [ a unit_interval //- "motion no confidence"
      , a unit_interval //- "committee normal"
      , a unit_interval //- "committee no confidence"
      , a unit_interval //- "hard fork initiation"
      , a unit_interval //- "security relevant parameter voting threshold"
      ]

conway_drep_voting_thresholds :: Rule
conway_drep_voting_thresholds =
  "drep_voting_thresholds"
    =:= arr
      [ a unit_interval //- "motion no confidence"
      , a unit_interval //- "committee normal"
      , a unit_interval //- "committee no confidence"
      , a unit_interval //- "update constitution"
      , a unit_interval //- "hard fork initiation"
      , a unit_interval //- "PP network group"
      , a unit_interval //- "PP economic group"
      , a unit_interval //- "PP technical group"
      , a unit_interval //- "PP governance group"
      , a unit_interval //- "treasury withdrawal"
      ]

transaction_witness_set :: Rule
transaction_witness_set =
  "transaction_witness_set"
    =:= mp
      [ opt $ idx 0 ==> tagged_nonempty_set vkey_witness
      , opt $ idx 1 ==> tagged_nonempty_set native_script
      , opt $ idx 2 ==> tagged_nonempty_set bootstrap_witness
      , opt $ idx 3 ==> tagged_nonempty_set conway_plutus_v1_script
      , opt $ idx 4 ==> tagged_nonempty_set alonzo_plutus_data
      , opt $ idx 5 ==> redeemers conway_redeemer_tag
      , opt $ idx 6 ==> tagged_nonempty_set conway_plutus_v2_script
      , opt $ idx 7 ==> tagged_nonempty_set conway_plutus_v3_script
      ]

conway_plutus_v1_script :: Rule
conway_plutus_v1_script =
  comment
    [str|The real type of plutus_v1_script, plutus_v2_script and
        |plutus_v3_script is bytes. However, because we enforce
        |uniqueness when many scripts are supplied, we need to hack
        |around for tests in order to avoid generating duplicates, since
        |the cddl tool we use for roundtrip testing doesn't generate
        |distinct collections.
        |]
    $ "plutus_v1_script" =:= distinct VBytes

conway_plutus_v2_script :: Rule
conway_plutus_v2_script = "plutus_v2_script" =:= distinct VBytes

conway_plutus_v3_script :: Rule
conway_plutus_v3_script = "plutus_v3_script" =:= distinct VBytes

redeemers :: Rule -> Rule
redeemers redeemer_tag =
  comment
    [str|Flat Array support is included for backwards compatibility and
        |will be removed in the next era. It is recommended for tools to
        |adopt using a Map instead of Array going forward.
        |]
    $ "redeemers"
      =:= sarr
        [ 1
            <+ a
              ( arr
                  [ "tag" ==> redeemer_tag
                  , "index" ==> (VUInt `sized` (4 :: Word64))
                  , "data" ==> alonzo_plutus_data
                  , "ex_units" ==> alonzo_ex_units
                  ]
              )
        ]
      / smp
        [ 1
            <+ asKey
              ( arr
                  [ "tag" ==> redeemer_tag
                  , "index" ==> (VUInt `sized` (4 :: Word64))
                  ]
              )
            ==> arr ["data" ==> alonzo_plutus_data, "ex_units" ==> alonzo_ex_units]
        ]

conway_redeemer_tag :: Rule
conway_redeemer_tag =
  "redeemer_tag"
    =:= (int 0 //- "spend")
    / (int 1 //- "mint")
    / (int 2 //- "cert")
    / (int 3 //- "reward")
    / (int 4 //- "voting")
    / (int 5 //- "proposing")

conway_ex_unit_prices :: Rule
conway_ex_unit_prices =
  "ex_unit_prices"
    =:= arr
      [ "mem_price" ==> nonnegative_interval
      , "step_price" ==> nonnegative_interval
      ]

language :: Rule
language =
  "language"
    =:= int 0 -- Plutus v1
    / int 1 -- Plutus v2
    / int 2 -- Plutus v3

conway_potential_languages :: Rule
conway_potential_languages = "potential_languages" =:= (0 :: Integer) ... (255 :: Integer)

cost_models :: Rule
cost_models =
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
        [ opt $ idx 0 ==> arr [0 <+ a int64]
        , opt $ idx 1 ==> arr [0 <+ a int64]
        , opt $ idx 2 ==> arr [0 <+ a int64]
        , 0 <+ asKey ((3 :: Integer) ... (255 :: Integer)) ==> arr [0 <+ a int64]
        ]

conway_transaction_metadatum_label :: Rule
conway_transaction_metadatum_label = "conway_transaction_metadatum_label" =:= (VUInt `sized` (8 :: Word64))

conway_auxiliary_data :: Rule
conway_auxiliary_data =
  "conway_auxiliary_data"
    =:= mp
      [ 0
          <+ asKey conway_transaction_metadatum_label
          ==> transaction_metadatum
      ]

conway_shelley_ma_auxiliary_data :: Rule
conway_shelley_ma_auxiliary_data =
  "shelley_ma_auxiliary_data"
    =:= arr
      [ "transaction_metadata" ==> conway_auxiliary_data
      , "auxiliary_scripts" ==> arr [0 <+ a native_script]
      ]

conway_alonzo_auxiliary_data :: Rule
conway_alonzo_auxiliary_data =
  "alonzo_auxiliary_data"
    =:= tag
      259
      ( mp
          [ opt (idx 0 ==> conway_auxiliary_data)
          , opt (idx 1 ==> arr [0 <+ a native_script])
          , opt (idx 2 ==> arr [0 <+ a conway_plutus_v1_script])
          , opt (idx 3 ==> arr [0 <+ a conway_plutus_v2_script])
          , opt (idx 4 ==> arr [0 <+ a conway_plutus_v3_script])
          ]
      )

auxiliary_data :: Rule
auxiliary_data =
  "auxiliary_data"
    =:= conway_auxiliary_data
    / conway_shelley_ma_auxiliary_data
    / conway_alonzo_auxiliary_data

native_script :: Rule
native_script =
  "native_script"
    =:= arr [a allegra_script_pubkey]
    / arr [a allegra_script_all]
    / arr [a allegra_script_any]
    / arr [a conway_script_n_of_k]
    / arr [a conway_invalid_before]
    -- Timelock validity intervals are half-open intervals [a, b).
    -- This field specifies the left (included) endpoint a.
    / arr [a conway_invalid_hereafter]

conway_script_n_of_k :: Named Group
conway_script_n_of_k =
  "script_n_of_k"
    =:~ grp [3, "n" ==> int64, a (arr [0 <+ a native_script])]

conway_invalid_before :: Named Group
conway_invalid_before = "invalid_before" =:~ grp [4, a conway_slot_no]

conway_invalid_hereafter :: Named Group
conway_invalid_hereafter = "invalid_hereafter" =:~ grp [5, a conway_slot_no]

conway_value :: Rule
conway_value = "conway_value" =:= coin / sarr [a coin, a (mary_multiasset positive_coin)]

conway_mint :: Rule
conway_mint = "mint" =:= mp [1 <+ asKey mary_policy_id ==> mp [1 <+ asKey mary_asset_name ==> nonzero_int64]]

conway_epoch :: Rule
conway_epoch = "epoch" =:= VUInt `sized` (8 :: Word64)

conway_epoch_interval :: Rule
conway_epoch_interval = "epoch_interval" =:= VUInt `sized` (4 :: Word64)

conway_slot_no :: Rule
conway_slot_no = "slot_no" =:= VUInt `sized` (8 :: Word64)

conway_block_no :: Rule
conway_block_no = "block_no" =:= VUInt `sized` (8 :: Word64)

conway_script :: Rule
conway_script =
  "script"
    =:= arr [0, a native_script]
    / arr [1, a conway_plutus_v1_script]
    / arr [2, a conway_plutus_v2_script]
    / arr [3, a conway_plutus_v3_script]
