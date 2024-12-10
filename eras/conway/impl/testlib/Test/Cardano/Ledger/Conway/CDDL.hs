{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Evaluate" #-}

module Test.Cardano.Ledger.Conway.CDDL (
  module Test.Cardano.Ledger.Babbage.CDDL,
  module Test.Cardano.Ledger.Conway.CDDL,
) where

import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Word (Word64)
import GHC.Num (Integer)
import Test.Cardano.Ledger.Babbage.CDDL hiding (
  auxiliary_data,
  babbage_transaction_output,
  block,
  certificate,
  certificates,
  cost_models,
  dns_name,
  ex_unit_prices,
  header,
  header_body,
  invalid_before,
  invalid_hereafter,
  language,
  major_protocol_version,
  metadata,
  mint,
  multi_host_name,
  native_script,
  next_major_protocol_version,
  nonempty_set,
  operational_cert,
  plutus_v1_script,
  plutus_v2_script,
  pool_metadata,
  pool_params,
  pool_registration,
  pool_retirement,
  protocol_param_update,
  protocol_version,
  redeemer_tag,
  redeemers,
  relay,
  required_signers,
  script,
  script_all,
  script_any,
  script_data_hash,
  script_pubkey,
  script_ref,
  set,
  shelley_transaction_output,
  single_host_name,
  transaction,
  transaction_body,
  transaction_input,
  transaction_metadatum_label,
  transaction_output,
  transaction_witness_set,
  url,
  value,
  withdrawals,
 )
import Text.Heredoc

conwayCDDL :: Huddle
conwayCDDL =
  collectFrom
    [ block
    , transaction
    , kes_signature
    , language
    , potential_languages
    , signkeyKES
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
            ==> mp [0 <+ asKey transaction_index ==> auxiliary_data]
        , "invalid_transactions" ==> arr [0 <+ a transaction_index]
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
      [ "block_number" ==> block_no
      , "slot" ==> slot_no
      , "prev_hash" ==> (hash32 / VNil)
      , "issuer_vkey" ==> vkey
      , "vrf_vkey" ==> vrf_vkey
      , "vrf_result" ==> vrf_cert
      , "block_body_size" ==> (VUInt `sized` (4 :: Word64))
      , "block_body_hash" ==> hash32
      , a operational_cert
      , a protocol_version
      ]

operational_cert :: Rule
operational_cert =
  "operational_cert"
    =:= arr
      [ "hot_vkey" ==> kes_vkey
      , "sequence_number" ==> (VUInt `sized` (8 :: Word64))
      , "kes_period" ==> VUInt
      , "sigma" ==> signature
      ]

protocol_version :: Rule
protocol_version = "protocol_version" =:= arr [a major_protocol_version, a VUInt]

-- TODO Replace with the following once
-- https://github.com/input-output-hk/cuddle/issues/29 is addressed in cuddle.
--
-- next_major_protocol_version :: Rule
-- next_major_protocol_version = "next_major_protocol_version" =:= (10 :: Integer)
next_major_protocol_version :: Integer
next_major_protocol_version = 10

major_protocol_version :: Rule
major_protocol_version = "major_protocol_version" =:= (1 :: Integer) ... next_major_protocol_version

transaction_body :: Rule
transaction_body =
  "transaction_body"
    =:= mp
      [ idx 0 ==> set transaction_input
      , idx 1 ==> arr [0 <+ a transaction_output]
      , idx 2 ==> coin
      , opt (idx 3 ==> slot_no)
      , opt (idx 4 ==> certificates)
      , opt (idx 5 ==> withdrawals)
      , opt (idx 7 ==> auxiliary_data_hash)
      , opt (idx 8 ==> slot_no) -- Validity interval start
      , opt (idx 9 ==> mint)
      , opt (idx 11 ==> script_data_hash)
      , opt (idx 13 ==> nonempty_set transaction_input)
      , opt (idx 14 ==> required_signers)
      , opt (idx 15 ==> network_id)
      , opt (idx 16 ==> transaction_output)
      , opt (idx 17 ==> coin)
      , opt (idx 18 ==> nonempty_set transaction_input)
      , opt (idx 19 ==> voting_procedures)
      , opt (idx 20 ==> proposal_procedures)
      , opt (idx 21 ==> coin)
      , opt (idx 22 ==> positive_coin)
      ]

voting_procedures :: Rule
voting_procedures =
  "voting_procedures"
    =:= mp [1 <+ asKey voter ==> mp [1 <+ asKey gov_action_id ==> voting_procedure]]

voting_procedure :: Rule
voting_procedure = "voting_procedure" =:= arr [a vote, a (anchor / VNil)]

proposal_procedure :: Rule
proposal_procedure =
  "proposal_procedure"
    =:= arr
      [ "deposit" ==> coin
      , a reward_account
      , a gov_action
      , a anchor
      ]

proposal_procedures :: Rule
proposal_procedures = "proposal_procedures" =:= nonempty_set proposal_procedure

certificates :: Rule
certificates = "certificates" =:= nonempty_set certificate

gov_action :: Rule
gov_action =
  "gov_action"
    =:= arr [a parameter_change_action]
    / arr [a hard_fork_initiation_action]
    / arr [a treasury_withdrawals_action]
    / arr [a no_confidence]
    / arr [a update_committee]
    / arr [a new_constitution]
    / arr [a info_action]

policy_hash :: Rule
policy_hash = "policy_hash" =:= script_hash

parameter_change_action :: Named Group
parameter_change_action =
  "parameter_change_action"
    =:~ grp
      [ 0
      , gov_action_id / VNil
      , a protocol_param_update
      , policy_hash / VNil
      ]

hard_fork_initiation_action :: Named Group
hard_fork_initiation_action =
  "hard_fork_initiation_action"
    =:~ grp [1, gov_action_id / VNil, a protocol_version]

treasury_withdrawals_action :: Named Group
treasury_withdrawals_action =
  "treasury_withdrawals_action"
    =:~ grp [2, a (mp [asKey reward_account ==> coin]), policy_hash / VNil]

no_confidence :: Named Group
no_confidence = "no_confidence" =:~ grp [3, gov_action_id / VNil]

update_committee :: Named Group
update_committee =
  "update_committee"
    =:~ grp
      [ 4
      , gov_action_id / VNil
      , a (set committee_cold_credential)
      , a (mp [asKey committee_cold_credential ==> epoch_no])
      , a unit_interval
      ]

new_constitution :: Named Group
new_constitution =
  "new_constitution"
    =:~ grp [5, gov_action_id / VNil, a constitution]

constitution :: Rule
constitution =
  "constitution"
    =:= arr
      [ a anchor
      , a (script_hash / VNil)
      ]

info_action :: Rule
info_action = "info_action" =:= int 6

voter :: Rule
voter =
  comment
    [str|0: constitutional committee hot keyhash
        |1: constitutional committee hot script_hash
        |2: drep keyhash
        |3: drep script_hash
        |4: stakingpool keyhash
        |]
    $ "voter"
      =:= arr [0, a addr_keyhash]
      / arr [1, a script_hash]
      / arr [2, a addr_keyhash]
      / arr [3, a script_hash]
      / arr [4, a addr_keyhash]

anchor :: Rule
anchor =
  "anchor"
    =:= arr
      [ "anchor_url" ==> url
      , "anchor_data_hash" ==> hash32
      ]

vote :: Rule
vote = "vote" =:= 0 ... 2

gov_action_id :: Rule
gov_action_id =
  "gov_action_id"
    =:= arr
      [ "transaction_id" ==> hash32
      , "gov_action_index" ==> (VUInt `sized` (2 :: Word64))
      ]

required_signers :: Rule
required_signers = "required_signers" =:= nonempty_set addr_keyhash

transaction_input :: Rule
transaction_input =
  "transaction_input"
    =:= arr
      [ "transaction_id" ==> hash32
      , "index" ==> (VUInt `sized` (2 :: Word64))
      ]

transaction_output :: Rule
transaction_output =
  comment
    [str|Both of the Alonzo and Babbage style TxOut formats are equally valid
        |and can be used interchangeably
        |]
    $ "transaction_output"
      =:= shelley_transaction_output
      / babbage_transaction_output

shelley_transaction_output :: Rule
shelley_transaction_output =
  comment
    [str|hash32: datum_hash
        |]
    $ "shelley_transaction_output"
      =:= arr [a address, "amount" ==> value, opt $ a hash32]

babbage_transaction_output :: Rule
babbage_transaction_output =
  "babbage_transaction_output"
    =:= mp
      [ idx 0 ==> address
      , idx 1 ==> value
      , opt $ idx 2 ==> datum_option
      , opt $ idx 3 ==> script_ref
      ]

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
    $ "script_data_hash" =:= hash32

certificate :: Rule
certificate =
  "certificate"
    =:= arr [a stake_registration]
    / arr [a stake_deregistration]
    / arr [a stake_delegation]
    / arr [a pool_registration]
    / arr [a pool_retirement]
    / arr [a reg_cert]
    / arr [a unreg_cert]
    / arr [a vote_deleg_cert]
    / arr [a stake_vote_deleg_cert]
    / arr [a stake_reg_deleg_cert]
    / arr [a vote_reg_deleg_cert]
    / arr [a stake_vote_reg_deleg_cert]
    / arr [a auth_committee_hot_cert]
    / arr [a resign_committee_cold_cert]
    / arr [a reg_drep_cert]
    / arr [a unreg_drep_cert]
    / arr [a update_drep_cert]

-- POOL
pool_registration :: Named Group
pool_registration = "pool_registration" =:~ grp [3, a pool_params]

pool_retirement :: Named Group
pool_retirement = "pool_retirement" =:~ grp [4, a pool_keyhash, a epoch_no]

-- numbers 5 and 6 used to be the Genesis and MIR certificates respectively,
-- which were deprecated in Conway

-- DELEG
reg_cert :: Named Group
reg_cert = "reg_cert" =:~ grp [7, a stake_credential, a coin]

unreg_cert :: Named Group
unreg_cert = "unreg_cert" =:~ grp [8, a stake_credential, a coin]

vote_deleg_cert :: Named Group
vote_deleg_cert = "vote_deleg_cert" =:~ grp [9, a stake_credential, a drep]

stake_vote_deleg_cert :: Named Group
stake_vote_deleg_cert =
  "stake_vote_deleg_cert"
    =:~ grp [10, a stake_credential, a pool_keyhash, a drep]

stake_reg_deleg_cert :: Named Group
stake_reg_deleg_cert =
  "stake_reg_deleg_cert"
    =:~ grp [11, a stake_credential, a pool_keyhash, a coin]

vote_reg_deleg_cert :: Named Group
vote_reg_deleg_cert =
  "vote_reg_deleg_cert"
    =:~ grp [12, a stake_credential, a drep, a coin]

stake_vote_reg_deleg_cert :: Named Group
stake_vote_reg_deleg_cert =
  "stake_vote_reg_deleg_cert"
    =:~ grp [13, a stake_credential, a pool_keyhash, a drep, a coin]

-- GOVCERT
auth_committee_hot_cert :: Named Group
auth_committee_hot_cert =
  "auth_committee_hot_cert"
    =:~ grp [14, a committee_cold_credential, a committee_hot_credential]

resign_committee_cold_cert :: Named Group
resign_committee_cold_cert =
  "resign_committee_cold_cert"
    =:~ grp [15, a committee_cold_credential, anchor / VNil]

reg_drep_cert :: Named Group
reg_drep_cert = "reg_drep_cert" =:~ grp [16, a drep_credential, a coin, anchor / VNil]

unreg_drep_cert :: Named Group
unreg_drep_cert = "unreg_drep_cert" =:~ grp [17, a drep_credential, a coin]

update_drep_cert :: Named Group
update_drep_cert = "update_drep_cert" =:~ grp [18, a drep_credential, anchor / VNil]

drep :: Rule
drep =
  "drep"
    =:= arr [0, a addr_keyhash]
    / arr [1, a script_hash]
    / arr [2] -- always abstain
    / arr [3] -- always no confidence

drep_credential :: Rule
drep_credential = "drep_credential" =:= credential

committee_cold_credential :: Rule
committee_cold_credential = "committee_cold_credential" =:= credential

committee_hot_credential :: Rule
committee_hot_credential = "committee_hot_credential" =:= credential

pool_params :: Named Group
pool_params =
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
        , "pool_owners" ==> set addr_keyhash
        , "relays" ==> arr [0 <+ a relay]
        , "pool_metadata" ==> (pool_metadata / VNil)
        ]

dns_name :: Rule
dns_name = "dns_name" =:= VText `sized` (0 :: Word64, 128 :: Word64)

single_host_name :: Named Group
single_host_name =
  comment
    [str|dns_name: An A or AAAA DNS record
        |]
    $ "single_host_name" =:~ grp [1, port / VNil, a dns_name]

multi_host_name :: Named Group
multi_host_name =
  comment
    [str|dns_name: An SRV DNS record
        |]
    $ "multi_host_name"
      =:~ grp [2, a dns_name]

relay :: Rule
relay =
  "relay"
    =:= arr [a single_host_addr]
    / arr [a single_host_name]
    / arr [a multi_host_name]

url :: Rule
url = "url" =:= VText `sized` (0 :: Word64, 128 :: Word64)

pool_metadata :: Rule
pool_metadata = "pool_metadata" =:= arr [a url, a metadata_hash]

withdrawals :: Rule
withdrawals = "withdrawals" =:= mp [1 <+ asKey reward_account ==> coin]

protocol_param_update :: Rule
protocol_param_update =
  comment
    [str| 0: minfee A
        | 1: minfee B
        | 2: max block body size
        | 3: max transaction size
        | 4: max block header size
        | 5: key deposit
        | 6: pool deposit
        | 7: maximum epoch
        | 8: n_opt: desired number of stake pools
        | 9: pool pledge influence
        |10: expansion rate
        |11: treasury growth rate
        |16: min pool cost
        |17: ada per utxo byte
        |18: cost models for script languages
        |19: execution costs
        |20: max tx ex units
        |21: max block ex units
        |22: max value size
        |23: collateral percentage
        |24: max collateral inputs
        |25: pool voting thresholds
        |26: drep voting thresholds
        |27: min committee size
        |28: committee term limit
        |29: governance action validity period
        |30: governance action deposit
        |31: drep deposit
        |32: drep inactivity period
        |33: minfee refscriptcoinsperbyte
        |]
    $ "protocol_param_update"
      =:= mp
        [ opt (idx 0 ==> coin)
        , opt (idx 1 ==> coin)
        , opt (idx 2 ==> (VUInt `sized` (4 :: Word64)))
        , opt (idx 3 ==> (VUInt `sized` (4 :: Word64)))
        , opt (idx 4 ==> (VUInt `sized` (2 :: Word64)))
        , opt (idx 5 ==> coin)
        , opt (idx 6 ==> coin)
        , opt (idx 7 ==> epoch_interval)
        , opt (idx 8 ==> (VUInt `sized` (2 :: Word64)))
        , opt (idx 9 ==> nonnegative_interval)
        , opt (idx 10 ==> unit_interval)
        , opt (idx 11 ==> unit_interval)
        , opt (idx 16 ==> coin)
        , opt (idx 17 ==> coin)
        , opt (idx 18 ==> cost_models)
        , opt (idx 19 ==> ex_unit_prices)
        , opt (idx 20 ==> ex_units)
        , opt (idx 21 ==> ex_units)
        , opt (idx 22 ==> (VUInt `sized` (4 :: Word64)))
        , opt (idx 23 ==> (VUInt `sized` (2 :: Word64)))
        , opt (idx 24 ==> (VUInt `sized` (2 :: Word64)))
        , opt (idx 25 ==> pool_voting_thresholds)
        , opt (idx 26 ==> drep_voting_thresholds)
        , opt (idx 27 ==> (VUInt `sized` (2 :: Word64)))
        , opt (idx 28 ==> epoch_interval)
        , opt (idx 29 ==> epoch_interval)
        , opt (idx 30 ==> coin)
        , opt (idx 31 ==> coin)
        , opt (idx 32 ==> epoch_interval)
        , opt (idx 33 ==> nonnegative_interval)
        ]

pool_voting_thresholds :: Rule
pool_voting_thresholds =
  "pool_voting_thresholds"
    =:= arr
      [ a unit_interval -- motion no confidence
      , a unit_interval -- committee normal
      , a unit_interval -- committee no confidence
      , a unit_interval -- hard fork initiation
      , a unit_interval -- security relevant parameter voting threshold
      ]

drep_voting_thresholds :: Rule
drep_voting_thresholds =
  "drep_voting_thresholds"
    =:= arr
      [ a unit_interval -- motion no confidence
      , a unit_interval -- committee normal
      , a unit_interval -- committee no confidence
      , a unit_interval -- update constitution
      , a unit_interval -- hard fork initiation
      , a unit_interval -- PP network group
      , a unit_interval -- PP economic group
      , a unit_interval -- PP technical group
      , a unit_interval -- PP governance group
      , a unit_interval -- treasury withdrawal
      ]

transaction_witness_set :: Rule
transaction_witness_set =
  "transaction_witness_set"
    =:= mp
      [ opt $ idx 0 ==> nonempty_set vkeywitness
      , opt $ idx 1 ==> nonempty_set native_script
      , opt $ idx 2 ==> nonempty_set bootstrap_witness
      , opt $ idx 3 ==> nonempty_set plutus_v1_script
      , opt $ idx 4 ==> nonempty_set plutus_data
      , opt $ idx 5 ==> redeemers
      , opt $ idx 6 ==> nonempty_set plutus_v2_script
      , opt $ idx 7 ==> nonempty_set plutus_v3_script
      ]

plutus_v1_script :: Rule
plutus_v1_script =
  comment
    [str|The real type of plutus_v1_script, plutus_v2_script and
        |plutus_v3_script is bytes. However, because we enforce
        |uniqueness when many scripts are supplied, we need to hack
        |around for tests in order to avoid generating duplicates, since
        |the cddl tool we use for roundtrip testing doesn't generate
        |distinct collections.
        |]
    $ "plutus_v1_script" =:= distinct VBytes

plutus_v2_script :: Rule
plutus_v2_script = "plutus_v2_script" =:= distinct VBytes

plutus_v3_script :: Rule
plutus_v3_script = "plutus_v3_script" =:= distinct VBytes

redeemers :: Rule
redeemers =
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
                  , "data" ==> plutus_data
                  , "ex_units" ==> ex_units
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
            ==> arr ["data" ==> plutus_data, "ex_units" ==> ex_units]
        ]

redeemer_tag :: Rule
redeemer_tag =
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

ex_unit_prices :: Rule
ex_unit_prices =
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

potential_languages :: Rule
potential_languages = "potential_languages" =:= 0 ... 255

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
        , 0 <+ asKey (3 ... 255) ==> arr [0 <+ a int64]
        ]

transaction_metadatum_label :: Rule
transaction_metadatum_label = "transaction_metadatum_label" =:= (VUInt `sized` (8 :: Word64))

metadata :: Rule
metadata =
  "metadata"
    =:= mp
      [ 0
          <+ asKey transaction_metadatum_label
          ==> transaction_metadatum
      ]

auxiliary_data :: Rule
auxiliary_data =
  comment
    [str|              metadata: shelley
        |  transaction_metadata: shelley-ma
        |#6.259(0 ==> metadata): alonzo onwards
        |]
    $ "auxiliary_data"
      =:= metadata
      / sarr
        [ "transaction_metadata" ==> metadata
        , "auxiliary_scripts" ==> arr [0 <+ a native_script]
        ]
      / tag
        259
        ( mp
            [ opt (idx 0 ==> metadata)
            , opt (idx 1 ==> arr [0 <+ a native_script])
            , opt (idx 2 ==> arr [0 <+ a plutus_v1_script])
            , opt (idx 3 ==> arr [0 <+ a plutus_v2_script])
            , opt (idx 4 ==> arr [0 <+ a plutus_v3_script])
            ]
        )

native_script :: Rule
native_script =
  "native_script"
    =:= arr [a script_pubkey]
    / arr [a script_all]
    / arr [a script_any]
    / arr [a script_n_of_k]
    / arr [a invalid_before]
    -- Timelock validity intervals are half-open intervals [a, b).
    -- This field specifies the left (included) endpoint a.
    / arr [a invalid_hereafter]

-- Timelock validity intervals are half-open intervals [a, b).
-- This field specifies the right (excluded) endpoint b.

script_pubkey :: Named Group
script_pubkey = "script_pubkey" =:~ grp [0, a addr_keyhash]

script_all :: Named Group
script_all = "script_all" =:~ grp [1, a (arr [0 <+ a native_script])]

script_any :: Named Group
script_any = "script_any" =:~ grp [2, a (arr [0 <+ a native_script])]

script_ref :: Rule
script_ref = "script_ref" =:= tag 24 (VBytes `cbor` script)

script_n_of_k :: Named Group
script_n_of_k =
  "script_n_of_k"
    =:~ grp [3, "n" ==> int64, a (arr [0 <+ a native_script])]

invalid_before :: Named Group
invalid_before = "invalid_before" =:~ grp [4, a slot_no]

invalid_hereafter :: Named Group
invalid_hereafter = "invalid_hereafter" =:~ grp [5, a slot_no]

multiasset :: IsType0 a => a -> GRuleCall
multiasset = binding $ \x ->
  "multiasset"
    =:= mp [1 <+ asKey policy_id ==> mp [1 <+ asKey asset_name ==> x]]

value :: Rule
value = "value" =:= coin / sarr [a coin, a (multiasset positive_coin)]

mint :: Rule
mint = "mint" =:= multiasset nonZeroInt64

epoch_no :: Rule
epoch_no = "epoch_no" =:= VUInt `sized` (8 :: Word64)

epoch_interval :: Rule
epoch_interval = "epoch_interval" =:= VUInt `sized` (4 :: Word64)

slot_no :: Rule
slot_no = "slot_no" =:= VUInt `sized` (8 :: Word64)

block_no :: Rule
block_no = "block_no" =:= VUInt `sized` (8 :: Word64)

script :: Rule
script =
  "script"
    =:= arr [0, a native_script]
    / arr [1, a plutus_v1_script]
    / arr [2, a plutus_v2_script]
    / arr [3, a plutus_v3_script]

-- | Conway era introduces an optional 258 tag for sets, which will
-- become mandatory in the second era after Conway. We recommend all the
-- tooling to account for this future breaking change sooner rather than
-- later, in order to provide a smooth transition for their users.
set :: IsType0 t0 => t0 -> GRuleCall
set = binding $ \x -> "set" =:= tag 258 (arr [0 <+ a x]) / sarr [0 <+ a x]

-- | Conway era introduces an optional 258 tag for sets, which will
-- become mandatory in the second era after Conway. We recommend all the
-- tooling to account for this future breaking change sooner rather than
-- later, in order to provide a smooth transition for their users.
nonempty_set :: IsType0 t0 => t0 -> GRuleCall
nonempty_set = binding $ \x ->
  "nonempty_set"
    =:= tag 258 (arr [1 <+ a x])
    / sarr [1 <+ a x]
