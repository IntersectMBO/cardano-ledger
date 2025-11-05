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

  -- * Certificates
  certificate,

  -- * Governance
  drep,
  anchor,
  gov_action_id,
  hard_fork_initiation_action,
  treasury_withdrawals_action,
  no_confidence,
  update_committee,
  new_constitution,
  info_action,
  policy_hash,

  -- * Voting
  voting_procedures,
  pool_voting_thresholds,
  drep_voting_thresholds,

  -- * Scripts
  plutus_v3_script,
  potential_languages,

  -- * Transaction
  transaction_input,
  shelley_transaction_output,
  withdrawals,
  mint,
  ex_unit_prices,

  -- * Sets
  maybe_tagged_set,
  maybe_tagged_nonempty_set,
  maybe_tagged_oset,
  maybe_tagged_nonempty_oset,
) where

import Cardano.Ledger.Conway (ConwayEra)
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Num (Integer)
import Test.Cardano.Ledger.Babbage.CDDL hiding (
  certificate,
  ex_unit_prices,
  mint,
  multiasset,
  redeemers,
  required_signers,
  transaction_input,
  untagged_set,
  value,
  withdrawals,
 )
import Text.Heredoc

drep_credential :: Rule
drep_credential = "drep_credential" =:= credential

committee_cold_credential :: Rule
committee_cold_credential = "committee_cold_credential" =:= credential

committee_hot_credential :: Rule
committee_hot_credential = "committee_hot_credential" =:= credential

drep :: Rule
drep =
  "drep"
    =:= arr [0, a addr_keyhash]
    / arr [1, a script_hash]
    / arr [2] -- always abstain
    / arr [3] -- always no confidence

anchor :: Rule
anchor = "anchor" =:= arr ["anchor_url" ==> url128, "anchor_data_hash" ==> hash32]

-- Pool primitives (size limits increased from 64 to 128)
dns_name128 :: Rule
dns_name128 = "dns_name" =:= VText `sized` (0 :: Word64, 128 :: Word64)

url128 :: Rule
url128 = "url" =:= VText `sized` (0 :: Word64, 128 :: Word64)

conwayCDDL :: Huddle
conwayCDDL =
  collectFromInit $
    [ HIRule block
    , HIRule transaction
    , HIRule kes_signature
    , HIRule language
    , HIRule potential_languages
    , HIRule signkey_kes
    , HIRule certificate
    ]
      <> conwayPoolRules

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
      [ "block_number" ==> block_number
      , "slot" ==> slot
      , "prev_hash" ==> (hash32 / VNil)
      , "issuer_vkey" ==> vkey
      , "vrf_vkey" ==> vrf_vkey
      , "vrf_result" ==> vrf_cert
      , "block_body_size" ==> (VUInt `sized` (4 :: Word64))
      , "block_body_hash" ==> hash32
      , a operational_cert
      , a (protocol_version @ConwayEra)
      ]

transaction_body :: Rule
transaction_body =
  "transaction_body"
    =:= mp
      [ idx 0 ==> maybe_tagged_set transaction_input
      , idx 1 ==> arr [0 <+ a transaction_output]
      , idx 2 ==> coin
      , opt (idx 3 ==> slot)
      , opt (idx 4 ==> certificates)
      , opt (idx 5 ==> withdrawals)
      , opt (idx 7 ==> auxiliary_data_hash)
      , opt (idx 8 ==> slot) -- Validity interval start
      , opt (idx 9 ==> mint)
      , opt (idx 11 ==> script_data_hash)
      , opt (idx 13 ==> maybe_tagged_nonempty_set transaction_input)
      , opt (idx 14 ==> required_signers)
      , opt (idx 15 ==> network_id)
      , opt (idx 16 ==> transaction_output)
      , opt (idx 17 ==> coin)
      , opt (idx 18 ==> maybe_tagged_nonempty_set transaction_input)
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
proposal_procedures = "proposal_procedures" =:= maybe_tagged_nonempty_oset proposal_procedure

certificates :: Rule
certificates = "certificates" =:= maybe_tagged_nonempty_oset certificate

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
      , a $ gov_action_id / VNil
      , a protocol_param_update
      , a $ policy_hash / VNil
      ]

hard_fork_initiation_action :: Named Group
hard_fork_initiation_action =
  "hard_fork_initiation_action"
    =:~ grp [1, a $ gov_action_id / VNil, a (protocol_version @ConwayEra)]

treasury_withdrawals_action :: Named Group
treasury_withdrawals_action =
  "treasury_withdrawals_action"
    =:~ grp [2, a (mp [0 <+ asKey reward_account ==> coin]), a $ policy_hash / VNil]

no_confidence :: Named Group
no_confidence = "no_confidence" =:~ grp [3, a $ gov_action_id / VNil]

update_committee :: Named Group
update_committee =
  "update_committee"
    =:~ grp
      [ 4
      , a $ gov_action_id / VNil
      , a (maybe_tagged_set committee_cold_credential)
      , a (mp [0 <+ asKey committee_cold_credential ==> epoch])
      , a unit_interval
      ]

new_constitution :: Named Group
new_constitution =
  "new_constitution"
    =:~ grp [5, a $ gov_action_id / VNil, a constitution]

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
  "voter"
    =:= (arr [0, a addr_keyhash] //- "constitutional committee hot key hash")
    / (arr [1, a script_hash] //- "consitutional committee script hash")
    / (arr [2, a addr_keyhash] //- "drep keyhash")
    / (arr [3, a script_hash] //- "drep script hash")
    / (arr [4, a addr_keyhash] //- "staking pool key hash")

vote :: Rule
vote = "vote" =:= (0 :: Integer) ... (2 :: Integer)

gov_action_id :: Rule
gov_action_id =
  "gov_action_id"
    =:= arr
      [ "transaction_id" ==> transaction_id
      , "gov_action_index" ==> (VUInt `sized` (2 :: Word64))
      ]

required_signers :: Rule
required_signers = "required_signers" =:= maybe_tagged_nonempty_set addr_keyhash

transaction_input :: Rule
transaction_input =
  "transaction_input"
    =:= arr
      [ "transaction_id" ==> transaction_id
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
      / babbage_transaction_output conway_script

shelley_transaction_output :: Rule
shelley_transaction_output =
  comment
    [str|hash32: datum_hash
        |]
    $ "shelley_transaction_output"
      =:= arr [a address, "amount" ==> value, opt $ a hash32]

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
    =:= arr [a account_registration_cert]
    / arr [a account_unregistration_cert]
    / arr [a delegation_to_stake_pool_cert]
    / arr [a pool_registration_cert]
    / arr [a pool_retirement_cert]
    / arr [a account_registration_deposit_cert]
    / arr [a account_unregistration_deposit_cert]
    / arr [a delegation_to_drep_cert]
    / arr [a delegation_to_stake_pool_and_drep_cert]
    / arr [a account_registration_delegation_to_stake_pool_cert]
    / arr [a account_registration_delegation_to_drep_cert]
    / arr [a account_registration_delegation_to_stake_pool_and_drep_cert]
    / arr [a committee_authorization_cert]
    / arr [a committee_resignation_cert]
    / arr [a drep_registration_cert]
    / arr [a drep_unregistration_cert]
    / arr [a drep_update_cert]

pool_registration_cert :: Named Group
pool_retirement_cert :: Named Group
conwayPoolRules :: [HuddleItem]
(pool_registration_cert, pool_retirement_cert, conwayPoolRules) = mkPoolRules dns_name128 url128

account_registration_deposit_cert :: Named Group
account_registration_deposit_cert = "account_registration_deposit_cert" =:~ grp [7, a stake_credential, a coin]

account_unregistration_deposit_cert :: Named Group
account_unregistration_deposit_cert = "account_unregistration_deposit_cert" =:~ grp [8, a stake_credential, a coin]

delegation_to_drep_cert :: Named Group
delegation_to_drep_cert = "delegation_to_drep_cert" =:~ grp [9, a stake_credential, a drep]

delegation_to_stake_pool_and_drep_cert :: Named Group
delegation_to_stake_pool_and_drep_cert =
  "delegation_to_stake_pool_and_drep_cert" =:~ grp [10, a stake_credential, a pool_keyhash, a drep]

account_registration_delegation_to_stake_pool_cert :: Named Group
account_registration_delegation_to_stake_pool_cert =
  "account_registration_delegation_to_stake_pool_cert"
    =:~ grp [11, a stake_credential, a pool_keyhash, a coin]

account_registration_delegation_to_drep_cert :: Named Group
account_registration_delegation_to_drep_cert =
  "account_registration_delegation_to_drep_cert" =:~ grp [12, a stake_credential, a drep, a coin]

account_registration_delegation_to_stake_pool_and_drep_cert :: Named Group
account_registration_delegation_to_stake_pool_and_drep_cert =
  "account_registration_delegation_to_stake_pool_and_drep_cert"
    =:~ grp [13, a stake_credential, a pool_keyhash, a drep, a coin]

committee_authorization_cert :: Named Group
committee_authorization_cert =
  comment "Authorize committee hot key for cold key" $
    "committee_authorization_cert"
      =:~ grp [14, a committee_cold_credential, a committee_hot_credential]

committee_resignation_cert :: Named Group
committee_resignation_cert =
  comment "Resign from committee with cold key" $
    "committee_resignation_cert"
      =:~ grp [15, a committee_cold_credential, a $ anchor / VNil]

drep_registration_cert :: Named Group
drep_registration_cert = "drep_registration_cert" =:~ grp [16, a drep_credential, a coin, a $ anchor / VNil]

drep_unregistration_cert :: Named Group
drep_unregistration_cert = "drep_unregistration_cert" =:~ grp [17, a drep_credential, a coin]

drep_update_cert :: Named Group
drep_update_cert = "drep_update_cert" =:~ grp [18, a drep_credential, a $ anchor / VNil]

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
      , opt (idx 7 ==> epoch_interval) //- "maximum epoch"
      , opt (idx 8 ==> (VUInt `sized` (2 :: Word64))) //- "n_opt: desired number of stake pools"
      , opt (idx 9 ==> nonnegative_interval) //- "pool pledge influence"
      , opt (idx 10 ==> unit_interval) //- "expansion rate"
      , opt (idx 11 ==> unit_interval) //- "treasury growth rate"
      , opt (idx 16 ==> coin) //- "min pool cost"
      , opt (idx 17 ==> coin) //- "ada per utxo byte"
      , opt (idx 18 ==> cost_models) //- "cost models for script languages"
      , opt (idx 19 ==> ex_unit_prices) //- "execution costs"
      , opt (idx 20 ==> ex_units) //- "max tx ex units"
      , opt (idx 21 ==> ex_units) //- "max block ex units"
      , opt (idx 22 ==> (VUInt `sized` (4 :: Word64))) //- "max value size"
      , opt (idx 23 ==> (VUInt `sized` (2 :: Word64))) //- "collateral percentage"
      , opt (idx 24 ==> (VUInt `sized` (2 :: Word64))) //- "max collateral inputs"
      , opt (idx 25 ==> pool_voting_thresholds) //- "pool voting thresholds"
      , opt (idx 26 ==> drep_voting_thresholds) //- "drep voting thresholds"
      , opt (idx 27 ==> (VUInt `sized` (2 :: Word64))) //- "min committee size"
      , opt (idx 28 ==> epoch_interval) //- "committee term limit"
      , opt (idx 29 ==> epoch_interval) //- "goveranance action validity period"
      , opt (idx 30 ==> coin) //- "governance action deposit"
      , opt (idx 31 ==> coin) //- "drep deposit"
      , opt (idx 32 ==> epoch_interval) //- "drep inactivity period"
      , opt (idx 33 ==> nonnegative_interval) //- "minfee refscriptcoinsperbyte"
      ]

pool_voting_thresholds :: Rule
pool_voting_thresholds =
  "pool_voting_thresholds"
    =:= arr
      [ a unit_interval //- "motion no confidence"
      , a unit_interval //- "committee normal"
      , a unit_interval //- "committee no confidence"
      , a unit_interval //- "hard fork initiation"
      , a unit_interval //- "security relevant parameter voting threshold"
      ]

drep_voting_thresholds :: Rule
drep_voting_thresholds =
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
      [ opt $ idx 0 ==> maybe_tagged_nonempty_set vkeywitness
      , opt $ idx 1 ==> maybe_tagged_nonempty_set allegra_native_script
      , opt $ idx 2 ==> maybe_tagged_nonempty_set bootstrap_witness
      , opt $ idx 3 ==> maybe_tagged_nonempty_set plutus_v1_script
      , opt $ idx 4 ==> maybe_tagged_nonempty_set plutus_data
      , opt $ idx 5 ==> redeemers conway_redeemer_tag
      , opt $ idx 6 ==> maybe_tagged_nonempty_set plutus_v2_script
      , opt $ idx 7 ==> maybe_tagged_nonempty_set plutus_v3_script
      ]

plutus_v3_script :: Rule
plutus_v3_script =
  comment
    [str|Conway introduces Plutus V3 with support for new governance features.
        |
        |Note: distinct VBytes ensures uniqueness in test generation.
        |The cddl tool we use for roundtrip testing doesn't generate
        |distinct collections, so we use sized variants to ensure uniqueness.
        |]
    $ "plutus_v3_script" =:= distinct VBytes

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

conway_redeemer_tag :: Rule
conway_redeemer_tag =
  "redeemer_tag"
    =:= (int 0 //- "spend")
    / (int 1 //- "mint")
    / (int 2 //- "cert")
    / (int 3 //- "reward")
    / (int 4 //- "voting")
    / (int 5 //- "proposing")

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
potential_languages = "potential_languages" =:= (0 :: Integer) ... (255 :: Integer)

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

auxiliary_data_map :: Rule
auxiliary_data_map =
  "auxiliary_data_map"
    =:= tag
      259
      ( mp
          [ opt (idx 0 ==> metadata)
          , opt (idx 1 ==> arr [0 <+ a allegra_native_script])
          , opt (idx 2 ==> arr [0 <+ a plutus_v1_script])
          , opt (idx 3 ==> arr [0 <+ a plutus_v2_script])
          , opt (idx 4 ==> arr [0 <+ a plutus_v3_script])
          ]
      )

auxiliary_data :: Rule
auxiliary_data =
  comment
    [str|auxiliary_data supports three serialization formats:
        |  1. metadata (raw) - Supported since Shelley
        |  2. auxiliary_data_array - Array format, introduced in Allegra
        |  3. auxiliary_data_map - Tagged map format, introduced in Alonzo
        |     Conway adds plutus_v3_script support at index 4
        |]
    $ "auxiliary_data"
      =:= metadata
      / auxiliary_data_array
      / auxiliary_data_map

multiasset :: IsType0 a => a -> GRuleCall
multiasset = binding $ \x ->
  "multiasset"
    =:= mp [0 <+ asKey policy_id ==> mp [1 <+ asKey asset_name ==> x]]

value :: Rule
value = "value" =:= coin / sarr [a coin, a (multiasset positive_coin)]

mint :: Rule
mint = "mint" =:= mp [1 <+ asKey policy_id ==> mp [1 <+ asKey asset_name ==> nonzero_int64]]

conway_script :: Rule
conway_script =
  comment
    [str|Conway supports four script types:
        |  0: Native scripts (timelock) - unchanged from Allegra
        |  1: Plutus V1 scripts
        |  2: Plutus V2 scripts
        |  3: Plutus V3 scripts (NEW)
        |]
    $ "script"
      =:= arr [0, a allegra_native_script]
      / arr [1, a plutus_v1_script]
      / arr [2, a plutus_v2_script]
      / arr [3, a plutus_v3_script]

-- | Conway era introduces an optional 258 tag for sets, which will become
-- mandatory in the second era after Conway.
mkMaybeTaggedSet :: IsType0 a => T.Text -> Word64 -> a -> GRuleCall
mkMaybeTaggedSet label n = binding $ \x -> label =:= tag 258 (arr [n <+ a x]) / sarr [n <+ a x]

maybe_tagged_set :: IsType0 a => a -> GRuleCall
maybe_tagged_set = mkMaybeTaggedSet "set" 0

maybe_tagged_nonempty_set :: IsType0 a => a -> GRuleCall
maybe_tagged_nonempty_set = mkMaybeTaggedSet "nonempty_set" 1

maybe_tagged_oset :: IsType0 a => a -> GRuleCall
maybe_tagged_oset = mkMaybeTaggedSet "oset" 0

maybe_tagged_nonempty_oset :: IsType0 a => a -> GRuleCall
maybe_tagged_nonempty_oset = mkMaybeTaggedSet "nonempty_oset" 1
