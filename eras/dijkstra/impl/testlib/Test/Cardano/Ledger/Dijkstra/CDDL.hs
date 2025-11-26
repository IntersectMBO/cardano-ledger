{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Evaluate" -}

module Test.Cardano.Ledger.Dijkstra.CDDL (
  module Test.Cardano.Ledger.Conway.CDDL,
  dijkstraCDDL,
  plutus_v4_script,
  dijkstra_native_script,
  script_require_guard,
) where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Word (Word64)
import GHC.Num (Integer)
import Test.Cardano.Ledger.Conway.CDDL
import Text.Heredoc

dijkstraCDDL :: Huddle
dijkstraCDDL =
  collectFromInit
    [ HIRule block
    , HIRule transaction
    , HIRule kes_signature
    , HIRule language
    , HIRule potential_languages
    , HIRule signkey_kes
    , HIRule certificate
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
      [ "block_number" ==> block_number
      , "slot" ==> slot
      , "prev_hash" ==> (hash32 / VNil)
      , "issuer_vkey" ==> vkey
      , "vrf_vkey" ==> vrf_vkey
      , "vrf_result" ==> vrf_cert
      , "block_body_size" ==> (VUInt `sized` (4 :: Word64))
      , "block_body_hash" ==> hash32
      , a operational_cert
      , a (protocol_version @DijkstraEra)
      ]

-- | Replaces required_signers (index 14) with guards supporting both keys and script credentials.
-- Ref: CIP-112
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
      , opt (idx 14 ==> guards)
      , opt (idx 15 ==> network_id)
      , opt (idx 16 ==> transaction_output)
      , opt (idx 17 ==> coin)
      , opt (idx 18 ==> maybe_tagged_nonempty_set transaction_input)
      , opt (idx 19 ==> voting_procedures)
      , opt (idx 20 ==> proposal_procedures)
      , opt (idx 21 ==> coin)
      , opt (idx 22 ==> positive_coin)
      , opt (idx 23 ==> sub_transactions)
      ]

sub_transactions :: Rule
sub_transactions =
  "sub_transactions" =:= maybe_tagged_nonempty_oset sub_transaction

sub_transaction :: Rule
sub_transaction =
  "sub_transaction"
    =:= arr
      [ a sub_transaction_body
      , a transaction_witness_set
      , a (auxiliary_data / VNil)
      ]

sub_transaction_body :: Rule
sub_transaction_body =
  "sub_transaction_body"
    =:= mp
      [ idx 0 ==> maybe_tagged_set transaction_input
      , idx 1 ==> arr [0 <+ a transaction_output]
      , opt (idx 3 ==> slot)
      , opt (idx 4 ==> certificates)
      , opt (idx 5 ==> withdrawals)
      , opt (idx 7 ==> auxiliary_data_hash)
      , opt (idx 8 ==> slot)
      , opt (idx 9 ==> mint)
      , opt (idx 11 ==> script_data_hash)
      , opt (idx 14 ==> guards)
      , opt (idx 15 ==> network_id)
      , opt (idx 18 ==> maybe_tagged_nonempty_set transaction_input)
      , opt (idx 19 ==> voting_procedures)
      , opt (idx 20 ==> proposal_procedures)
      , opt (idx 21 ==> coin)
      , opt (idx 22 ==> positive_coin)
      , opt (idx 24 ==> required_top_level_guards)
      ]

required_top_level_guards :: Rule
required_top_level_guards =
  "required_top_level_guards"
    =:= mp [1 <+ asKey credential ==> (plutus_data / VNil)]

guards :: Rule
guards =
  "guards"
    =:= maybe_tagged_nonempty_set addr_keyhash
    / maybe_tagged_nonempty_oset credential

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

parameter_change_action :: Named Group
parameter_change_action =
  "parameter_change_action"
    =:~ grp
      [ 0
      , a $ gov_action_id / VNil
      , a protocol_param_update
      , a $ policy_hash / VNil
      ]

-- | Uses dijkstra_script supporting Plutus V4 script references and dijkstra_native_script with guards.
transaction_output :: Rule
transaction_output =
  comment
    [str|Both of the Alonzo and Babbage style TxOut formats are equally valid
        |and can be used interchangeably
        |]
    $ "transaction_output"
      =:= shelley_transaction_output
      / babbage_transaction_output dijkstra_script

-- | Empty redeemers now use map format (A0) instead of array format (80).
-- TODO: Update comment with Plutus v4 when necessary
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

-- | Adds 5 reference script cost parameters (indices 33-37) for new pricing model.
-- TODO: adjust with new params
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
      , opt (idx 33 ==> nonnegative_interval) //- "minfee refScript coins per byte"
      , opt (idx 34 ==> (VUInt `sized` (4 :: Word64))) //- "max refScript size per block"
      , opt (idx 35 ==> (VUInt `sized` (4 :: Word64))) //- "max refScript size per tx"
      , opt (idx 36 ==> positive_word32) //- "refScript cost stride"
      , opt (idx 37 ==> positive_interval) //- "refScript cost multiplier"
      ]

-- | Uses map format only; adds guarding (tag 6) redeemer purpose for guard scripts.
-- Ref: CIP-112
redeemers :: Rule -> Rule
redeemers redeemer_tag =
  "redeemers"
    =:= mp
      [ 1
          <+ asKey
            ( arr
                [ "tag" ==> redeemer_tag
                , "index" ==> (VUInt `sized` (4 :: Word64))
                ]
            )
          ==> arr ["data" ==> plutus_data, "ex_units" ==> ex_units]
      ]

-- | Uses maybe_tagged_nonempty_set for all witness arrays and dijkstra_native_script with guard support.
-- TODO: add entry for Plutus v4
transaction_witness_set :: Rule
transaction_witness_set =
  "transaction_witness_set"
    =:= mp
      [ opt $ idx 0 ==> maybe_tagged_nonempty_set vkeywitness
      , opt $ idx 1 ==> maybe_tagged_nonempty_set dijkstra_native_script
      , opt $ idx 2 ==> maybe_tagged_nonempty_set bootstrap_witness
      , opt $ idx 3 ==> maybe_tagged_nonempty_set plutus_v1_script
      , opt $ idx 4 ==> maybe_tagged_nonempty_set plutus_data
      , opt $ idx 5 ==> redeemers dijkstra_redeemer_tag
      , opt $ idx 6 ==> maybe_tagged_nonempty_set plutus_v2_script
      , opt $ idx 7 ==> maybe_tagged_nonempty_set plutus_v3_script
      ]

-- TODO: adjust with new script purpose
dijkstra_redeemer_tag :: Rule
dijkstra_redeemer_tag =
  "redeemer_tag"
    =:= (int 0 //- "spend")
    / (int 1 //- "mint")
    / (int 2 //- "cert")
    / (int 3 //- "reward")
    / (int 4 //- "voting")
    / (int 5 //- "proposing")
    / (int 6 //- "guarding")

-- TODO: add Plutus V4
language :: Rule
language =
  "language"
    =:= int 0 -- Plutus v1
    / int 1 -- Plutus v2
    / int 2 -- Plutus v3

-- | Flexible schema supporting arbitrary language versions (keys 0-255) for future Plutus versions.
-- TODO: add entry for Plutus v4
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

plutus_v4_script :: Rule
plutus_v4_script =
  comment
    [str|Dijkstra introduces Plutus V4.
        |
        |Note: distinct VBytes ensures uniqueness in test generation.
        |]
    $ "plutus_v4_script" =:= distinct VBytes

script_require_guard :: Named Group
script_require_guard =
  comment
    [str|Dijkstra adds guard scripts for enhanced security.
        |A guard script requires a credential to authorize execution.
        |]
    $ "script_require_guard" =:~ grp [6, a credential]

dijkstra_native_script :: Rule
dijkstra_native_script =
  comment
    [str|Dijkstra native scripts extend Allegra's 6-variant format
        |with a 7th variant for guard scripts.
        |]
    $ "native_script"
      =:= arr [a script_pubkey]
      / arr [a script_all]
      / arr [a script_any]
      / arr [a script_n_of_k]
      / arr [a script_invalid_before]
      / arr [a script_invalid_hereafter]
      / arr [a script_require_guard]

script_pubkey :: Named Group
script_pubkey = mkScriptPubkey

script_all :: Named Group
script_all = mkScriptAll dijkstra_native_script

script_any :: Named Group
script_any = mkScriptAny dijkstra_native_script

script_n_of_k :: Named Group
script_n_of_k = mkScriptNOfK int64 dijkstra_native_script

script_invalid_before :: Named Group
script_invalid_before = mkScriptInvalidBefore

script_invalid_hereafter :: Named Group
script_invalid_hereafter = mkScriptInvalidHereafter

auxiliary_data_map :: Rule
auxiliary_data_map =
  "auxiliary_data_map"
    =:= tag
      259
      ( mp
          [ opt (idx 0 ==> metadata)
          , opt (idx 1 ==> arr [0 <+ a dijkstra_native_script])
          , opt (idx 2 ==> arr [0 <+ a plutus_v1_script])
          , opt (idx 3 ==> arr [0 <+ a plutus_v2_script])
          , opt (idx 4 ==> arr [0 <+ a plutus_v3_script])
          , opt (idx 5 ==> arr [0 <+ a plutus_v4_script])
          ]
      )

-- | Adds Plutus V4 scripts (index 5) and uses dijkstra_native_script with guard support (index 1).
auxiliary_data :: Rule
auxiliary_data =
  comment
    [str|auxiliary_data supports three serialization formats:
        |  1. metadata (raw) - Supported since Shelley
        |  2. auxiliary_data_array - Array format, introduced in Allegra
        |  3. auxiliary_data_map - Tagged map format, introduced in Alonzo
        |     Dijkstra adds plutus_v4_script support at index 5
        |]
    $ "auxiliary_data"
      =:= metadata
      / auxiliary_data_array
      / auxiliary_data_map

dijkstra_script :: Rule
dijkstra_script =
  comment
    [str|Dijkstra supports five script types:
        |  0: Native scripts with guard support (7 variants)
        |  1: Plutus V1 scripts
        |  2: Plutus V2 scripts
        |  3: Plutus V3 scripts
        |  4: Plutus V4 scripts (NEW)
        |]
    $ "script"
      =:= arr [0, a dijkstra_native_script]
      / arr [1, a plutus_v1_script]
      / arr [2, a plutus_v2_script]
      / arr [3, a plutus_v3_script]
      / arr [4, a plutus_v4_script]
