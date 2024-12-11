{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Evaluate" #-}

module Test.Cardano.Ledger.Babbage.CDDL (
  module Test.Cardano.Ledger.Alonzo.CDDL,
  module Test.Cardano.Ledger.Babbage.CDDL,
) where

import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Word (Word64)
import GHC.Num (Integer)
import Test.Cardano.Ledger.Alonzo.CDDL hiding (
  auxiliary_data,
  block,
  cost_models,
  header,
  header_body,
  language,
  major_protocol_version,
  multiasset,
  next_major_protocol_version,
  plutus_data,
  plutus_script,
  proposed_protocol_parameter_updates,
  protocol_param_update,
  protocol_version,
  script_data_hash,
  script_n_of_k,
  transaction,
  transaction_body,
  transaction_output,
  transaction_witness_set,
  update,
 )
import Text.Heredoc

babbageCDDL :: Huddle
babbageCDDL = collectFrom [block, transaction, kes_signature, language, signkeyKES]

block :: Rule
block =
  comment
    [str|Valid blocks must also satisfy the following two constraints:
        |  1) the length of transaction_bodies and transaction_witness_sets must be
        |     the same
        |  2) every transaction_index must be strictly smaller than the length of
        |     transaction_bodies
        |]
    $ "block"
      =:= arr
        [ a header
        , "transaction_bodies" ==> arr [0 <+ a transaction_body]
        , "transaction_witness_sets" ==> arr [0 <+ a transaction_witness_set]
        , "auxiliary_data_set" ==> mp [0 <+ asKey transaction_index ==> auxiliary_data]
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
header = "header" =:= arr [a header_body, "body_signature" ==> kes_signature]

header_body :: Rule
header_body =
  comment
    [str| block_body_size: merkle triple root
        |      vrf_result: NEW, replaces nonce_vrf and leader_vrf
        |]
    $ "header_body"
      =:= arr
        [ "block_number" ==> VUInt
        , "slot" ==> VUInt
        , "prev_hash" ==> (hash32 / VNil)
        , "issuer_vkey" ==> vkey
        , "vrf_vkey" ==> vrf_vkey
        , "vrf_result" ==> vrf_cert
        , "block_body_size" ==> VUInt
        , "block_body_hash" ==> hash32
        , a operational_cert
        , a protocol_version
        ]

-- TODO Replace with the following once
-- https://github.com/input-output-hk/cuddle/issues/29 is addressed in cuddle.
--
-- next_major_protocol_version :: Rule
-- next_major_protocol_version = "next_major_protocol_version" =:= (7 :: Integer)
next_major_protocol_version :: Integer
next_major_protocol_version = 9

major_protocol_version :: Rule
major_protocol_version = "major_protocol_version" =:= (1 :: Integer) ... next_major_protocol_version

protocol_version :: Rule
protocol_version = "protocol_version" =:= arr [a major_protocol_version, a VUInt]

transaction_body :: Rule
transaction_body =
  comment
    [str| 2: fee
        | 3: time to live
        | 8: validity interval start
        |13: collateral inputs
        |NEW: 
        |  16: collateral return
        |  17: total collateral
        |  18: reference inputs
        |]
    $ "transaction_body"
      =:= mp
        [ idx 0 ==> set transaction_input
        , idx 1 ==> arr [0 <+ a transaction_output]
        , idx 2 ==> coin
        , opt (idx 3 ==> VUInt)
        , opt (idx 4 ==> arr [0 <+ a certificate])
        , opt (idx 5 ==> withdrawals)
        , opt (idx 6 ==> update)
        , opt (idx 7 ==> auxiliary_data_hash)
        , opt (idx 8 ==> VUInt)
        , opt (idx 9 ==> mint)
        , opt (idx 11 ==> script_data_hash)
        , opt (idx 13 ==> set transaction_input)
        , opt (idx 14 ==> required_signers)
        , opt (idx 15 ==> network_id)
        , opt (idx 16 ==> transaction_output)
        , opt (idx 17 ==> coin)
        , opt (idx 18 ==> set transaction_input)
        ]

-- TODO: Allow for adding to the comments of a Rule in order to not have to
-- redifine them in the subsequent eras.
script_data_hash :: Rule
script_data_hash =
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
    $ "script_data_hash" =:= hash32

transaction_output :: Rule
transaction_output =
  comment
    [str|Both of the Alonzo and Babbage style TxOut formats are equally valid
        |and can be used interchangeably.
        |NEW:
        |  babbage_transaction_output
        |]
    $ "transaction_output"
      =:= shelley_transaction_output
      / babbage_transaction_output

-- TODO: Make it possible to override names in Cuddle to obviate the need for
-- this redefinition here.
shelley_transaction_output :: Rule
shelley_transaction_output =
  "shelley_transaction_output"
    =:= arr [a address, "amount" ==> value, opt ("datum_hash" ==> hash32)]

babbage_transaction_output :: Rule
babbage_transaction_output =
  comment
    [str|NEW
        |  datum_option
        |  script_ref
        |]
    $ "babbage_transaction_output"
      =:= mp
        [ idx 0 ==> address
        , idx 1 ==> value
        , opt $ idx 2 ==> datum_option
        , opt $ idx 3 ==> script_ref
        ]

transaction_witness_set :: Rule
transaction_witness_set =
  comment
    [str|
        |NEW: 
        |  6: [* plutus_v2_script]
        |]
    $ "transaction_witness_set"
      =:= mp
        [ opt $ idx 0 ==> arr [0 <+ a vkeywitness]
        , opt $ idx 1 ==> arr [0 <+ a native_script]
        , opt $ idx 2 ==> arr [0 <+ a bootstrap_witness]
        , opt $ idx 3 ==> arr [0 <+ a plutus_v1_script]
        , opt $ idx 4 ==> arr [0 <+ a plutus_data]
        , opt $ idx 5 ==> redeemers
        , opt $ idx 6 ==> arr [0 <+ a plutus_v2_script]
        ]

-- TODO Allow to override the comment in addition to extending it.
plutus_data :: Rule
plutus_data =
  "plutus_data"
    =:= constr plutus_data
    / smp [0 <+ asKey plutus_data ==> plutus_data]
    / sarr [0 <+ a plutus_data]
    / big_int
    / bounded_bytes

plutus_v1_script :: Rule
plutus_v1_script = "plutus_v1_script" =:= VBytes

plutus_v2_script :: Rule
plutus_v2_script = "plutus_v2_script" =:= VBytes

language :: Rule
language =
  comment
    [str|0: Plutus v1
        |1: Plutus v2
        |]
    $ "language" =:= int 0 / int 1

cost_models :: Rule
cost_models =
  comment
    [str|0: Plutus v1
        |1: Plutus v2
        |]
    $ "cost_models"
      =:= mp
        [ opt $ idx 0 ==> arr [166 <+ a int64 +> 166]
        , opt $ idx 1 ==> arr [175 <+ a int64 +> 175]
        ]

auxiliary_data :: Rule
auxiliary_data =
  comment
    [str|              metadata: shelley
        |  transaction_metadata: shelley-ma
        |#6.259(0 ==> metadata): alonzo onwards
        |NEW:
        |  3: [* plutus_v2_script]
        |]
    $ "auxiliary_data"
      =:= metadata
      / sarr
        [ "transaction_metadata" ==> metadata
        , "auxiliary_scripts" ==> auxiliary_scripts
        ]
      / tag
        259
        ( mp
            [ opt (idx 0 ==> metadata)
            , opt (idx 1 ==> arr [0 <+ a native_script])
            , opt (idx 2 ==> arr [0 <+ a plutus_v1_script])
            , opt (idx 3 ==> arr [0 <+ a plutus_v2_script])
            ]
        )

data' :: Rule
data' = "data" =:= tag 24 (VBytes `cbor` plutus_data)

datum_option :: Rule
datum_option = "datum_option" =:= arr [0, a hash32] / arr [1, a data']

script_ref :: Rule
script_ref = "script_ref" =:= tag 24 (VBytes `cbor` script)

script :: Rule
script =
  "script"
    =:= arr [0, a native_script]
    / arr [1, a plutus_v1_script]
    / arr [2, a plutus_v2_script]

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
        |14: protocol version
        |16: min pool cost
        |17: ada per utxo byte
        |18: cost models for script languages
        |19: execution costs
        |20: max tx ex units
        |21: max block ex units
        |22: max value size
        |23: collateral percentage
        |24: max collateral inputs
        |]
    $ "protocol_param_update"
      =:= mp
        [ opt (idx 0 ==> VUInt)
        , opt (idx 1 ==> VUInt)
        , opt (idx 2 ==> (VUInt `sized` (4 :: Word64)))
        , opt (idx 3 ==> (VUInt `sized` (4 :: Word64)))
        , opt (idx 4 ==> (VUInt `sized` (2 :: Word64)))
        , opt (idx 5 ==> coin)
        , opt (idx 6 ==> coin)
        , opt (idx 7 ==> epoch)
        , opt (idx 8 ==> VUInt `sized` (2 :: Word64))
        , opt (idx 9 ==> nonnegative_interval)
        , opt (idx 10 ==> unit_interval)
        , opt (idx 11 ==> unit_interval)
        , opt (idx 14 ==> protocol_version)
        , opt (idx 16 ==> coin)
        , opt (idx 17 ==> coin)
        , opt (idx 18 ==> cost_models)
        , opt (idx 19 ==> ex_unit_prices)
        , opt (idx 20 ==> ex_units)
        , opt (idx 21 ==> ex_units)
        , opt (idx 22 ==> VUInt)
        , opt (idx 23 ==> VUInt)
        , opt (idx 24 ==> VUInt)
        ]

proposed_protocol_parameter_updates :: Rule
proposed_protocol_parameter_updates =
  "proposed_protocol_parameter_updates"
    =:= mp [0 <+ asKey genesis_hash ==> protocol_param_update]

update :: Rule
update = "update" =:= arr [a proposed_protocol_parameter_updates, a epoch]
