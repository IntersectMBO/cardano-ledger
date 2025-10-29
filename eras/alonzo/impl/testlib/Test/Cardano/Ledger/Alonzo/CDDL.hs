{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Evaluate" -}

module Test.Cardano.Ledger.Alonzo.CDDL (
  module Test.Cardano.Ledger.Mary.CDDL,
  alonzoCDDL,
  certificates,
  auxiliary_data_hash,
  required_signers,
  network_id,
  native_script,
  redeemers,
  constr,
  ex_unit_prices,
  ex_units,
  positive_interval,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Word (Word64)
import Test.Cardano.Ledger.Mary.CDDL hiding (
  auxiliary_data,
  header,
  transaction_witness_set,
  update,
 )
import Text.Heredoc

alonzoCDDL :: Huddle
alonzoCDDL =
  collectFrom
    [ HIRule block
    , HIRule transaction
    , HIRule kes_signature
    , HIRule language
    , HIRule signkey_kes
    ]

block :: Rule
block =
  comment
    [str|Valid blocks must also satisfy the following two constraints:
        |  1) the length of transaction_bodies and transaction_witness_sets must be
        |     the same
        |  2) every transaction_index must be strictly smaller than the length of
        |     transaction_bodies
        |NEW:
        |  invalid_transactions
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

transaction_body :: Rule
transaction_body =
  comment
    [str| 2: fee
        | 3: time to live
        | 8: validity interval start
        |13: collateral
        |NEW:
        |  11: script_data_hash
        |  13: set transaction_input
        |  14: required_signers
        |  15: network_id
        |]
    $ "transaction_body"
      =:= mp
        [ idx 0 ==> untagged_set transaction_input
        , idx 1 ==> arr [0 <+ a transaction_output]
        , idx 2 ==> coin
        , opt (idx 3 ==> slot)
        , opt (idx 4 ==> arr [0 <+ a certificate])
        , opt (idx 5 ==> withdrawals)
        , opt (idx 6 ==> update)
        , opt (idx 7 ==> auxiliary_data_hash)
        , opt (idx 8 ==> slot)
        , opt (idx 9 ==> mint)
        , opt (idx 11 ==> script_data_hash)
        , opt (idx 13 ==> untagged_set transaction_input)
        , opt (idx 14 ==> required_signers)
        , opt (idx 15 ==> network_id)
        ]

required_signers :: Rule
required_signers = "required_signers" =:= untagged_set addr_keyhash

transaction_output :: Rule
transaction_output =
  comment
    [str|NEW:
        |  datum_hash: $hash32
        |]
    $ "transaction_output"
      =:= arr [a address, "amount" ==> value, opt ("datum_hash" ==> hash32)]

script_data_hash :: Rule
script_data_hash =
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
        |apologies).,
        |
        |NEW:
        |  script_data_hash
        |]
    $ "script_data_hash" =:= hash32

certificates :: Rule
certificates = "certificates" =:= arr [0 <+ a certificate]

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
        |12: d. decentralization constant
        |13: extra entropy
        |14: protocol version
        |16: min pool cost ; NEW
        |17: ada per utxo byte ; NEW
        |18: cost models for script languages ; NEW
        |19: execution costs ; NEW
        |20: max tx ex units ; NEW
        |21: max block ex units ; NEW
        |22: max value size ; NEW
        |23: collateral percentage ; NEW
        |24: max collateral inputs ; NEW
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
        , opt (idx 12 ==> unit_interval)
        , opt (idx 13 ==> nonce)
        , opt (idx 14 ==> arr [a (protocol_version @AlonzoEra)])
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

transaction_witness_set :: Rule
transaction_witness_set =
  comment
    [str|
        |NEW:
        |  3: [* plutus_script ]
        |  4: [* plutus_data ]
        |  5: redeemers
        |]
    $ "transaction_witness_set"
      =:= mp
        [ opt $ idx 0 ==> arr [0 <+ a vkeywitness]
        , opt $ idx 1 ==> arr [0 <+ a native_script]
        , opt $ idx 2 ==> arr [0 <+ a bootstrap_witness]
        , opt $ idx 3 ==> arr [0 <+ a plutus_script]
        , opt $ idx 4 ==> arr [0 <+ a plutus_data]
        , opt $ idx 5 ==> redeemers
        ]

redeemers :: Rule
redeemers = "redeemers" =:= arr [0 <+ a redeemer]

plutus_script :: Rule
plutus_script = "plutus_script" =:= VBytes

plutus_data :: Rule
plutus_data =
  comment [str|NEW|] $
    "plutus_data"
      =:= constr plutus_data
      / smp [0 <+ asKey plutus_data ==> plutus_data]
      / sarr [0 <+ a plutus_data]
      / big_int
      / bounded_bytes

-- FIXME: `GRuleCall` does not serialise the comment in the resulting CDDL
constr :: IsType0 x => x -> GRuleCall
constr = binding $ \x ->
  comment
    [str|NEW
        |  #6.102([uint, [* a]]): For tag range 6.1280 .. 6.1400 inclusive
        |]
    $ "constr"
      =:= tag 121 (arr [0 <+ a x])
      / tag 122 (arr [0 <+ a x])
      / tag 123 (arr [0 <+ a x])
      / tag 124 (arr [0 <+ a x])
      / tag 125 (arr [0 <+ a x])
      / tag 126 (arr [0 <+ a x])
      / tag 127 (arr [0 <+ a x])
      / tag 102 (arr [a VUInt, a $ arr [0 <+ a x]])

redeemer :: Rule
redeemer =
  comment [str|NEW|] $
    "redeemer"
      =:= arr
        [ "tag" ==> redeemer_tag
        , "index" ==> VUInt
        , "data" ==> plutus_data
        , "ex_units" ==> ex_units
        ]

redeemer_tag :: Rule
redeemer_tag =
  comment
    [str|0: spend
        |1: mint
        |2: cert
        |3: reward
        |]
    $ "redeemer_tag" =:= int 0 / int 1 / int 2 / int 3

ex_units :: Rule
ex_units = "ex_units" =:= arr ["mem" ==> VUInt, "steps" ==> VUInt]

ex_unit_prices :: Rule
ex_unit_prices =
  "ex_unit_prices"
    =:= arr
      [ "mem_price" ==> positive_interval
      , "step_price" ==> positive_interval
      ]

language :: Rule
language =
  comment
    [str|NOTE: NEW
        |  This is an enumeration. for now there's only one value. Plutus V1
        |]
    $ "language" =:= int 0

cost_models :: Rule
cost_models = "cost_models" =:= mp [0 <+ asKey language ==> cost_model]

cost_model :: Rule
cost_model =
  comment
    [str|NOTE: NEW
        |  The keys to the cost model map are not present in the serialization.
        |  The values in the serialization are assumed to be ordered
        |  lexicographically by their correpsonding key value.
        |  See Plutus' `ParamName` for parameter ordering
        |]
    $ "cost_model" =:= arr [166 <+ a int64 +> 166]

auxiliary_data :: Rule
auxiliary_data =
  comment
    [str|            metadata: shelley
        |transaction_metadata: shelley-ma
        |NEW
        |  #6.259(0 ==> metadata): alonzo onwards
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
            , opt (idx 2 ==> arr [0 <+ a plutus_script])
            ]
        )

header :: Rule
header = "header" =:= arr [a header_body, "body_signature" ==> kes_signature]

header_body :: Rule
header_body =
  comment
    [str| block_body_size: merkle triple root
        |]
    $ "header_body"
      =:= arr
        [ "block_number" ==> block_number
        , "slot" ==> slot
        , "prev_hash" ==> (hash32 / VNil)
        , "issuer_vkey" ==> vkey
        , "vrf_vkey" ==> vrf_vkey
        , "nonce_vrf" ==> vrf_cert
        , "leader_vrf" ==> vrf_cert
        , "block_body_size" ==> VUInt
        , "block_body_hash" ==> hash32
        , a operational_cert
        , a (protocol_version @AlonzoEra)
        ]

native_script :: Rule
native_script =
  comment
    [str|Timelock validity intervals are half-open intervals [a, b).
        |
        |  invalid_before:
        |    specifies the left (included) endpoint a.
        |
        |  invalid_hereafter:
        |    specifies the right (excluded) endpoint b.
        |]
    $ "native_script"
      =:= arr [a script_pubkey]
      / arr [a script_all]
      / arr [a script_any]
      / arr [a script_n_of_k]
      / arr [a invalid_before]
      / arr [a invalid_hereafter]

script_n_of_k :: Named Group
script_n_of_k = "script_n_of_k" =:~ grp [3, "n" ==> VUInt, a (arr [0 <+ a native_script])]

positive_interval :: Rule
positive_interval = "positive_interval" =:= tag 30 (arr [a positive_int, a positive_int])

network_id :: Rule
network_id = "network_id" =:= int 0 / int 1

auxiliary_data_hash :: Rule
auxiliary_data_hash = "auxiliary_data_hash" =:= hash32
