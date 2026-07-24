{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Common CDDL Definitions
module Cardano.Ledger.CanonicalState.Namespace.CDDL.Common where

import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Int (Int64)
import Data.Word
import GHC.Integer (Integer)
import GHC.Real (Integral (toInteger))
-- after drop of the GHC-9.10 we can switch to Data.Bounded
import Text.Heredoc
import Prelude (Bounded (..))

--------------------------------------------------------------------------------
-- Coins and Assets
--------------------------------------------------------------------------------

coin :: Rule
coin = "coin" =:= VUInt

positive_coin :: Rule
positive_coin =
  "positive_coin"
    =:= (1 :: Integer)
    ... toInteger (maxBound @Word64)

policy_id :: Rule
policy_id = "policy_id" =:= hash28

asset_name :: Rule
asset_name = "asset_name" =:= VBytes `sized` (0 :: Word64, 32 :: Word64)

multiasset :: IsType0 a => a -> GRuleCall
multiasset = binding $ \x ->
  "multiasset"
    =:= mp [0 <+ asKey policy_id ==> mp [1 <+ asKey asset_name ==> x]]

value :: Rule
value = "value" =:= coin / sarr [a coin, a (multiasset positive_coin)]

--------------------------------------------------------------------------------
-- Slots and Blocks
--------------------------------------------------------------------------------

slot_no :: Rule
slot_no = "slot_no" =:= VUInt `sized` (8 :: Word)

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

address :: Rule
address = "address" =:= VBytes

--------------------------------------------------------------------------------
-- Crypto
--------------------------------------------------------------------------------

keyhash32 :: Rule
keyhash32 = "keyhash32" =:= hash32

keyhash28 :: Rule
keyhash28 = "keyhash28" =:= hash28

hash28 :: Rule
hash28 = "hash28" =:= VBytes `sized` (28 :: Word64)

hash32 :: Rule
hash32 = "hash32" =:= VBytes `sized` (32 :: Word64)

vkey :: Rule
vkey = "vkey" =:= VBytes `sized` (32 :: Word64)

vrf_vkey :: Rule
vrf_vkey = "vrf_vkey" =:= VBytes `sized` (32 :: Word64)

vrf_keyhash :: Rule
vrf_keyhash = "vrf_keyhash" =:= hash32

vrf_cert :: Rule
vrf_cert = "vrf_cert" =:= arr [a VBytes, a (VBytes `sized` (80 :: Word64))]

kes_vkey :: Rule
kes_vkey = "kes_vkey" =:= VBytes `sized` (32 :: Word64)

kes_signature :: Rule
kes_signature = "kes_signature" =:= VBytes `sized` (448 :: Word64)

signkeyKES :: Rule
signkeyKES = "signkeyKES" =:= VBytes `sized` (64 :: Word64)

signature :: Rule
signature = "signature" =:= VBytes `sized` (64 :: Word64)

-------------------------------------------------------------------------------
-- Numbers
--------------------------------------------------------------------------------

big_int :: Rule
big_int = "big_int" =:= VInt / big_uint / big_nint

big_uint :: Rule
big_uint = "big_uint" =:= tag 2 bounded_bytes

big_nint :: Rule
big_nint = "big_nint" =:= tag 3 bounded_bytes

int64 :: Rule
int64 = "int64" =:= toInteger (minBound @Int64) ... toInteger (maxBound @Int64)

word64 :: Rule
word64 = "word64" =:= VUInt `sized` (8 :: Word)

-------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

bounded_bytes :: Rule
bounded_bytes =
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
    $ "bounded_bytes"
      =:= VBytes
      `sized` (0 :: Word64, 64 :: Word64)

url :: Rule
url = "url" =:= VText `sized` (0 :: Word64, 128 :: Word64)

dns_name :: Rule
dns_name = "dns_name" =:= VText `sized` (0 :: Word64, 128 :: Word64)

port :: Rule
port = "port" =:= VUInt `le` 65535

ipv4 :: Rule
ipv4 = "ipv4" =:= VBytes `sized` (4 :: Word64)

ipv6 :: Rule
ipv6 = "ipv6" =:= VBytes `sized` (16 :: Word64)

unit_interval :: Rule
unit_interval =
  comment
    [str|NOTE: The real unit_interval is: #6.30([uint, uint])
        |
        | A unit interval is a number in the range between 0 and 1, which
        | means there are two extra constraints:
        |    1. numerator <= denominator
        |    2. denominator > 0
        |]
    $ "unit_interval"
      =:= tag
        30
        ( arr
            [ a (VUInt `le` (maxBound :: Word64))
            , a (VUInt `le` (maxBound :: Word64))
            ]
        )

set :: IsType0 t0 => t0 -> GRuleCall
set = binding $ \x -> "set" =:= tag 258 (arr [0 <+ a x])

-- | nonnegative_interval = tag 30 [uint, positive_int]
nonnegative_interval :: Rule
nonnegative_interval = "nonnegative_interval" =:= tag 30 (arr [a VUInt, a positive_int])

positive_int :: Rule
positive_int = "positive_int" =:= (1 :: Integer) ... maxWord64

maxWord64 :: Integer
maxWord64 = 18446744073709551615

credential :: Rule
credential =
  "credential"
    =:= arr [0, a addr_keyhash]
    / arr [1, a script_hash]

addr_keyhash :: Rule
addr_keyhash = "addr_keyhash" =:= hash28

script_hash :: Rule
script_hash =
  comment
    [str| To compute a script hash, note that you must prepend
        | a tag to the bytes of the script before hashing.
        | The tag is determined by the language.
        | The tags in the Conway era are:
        |  - "\x00" for multisig scripts
        |  - "\x01" for Plutus V1 scripts
        |  - "\x02" for Plutus V2 scripts
        |  - "\x03" for Plutus V3 scripts
    |]
    $ "script_hash" =:= hash28

anchor :: Rule
anchor =
  comment
    [str|
    | Signed url
    |]
    $ "anchor"
      =:= arr
        [ "anchor_url" ==> url
        , "anchor_data_hash" ==> VBytes
        ]

epoch_no :: Rule
epoch_no = "epoch_no" =:= VUInt `sized` (8 :: Word64)

pool_keyhash :: Rule
pool_keyhash = "pool_keyhash" =:= hash28

reward_account :: Rule
reward_account =
  comment
    [str| 28 bytes hash and one byte for the network type |]
    $ "reward_account" =:= VBytes `sized` (29 :: Word64)

protocol_version :: Rule
protocol_version = "protocol_version" =:= arr [a major_protocol_version, a VUInt]

major_protocol_version :: Rule
major_protocol_version = "major_protocol_version" =:= VUInt

gov_action_id :: Rule
gov_action_id =
  "gov_action_id"
    =:= arr ["transaction_id" ==> hash32, "gov_action_index" ==> VUInt `sized` (2 :: Word64)]

staking_keyhash :: Rule
staking_keyhash = "staking_keyhash" =:= hash28

account_id :: Rule
account_id = "account_id" =:= credential

relay :: Rule
relay =
  "relay"
    =:= arr
      [ 0
      , a (port / VNil)
      , a (ipv4 / VNil)
      , a (ipv6 / VNil)
      ]
    / arr [1, a (port / VNil), a dns_name]
    / arr [2, a dns_name]

pool_metadata :: Rule
pool_metadata =
  "pool_metadata"
    =:= arr
      [a url, a VBytes]
