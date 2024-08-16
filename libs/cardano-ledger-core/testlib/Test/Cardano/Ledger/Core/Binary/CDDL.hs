{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Evaluate" #-}

module Test.Cardano.Ledger.Core.Binary.CDDL where

import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.String.Here (here)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Show (Show (show))
import Prelude (Integer)

--------------------------------------------------------------------------------
-- Base Types
--------------------------------------------------------------------------------
coin :: Rule
coin = "coin" =:= VUInt

positive_coin :: Rule
positive_coin = "positive_coin" =:= 1 ... maxWord64

address :: Rule
address =
  comment
    [here|
    address = bytes
    reward_account = bytes

    address format:
    [ 8 bit header | payload ];

    shelley payment addresses:
    bit 7: 0
    bit 6: base/other
    bit 5: pointer/enterprise [for base: stake cred is keyhash/scripthash]
    bit 4: payment cred is keyhash/scripthash
    bits 3-0: network id

    reward addresses:
    bits 7-5: 111
    bit 4: credential is keyhash/scripthash
    bits 3-0: network id

    byron addresses:
    bits 7-4: 1000

    0000: base address: keyhash28,keyhash28
    0001: base address: scripthash28,keyhash28
    0010: base address: keyhash28,scripthash28
    0011: base address: scripthash28,scripthash28
    0100: pointer address: keyhash28, 3 variable length uint
    0101: pointer address: scripthash28, 3 variable length uint
    0110: enterprise address: keyhash28
    0111: enterprise address: scripthash28
    1000: byron address
    1110: reward account: keyhash28
    1111: reward account: scripthash28
    1001 - 1101: future formats
  |]
    $ "address"
      =:= bstr
        "001000000000000000000000000000000000000000000000000000000011000000000000000000000000000000000000000000000000000000"
      / bstr
        "102000000000000000000000000000000000000000000000000000000022000000000000000000000000000000000000000000000000000000"
      / bstr
        "203000000000000000000000000000000000000000000000000000000033000000000000000000000000000000000000000000000000000000"
      / bstr
        "304000000000000000000000000000000000000000000000000000000044000000000000000000000000000000000000000000000000000000"
      / bstr "405000000000000000000000000000000000000000000000000000000087680203"
      / bstr "506000000000000000000000000000000000000000000000000000000087680203"
      / bstr "6070000000000000000000000000000000000000000000000000000000"
      / bstr "7080000000000000000000000000000000000000000000000000000000"

reward_account :: Rule
reward_account =
  "reward_account"
    =:= bstr "E090000000000000000000000000000000000000000000000000000000"
    / bstr "F0A0000000000000000000000000000000000000000000000000000000"

addr_keyhash :: Rule
addr_keyhash = "addr_keyhash" =:= hash28

pool_keyhash :: Rule
pool_keyhash = "pool_keyhash" =:= hash28

vrf_keyhash :: Rule
vrf_keyhash = "vrf_keyhash" =:= hash32

--------------------------------------------------------------------------------
-- Crypto
--------------------------------------------------------------------------------

hash28 :: Rule
hash28 = "$hash28" =:= VBytes `sized` (28 :: Word64)

hash32 :: Rule
hash32 = "$hash32" =:= VBytes `sized` (32 :: Word64)

vkey :: Rule
vkey = "$vkey" =:= VBytes `sized` (32 :: Word64)

vrf_vkey :: Rule
vrf_vkey = "$vrf_vkey" =:= VBytes `sized` (32 :: Word64)

vrf_cert :: Rule
vrf_cert = "$vrf_cert" =:= arr [a VBytes, a (VBytes `sized` (80 :: Word64))]

kes_vkey :: Rule
kes_vkey = "$kes_vkey" =:= VBytes `sized` (32 :: Word64)

kes_signature :: Rule
kes_signature = "$kes_signature" =:= VBytes `sized` (448 :: Word64)

signkeyKES :: Rule
signkeyKES = "signkeyKES" =:= VBytes `sized` (64 :: Word64)

signature :: Rule
signature = "$signature" =:= VBytes `sized` (64 :: Word64)

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

big_int :: Rule
big_int = "big_int" =:= VInt / big_uint / big_nint

big_uint :: Rule
big_uint = "big_uint" =:= tag 2 bounded_bytes

big_nint :: Rule
big_nint = "big_nint" =:= tag 3 bounded_bytes

-- Once https://github.com/input-output-hk/cuddle/issues/29 is in place, replace
-- with:
--
-- minInt64 :: Rule
-- minInt64 = "minInt64" =:= -9223372036854775808
minInt64 :: Integer
minInt64 = -9223372036854775808

-- Once https://github.com/input-output-hk/cuddle/issues/29 is in place, replace
-- with:
--
-- maxInt64 :: Rule
-- maxInt64 = "maxInt64" =:= 9223372036854775807
maxInt64 :: Integer
maxInt64 = 9223372036854775807

-- Once https://github.com/input-output-hk/cuddle/issues/29 is in place, replace
-- with:
--
-- maxWord64 :: Rule
-- maxWord64 = "maxWord64" =:= 18446744073709551615
maxWord64 :: Integer
maxWord64 = 18446744073709551615

negInt64 :: Rule
negInt64 = "negInt64" =:= minInt64 ... (-1)

posInt64 :: Rule
posInt64 = "posInt64" =:= 1 ... maxInt64

nonZeroInt64 :: Rule
nonZeroInt64 = "nonZeroInt64" =:= negInt64 / posInt64 -- this is the same as the current int64 definition but without zero

int64 :: Rule
int64 = "int64" =:= minInt64 ... maxInt64

positive_int :: Rule
positive_int = "positive_int" =:= 1 ... 18446744073709551615

unit_interval :: Rule
unit_interval = "unit_interval" =:= tag 30 (arr [1, 2])

-- unit_interval = tag 0 [uint, uint]
--
-- Comment above depicts the actual definition for `unit_interval`.
--
-- Unit interval is a number in the range between 0 and 1, which
-- means there are two extra constraints:
-- \* numerator <= denominator
-- \* denominator > 0
--
-- Relation between numerator and denominator cannot be expressed in CDDL, which
-- poses a problem for testing. We need to be able to generate random valid data
-- for testing implementation of our encoders/decoders. Which means we cannot use
-- the actual definition here and we hard code the value to 1/2

-- nonnegative_interval = tag 0 [uint, positive_int]
nonnegative_interval :: Rule
nonnegative_interval = "nonnegative_interval" =:= tag 30 (arr [a VUInt, a positive_int])

bounded_bytes :: Rule
bounded_bytes = "bounded_bytes" =:= VBytes `sized` (0 :: Word64, 64 :: Word64)

-- the real bounded_bytes does not have this limit. it instead has a different
-- limit which cannot be expressed in CDDL.
-- The limit is as follows:
--  - bytes with a definite-length encoding are limited to size 0..64
--  - for bytes with an indefinite-length CBOR encoding, each chunk is
--    limited to size 0..64
--  ( reminder: in CBOR, the indefinite-length encoding of bytestrings
--    consists of a token #2.31 followed by a sequence of definite-length
--    encoded bytestrings and a stop code )

-- a type for distinct values.
-- The type parameter must support .size, for example: bytes or uint
distinct :: IsSizeable s => Value s -> Rule
distinct x =
  "distinct_"
    <> T.pack (show x)
      =:= (x `sized` (8 :: Word64))
      / (x `sized` (16 :: Word64))
      / (x `sized` (20 :: Word64))
      / (x `sized` (24 :: Word64))
      / (x `sized` (30 :: Word64))
      / (x `sized` (32 :: Word64))
