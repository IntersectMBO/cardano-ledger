{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Evaluate" -}

module Test.Cardano.Ledger.Core.Binary.CDDL (
  -- * Base sized bytes
  hash28,
  hash32,
  hash64,
  bytes80,

  -- * Numbers
  positive_int,
  max_word32,
  positive_word32,

  -- * Unit intervals
  unit_interval,
  nonnegative_interval,

  -- * Distinct uint/bytes
  distinct,

  -- * Sets
  untagged_set,
  untagged_nonempty_set,

  -- * Network
  nonce,
  epoch,
  epoch_interval,
  slot,
  block_number,

  -- * Credentials
  credential,
  stake_credential,

  -- * Pool primitives
  port,
  ipv4,
  ipv6,
  dns_name64,
  url64,
  single_host_addr,
  mkPoolRules,

  -- * Hashes
  addr_keyhash,
  pool_keyhash,
  vrf_keyhash,
  vkey,
  vrf_vkey,
  vrf_cert,
  kes_vkey,
  kes_signature,
  signkey_kes,
  signature,

  -- * Value
  coin,
  positive_coin,

  -- * Addresses and accounts
  address,
  reward_account,

  -- * Protocol version

  -- * Transactions
  transaction_index,

  -- * Metadata and Auxiliary Data
  metadatum_label,
  metadatum,
  metadata,
  auxiliary_data_hash,

  -- * Scripts
  script_hash,
  mkScriptPubkey,
  mkScriptAll,
  mkScriptAny,
  mkScriptNOfK,
  mkScriptInvalidBefore,
  mkScriptInvalidHereafter,

  -- * Misc.
) where

import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Base (error)
import GHC.Show (Show (show))
import Text.Heredoc
import Prelude (Integer)

coin :: Rule
coin = "coin" =:= VUInt

positive_coin :: Rule
positive_coin = "positive_coin" =:= (1 :: Integer) ... max_word64

address :: Rule
address =
  comment
    [str|address = bytes
        |
        |address format:
        |  [ 8 bit header | payload ];
        |
        |shelley payment addresses:
        |     bit 7: 0
        |     bit 6: base/other
        |     bit 5: pointer/enterprise [for base: stake cred is keyhash/scripthash]
        |     bit 4: payment cred is keyhash/scripthash
        |  bits 3-0: network id
        |
        |reward addresses:
        |  bits 7-5: 111
        |     bit 4: credential is keyhash/scripthash
        |  bits 3-0: network id
        |
        |byron addresses:
        |  bits 7-4: 1000
        |
        |     0000: base address: keyhash28,keyhash28
        |     0001: base address: scripthash28,keyhash28
        |     0010: base address: keyhash28,scripthash28
        |     0011: base address: scripthash28,scripthash28
        |     0100: pointer address: keyhash28, 3 variable length uint
        |     0101: pointer address: scripthash28, 3 variable length uint
        |     0110: enterprise address: keyhash28
        |     0111: enterprise address: scripthash28
        |     1000: byron address
        |     1110: reward account: keyhash28
        |     1111: reward account: scripthash28
        |1001-1101: future formats
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
  comment
    [str|reward_account = bytes
        |]
    $ "reward_account"
      =:= bstr "E090000000000000000000000000000000000000000000000000000000"
      / bstr "F0A0000000000000000000000000000000000000000000000000000000"

addr_keyhash :: Rule
addr_keyhash = "addr_keyhash" =:= hash28

pool_keyhash :: Rule
pool_keyhash = "pool_keyhash" =:= hash28

vrf_keyhash :: Rule
vrf_keyhash = "vrf_keyhash" =:= hash32

mkHashSized :: Word64 -> Rule
mkHashSized size = "hash" <> T.pack (show size) =:= VBytes `sized` size

hash28 :: Rule
hash28 = mkHashSized 28

hash32 :: Rule
hash32 = mkHashSized 32

hash64 :: Rule
hash64 = mkHashSized 64

bytes80 :: Rule
bytes80 = "bytes80" =:= VBytes `sized` (80 :: Word64)

vkey :: Rule
vkey = "vkey" =:= VBytes `sized` (32 :: Word64)

vrf_vkey :: Rule
vrf_vkey = "vrf_vkey" =:= VBytes `sized` (32 :: Word64)

vrf_cert :: Rule
vrf_cert = "vrf_cert" =:= arr [a VBytes, a (VBytes `sized` (80 :: Word64))]

kes_vkey :: Rule
kes_vkey = "kes_vkey" =:= VBytes `sized` (32 :: Word64)

kes_signature :: Rule
kes_signature = "kes_signature" =:= VBytes `sized` (448 :: Word64)

signkey_kes :: Rule
signkey_kes = "signkey_kes" =:= VBytes `sized` (64 :: Word64)

signature :: Rule
signature = "signature" =:= VBytes `sized` (64 :: Word64)

max_word64 :: Rule
max_word64 = "max_word64" =:= (18446744073709551615 :: Integer)

positive_int :: Rule
positive_int = "positive_int" =:= (1 :: Integer) ... max_word64

max_word32 :: Rule
max_word32 = "max_word32" =:= (4294967295 :: Integer)

positive_word32 :: Rule
positive_word32 = "positive_word32" =:= (1 :: Integer) ... max_word32

unit_interval :: Rule
unit_interval =
  comment
    [str|The real unit_interval is: #6.30([uint, uint])
        |
        |A unit interval is a number in the range between 0 and 1, which
        |means there are two extra constraints:
        |  1. numerator <= denominator
        |  2. denominator > 0
        |
        |The relation between numerator and denominator can be
        |expressed in CDDL, but we have a limitation currently
        |(see: https://github.com/input-output-hk/cuddle/issues/30)
        |which poses a problem for testing. We need to be able to
        |generate random valid data for testing implementation of
        |our encoders/decoders. Which means we cannot use the actual
        |definition here and we hard code the value to 1/2
        |]
    $ "unit_interval" =:= tag 30 (arr [1, 2])

-- | nonnegative_interval = tag 0 [uint, positive_int]
nonnegative_interval :: Rule
nonnegative_interval = "nonnegative_interval" =:= tag 30 (arr [a VUInt, a positive_int])

distinct :: IsSizeable s => Value s -> Rule
distinct x =
  comment
    [str|A type for distinct values.
        |The type parameter must support .size, for example: bytes or uint
        |]
    $ "distinct_"
      <> show' x
        =:= (x `sized` (8 :: Word64))
        / (x `sized` (16 :: Word64))
        / (x `sized` (20 :: Word64))
        / (x `sized` (24 :: Word64))
        / (x `sized` (30 :: Word64))
        / (x `sized` (32 :: Word64))
  where
    show' :: Value s -> T.Text
    show' = \case
      VBytes -> T.pack "bytes"
      VUInt -> T.pack "uint"
      _ -> error "Unsupported Value for `distinct`"

untagged_set :: IsType0 a => a -> GRuleCall
untagged_set = binding $ \x -> "set" =:= arr [0 <+ a x]

untagged_nonempty_set :: IsType0 a => a -> GRuleCall
untagged_nonempty_set = binding $ \x -> "nonempty_set" =:= arr [1 <+ a x]

-- | The era after dijkstra enforces the 258 tag for sets.
mkTaggedSet :: IsType0 a => T.Text -> Word64 -> a -> GRuleCall
mkTaggedSet label n = binding $ \x -> label =:= tag 258 (arr [n <+ a x])

_tagged_set :: IsType0 a => a -> GRuleCall
_tagged_set = mkTaggedSet "set" 0

_tagged_nonempty_set :: IsType0 a => a -> GRuleCall
_tagged_nonempty_set = mkTaggedSet "nonempty_set" 1

_tagged_oset :: IsType0 a => a -> GRuleCall
_tagged_oset = mkTaggedSet "oset" 0

_tagged_nonempty_oset :: IsType0 a => a -> GRuleCall
_tagged_nonempty_oset = mkTaggedSet "nonempty_oset" 1

nonce :: Rule
nonce = "nonce" =:= arr [0] / arr [1, a (VBytes `sized` (32 :: Word64))]

epoch :: Rule
epoch = "epoch" =:= VUInt `sized` (8 :: Word64)

epoch_interval :: Rule
epoch_interval = "epoch_interval" =:= VUInt `sized` (4 :: Word64)

slot :: Rule
slot = "slot" =:= VUInt `sized` (8 :: Word64)

block_number :: Rule
block_number = "block_number" =:= VUInt `sized` (8 :: Word64)

transaction_index :: Rule
transaction_index = "transaction_index" =:= VUInt `sized` (2 :: Word64)

metadatum_label :: Rule
metadatum_label = "metadatum_label" =:= VUInt `sized` (8 :: Word64)

metadatum :: Rule
metadatum =
  "metadatum"
    =:= smp [0 <+ asKey metadatum ==> metadatum]
    / sarr [0 <+ a metadatum]
    / VInt
    / (VBytes `sized` (0 :: Word64, 64 :: Word64))
    / (VText `sized` (0 :: Word64, 64 :: Word64))

metadata :: Rule
metadata = "metadata" =:= mp [0 <+ asKey metadatum_label ==> metadatum]

auxiliary_data_hash :: Rule
auxiliary_data_hash = "auxiliary_data_hash" =:= hash32

script_hash :: Rule
script_hash =
  comment
    [str|To compute a script hash, note that you must prepend
        |a tag to the bytes of the script before hashing.
        |The tag is determined by the language.
        |The tags are:
        |  "\x00" for multisig/native scripts
        |  "\x01" for Plutus V1 scripts
        |  "\x02" for Plutus V2 scripts
        |  "\x03" for Plutus V3 scripts
        |  "\x04" for Plutus V4 scripts
        |]
    $ "script_hash" =:= hash28

mkScriptPubkey :: Named Group
mkScriptPubkey = "script_pubkey" =:~ grp [0, a addr_keyhash]

mkScriptAll :: IsType0 script => script -> Named Group
mkScriptAll s = "script_all" =:~ grp [1, a (arr [0 <+ a s])]

mkScriptAny :: IsType0 script => script -> Named Group
mkScriptAny s = "script_any" =:~ grp [2, a (arr [0 <+ a s])]

mkScriptNOfK :: (IsType0 threshold, IsType0 script) => threshold -> script -> Named Group
mkScriptNOfK threshold s =
  "script_n_of_k" =:~ grp [3, "n" ==> threshold, a (arr [0 <+ a s])]

mkScriptInvalidBefore :: Named Group
mkScriptInvalidBefore =
  comment
    [str|Timelock validity intervals are half-open intervals [a, b).
        |This field specifies the left (included) endpoint a.
        |]
    $ "script_invalid_before" =:~ grp [4, a slot]

mkScriptInvalidHereafter :: Named Group
mkScriptInvalidHereafter =
  comment
    [str|Timelock validity intervals are half-open intervals [a, b).
        |This field specifies the right (excluded) endpoint b.
        |]
    $ "script_invalid_hereafter" =:~ grp [5, a slot]

stake_credential :: Rule
stake_credential = "stake_credential" =:= credential

credential :: Rule
credential = "credential" =:= arr [0, a addr_keyhash] / arr [1, a script_hash]

port :: Rule
port = "port" =:= VUInt `le` 65535

ipv4 :: Rule
ipv4 = "ipv4" =:= VBytes `sized` (4 :: Word64)

ipv6 :: Rule
ipv6 = "ipv6" =:= VBytes `sized` (16 :: Word64)

dns_name64 :: Rule
dns_name64 = "dns_name" =:= VText `sized` (0 :: Word64, 64 :: Word64)

url64 :: Rule
url64 = "url" =:= VText `sized` (0 :: Word64, 64 :: Word64)

single_host_addr :: Named Group
single_host_addr =
  "single_host_addr"
    =:~ grp
      [ 0
      , a $ port / VNil
      , a $ ipv4 / VNil
      , a $ ipv6 / VNil
      ]

-- | Generate pool-related definitions with protocol-version-specific dns_name and url sizes.
mkPoolRules :: Rule -> Rule -> (Named Group, Named Group, [HuddleItem])
mkPoolRules dns_name url = (pool_registration_cert, pool_retirement_cert, subFields)
  where
    subFields =
      [ HIGroup single_host_name
      , HIGroup multi_host_name
      , HIRule pool_metadata
      , HIRule relay
      , HIGroup pool_params
      ]

    single_host_name :: Named Group
    single_host_name =
      comment
        [str|dns_name: An A or AAAA DNS record
            |]
        $ "single_host_name" =:~ grp [1, a $ port / VNil, a dns_name]

    multi_host_name :: Named Group
    multi_host_name =
      comment
        [str|dns_name: An SRV DNS record
            |]
        $ "multi_host_name"
          =:~ grp [2, a dns_name]

    pool_metadata :: Rule
    pool_metadata = "pool_metadata" =:= arr [a url, a VBytes]

    relay :: Rule
    relay =
      "relay"
        =:= arr [a single_host_addr]
        / arr [a single_host_name]
        / arr [a multi_host_name]

    pool_params :: Named Group
    pool_params =
      "pool_params"
        =:~ grp
          [ "operator" ==> pool_keyhash
          , "vrf_keyhash" ==> vrf_keyhash
          , "pledge" ==> coin
          , "cost" ==> coin
          , "margin" ==> unit_interval
          , "reward_account" ==> reward_account
          , "pool_owners" ==> untagged_set addr_keyhash
          , "relays" ==> arr [0 <+ a relay]
          , "pool_metadata" ==> pool_metadata / VNil
          ]

    pool_registration_cert :: Named Group
    pool_registration_cert = "pool_registration_cert" =:~ grp [3, a pool_params]

    pool_retirement_cert :: Named Group
    pool_retirement_cert = "pool_retirement_cert" =:~ grp [4, a pool_keyhash, a epoch]
