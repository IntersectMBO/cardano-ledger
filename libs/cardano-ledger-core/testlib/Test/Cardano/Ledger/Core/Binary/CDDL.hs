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
  bytes28,
  bytes32,
  bytes64,
  bytes80,
  bytes448,

  -- * Hashes, keys and certs
  addr_keyhash,
  script_hash,
  pool_keyhash,
  vrf_keyhash,
  auxiliary_data_hash,
  vkey,
  vkey_witness,
  bootstrap_witness,
  vrf_vkey,
  kes_vkey,
  vrf_cert,
  signature,
  kes_signature,
  signkey_kes,
  credential,

  -- * Numbers
  max_word32,
  pword32,
  pint,
  big_int,
  min_int64,
  max_int64,
  int64,
  nint64,
  pint64,
  nonzero_int64,

  -- * Unit intervals
  unit_interval,
  nonnegative_interval,

  -- * Distinct uint/bytes, bounded bytes
  distinct,
  bounded_bytes,

  -- * Sets
  untagged_set,
  untagged_nonempty_set,
  tagged_set,
  tagged_nonempty_set,
  tagged_oset,
  tagged_nonempty_oset,
  -- untagged_set',
  -- untagged_nonempty_set',

  -- * Network
  port,
  ipv4,
  ipv6,
  single_host_addr,

  -- * Value
  coin,
  positive_coin,

  -- * Protocol version
  protocol_version,

  -- * Addresses and accounts
  address,
  reward_account,

  -- * Tx
  transaction_id,
  transaction_ix,
  transaction_input,
  transaction_metadatum,
) where

import Cardano.Ledger.BaseTypes hiding ((==>))
import Cardano.Ledger.Core (ByronEra, Era, eraProtVerHigh, eraProtVerLow)
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Base (error)
import GHC.Show (Show (show))
import Text.Heredoc
import Prelude (Integer, succ)

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
addr_keyhash = "addr_keyhash" =:= bytes28

script_hash :: Rule
script_hash =
  comment
    [str|To compute a script hash, note that you must prepend
        |a tag to the bytes of the script before hashing.
        |The tag is determined by the language.
        |The tags in the Conway era are:
        |  "\x00" for multisig scripts
        |  "\x01" for Plutus V1 scripts
        |  "\x02" for Plutus V2 scripts
        |  "\x03" for Plutus V3 scripts
        |  "\x04" for Plutus V4 scripts
        |]
    $ "script_hash" =:= bytes28

pool_keyhash :: Rule
pool_keyhash = "pool_keyhash" =:= bytes28

vrf_keyhash :: Rule
vrf_keyhash = "vrf_keyhash" =:= bytes32

mkBytesSized :: Word64 -> Rule
mkBytesSized size = "bytes" <> T.pack (show size) =:= VBytes `sized` size

bytes28 :: Rule
bytes28 = mkBytesSized 28

bytes32 :: Rule
bytes32 = mkBytesSized 32

bytes64 :: Rule
bytes64 = mkBytesSized 64

bytes80 :: Rule
bytes80 = mkBytesSized 80

bytes448 :: Rule
bytes448 = mkBytesSized 448

vkey :: Rule
vkey = "vkey" =:= bytes32

vkey_witness :: Rule
vkey_witness = "vkey_witness" =:= arr [a vkey, a signature]

bootstrap_witness :: Rule
bootstrap_witness =
  "bootstrap_witness"
    =:= arr
      [ "public_key" ==> vkey
      , "signature" ==> signature
      , "chain_code" ==> bytes32
      , "attributes" ==> VBytes
      ]

vrf_vkey :: Rule
vrf_vkey = "vrf_vkey" =:= bytes32

vrf_cert :: Rule
vrf_cert = "vrf_cert" =:= arr [a VBytes, a bytes80]

kes_vkey :: Rule
kes_vkey = "kes_vkey" =:= bytes32

kes_signature :: Rule
kes_signature = "kes_signature" =:= bytes448

signkey_kes :: Rule
signkey_kes = "signkey_kes" =:= bytes64

signature :: Rule
signature = "signature" =:= bytes64

credential :: Rule
credential = "credential" =:= arr [0, a addr_keyhash] / arr [1, a script_hash]

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

big_int :: Rule
big_int = "big_int" =:= VInt / big_uint / big_nint

big_uint :: Rule
big_uint = "big_uint" =:= tag 2 bounded_bytes

big_nint :: Rule
big_nint = "big_nint" =:= tag 3 bounded_bytes

min_int64 :: Rule
min_int64 = "min_int64" =:= (-9223372036854775808 :: Integer)

max_int64 :: Rule
max_int64 = "max_int64" =:= (9223372036854775807 :: Integer)

max_word64 :: Rule
max_word64 = "max_word64" =:= (18446744073709551615 :: Integer)

nint64 :: Rule
nint64 = "nint64" =:= min_int64 ... (-1 :: Integer)

pint64 :: Rule
pint64 = "pint64" =:= (1 :: Integer) ... max_int64

-- | This is the same as the current int64 definition but without zero
nonzero_int64 :: Rule
nonzero_int64 = "nonzero_int64" =:= nint64 / pint64

int64 :: Rule
int64 = "int64" =:= min_int64 ... max_int64

pint :: Rule
pint = "pint" =:= (1 :: Integer) ... max_word64

max_word32 :: Rule
max_word32 = "max_word32" =:= (4294967295 :: Integer)

pword32 :: Rule
pword32 = "pword32" =:= (1 :: Integer) ... max_word32

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

-- | nonnegative_interval = tag 0 [uint, pint]
nonnegative_interval :: Rule
nonnegative_interval = "nonnegative_interval" =:= tag 30 (arr [a VUInt, a pint])

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
    $ "bounded_bytes" =:= VBytes `sized` (0 :: Word64, 64 :: Word64)

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

-- Shelley does not support some of the tagged core datastructures that we rely
-- on in future eras. In order to have the "correct" common specification in
-- core, we override them here
untagged_set :: IsType0 a => a -> GRuleCall
untagged_set = binding $ \x -> "set" =:= arr [0 <+ a x]

untagged_nonempty_set :: IsType0 a => a -> GRuleCall
untagged_nonempty_set = binding $ \x -> "nonempty_set" =:= arr [1 <+ a x]

-- | Conway era introduces an optional 258 tag for sets, which will
-- become mandatory in the second era after Conway. We recommend all the
-- tooling to account for this future breaking change sooner rather than
-- later, in order to provide a smooth transition for their users.
mkTaggedSet :: IsType0 a => T.Text -> Word64 -> a -> GRuleCall
mkTaggedSet label n = binding $ \x -> label =:= tag 258 (arr [n <+ a x]) / sarr [n <+ a x]

tagged_set :: IsType0 a => a -> GRuleCall
tagged_set = mkTaggedSet "set" 0

tagged_nonempty_set :: IsType0 a => a -> GRuleCall
tagged_nonempty_set = mkTaggedSet "nonempty_set" 1

-- | An OSet is a Set that preserves the order of its elements.
tagged_oset :: IsType0 a => a -> GRuleCall
tagged_oset = mkTaggedSet "oset" 0

-- | An OSet is a Set that preserves the order of its elements.
tagged_nonempty_oset :: IsType0 a => a -> GRuleCall
tagged_nonempty_oset = mkTaggedSet "nonempty_oset" 1

port :: Rule
port = "port" =:= VUInt `le` 65535

ipv4 :: Rule
ipv4 = "ipv4" =:= VBytes `sized` (4 :: Word64)

ipv6 :: Rule
ipv6 = "ipv6" =:= VBytes `sized` (16 :: Word64)

transaction_id :: Rule
transaction_id = "transaction_id" =:= bytes32

transaction_ix :: Rule
transaction_ix = "transaction_ix" =:= VUInt `sized` (2 :: Word64)

single_host_addr :: Named Group
single_host_addr =
  "single_host_addr" =:~ grp [0, a $ port / VNil, a $ ipv4 / VNil, a $ ipv6 / VNil]

auxiliary_data_hash :: Rule
auxiliary_data_hash = "auxiliary_data_hash" =:= bytes32

transaction_input :: Rule
transaction_input =
  "transaction_input"
    =:= arr
      [ "id" ==> transaction_id
      , "index" ==> VUInt `sized` (2 :: Word64)
      ]

transaction_metadatum :: Rule
transaction_metadatum =
  "transaction_metadatum"
    =:= smp [0 <+ asKey transaction_metadatum ==> transaction_metadatum]
    / sarr [0 <+ a transaction_metadatum]
    / VInt
    / (VBytes `sized` (0 :: Word64, 64 :: Word64))
    / (VText `sized` (0 :: Word64, 64 :: Word64))

major_protocol_version :: forall era. Era era => Rule
major_protocol_version =
  "major_protocol_version"
    =:= (getVersion @Integer (eraProtVerLow @ByronEra) ... succ (getVersion @Integer (eraProtVerHigh @era)))

protocol_version :: forall era. Era era => Rule
protocol_version = "protocol_version" =:= arr [a $ major_protocol_version @era, a VUInt]
