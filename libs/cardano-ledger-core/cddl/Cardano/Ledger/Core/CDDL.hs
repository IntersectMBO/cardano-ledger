{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Core.CDDL where

import Cardano.Ledger.CDDL
import Cardano.Ledger.Core (Era)
import Codec.CBOR.Cuddle.Huddle
import qualified Data.Text as T
import Data.Word (Word64)
import Text.Heredoc
import Prelude hiding ((/))

instance Era era => HasCDDL "hash28" era where
  huddleItem = HIRule ("hash28" =:= VBytes `sized` (28 :: Word64))

instance Era era => HasCDDL "hash32" era where
  huddleItem = HIRule ("hash32" =:= VBytes `sized` (32 :: Word64))

instance Era era => HasCDDL "max_word64" era where
  huddleItem = HIRule ("max_word64" =:= (18446744073709551615 :: Integer))

instance Era era => HasCDDL "positive_int" era where
  huddleItem = HIRule ("positive_int" =:= (1 :: Integer) ... huddleRule @"max_word64" @era)

instance Era era => HasCDDL "max_word32" era where
  huddleItem = HIRule ("max_word32" =:= (4294967295 :: Integer))

instance Era era => HasCDDL "positive_word32" era where
  huddleItem = HIRule ("positive_word32" =:= (1 :: Integer) ... huddleRule @"max_word32" @era)

instance Era era => HasCDDL "unit_interval" era where
  huddleItem =
    HIRule
      $ comment
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
      $ "unit_interval"
        =:= tag 30 (arr [1, 2])

instance Era era => HasCDDL "nonnegative_interval" era where
  huddleItem = HIRule ("nonnegative_interval" =:= tag 30 (arr [a VUInt, a (huddleRule @"positive_int" @era)]))

distinct :: IsSizeable s => Value s -> HuddleItem
distinct x =
  HIRule
    $ comment
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

instance Era era => HasCDDL "nonce" era where
  huddleItem = HIRule ("nonce" =:= arr [0] / arr [1, a (huddleRule @"hash32" @era)])

instance Era era => HasCDDL "epoch" era where
  huddleItem = HIRule ("epoch" =:= VUInt `sized` (8 :: Word64))

instance Era era => HasCDDL "epoch_interval" era where
  huddleItem = HIRule ("epoch_interval" =:= VUInt `sized` (4 :: Word64))

instance Era era => HasCDDL "slot" era where
  huddleItem = HIRule ("slot" =:= VUInt `sized` (8 :: Word64))

instance Era era => HasCDDL "block_number" era where
  huddleItem = HIRule ("block_number" =:= VUInt `sized` (8 :: Word64))

instance Era era => HasCDDL "addr_keyhash" era where
  huddleItem = HIRule ("addr_keyhash" =:= huddleRule @"hash28" @era)

instance Era era => HasCDDL "pool_keyhash" era where
  huddleItem = HIRule ("pool_keyhash" =:= huddleRule @"hash28" @era)

instance Era era => HasCDDL "vrf_keyhash" era where
  huddleItem = HIRule ("vrf_keyhash" =:= huddleRule @"hash32" @era)

instance Era era => HasCDDL "vkey" era where
  huddleItem = HIRule ("vkey" =:= VBytes `sized` (32 :: Word64))

instance Era era => HasCDDL "vrf_vkey" era where
  huddleItem = HIRule ("vrf_vkey" =:= VBytes `sized` (32 :: Word64))

instance Era era => HasCDDL "vrf_cert" era where
  huddleItem = HIRule ("vrf_cert" =:= arr [a VBytes, a (VBytes `sized` (80 :: Word64))])

instance Era era => HasCDDL "kes_vkey" era where
  huddleItem = HIRule ("kes_vkey" =:= VBytes `sized` (32 :: Word64))

instance Era era => HasCDDL "kes_signature" era where
  huddleItem = HIRule ("kes_signature" =:= VBytes `sized` (448 :: Word64))

instance Era era => HasCDDL "signkey_kes" era where
  huddleItem = HIRule ("signkey_kes" =:= VBytes `sized` (64 :: Word64))

instance Era era => HasCDDL "sequence_number" era where
  huddleItem = HIRule ("sequence_number" =:= VUInt `sized` (8 :: Word64))

instance Era era => HasCDDL "kes_period" era where
  huddleItem = HIRule ("kes_period" =:= VUInt `sized` (8 :: Word64))

instance Era era => HasCDDL "signature" era where
  huddleItem = HIRule ("signature" =:= VBytes `sized` (64 :: Word64))

instance Era era => HasCDDL "coin" era where
  huddleItem = HIRule ("coin" =:= VUInt)

instance Era era => HasCDDL "positive_coin" era where
  huddleItem = HIRule ("positive_coin" =:= (1 :: Integer) ... huddleRule @"max_word64" @era)

instance Era era => HasCDDL "address" era where
  huddleItem =
    HIRule
      $ comment
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

instance Era era => HasCDDL "reward_account" era where
  huddleItem =
    HIRule
      $ comment
        "reward_account = bytes"
      $ "reward_account"
        =:= bstr "E090000000000000000000000000000000000000000000000000000000"
        / bstr "F0A0000000000000000000000000000000000000000000000000000000"

instance Era era => HasCDDL "transaction_index" era where
  huddleItem = HIRule ("transaction_index" =:= VUInt `sized` (2 :: Word64))

instance Era era => HasCDDL "metadatum_label" era where
  huddleItem = HIRule ("metadatum_label" =:= VUInt `sized` (8 :: Word64))

instance Era era => HasCDDL "metadatum" era where
  huddleItem =
    HIRule $
      "metadatum"
        =:= smp [0 <+ asKey (huddleRule @"metadatum" @era) ==> huddleRule @"metadatum" @era]
        / sarr [0 <+ a (huddleRule @"metadatum" @era)]
        / VInt
        / (VBytes `sized` (0 :: Word64, 64 :: Word64))
        / (VText `sized` (0 :: Word64, 64 :: Word64))

instance Era era => HasCDDL "metadata" era where
  huddleItem =
    HIRule
      ("metadata" =:= mp [0 <+ asKey (huddleRule @"metadatum_label" @era) ==> huddleRule @"metadatum" @era])

instance Era era => HasCDDL "auxiliary_data_hash" era where
  huddleItem = HIRule ("auxiliary_data_hash" =:= huddleRule @"hash32" @era)

instance Era era => HasCDDL "script_hash" era where
  huddleItem =
    HIRule
      $ comment
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
      $ "script_hash"
        =:= huddleRule @"hash28" @era

instance Era era => HasCDDL "credential" era where
  huddleItem =
    HIRule $
      "credential"
        =:= arr [0, a (huddleRule @"addr_keyhash" @era)]
        / arr [1, a (huddleRule @"script_hash" @era)]

instance Era era => HasCDDL "stake_credential" era where
  huddleItem = HIRule ("stake_credential" =:= huddleRule @"credential" @era)

instance Era era => HasCDDL "port" era where
  huddleItem = HIRule ("port" =:= VUInt `le` 65535)

instance Era era => HasCDDL "ipv4" era where
  huddleItem = HIRule ("ipv4" =:= VBytes `sized` (4 :: Word64))

instance Era era => HasCDDL "ipv6" era where
  huddleItem = HIRule ("ipv6" =:= VBytes `sized` (16 :: Word64))
