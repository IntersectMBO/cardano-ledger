{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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

module Cardano.Ledger.Core.HuddleSpec where

import Cardano.Ledger.BaseTypes (getVersion)
import Cardano.Ledger.Core (ByronEra, eraProtVerHigh, eraProtVerLow)
import Cardano.Ledger.Huddle (Era, HuddleRule (..), Type0 (..), huddleRule)
import Codec.CBOR.Cuddle.Huddle (
  HuddleItem (HIRule),
  IsSizeable,
  Value (VBytes, VInt, VText, VUInt),
  a,
  arr,
  asKey,
  bstr,
  comment,
  le,
  mp,
  sarr,
  sized,
  smp,
  tag,
  (...),
  (/),
  (<+),
  (=:=),
  (==>),
 )
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Word (Word64)
import Text.Heredoc
import Prelude hiding ((/))

instance Era era => HuddleRule "hash28" era where
  huddleRuleBody _ = Type0 $ VBytes `sized` (28 :: Word64)

instance Era era => HuddleRule "hash32" era where
  huddleRuleBody _ = Type0 $ VBytes `sized` (32 :: Word64)

instance Era era => HuddleRule "max_word64" era where
  huddleRuleBody _ = Type0 (18446744073709551615 :: Integer)

instance Era era => HuddleRule "positive_int" era where
  huddleRuleBody p = Type0 $ (1 :: Integer) ... huddleRule @"max_word64" p

instance Era era => HuddleRule "max_word32" era where
  huddleRuleBody _ = Type0 (4294967295 :: Integer)

instance Era era => HuddleRule "positive_word32" era where
  huddleRuleBody p = Type0 $ (1 :: Integer) ... huddleRule @"max_word32" p

instance Era era => HuddleRule "unit_interval" era where
  huddleRuleBody _ = Type0 $ tag 30 (arr [1, 2])
  huddleRuleComment _ =
    Just
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

instance Era era => HuddleRule "nonnegative_interval" era where
  huddleRuleBody p = Type0 $ tag 30 (arr [a VUInt, a (huddleRule @"positive_int" p)])

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

instance Era era => HuddleRule "nonce" era where
  huddleRuleBody p = Type0 $ arr [0] / arr [1, a (huddleRule @"hash32" p)]

instance Era era => HuddleRule "epoch" era where
  huddleRuleBody _ = Type0 $ VUInt `sized` (8 :: Word64)

instance Era era => HuddleRule "epoch_interval" era where
  huddleRuleBody _ = Type0 $ VUInt `sized` (4 :: Word64)

instance Era era => HuddleRule "slot" era where
  huddleRuleBody _ = Type0 $ VUInt `sized` (8 :: Word64)

instance Era era => HuddleRule "block_number" era where
  huddleRuleBody _ = Type0 $ VUInt `sized` (8 :: Word64)

instance Era era => HuddleRule "addr_keyhash" era where
  huddleRuleBody p = Type0 $ huddleRule @"hash28" p

instance Era era => HuddleRule "pool_keyhash" era where
  huddleRuleBody p = Type0 $ huddleRule @"hash28" p

instance Era era => HuddleRule "vrf_keyhash" era where
  huddleRuleBody p = Type0 $ huddleRule @"hash32" p

instance Era era => HuddleRule "vkey" era where
  huddleRuleBody _ = Type0 $ VBytes `sized` (32 :: Word64)

instance Era era => HuddleRule "vrf_vkey" era where
  huddleRuleBody _ = Type0 $ VBytes `sized` (32 :: Word64)

instance Era era => HuddleRule "vrf_cert" era where
  huddleRuleBody _ = Type0 $ arr [a VBytes, a (VBytes `sized` (80 :: Word64))]

instance Era era => HuddleRule "kes_vkey" era where
  huddleRuleBody _ = Type0 $ VBytes `sized` (32 :: Word64)

instance Era era => HuddleRule "kes_signature" era where
  huddleRuleBody _ = Type0 $ VBytes `sized` (448 :: Word64)

instance Era era => HuddleRule "signkey_kes" era where
  huddleRuleBody _ = Type0 $ VBytes `sized` (64 :: Word64)

instance Era era => HuddleRule "sequence_number" era where
  huddleRuleBody _ = Type0 $ VUInt `sized` (8 :: Word64)

instance Era era => HuddleRule "kes_period" era where
  huddleRuleBody _ = Type0 $ VUInt `sized` (8 :: Word64)

instance Era era => HuddleRule "signature" era where
  huddleRuleBody _ = Type0 $ VBytes `sized` (64 :: Word64)

instance Era era => HuddleRule "coin" era where
  huddleRuleBody _ = Type0 VUInt

instance Era era => HuddleRule "positive_coin" era where
  huddleRuleBody p = Type0 $ (1 :: Integer) ... huddleRule @"max_word64" p

instance Era era => HuddleRule "address" era where
  huddleRuleBody _ =
    Type0 $
      bstr
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
  huddleRuleComment _ =
    Just
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

instance Era era => HuddleRule "reward_account" era where
  huddleRuleBody _ =
    Type0 $
      bstr "E090000000000000000000000000000000000000000000000000000000"
        / bstr "F0A0000000000000000000000000000000000000000000000000000000"
  huddleRuleComment _ = Just "reward_account = bytes"

instance Era era => HuddleRule "transaction_index" era where
  huddleRuleBody _ = Type0 $ VUInt `sized` (2 :: Word64)

instance Era era => HuddleRule "metadatum_label" era where
  huddleRuleBody _ = Type0 $ VUInt `sized` (8 :: Word64)

instance Era era => HuddleRule "metadatum" era where
  huddleRuleBody p =
    Type0 $
      smp
        [ 0 <+ asKey (huddleRule @"metadatum" p) ==> huddleRule @"metadatum" p
        ]
        / sarr [0 <+ a (huddleRule @"metadatum" p)]
        / VInt
        / (VBytes `sized` (0 :: Word64, 64 :: Word64))
        / (VText `sized` (0 :: Word64, 64 :: Word64))

instance Era era => HuddleRule "metadata" era where
  huddleRuleBody p =
    Type0 $
      mp
        [ 0
            <+ asKey (huddleRule @"metadatum_label" p)
            ==> huddleRule @"metadatum" p
        ]

instance Era era => HuddleRule "auxiliary_data_hash" era where
  huddleRuleBody p = Type0 $ huddleRule @"hash32" p

instance Era era => HuddleRule "script_hash" era where
  huddleRuleBody p = Type0 $ huddleRule @"hash28" p
  huddleRuleComment _ =
    Just
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

instance Era era => HuddleRule "credential" era where
  huddleRuleBody p =
    Type0 $
      arr [0, a (huddleRule @"addr_keyhash" p)]
        / arr [1, a (huddleRule @"script_hash" p)]

instance Era era => HuddleRule "stake_credential" era where
  huddleRuleBody p = Type0 $ huddleRule @"credential" p

instance Era era => HuddleRule "port" era where
  huddleRuleBody _ = Type0 $ VUInt `le` 65535

instance Era era => HuddleRule "ipv4" era where
  huddleRuleBody _ = Type0 $ VBytes `sized` (4 :: Word64)

instance Era era => HuddleRule "ipv6" era where
  huddleRuleBody _ = Type0 $ VBytes `sized` (16 :: Word64)

majorProtocolVersionRule :: forall era. Era era => Proxy era -> Type0
majorProtocolVersionRule _ =
  Type0 $
    getVersion @Integer (eraProtVerLow @ByronEra)
      ... succ (getVersion @Integer (eraProtVerHigh @era))
