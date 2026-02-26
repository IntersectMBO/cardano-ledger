{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
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
import Cardano.Ledger.Huddle
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (WrappedTerm (..))
import Codec.CBOR.Cuddle.Huddle
import Codec.CBOR.Term (Term (..))
import Control.Monad (join)
import Data.Bits (Bits (..))
import qualified Data.ByteString as BS
import Data.MemPack (VarLen (..), packByteString)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64)
import System.Random.Stateful (
  Uniform (..),
  UniformRange (..),
  uniformByteStringM,
 )
import Text.Heredoc
import Prelude hiding ((/))
import Test.Cardano.Ledger.Common (choose)

instance Era era => HuddleRule "hash28" era where
  huddleRuleNamed pname _ = pname =.= VBytes `sized` (28 :: Word64)

instance Era era => HuddleRule "hash32" era where
  huddleRuleNamed pname _ = pname =.= VBytes `sized` (32 :: Word64)

instance Era era => HuddleRule "max_word64" era where
  huddleRuleNamed pname _ = pname =.= (18446744073709551615 :: Integer)

instance Era era => HuddleRule "positive_int" era where
  huddleRuleNamed pname p = pname =.= (1 :: Integer) ... huddleRule @"max_word64" p

instance Era era => HuddleRule "max_word32" era where
  huddleRuleNamed pname _ = pname =.= (4294967295 :: Integer)

instance Era era => HuddleRule "positive_word32" era where
  huddleRuleNamed pname p = pname =.= (1 :: Integer) ... huddleRule @"max_word32" p

instance Era era => HuddleRule "unit_interval" era where
  huddleRuleNamed pname _ =
    comment
      [str|A unit interval is a number in the range between 0 and 1, which
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
      . withGenerator generator
      $ pname =.= tag 30 (arr [a VUInt, a VUInt])
    where
      generator = do
        let genUnitInterval64 l u = do
              d <- choose (max 1 l, u)
              n <- choose (l, d)
              pure (n, d)
            max64 = toInteger (maxBound @Word64)
        (n, d) <-
          join $
            pickOne
              [ genUnitInterval64 0 max64
              , genUnitInterval64 0 1000
              , genUnitInterval64 (max64 - 1000) max64
              ]
              g
        S . TTagged 30
          <$> genArrayTerm [TInteger $ toInteger n, TInteger $ toInteger d] g

instance Era era => HuddleRule "nonnegative_interval" era where
  huddleRuleNamed pname p =
    pname =.= tag 30 (arr [a VUInt, a (huddleRule @"positive_int" p)])

distinct :: IsSizeable s => Value s -> HuddleItem
distinct x =
  HIRule
    $ comment
      [str|A type for distinct values.
          |The type parameter must support .size, for example: bytes or uint
          |]
    $ "distinct_"
      <> Name (show' x)
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
  huddleRuleNamed pname p = pname =.= arr [0] / arr [1, a (huddleRule @"hash32" p)]

instance Era era => HuddleRule "epoch" era where
  huddleRuleNamed pname _ = pname =.= VUInt `sized` (8 :: Word64)

instance Era era => HuddleRule "epoch_interval" era where
  huddleRuleNamed pname _ = pname =.= VUInt `sized` (4 :: Word64)

instance Era era => HuddleRule "slot" era where
  huddleRuleNamed pname _ = pname =.= VUInt `sized` (8 :: Word64)

instance Era era => HuddleRule "block_number" era where
  huddleRuleNamed pname _ = pname =.= VUInt `sized` (8 :: Word64)

instance Era era => HuddleRule "addr_keyhash" era where
  huddleRuleNamed pname p = pname =.= huddleRule @"hash28" p

instance Era era => HuddleRule "pool_keyhash" era where
  huddleRuleNamed pname p = pname =.= huddleRule @"hash28" p

instance Era era => HuddleRule "vrf_keyhash" era where
  huddleRuleNamed pname p = pname =.= huddleRule @"hash32" p

instance Era era => HuddleRule "vkey" era where
  huddleRuleNamed pname _ = pname =.= VBytes `sized` (32 :: Word64)

instance Era era => HuddleRule "vrf_vkey" era where
  huddleRuleNamed pname _ = pname =.= VBytes `sized` (32 :: Word64)

instance Era era => HuddleRule "vrf_cert" era where
  huddleRuleNamed pname _ = pname =.= arr [a VBytes, a (VBytes `sized` (80 :: Word64))]

instance Era era => HuddleRule "kes_vkey" era where
  huddleRuleNamed pname _ = pname =.= VBytes `sized` (32 :: Word64)

instance Era era => HuddleRule "kes_signature" era where
  huddleRuleNamed pname _ = pname =.= VBytes `sized` (448 :: Word64)

instance Era era => HuddleRule "signkey_kes" era where
  huddleRuleNamed pname _ = pname =.= VBytes `sized` (64 :: Word64)

instance Era era => HuddleRule "sequence_number" era where
  huddleRuleNamed pname _ = pname =.= VUInt `sized` (8 :: Word64)

instance Era era => HuddleRule "kes_period" era where
  huddleRuleNamed pname _ = pname =.= VUInt `sized` (8 :: Word64)

instance Era era => HuddleRule "signature" era where
  huddleRuleNamed pname _ = pname =.= VBytes `sized` (64 :: Word64)

instance Era era => HuddleRule "coin" era where
  huddleRuleNamed pname _ = pname =.= VUInt

instance Era era => HuddleRule "positive_coin" era where
  huddleRuleNamed pname p = pname =.= (1 :: Integer) ... huddleRule @"max_word64" p

instance Era era => HuddleRule "address" era where
  huddleRuleNamed pname _ =
    comment
      [str|address format:
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
          |     1110: account address: keyhash28
          |     1111: account address: scripthash28
          |1001-1101: future formats
          |]
      . withGenerator generator
      $ pname =.= VBytes
    where
      generator g = do
        stakeRef <- uniformRM (0, 0b11) g
        let
          stakeRefMask = stakeRef `shiftL` 5 -- 0b0xx00000
          mkMask mask isMask = if isMask then mask else 0
        isPaymentScriptMask <- mkMask 0b00010000 <$> uniformM g
        isMainnetMask <- mkMask 0b00000001 <$> uniformM g
        let
          header = stakeRefMask .|. isPaymentScriptMask .|. isMainnetMask
          genHash28 = uniformByteStringM 28 g
          genVar32 = VarLen <$> uniformM @Word32 g
          genVar16 = VarLen <$> uniformM @Word16 g
        stakeCred <- case stakeRef of
          0b00 -> genHash28 -- staking payment hash
          0b01 -> genHash28 -- staking script hash
          0b10 -> do
            -- Ptr
            slotNo <- genVar32
            txIx <- genVar16
            certIx <- genVar16
            pure $ packByteString slotNo <> packByteString txIx <> packByteString certIx
          _ -> pure mempty
        paymentCred <- genHash28
        -- TODO use genBytesTerm once indefinite bytestring decoding has been fixed
        let bytesTerm = TBytes . BS.cons header $ paymentCred <> stakeCred
        pure $ S bytesTerm

instance Era era => HuddleRule "reward_account" era where
  huddleRuleNamed pname _ = withGenerator generator $ pname =.= VBytes
    where
      generator g = do
        isMainnet <- uniformM g
        isScript <- uniformM g
        let
          mainnetMask | isMainnet = 0b00000001 | otherwise = 0x00
          scriptMask | isScript = 0b00010000 | otherwise = 0x00
          header = 0b11100000 .|. mainnetMask .|. scriptMask
        payload <- uniformByteStringM 28 g
        let term = TBytes $ BS.cons header payload
        pure $ S term

instance Era era => HuddleRule "transaction_index" era where
  huddleRuleNamed pname _ = pname =.= VUInt `sized` (2 :: Word64)

instance Era era => HuddleRule "metadatum_label" era where
  huddleRuleNamed pname _ = pname =.= VUInt `sized` (8 :: Word64)

instance Era era => HuddleRule "metadatum" era where
  huddleRuleNamed pname p =
    pname
      =.= smp
        [ 0 <+ asKey (huddleRule @"metadatum" p) ==> huddleRule @"metadatum" p
        ]
      / sarr [0 <+ a (huddleRule @"metadatum" p)]
      / VInt
      / (VBytes `sized` (0 :: Word64, 64 :: Word64))
      / (VText `sized` (0 :: Word64, 64 :: Word64))

instance Era era => HuddleRule "metadata" era where
  huddleRuleNamed pname p =
    pname
      =.= mp
        [ 0
            <+ asKey (huddleRule @"metadatum_label" p)
            ==> huddleRule @"metadatum" p
        ]

instance Era era => HuddleRule "auxiliary_data_hash" era where
  huddleRuleNamed pname p = pname =.= huddleRule @"hash32" p

instance Era era => HuddleRule "script_hash" era where
  huddleRuleNamed pname p =
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
      $ pname
        =.= huddleRule @"hash28" p

instance Era era => HuddleRule "credential" era where
  huddleRuleNamed pname p =
    pname
      =.= arr [0, a (huddleRule @"addr_keyhash" p)]
      / arr [1, a (huddleRule @"script_hash" p)]

instance Era era => HuddleRule "stake_credential" era where
  huddleRuleNamed pname p = pname =.= huddleRule @"credential" p

instance Era era => HuddleRule "port" era where
  huddleRuleNamed pname _ = pname =.= VUInt `le` 65535

instance Era era => HuddleRule "ipv4" era where
  huddleRuleNamed pname _ = pname =.= VBytes `sized` (4 :: Word64)

instance Era era => HuddleRule "ipv6" era where
  huddleRuleNamed pname _ = pname =.= VBytes `sized` (16 :: Word64)

majorProtocolVersionRule ::
  forall era. Era era => Proxy "major_protocol_version" -> Proxy era -> Rule
majorProtocolVersionRule pname _ =
  pname
    =.= getVersion @Integer (eraProtVerLow @ByronEra)
    ... succ (getVersion @Integer (eraProtVerHigh @era))
