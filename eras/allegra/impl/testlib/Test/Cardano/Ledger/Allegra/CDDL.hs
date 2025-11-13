{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Evaluate" -}

module Test.Cardano.Ledger.Allegra.CDDL (
  module Test.Cardano.Ledger.Shelley.CDDL,
  allegraCDDL,

  -- * 64-bit integers
  int64,
  nonzero_int64,

  -- * Transaction
  transaction_witness_set,

  -- * Auxiliary data
  auxiliary_data,
  auxiliary_data_array,

  -- * Native scripts
  allegra_native_script,
) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Core (Era)
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import GHC.Num (Integer)
import Test.Cardano.Ledger.Shelley.CDDL
import Text.Heredoc

allegraCDDL :: Huddle
allegraCDDL =
  collectFrom
    [ HIRule $ block @AllegraEra
    , HIRule $ transaction @AllegraEra
    ]

min_int64 :: Rule
min_int64 = "min_int64" =:= (-9223372036854775808 :: Integer)

max_int64 :: Rule
max_int64 = "max_int64" =:= (9223372036854775807 :: Integer)

negative_int64 :: Rule
negative_int64 = "negative_int64" =:= min_int64 ... (-1 :: Integer)

positive_int64 :: Rule
positive_int64 = "positive_int64" =:= (1 :: Integer) ... max_int64

nonzero_int64 :: Rule
nonzero_int64 = "nonzero_int64" =:= negative_int64 / positive_int64

-- | 64-bit signed integers for native script timelock thresholds.
int64 :: Rule
int64 = "int64" =:= min_int64 ... max_int64

allegra_native_script :: Rule
allegra_native_script =
  comment
    [str|Allegra introduces timelock support for native scripts.
        |This is the 6-variant native script format used by
        |Allegra, Mary, Alonzo, Babbage, and Conway.
        |
        |Timelock validity intervals are half-open intervals [a, b).
        |  script_invalid_before: specifies the left (included) endpoint a.
        |  script_invalid_hereafter: specifies the right (excluded) endpoint b.
        |
        |Note: Allegra switched to int64 for script_n_of_k thresholds.
        |]
    $ "native_script"
      =:= arr [a script_pubkey]
      / arr [a script_all]
      / arr [a script_any]
      / arr [a script_n_of_k]
      / arr [a script_invalid_before]
      / arr [a script_invalid_hereafter]

script_pubkey :: Named Group
script_pubkey = mkScriptPubkey

script_all :: Named Group
script_all = mkScriptAll allegra_native_script

script_any :: Named Group
script_any = mkScriptAny allegra_native_script

script_n_of_k :: Named Group
script_n_of_k = mkScriptNOfK int64 allegra_native_script

script_invalid_before :: Named Group
script_invalid_before = mkScriptInvalidBefore

script_invalid_hereafter :: Named Group
script_invalid_hereafter = mkScriptInvalidHereafter

auxiliary_scripts :: Rule
auxiliary_scripts = "auxiliary_scripts" =:= arr [0 <+ a allegra_native_script]

auxiliary_data_array :: Rule
auxiliary_data_array =
  "auxiliary_data_array"
    =:= arr
      [ "transaction_metadata" ==> metadata
      , "auxiliary_scripts" ==> auxiliary_scripts
      ]

-- | Adds auxiliary_data_array format for batching native scripts with metadata.
auxiliary_data :: Rule
auxiliary_data =
  "auxiliary_data"
    =:= metadata
    / auxiliary_data_array

-- | Adds validity interval start (index 8) for timelock script support.
transaction_body :: forall era. Era era => Rule
transaction_body =
  comment
    [str|Allegra transaction body adds the validity interval start at index 8
        |]
    $ "transaction_body"
      =:= mp
        [ idx 0 ==> untagged_set transaction_input
        , idx 1 ==> arr [0 <+ a transaction_output]
        , idx 2 ==> coin
        , opt (idx 3 ==> slot)
        , opt (idx 4 ==> arr [0 <+ a certificate])
        , opt (idx 5 ==> withdrawals)
        , opt (idx 6 ==> update @era)
        , opt (idx 7 ==> auxiliary_data_hash)
        , opt (idx 8 ==> slot)
        ]

block :: forall era. Era era => Rule
block =
  "block"
    =:= arr
      [ a $ header @era
      , "transaction_bodies" ==> arr [0 <+ a (transaction_body @era)]
      , "transaction_witness_sets" ==> arr [0 <+ a transaction_witness_set]
      , "auxiliary_data_set" ==> mp [0 <+ asKey transaction_index ==> auxiliary_data]
      ]

transaction :: forall era. Era era => Rule
transaction =
  "transaction"
    =:= arr
      [ a $ transaction_body @era
      , a transaction_witness_set
      , a (auxiliary_data / VNil)
      ]

-- | Uses allegra_native_script with timelock support (invalid_before/hereafter).
transaction_witness_set :: Rule
transaction_witness_set =
  "transaction_witness_set"
    =:= mp
      [ opt $ idx 0 ==> arr [0 <+ a vkeywitness]
      , opt $ idx 1 ==> arr [0 <+ a allegra_native_script]
      , opt $ idx 2 ==> arr [0 <+ a bootstrap_witness]
      ]
