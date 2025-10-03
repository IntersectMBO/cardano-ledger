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
  allegra_native_script,
  allegra_transaction_witness_set,
  allegra_auxiliary_data,
  allegra_auxiliary_scripts,
  allegra_script_pubkey,
  allegra_script_all,
  allegra_script_any,
  allegra_invalid_before,
  allegra_invalid_hereafter,
) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Core (Era)
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Test.Cardano.Ledger.Shelley.CDDL
import Text.Heredoc

allegraCDDL :: Huddle
allegraCDDL =
  collectFrom
    [ HIRule $ block @AllegraEra
    , HIRule $ transaction @AllegraEra
    ]

allegra_native_script :: Rule
allegra_native_script =
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
      =:= arr [a allegra_script_pubkey]
      / arr [a allegra_script_all]
      / arr [a allegra_script_any]
      / arr [a script_n_of_k]
      / arr [a allegra_invalid_before]
      / arr [a allegra_invalid_hereafter]

allegra_script_pubkey :: Named Group
allegra_script_pubkey = "script_pubkey" =:~ grp [0, a addr_keyhash]

allegra_script_all :: Named Group
allegra_script_all = "script_all" =:~ grp [1, a (arr [0 <+ a allegra_native_script])]

allegra_script_any :: Named Group
allegra_script_any = "script_any" =:~ grp [2, a (arr [0 <+ a allegra_native_script])]

script_n_of_k :: Named Group
script_n_of_k = "script_n_of_k" =:~ grp [3, "n" ==> int64, a (arr [0 <+ a allegra_native_script])]

allegra_invalid_before :: Named Group
allegra_invalid_before = "invalid_before" =:~ grp [4, a VUInt]

allegra_invalid_hereafter :: Named Group
allegra_invalid_hereafter = "invalid_hereafter" =:~ grp [5, a VUInt]

allegra_auxiliary_scripts :: Rule
allegra_auxiliary_scripts = "auxiliary_scripts" =:= arr [0 <+ a allegra_native_script]

allegra_auxiliary_data :: Rule
allegra_auxiliary_data =
  "auxiliary_data"
    =:= shelley_auxiliary_data
    / sarr
      [ "transaction_metadata" ==> shelley_auxiliary_data
      , "auxiliary_scripts" ==> allegra_auxiliary_scripts
      ]

transaction_body :: forall era. Era era => Rule
transaction_body =
  comment
    [str|Allegra transaction body adds the validity interval start at index 8
        |]
    $ "transaction_body"
      =:= mp
        [ idx 0 ==> untagged_set transaction_input
        , idx 1 ==> arr [0 <+ a shelley_transaction_output]
        , idx 2 ==> coin
        , opt (idx 3 ==> VUInt)
        , opt (idx 4 ==> arr [0 <+ a shelley_certificate])
        , opt (idx 5 ==> shelley_withdrawals)
        , opt (idx 6 ==> arr [a $ shelley_protocol_param_updates @era, a shelley_epoch])
        , opt (idx 7 ==> auxiliary_data_hash)
        , opt (idx 8 ==> VUInt)
        ]

block :: forall era. Era era => Rule
block =
  "block"
    =:= arr
      [ a $ shelley_header @era
      , "transaction_bodies" ==> arr [0 <+ a (transaction_body @era)]
      , "transaction_witness_sets" ==> arr [0 <+ a allegra_transaction_witness_set]
      , "auxiliary_data_set" ==> mp [0 <+ asKey transaction_ix ==> allegra_auxiliary_data]
      ]

transaction :: forall era. Era era => Rule
transaction =
  "transaction"
    =:= arr
      [ a $ transaction_body @era
      , a allegra_transaction_witness_set
      , a (allegra_auxiliary_data / VNil)
      ]

allegra_transaction_witness_set :: Rule
allegra_transaction_witness_set =
  "transaction_witness_set"
    =:= mp
      [ opt $ idx 0 ==> arr [0 <+ a vkey_witness]
      , opt $ idx 1 ==> arr [0 <+ a allegra_native_script]
      , opt $ idx 2 ==> arr [0 <+ a bootstrap_witness]
      ]
