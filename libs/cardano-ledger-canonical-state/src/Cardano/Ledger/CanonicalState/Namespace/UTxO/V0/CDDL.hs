{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Cardano.Ledger.CanonicalState.Namespace.UTxO.V0.CDDL where

import Cardano.Ledger.CanonicalState.Namespace.CDDL.Common
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Text.Heredoc (str)
import Prelude (foldr, (++))

record_entry :: Rule
record_entry =
  comment
    [str| The key for the entry is one of the following:
        |
        | ```
        | meta:
        |   endian: be
        |
        | seq:
        |   - id: key
        |     type: utxo_key
        |
        | types:
        |   utxo_key:
        |     seq:
        |      - id: tx_addr
        |        doc: transaction
        |        type: bytes
        |        size: 28
        |      - id: tx_idx
        |        doc: index inside transaction
        |        type: u4
        | ```
        |
        |]
    $ "record_entry" =:= tx_out

tx_out :: Rule
tx_out = "tx_out" =:= VBytes `cbor` tx_rule

tx_rule :: Rule
tx_rule = "rule" =:= shelley_tx_out / babbage_tx_out

shelley_tx_out :: Rule
shelley_tx_out =
  "shelley_tx_out"
    =:= arr [a address, "amount" ==> value, opt ("datum_hash" ==> hash32)]

babbage_tx_out :: Rule
babbage_tx_out =
  comment
    [str|NEW starting with babbage
        |  datum_option
        |  script_ref
        |]
    $ "babbage_tx_out"
      =:= mp
        [ idx 0 ==> address
        , idx 1 ==> value
        , opt $ idx 2 ==> datum_option
        , opt $ idx 3 ==> script_ref
        ]

plutus_data :: Rule
plutus_data =
  "plutus_data"
    =:= constr plutus_data
    / smp [0 <+ asKey plutus_data ==> plutus_data]
    / sarr [0 <+ a plutus_data]
    / big_int
    / VBytes

data' :: Rule
data' = "data" =:= tag 24 (VBytes `cbor` plutus_data)

datum_option :: Rule
datum_option = "datum_option" =:= arr [0, a hash32] / arr [1, a data']

constr :: IsType0 x => x -> GRuleCall
constr = binding $ \x ->
  comment
    [str|NEW
        |  #6.102([uint, [* a]]): For tag range 6.1280 .. 6.1400 inclusive
        |]
    $ "constr"
      =:= foldr
        (/)
        ( tag 121 (arr [0 <+ a x])
            / tag 102 (arr [a VUInt, a $ arr [0 <+ a x]])
        )
        ( [tag i (arr [0 <+ a x]) | i <- [122 .. 127]]
            ++ [tag i (arr [0 <+ a x]) | i <- [1280 .. 1400]]
        )

script_ref :: Rule
script_ref = "script_ref" =:= tag 24 (VBytes `cbor` script)

script :: Rule
script =
  "script"
    =:= arr [0, a native_script]
    / (arr [1, a VBytes] //- "Plutus V1")
    / (arr [2, a VBytes] //- "Plutus V2")
    / (arr [3, a VBytes] //- "Plutus V3")

native_script :: Rule
native_script =
  "native_script"
    =:= arr [a script_pubkey]
    / arr [a script_all]
    / arr [a script_any]
    / arr [a script_n_of_k]
    / arr [a invalid_before]
    -- Timelock validity intervals are half-open intervals [a, b).
    -- This field specifies the left (included) endpoint a.
    / arr [a invalid_hereafter]

script_pubkey :: GroupDef
script_pubkey = "script_pubkey" =:~ grp [0, a hash28]

script_all :: GroupDef
script_all = "script_all" =:~ grp [1, a (arr [0 <+ a native_script])]

script_any :: GroupDef
script_any = "script_any" =:~ grp [2, a (arr [0 <+ a native_script])]

script_n_of_k :: GroupDef
script_n_of_k =
  "script_n_of_k"
    =:~ grp [3, "n" ==> int64, a (arr [0 <+ a native_script])]

invalid_before :: GroupDef
invalid_before = "invalid_before" =:~ grp [4, a slot_no]

invalid_hereafter :: GroupDef
invalid_hereafter = "invalid_hereafter" =:~ grp [5, a slot_no]
