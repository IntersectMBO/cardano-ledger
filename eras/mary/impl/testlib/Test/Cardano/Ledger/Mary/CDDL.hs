{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Evaluate" #-}

module Test.Cardano.Ledger.Mary.CDDL (
  module Test.Cardano.Ledger.Allegra.CDDL,
  module Test.Cardano.Ledger.Mary.CDDL,
) where

import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Word (Word64)
import Test.Cardano.Ledger.Allegra.CDDL hiding (
  block,
  transaction,
  transaction_body,
  transaction_output,
 )

maryCDDL :: Huddle
maryCDDL = collectFrom [HIRule block, HIRule transaction]

multiasset :: IsType0 a => a -> GRuleCall
multiasset =
  binding $ \x ->
    "multiasset"
      =:= mp [1 <+ asKey policy_id ==> mp [1 <+ asKey asset_name ==> x]]

policy_id :: Rule
policy_id = "policy_id" =:= script_hash

asset_name :: Rule
asset_name = "asset_name" =:= VBytes `sized` (0 :: Word64, 32 :: Word64)

value :: Rule
value = "value" =:= coin / sarr [a coin, a (multiasset VUInt)]

mint :: Rule
mint = "mint" =:= multiasset int64

transaction_body :: Rule
transaction_body =
  "transaction_body"
    =:= mp
      [ idx 0 ==> set transaction_input
      , idx 1 ==> arr [0 <+ a transaction_output]
      , idx 2 ==> coin
      , opt (idx 3 ==> VUInt)
      , opt (idx 4 ==> arr [0 <+ a certificate])
      , opt (idx 5 ==> withdrawals)
      , opt (idx 6 ==> update)
      , opt (idx 7 ==> metadata_hash)
      , opt (idx 8 ==> VUInt)
      , opt (idx 9 ==> mint)
      ]

transaction_output :: Rule
transaction_output = "transaction_output" =:= arr [a address, "amount" ==> value]

block :: Rule
block =
  "block"
    =:= arr
      [ a header
      , "transaction_bodies" ==> arr [0 <+ a transaction_body]
      , "transaction_witness_sets" ==> arr [0 <+ a transaction_witness_set]
      , "auxiliary_data_set" ==> mp [0 <+ asKey transaction_index ==> auxiliary_data]
      ]

transaction :: Rule
transaction =
  "transaction"
    =:= arr
      [ a transaction_body
      , a transaction_witness_set
      , a (auxiliary_data / VNil)
      ]
