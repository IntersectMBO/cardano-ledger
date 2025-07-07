{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Evaluate" -}

module Test.Cardano.Ledger.Mary.CDDL (
  module Test.Cardano.Ledger.Allegra.CDDL,
  module Test.Cardano.Ledger.Mary.CDDL,
) where

import Cardano.Ledger.Core (Era)
import Cardano.Ledger.Mary (MaryEra)
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
maryCDDL =
  collectFrom
    [ HIRule $ block @MaryEra
    , HIRule $ transaction @MaryEra
    ]

multiasset :: IsType0 a => a -> GRuleCall
multiasset =
  binding $ \x ->
    "multiasset"
      =:= mp [0 <+ asKey policy_id ==> mp [1 <+ asKey asset_name ==> x]]

policy_id :: Rule
policy_id = "policy_id" =:= script_hash

asset_name :: Rule
asset_name = "asset_name" =:= VBytes `sized` (0 :: Word64, 32 :: Word64)

value :: Rule
value = "value" =:= coin / sarr [a coin, a (multiasset VUInt)]

mint :: Rule
mint = "mint" =:= multiasset int64

transaction_body :: forall era. Era era => Rule
transaction_body =
  "transaction_body"
    =:= mp
      [ idx 0 ==> set transaction_input
      , idx 1 ==> arr [0 <+ a transaction_output]
      , idx 2 ==> coin
      , opt (idx 3 ==> VUInt)
      , opt (idx 4 ==> arr [0 <+ a certificate])
      , opt (idx 5 ==> withdrawals)
      , opt (idx 6 ==> update @era)
      , opt (idx 7 ==> metadata_hash)
      , opt (idx 8 ==> VUInt)
      , opt (idx 9 ==> mint)
      ]

transaction_output :: Rule
transaction_output = "transaction_output" =:= arr [a address, "amount" ==> value]

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
