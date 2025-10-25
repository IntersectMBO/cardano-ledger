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
  maryCDDL,
  mary_multiasset,
  mary_transaction_output,
  mary_mint,
  mary_value,
  mary_asset_name,
  mary_policy_id,
) where

import Cardano.Ledger.Core (Era)
import Cardano.Ledger.Mary (MaryEra)
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Word (Word64)
import Test.Cardano.Ledger.Allegra.CDDL

maryCDDL :: Huddle
maryCDDL =
  collectFrom
    [ HIRule $ block @MaryEra
    , HIRule $ transaction @MaryEra
    ]

mary_multiasset :: IsType0 a => a -> GRuleCall
mary_multiasset =
  binding $ \x ->
    "multiasset"
      =:= mp [0 <+ asKey mary_policy_id ==> mp [1 <+ asKey mary_asset_name ==> x]]

mary_policy_id :: Rule
mary_policy_id = "policy_id" =:= script_hash

mary_asset_name :: Rule
mary_asset_name = "asset_name" =:= VBytes `sized` (0 :: Word64, 32 :: Word64)

mary_value :: Rule
mary_value = "mary_value" =:= coin / sarr [a coin, a (mary_multiasset VUInt)]

mary_mint :: Rule
mary_mint = "mint" =:= mary_multiasset int64

transaction_body :: forall era. Era era => Rule
transaction_body =
  "transaction_body"
    =:= mp
      [ idx 0 ==> untagged_set transaction_input
      , idx 1 ==> arr [0 <+ a mary_transaction_output]
      , idx 2 ==> coin
      , opt (idx 3 ==> VUInt)
      , opt (idx 4 ==> arr [0 <+ a shelley_certificate])
      , opt (idx 5 ==> shelley_withdrawals)
      , opt (idx 6 ==> arr [a $ shelley_protocol_param_updates @era, a shelley_epoch])
      , opt (idx 7 ==> auxiliary_data_hash)
      , opt (idx 8 ==> VUInt)
      , opt (idx 9 ==> mary_mint)
      ]

mary_transaction_output :: Rule
mary_transaction_output = "mary_transaction_output" =:= arr [a address, "amount" ==> mary_value]

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
