{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Cardano.Ledger.CanonicalState.Namespace.GovPParams.V0.CDDL where

import Cardano.Ledger.CanonicalState.Namespace.CDDL.Common
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Word (Word64)
import Text.Heredoc (str)
import Prelude (Integer, ($))

record_entry :: Rule
record_entry =
  comment
    [str| Specification for parameters
        |
        | ```
        | meta:
        |   endian: be
        |
        | seq:
        |   - id: key
        |     type: gov_pparams
        |
        | types:
        |   gov_pparams:
        |     seq:
        |       - id: value
        |         type: strz
        |         size: 4
        |         encoding: ASCII
        |       - id: variant
        |         size: 0
        |         type:
        |           switch-on: value
        |           cases:
        |             '"prev"': kprev
        |             '"curr"': kcurr
        |             '"fut0"': pfut
        |             '"fut1"': dfut
        |   kprev:
        |     doc: previous values
        |   kcurr:
        |     doc: current values
        |   pfut:
        |     doc: possible future
        |   dfut:
        |     doc: definitive future
        | ```
        |
        | fut0 with missig parameters should be omitted.
        |]
    $ "record_entry" =:= gov_pparams_out

gov_pparams_out :: Rule
gov_pparams_out =
  comment
    [str| Governance protocol parameters |]
    $ "gov_pparams_out"
      =:= mp
        [ (idx 0 ==> coin) //- "min_fee_a: the linear factor for the minimum fee calculation"
        , (idx 1 ==> coin) //- "min_fee_b: the constant factor for the minimum fee calculation"
        , (idx 2 ==> VUInt `sized` (4 :: Word64)) //- "max_block_size: maximal block body size in bytes"
        , (idx 3 ==> VUInt `sized` (4 :: Word64)) //- "max_tx_size: maximal transaction size in bytes"
        , (idx 4 ==> VUInt `sized` (2 :: Word64))
            //- "max_block_header_size: maximal block header size in bytes"
        , (idx 5 ==> coin) //- "key_deposit: The amount of a key registration deposit"
        , (idx 6 ==> coin) //- "pool_deposit: The amount of a pool registration deposit"
        , (idx 7 ==> epoch_interval) //- "maximum_epoch: maximum epoch"
        , (idx 8 ==> VUInt `sized` (2 :: Word64)) //- "n_opt: desired number of pools"
        , (idx 9 ==> nonnegative_interval) //- "a0: pool pledge influence factor"
        , (idx 10 ==> unit_interval) //- "rho: monetary expansion rate"
        , (idx 11 ==> unit_interval) //- "tau: treasury expansion"
        , (idx 14 ==> protocol_version) //- "protocol_version: protocol version"
        , (idx 16 ==> coin) //- "min_pool_cost: minimum pool cost"
        , (idx 17 ==> coin)
            //- "ada_per_utxo_byte: Cost in ada per 1 byte of UTxO storage instead of _coinsPerUTxOWord"
        , (idx 18 ==> cost_models) //- "cost_models: Cost models for non-native script languages"
        , (idx 19 ==> ex_unit_prices) //- "prices: Prices of execution units for non-native script languages"
        , (idx 20 ==> ex_units)
            //- "max_tx_ex_units: Max total script execution resources units allowed per tx"
        , (idx 21 ==> ex_units)
            //- "max_block_ex_units: Max total script execution resources units allowed per block"
        , (idx 22 ==> VUInt `sized` (4 :: Word64)) //- "max_value_size: Max size of a Value in an output"
        , (idx 23 ==> VUInt `sized` (2 :: Word64))
            //- "collateral_percentage: The scaling percentage of the collateral relative to the fee"
        , (idx 24 ==> VUInt `sized` (2 :: Word64))
            //- "max_collateral_inputs: Maximum number of collateral inputs allowed in a transaction"
        , (idx 25 ==> pool_voting_thresholds) //- "pool voting thresholds"
        , (idx 26 ==> drep_voting_thresholds) //- "drep voting thresholds"
        , (idx 27 ==> VUInt `sized` (2 :: Word64)) //- "min committee size"
        , (idx 28 ==> epoch_interval) //- "committee term limit"
        , (idx 29 ==> epoch_interval) //- "governance action validity period"
        , (idx 30 ==> coin) //- "governance action deposit"
        , (idx 31 ==> coin) //- "drep deposit"
        , (idx 32 ==> epoch_interval) //- "drep inactivity period"
        , (idx 33 ==> nonnegative_interval)
            //- "min_fee_ref_script_cost_per_byte: Reference scripts fee for the minimum fee calculation"
        ]

epoch_interval :: Rule
epoch_interval = "epoch_interval" =:= VUInt `sized` (4 :: Word64)

cost_models :: Rule
cost_models =
  comment
    [str|The format for cost_models is flexible enough to allow adding
        |Plutus built-ins and language versions in the future.
        |
        |Plutus v1: only 166 integers are used, but more are accepted (and ignored)
        |Plutus v2: only 175 integers are used, but more are accepted (and ignored)
        |Plutus v3: only 223 integers are used, but more are accepted (and ignored)
        |
        |Any 8-bit unsigned number can be used as a key.
        |]
    $ "cost_models"
      =:= mp
        [ opt $ idx 0 ==> arr [0 <+ a int64]
        , opt $ idx 1 ==> arr [0 <+ a int64]
        , opt $ idx 2 ==> arr [0 <+ a int64]
        , 0 <+ asKey ((3 :: Integer) ... (255 :: Integer)) ==> arr [0 <+ a int64]
        ]

ex_unit_prices :: Rule
ex_unit_prices =
  "ex_unit_prices"
    =:= arr
      [ "mem_price" ==> nonnegative_interval
      , "step_price" ==> nonnegative_interval
      ]

ex_units :: Rule
ex_units = "ex_units" =:= arr ["mem" ==> VUInt, "steps" ==> VUInt]

pool_voting_thresholds :: Rule
pool_voting_thresholds =
  comment
    [str|
    | 0 - motion no confidence
    | 1 - committee normal
    | 2 - committee no confidence
    | 3 - hard fork initiation
    | 4 - security relevant parameter voting threshold
    |]
    $ "pool_voting_thresholds"
      =:= arr
        [ a unit_interval -- motion no confidence
        , a unit_interval -- committee normal
        , a unit_interval -- committee no confidence
        , a unit_interval -- hard fork initiation
        , a unit_interval -- security relevant parameter voting threshold
        ]

drep_voting_thresholds :: Rule
drep_voting_thresholds =
  comment
    [str|
    | 0 - motion no confidence
    | 1 - committee normal
    | 2 - committee no confidence
    | 3 - update constitution
    | 4 - hard fork initiation
    | 5 - PP network group
    | 6 - PP economic group
    | 7 - PP technical group
    | 8 - PP governance group
    | 9 - treasury withdrawal
    |]
    $ "drep_voting_thresholds"
      =:= arr
        [ a unit_interval -- motion no confidence
        , a unit_interval -- committee normal
        , a unit_interval -- committee no confidence
        , a unit_interval -- update constitution
        , a unit_interval -- hard fork initiation
        , a unit_interval -- PP network group
        , a unit_interval -- PP economic group
        , a unit_interval -- PP technical group
        , a unit_interval -- PP governance group
        , a unit_interval -- treasury withdrawal
        ]
