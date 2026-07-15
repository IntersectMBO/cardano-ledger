{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.Ledger.CanonicalState.Namespace.GovProposals.V0.CDDL where

import Cardano.Ledger.CanonicalState.Namespace.CDDL.Common
import Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0.CDDL (constitution)
import Cardano.Ledger.CanonicalState.Namespace.GovPParams.V0.CDDL
import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Word (Word64)
import Text.Heredoc (str)
import Prelude (Integer)

record_entry :: Rule
record_entry =
  comment
    [str| Size of the key
        |
        | ```
        | meta:
        |   endian: be
        |
        | seq:
        |   - id: key
        |     type: gov_proposals
        |
        | gov_proposals:
        |   seq:
        |     - id: tx_addr
        |       doc: transaction
        |       type: bytes
        |       size: 28
        |     - id: tx_idx
        |       doc: index inside transaction
        |       type: u4
        |     - id: gov_action_idx
        |       doc: governance action index
        |       type: u2
        | ```
        |
        |]
    $ "record_entry" =:= arr [a word64, a proposal]

committee_cold_credential :: Rule
committee_cold_credential = "committee_cold_credential" =:= credential

proposal :: Rule
proposal =
  "proposal"
    =:= mp
      [ "drep_votes" ==> mp [0 <+ asKey credential ==> vote]
      , "proposed_in" ==> epoch_no
      , "expires_after" ==> epoch_no
      , "committee_votes" ==> mp [0 <+ asKey committee_cold_credential ==> vote]
      , "stake_pool_votes" ==> mp [0 <+ asKey pool_keyhash ==> vote]
      , "proposal_procedure" ==> VBytes `cbor` proposal_procedure
      ]

vote :: Rule
vote = "vote" =:= (0 :: Integer) ... (2 :: Integer)

proposal_procedure :: Rule
proposal_procedure =
  "proposal_procedure"
    =:= arr
      [ "deposit" ==> coin
      , "return_address" ==> reward_account
      , "gov_action" ==> gov_action
      , "anchor" ==> anchor
      ]

gov_action :: Rule
gov_action =
  "gov_action"
    =:= ( arr
            [ 0
            , "purpose" ==> (gov_action_id / VNil)
            , "update" ==> gov_params_update
            , "hash" ==> (script_hash / VNil)
            ]
            //- "Params update"
        )
    / (arr [1, a (gov_action_id / VNil), a protocol_version] //- "Hard fork")
    / ( arr
          [ 2
          , "withdrawls" ==> mp [0 <+ asKey reward_account ==> coin]
          , a (script_hash / VNil)
          ]
          //- "Treasury withdraw"
      )
    / (arr [3, "purpose" ==> (gov_action_id / VNil)] //- "No confidence")
    / ( arr
          [ 4
          , "purpose" ==> (gov_action_id / VNil)
          , "removed" ==> set (credential)
          , "added" ==> mp [0 <+ asKey credential ==> epoch_no]
          , "threshold" ==> unit_interval
          ]
          //- "Committee membership update"
      )
    / ( arr [5, "purpose" ==> (gov_action_id / VNil), "constitution" ==> constitution]
          //- "New constitution"
      )
    / (arr [6] //- "Info action")

gov_params_update :: Rule
gov_params_update =
  comment
    [str| Governance protocol parameters |]
    $ "gov_pparams_update"
      =:= mp
        [ opt (idx 0 ==> coin) //- "min_fee_a: the linear factor for the minimum fee calculation"
        , opt (idx 1 ==> coin) //- "min_fee_b: the constant factor for the minimum fee calculation"
        , opt (idx 2 ==> VUInt `sized` (4 :: Word64)) //- "max_block_size: maximal block body size in bytes"
        , opt (idx 3 ==> VUInt `sized` (4 :: Word64)) //- "max_tx_size: maximal transaction size in bytes"
        , opt (idx 4 ==> VUInt `sized` (2 :: Word64))
            //- "max_block_header_size: maximal block header size in bytes"
        , opt (idx 5 ==> coin) //- "key_deposit: The amount of a key registration deposit"
        , opt (idx 6 ==> coin) //- "pool_deposit: The amount of a pool registration deposit"
        , opt (idx 7 ==> epoch_interval) //- "maximum_epoch: maximum epoch"
        , opt (idx 8 ==> VUInt `sized` (2 :: Word64)) //- "n_opt: desired number of pools"
        , opt (idx 9 ==> nonnegative_interval) //- "a0: pool pledge influence factor"
        , opt (idx 10 ==> unit_interval) //- "rho: monetary expansion rate"
        , opt (idx 11 ==> unit_interval) //- "tau: treasury expansion"
        , opt (idx 16 ==> coin) //- "min_pool_cost: minimum pool cost"
        , opt (idx 17 ==> coin)
            //- "ada_per_utxo_byte: Cost in ada per 1 byte of UTxO storage instead of _coinsPerUTxOWord"
        , opt (idx 18 ==> cost_models) //- "cost_models: Cost models for non-native script languages"
        , opt (idx 19 ==> ex_unit_prices)
            //- "prices: Prices of execution units for non-native script languages"
        , opt (idx 20 ==> ex_units)
            //- "max_tx_ex_units: Max total script execution resources units allowed per tx"
        , opt (idx 21 ==> ex_units)
            //- "max_block_ex_units: Max total script execution resources units allowed per block"
        , opt (idx 22 ==> VUInt `sized` (4 :: Word64)) //- "max_value_size: Max size of a Value in an output"
        , opt (idx 23 ==> VUInt `sized` (2 :: Word64))
            //- "collateral_percentage: The scaling percentage of the collateral relative to the fee"
        , opt (idx 24 ==> VUInt `sized` (2 :: Word64))
            //- "max_collateral_inputs: Maximum number of collateral inputs allowed in a transaction"
        , opt (idx 25 ==> pool_voting_thresholds) //- "pool voting thresholds"
        , opt (idx 26 ==> drep_voting_thresholds) //- "drep voting thresholds"
        , opt (idx 27 ==> VUInt `sized` (2 :: Word64)) //- "min committee size"
        , opt (idx 28 ==> epoch_interval) //- "committee term limit"
        , opt (idx 29 ==> epoch_interval) //- "governance action validity period"
        , opt (idx 30 ==> coin) //- "governance action deposit"
        , opt (idx 31 ==> coin) //- "drep deposit"
        , opt (idx 32 ==> epoch_interval) //- "drep inactivity period"
        , opt (idx 33 ==> nonnegative_interval)
            //- "min_fee_ref_script_cost_per_byte: Reference scripts fee for the minimum fee calculation"
        ]
