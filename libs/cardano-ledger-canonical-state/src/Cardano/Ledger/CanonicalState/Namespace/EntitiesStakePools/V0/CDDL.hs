{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.Ledger.CanonicalState.Namespace.EntitiesStakePools.V0.CDDL where

import Cardano.Ledger.CanonicalState.Namespace.CDDL.Common
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Text.Heredoc (str)

record_entry :: Rule
record_entry =
  comment
    [str| The key for the namespace
        |
        | ```
        | meta:
        |   endian: be
        |
        | seq:
        |   - id: key
        |     type: keyhash
        |
        | types:
        |   keyhash:
        |     seq:
        |       - id: keyhash_data
        |         size: 28
        | ```
        |]
    $ "record_entry" =:= stake_pool

stake_pool :: Rule
stake_pool =
  "stake_pool"
    =:= mp
      [ "stake_pool_state" ==> stake_pool_state / VNil
      , "retiring_epoch_no" ==> epoch_no / VNil
      , "future_stake_pool_params" ==> stake_pool_params / VNil
      ]

stake_pool_state :: Rule
stake_pool_state =
  "stake_pool_state"
    =:= mp
      [ "vrf" ==> vrf_keyhash
      , "cost" ==> coin
      , "margin" ==> unit_interval
      , "owners" ==> set staking_keyhash
      , "pledge" ==> coin
      , "relays" ==> arr [0 <+ a relay]
      , "deposit" ==> coin
      , "metadata" ==> pool_metadata / VNil
      , "account_id" ==> account_id
      , "delegators" ==> set credential
      ]

stake_pool_params :: Rule
stake_pool_params =
  "stake_pool_params"
    =:= mp
      [ "id" ==> pool_keyhash
      , "vrf" ==> vrf_keyhash
      , "cost" ==> coin
      , "margin" ==> unit_interval
      , "owners" ==> set staking_keyhash
      , "pledge" ==> coin
      , "relays" ==> arr [0 <+ a relay]
      , "metadata" ==> pool_metadata / VNil
      , "account_address" ==> address
      ]
