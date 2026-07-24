{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.Ledger.CanonicalState.Namespace.EntitiesAccounts.V0.CDDL where

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
        |     type: credential
        |
        | types:
        |   credential:
        |     seq:
        |       - id: tag
        |         size: 1
        |       - id: hash
        |         size: 28
        | ```
        |]
    $ "record_entry" =:= account_state

account_state :: Rule
account_state =
  "account_state"
    =:= mp
      [ "balance" ==> coin
      , "deposit" ==> coin
      , "drep_delegation" ==> drep / VNil
      , "stake_pool_delegation" ==> pool_keyhash / VNil
      ]

drep :: Rule
drep =
  comment
    [str| 0 - key hash
              | 1 - script hash
              | 2 - always abstain
              | 3 - always no confidence
        |]
    $ "drep"
      =:= arr [0, a keyhash28]
      / arr [1, a script_hash]
      / arr [2]
      / arr [3]
