{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.Ledger.CanonicalState.Namespace.EntitiesDReps.V0.CDDL where

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
    $ "record_entry" =:= drep_state

drep_state :: Rule
drep_state =
  "drep_state"
    =:= mp
      [ "expiry" ==> epoch_no
      , "anchor" ==> anchor / VNil
      , "deposit" ==> coin
      , "delegations" ==> set credential
      ]
