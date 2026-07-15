{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.Ledger.CanonicalState.Namespace.GovProposals.Roots.V0.CDDL where

import Cardano.Ledger.CanonicalState.Namespace.CDDL.Common
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Text.Heredoc (str)

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
        |     type: u1
        |     enum: gov_proposals_purpose
        | 
        |   enums:
        |     gov_proposals_purpose:
        |       0: pparams_update
        |       1: hard_fork
        |       2: committee
        |       3: constitution
        | ```
        |
        |]
    $ "record_entry" =:= gov_action_id
