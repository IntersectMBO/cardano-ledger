{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.Ledger.CanonicalState.Namespace.EntitiesCommittee.V0.CDDL where

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
        |     type: entities_committee
        |
        | types:
        |   entities_committee:
        |     seq:
        |       - id: singleton
        |         doc: singleton key with value 0
        |         type: u1
        |         valid: 0
        | ```
        |]
    $ "record_entry" =:= committee_state

committee_state :: Rule
committee_state =
  comment
    [str| Storage of the committee state
        |]
    $ "committee_state" =:= (mp [0 <+ asKey credential ==> committee_authorization])

committee_authorization :: Rule
committee_authorization =
  comment
    [str| 0 - hot committee member
              | 1 - resignation
              |]
    $ "committee_authorization"
      =:= arr [0, a credential]
      / arr [1, a (anchor / VNil)]
