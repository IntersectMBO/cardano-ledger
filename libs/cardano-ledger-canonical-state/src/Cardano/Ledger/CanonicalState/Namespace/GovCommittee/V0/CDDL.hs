{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Cardano.Ledger.CanonicalState.Namespace.GovCommittee.V0.CDDL where

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
        |     type: gov_committee
        |
        | types:
        |   gov_committee:
        |     seq:
        |       - id: singleton
        |         doc: singleton key with value 0
        |         type: u1
        |         valid: 0
        | ```
        |]
    $ "record_entry" =:= committee / VNil

committee :: Rule
committee =
  comment
    [str| Storage of the committee members
        |]
    $ "committee"
      =:= arr
        [ a (mp [0 <+ asKey credential ==> epoch_no])
        , a unit_interval
        ]
