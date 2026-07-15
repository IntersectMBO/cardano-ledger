{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0.CDDL where

import Cardano.Ledger.CanonicalState.Namespace.CDDL.Common
import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Text.Heredoc (str)

record_entry :: Rule
record_entry =
  comment
    [str| Constitution record entry
        |
        | ```
        | meta:
        |   endian: be
        |
        | seq:
        |   - id: key
        |     type: gov_constitution
        |
        | types:
        |   gov_constitution:
        |     seq:
        |       - id: singleton
        |         doc: singleton key with value 0
        |         type: u1
        |.        valid: 0
        | ```
        |]
    $ "record_entry" =:= constitution

constitution :: Rule
constitution =
  comment
    [str| address of the constitution
        |]
    $ "constitution"
      =:= arr
        [ a anchor
        , a (script_hash / VNil)
        ]
