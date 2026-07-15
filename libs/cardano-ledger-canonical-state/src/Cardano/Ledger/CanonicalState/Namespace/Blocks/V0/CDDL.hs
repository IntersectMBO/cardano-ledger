{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Cardano.Ledger.CanonicalState.Namespace.Blocks.V0.CDDL where

import Codec.CBOR.Cuddle.Huddle
import Text.Heredoc (str)
import Prelude (($))

record_entry :: Rule
record_entry =
  comment
    [str| Values for the blocks.
       |
       | Key definition:
       |
       | ```
       | meta:
       |   endian: be
       |
       | seq:
       |   - id: key
       |     type: blocks
       |
       | types:
       |   block:
       |     seq:
       |       - id: keyhash_stakepool
       |         doc: keyhash of the stake pool
       |         size: 28
       |       - id: epoch
       |         doc: epoch
       |         type: u8
       | ```
       |]
    $ "record_entry" =:= VInt
