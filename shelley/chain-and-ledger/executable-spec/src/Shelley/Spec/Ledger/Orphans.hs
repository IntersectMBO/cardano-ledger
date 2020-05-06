{-# OPTIONS_GHC -Wno-orphans #-}

module Shelley.Spec.Ledger.Orphans where

import Cardano.Prelude (NoUnexpectedThunks)
import Data.IP (IPv4, IPv6)

instance NoUnexpectedThunks IPv4

instance NoUnexpectedThunks IPv6
