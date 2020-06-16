{-# OPTIONS_GHC -Wno-orphans #-}

module Shelley.Spec.Ledger.Orphans where

import Cardano.Prelude (NFData (..), NoUnexpectedThunks)
import Cardano.Slotting.Slot (WithOrigin (..))
import Data.IP (IPv4, IPv6)
import Data.Sequence.Strict (StrictSeq, getSeq)
import Shelley.Spec.Ledger.Slot (BlockNo, EpochNo)

instance NoUnexpectedThunks IPv4

instance NoUnexpectedThunks IPv6

instance NFData IPv4

instance NFData IPv6

{- The following NFData instances probably belong in base -}
instance NFData EpochNo

instance NFData (StrictSeq a) where
  rnf x = case getSeq x of _any -> ()

instance NFData a => NFData (WithOrigin a)

instance NFData BlockNo

{- ------------------------------------------------------ -}
