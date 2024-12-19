module Cardano.Ledger.SafeHash
  {-# DEPRECATED "Use `Cardano.Ledger.Hashes` instead" #-} (
  -- * SafeHash and SafeToHash

  --
  -- $SAFE
  SafeHash,
  SafeToHash (..),

  -- * Creating SafeHash

  --
  -- $MAKE
  HashAnnotated,
  hashAnnotated,
  unsafeMakeSafeHash,

  -- * Other operations

  --
  -- $OTHER
  castSafeHash,
  extractHash,
  indexProxy,
)
where

import Cardano.Ledger.Hashes
