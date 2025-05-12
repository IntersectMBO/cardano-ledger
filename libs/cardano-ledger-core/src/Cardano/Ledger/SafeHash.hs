{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
) where

import Cardano.Ledger.Hashes
import Data.Proxy

indexProxy :: forall x i. HashAnnotated x i => x -> Proxy i
indexProxy _ = Proxy @i
{-# DEPRECATED indexProxy "As useless and unused" #-}
