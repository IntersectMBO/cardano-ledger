{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Shelley where

import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

data Shelley c

instance CryptoClass.Crypto c => Era (Shelley c) where
  type Crypto (Shelley c) = c
