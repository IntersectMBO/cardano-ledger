{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module Cardano.Ledger.EraDefinitions where

import qualified Cardano.Ledger.Crypto as CryptoClass
import Shelley.Spec.Ledger.Coin
import Cardano.Ledger.Era

--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

data Shelley c

instance CryptoClass.Crypto c => Era (Shelley c) where
  type Crypto (Shelley c) = c
  type ValueType (Shelley c) = Coin
