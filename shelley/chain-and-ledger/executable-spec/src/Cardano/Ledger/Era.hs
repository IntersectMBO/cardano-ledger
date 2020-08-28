{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Support for multiple (Shelley-based) eras in the ledger.
module Cardano.Ledger.Era
  ( Era,
    Crypto,
  )
where

import qualified Cardano.Ledger.Crypto as CryptoClass
import Data.Kind (Type)
import Data.Typeable (Typeable)

class
  ( CryptoClass.Crypto (Crypto e),
    Typeable e
  ) =>
  Era e
  where
  type Crypto e :: Type
