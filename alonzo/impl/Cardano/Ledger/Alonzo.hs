{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo where

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto
import Cardano.Ledger.Era
import Data.Kind (Type)
import Data.Typeable (Typeable)

-- | The Alonzo era
data AlonzoEra c

instance
  (Cardano.Ledger.Crypto.Crypto c) =>
  Era (AlonzoEra c)
  where
  type Crypto (AlonzoEra c) = c
