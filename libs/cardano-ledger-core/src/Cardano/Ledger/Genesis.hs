{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Genesis (
  EraGenesis (..),
  NoGenesis (..),
) where

import Cardano.Ledger.Core.Era (Era)
import Data.Kind (Type)

class Era era => EraGenesis era where
  type Genesis era :: Type
  type Genesis era = NoGenesis era

data NoGenesis era = NoGenesis
  deriving (Eq, Show)
