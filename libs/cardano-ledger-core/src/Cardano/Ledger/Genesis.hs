{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Genesis (
  EraGenesis (..),
  NoGenesis (..),
) where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
 )
import Cardano.Ledger.Core.Era (Era)
import Data.Kind (Type)

class Era era => EraGenesis era where
  type Genesis era :: Type
  type Genesis era = NoGenesis era

data NoGenesis era = NoGenesis
  deriving (Eq, Show)

instance Era era => ToCBOR (NoGenesis era) where
  toCBOR _ = toCBOR ()

instance Era era => FromCBOR (NoGenesis era) where
  fromCBOR = NoGenesis <$ fromCBOR @()

instance Era era => EncCBOR (NoGenesis era)

instance Era era => DecCBOR (NoGenesis era)
