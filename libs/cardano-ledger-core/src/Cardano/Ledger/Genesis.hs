{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Genesis (
  EraGenesis (..),
  NoGenesis (..),
) where

import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode)
import Cardano.Ledger.Core.Era (Era)
import Data.Kind (Type)

class Era era => EraGenesis era where
  type Genesis era :: Type
  type Genesis era = NoGenesis era

data NoGenesis era = NoGenesis
  deriving (Eq, Show)

instance Era era => EncCBOR (NoGenesis era) where
  encCBOR _ = encode $ Rec NoGenesis

instance Era era => DecCBOR (NoGenesis era) where
  decCBOR = decode $ RecD NoGenesis
