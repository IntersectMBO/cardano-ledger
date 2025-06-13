{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Genesis (
  EraGenesis (..),
  NoGenesis (..),
) where

import Cardano.Ledger.BaseTypes (KeyValuePairs (..), ToKeyValuePairs (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
 )
import Cardano.Ledger.Core.Era (Era)
import Control.Monad (unless)
import Data.Aeson (
  FromJSON (..),
  ToJSON,
  Value (..),
 )
import qualified Data.Aeson.KeyMap as KV
import Data.Kind (Type)

class Era era => EraGenesis era where
  type Genesis era :: Type
  type Genesis era = NoGenesis era

data NoGenesis era = NoGenesis
  deriving (Eq, Show)
  deriving (ToJSON) via KeyValuePairs (NoGenesis era)

instance Era era => ToCBOR (NoGenesis era) where
  toCBOR _ = toCBOR ()

instance Era era => FromCBOR (NoGenesis era) where
  fromCBOR = NoGenesis <$ fromCBOR @()

instance Era era => EncCBOR (NoGenesis era)

instance Era era => DecCBOR (NoGenesis era)

instance ToKeyValuePairs (NoGenesis era) where
  toKeyValuePairs _ = []

instance FromJSON (NoGenesis era) where
  parseJSON = \case
    Null -> pure NoGenesis
    Object o -> do
      unless (KV.null o) $ fail "NoGenesis cannot have any fields"
      pure NoGenesis
    _ -> fail "Unexpected value type for NoGenesis"
