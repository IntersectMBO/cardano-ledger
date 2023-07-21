{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Genesis (
  ConwayGenesis (..),
)
where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Conway.PParams (UpgradeConwayPParams)
import Cardano.Ledger.Crypto (Crypto)
import Data.Aeson (FromJSON (..), ToJSON)
import Data.Aeson.Types (ToJSON (..))
import Data.Functor.Identity (Identity)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

newtype ConwayGenesis c = ConwayGenesis
  { cgUpgradePParams :: UpgradeConwayPParams Identity
  }
  deriving (Eq, Generic, Show)

instance NoThunks (ConwayGenesis c)

-- | Genesis are always encoded with the version of era they are defined in.
instance Crypto c => DecCBOR (ConwayGenesis c) where
  decCBOR = ConwayGenesis <$> decCBOR

instance Crypto c => EncCBOR (ConwayGenesis c) where
  encCBOR (ConwayGenesis x) = encCBOR x

instance Crypto c => ToJSON (ConwayGenesis c) where
  toJSON ConwayGenesis {..} = toJSON cgUpgradePParams

instance Crypto c => FromJSON (ConwayGenesis c) where
  parseJSON x = ConwayGenesis <$> parseJSON x
