{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Genesis (
  DijkstraGenesis (..),
  toDijkstraGenesisPairs,
) where

import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Genesis (EraGenesis (..))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import NoThunks.Class (NoThunks)

-- TODO: Currently it is just a placeholder for all the new protocol parameters that will be added
-- in the Dijkstra era
data DijkstraGenesis = DijkstraGenesis
  deriving (Eq, Show, Generic)

instance ToJSON DijkstraGenesis

instance FromJSON DijkstraGenesis

instance NoThunks DijkstraGenesis

instance EraGenesis DijkstraEra where
  type Genesis DijkstraEra = DijkstraGenesis

-- TODO: Implement this and use for ToJSON instance
toDijkstraGenesisPairs :: DijkstraGenesis -> [a]
toDijkstraGenesisPairs _ = []
