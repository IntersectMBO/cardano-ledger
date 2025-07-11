{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Genesis (
  DijkstraGenesis (..),
) where

import Cardano.Ledger.BaseTypes (KeyValuePairs (..), ToKeyValuePairs (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams (UpgradeDijkstraPParams)
import Cardano.Ledger.Genesis (EraGenesis (..))
import Data.Aeson (FromJSON (..), ToJSON, Value (..), withObject)
import Data.Functor.Identity (Identity)
import GHC.Generics
import NoThunks.Class (NoThunks)

-- TODO: Currently it is just a placeholder for all the new protocol parameters that will be added
-- in the Dijkstra era
data DijkstraGenesis = DijkstraGenesis
  { dgUpgradePParams :: !(UpgradeDijkstraPParams Identity DijkstraEra)
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON) via KeyValuePairs DijkstraGenesis

instance FromJSON DijkstraGenesis where
  parseJSON = withObject "DijkstraGenesis" $ \obj -> do
    dgUpgradePParams <- parseJSON (Object obj)
    pure DijkstraGenesis {..}

instance NoThunks DijkstraGenesis

instance EraGenesis DijkstraEra where
  type Genesis DijkstraEra = DijkstraGenesis

-- TODO: Implement this and use for ToJSON instance
instance ToKeyValuePairs DijkstraGenesis where
  toKeyValuePairs dg@(DijkstraGenesis _) =
    let DijkstraGenesis {..} = dg
     in toKeyValuePairs dgUpgradePParams

instance FromCBOR DijkstraGenesis where
  fromCBOR =
    eraDecoder @DijkstraEra $
      decode $
        RecD DijkstraGenesis
          <! From

instance ToCBOR DijkstraGenesis where
  toCBOR dg@(DijkstraGenesis _) =
    let DijkstraGenesis {..} = dg
     in toEraCBOR @DijkstraEra . encode $
          Rec DijkstraGenesis
            !> To dgUpgradePParams

instance DecCBOR DijkstraGenesis

instance EncCBOR DijkstraGenesis
