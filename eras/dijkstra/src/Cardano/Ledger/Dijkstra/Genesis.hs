{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Genesis (
  DijkstraGenesis (..),
) where

import Cardano.Ledger.BaseTypes (KeyValuePairs (..), ToKeyValuePairs (..))
import Cardano.Ledger.Binary (
  FromCBOR (..),
  ToCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Genesis (EraGenesis (..))
import Data.Aeson (FromJSON (..), ToJSON, withObject)
import GHC.Generics
import NoThunks.Class (NoThunks)

-- TODO: Currently it is just a placeholder for all the new protocol parameters that will be added
-- in the Dijkstra era
data DijkstraGenesis = DijkstraGenesis
  deriving (Eq, Show, Generic)
  deriving (ToJSON) via KeyValuePairs DijkstraGenesis

instance FromJSON DijkstraGenesis where
  parseJSON = withObject "DijkstraGenesis" $ \_ -> pure DijkstraGenesis

instance NoThunks DijkstraGenesis

instance EraGenesis DijkstraEra where
  type Genesis DijkstraEra = DijkstraGenesis

-- TODO: Implement this and use for ToJSON instance
instance ToKeyValuePairs DijkstraGenesis where
  toKeyValuePairs DijkstraGenesis = []

instance FromCBOR DijkstraGenesis where
  fromCBOR =
    eraDecoder @DijkstraEra $
      decode $
        RecD DijkstraGenesis

instance ToCBOR DijkstraGenesis where
  toCBOR DijkstraGenesis =
    toEraCBOR @DijkstraEra . encode $
      Rec DijkstraGenesis

instance DecCBOR DijkstraGenesis

instance EncCBOR DijkstraGenesis
