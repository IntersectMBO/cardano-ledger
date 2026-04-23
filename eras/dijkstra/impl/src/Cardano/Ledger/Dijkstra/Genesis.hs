{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Genesis (
  DijkstraGenesis (..),
) where

import Cardano.Ledger.BaseTypes (ToKeyValuePairs (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  decodeRecordNamed,
  encodeListLen,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams (UpgradeDijkstraPParams)
import Cardano.Ledger.Genesis (EraGenesis (..))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (..), ToJSON)
import Data.Functor.Identity (Identity)
import GHC.Generics
import NoThunks.Class (NoThunks)

newtype DijkstraGenesis = DijkstraGenesis
  { dgUpgradePParams :: UpgradeDijkstraPParams Identity DijkstraEra
  }
  deriving (Eq, Show, Generic, NoThunks, ToJSON, FromJSON, ToKeyValuePairs, NFData)

instance EraGenesis DijkstraEra where
  type Genesis DijkstraEra = DijkstraGenesis

instance FromCBOR DijkstraGenesis where
  fromCBOR = fromEraCBOR @DijkstraEra
  {-# INLINE fromCBOR #-}

instance ToCBOR DijkstraGenesis where
  toCBOR dg@(DijkstraGenesis _) =
    toEraCBOR @DijkstraEra $
      let DijkstraGenesis {..} = dg
       in encodeListLen 1
            <> encCBOR dgUpgradePParams

instance DecCBOR DijkstraGenesis where
  decCBOR =
    decodeRecordNamed "DijkstraGenesis" (const 1) $
      DijkstraGenesis <$> decCBOR
  {-# INLINE decCBOR #-}

instance EncCBOR DijkstraGenesis
