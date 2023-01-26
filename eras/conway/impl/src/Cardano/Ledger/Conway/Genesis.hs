{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Genesis (
  ConwayGenesis (..),
)
where

import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis, alonzoGenesisAesonPairs)
import Cardano.Ledger.Binary (EncCBOR (..), DecCBOR (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (GenDelegs (..))
import Data.Aeson (FromJSON (..), ToJSON, object, withObject, (.:))
import qualified Data.Aeson as Aeson (Value (Object))
import Data.Aeson.Types (KeyValue (..), ToJSON (..))
import Data.Unit.Strict (forceElemsToWHNF)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

data ConwayGenesis c = ConwayGenesis
  { cgAlonzoGenesis :: !AlonzoGenesis
  , cgGenDelegs :: !(GenDelegs c)
  }
  deriving (Eq, Generic, Show)

instance NoThunks (ConwayGenesis c)

instance Crypto c => DecCBOR (ConwayGenesis c) where
  decCBOR = do
    (cgAlonzoGenesis, cgGenDelegs) <- decCBOR
    pure ConwayGenesis {..}

instance Crypto c => EncCBOR (ConwayGenesis c) where
  encCBOR ConwayGenesis {cgAlonzoGenesis, cgGenDelegs} =
    encCBOR (cgAlonzoGenesis, cgGenDelegs)

instance Crypto c => ToJSON (ConwayGenesis c) where
  toJSON (ConwayGenesis alonzoGenesis genDelegs) =
    object (alonzoGenesisAesonPairs alonzoGenesis ++ ["genDelegs" .= toJSON genDelegs])

instance Crypto c => FromJSON (ConwayGenesis c) where
  parseJSON = withObject "ConwayGenesis" $ \obj -> do
    alonzoGenesis <- parseJSON (Aeson.Object obj)
    genDelegs <- forceElemsToWHNF obj .: "genDelegs"
    pure $ ConwayGenesis alonzoGenesis genDelegs
