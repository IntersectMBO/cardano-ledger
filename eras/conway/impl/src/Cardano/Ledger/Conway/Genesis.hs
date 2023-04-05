{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Genesis (
  ConwayGenesis (..),
)
where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (GenDelegs (..))
import Data.Aeson (FromJSON (..), ToJSON, object, withObject, (.:))
import Data.Aeson.Types (KeyValue (..), ToJSON (..))
import Data.Unit.Strict (forceElemsToWHNF)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

newtype ConwayGenesis c = ConwayGenesis
  { cgGenDelegs :: GenDelegs c
  }
  deriving (Eq, Generic, Show)

instance NoThunks (ConwayGenesis c)

-- | Genesis are always encoded with the version of era they are defined in.
instance Crypto c => DecCBOR (ConwayGenesis c)

instance Crypto c => EncCBOR (ConwayGenesis c)

instance Crypto c => ToCBOR (ConwayGenesis c) where
  toCBOR cg@(ConwayGenesis _) =
    let ConwayGenesis {cgGenDelegs} = cg
     in toEraCBOR @(ConwayEra c) $
          encode $
            Rec ConwayGenesis
              !> To cgGenDelegs

instance Crypto c => FromCBOR (ConwayGenesis c) where
  fromCBOR =
    eraDecoder @(ConwayEra c) $
      decode $
        RecD ConwayGenesis
          <! From

instance Crypto c => ToJSON (ConwayGenesis c) where
  toJSON cg@(ConwayGenesis _) =
    let ConwayGenesis {cgGenDelegs} = cg
     in object ["genDelegs" .= toJSON cgGenDelegs]

instance Crypto c => FromJSON (ConwayGenesis c) where
  parseJSON = withObject "ConwayGenesis" $ \obj -> do
    cgGenDelegs <- forceElemsToWHNF obj .: "genDelegs"
    pure $ ConwayGenesis {cgGenDelegs}
