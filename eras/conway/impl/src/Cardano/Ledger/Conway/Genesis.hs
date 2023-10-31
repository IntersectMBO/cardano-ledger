{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Genesis (
  ConwayGenesis (..),
  toConwayGenesisPairs,
)
where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (UpgradeConwayPParams, toUpgradeConwayPParamsUpdatePairs)
import Cardano.Ledger.Crypto (Crypto)
import Data.Aeson (
  FromJSON (..),
  KeyValue (..),
  ToJSON (..),
  Value (..),
  object,
  pairs,
  withObject,
  (.:),
 )
import Data.Default.Class (Default (def))
import Data.Functor.Identity (Identity)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

data ConwayGenesis c = ConwayGenesis
  { cgUpgradePParams :: !(UpgradeConwayPParams Identity)
  , cgConstitution :: !(Constitution (ConwayEra c))
  , cgCommittee :: !(Committee (ConwayEra c))
  }
  deriving (Eq, Generic, Show)

instance Crypto c => NoThunks (ConwayGenesis c)

-- | Genesis are always encoded with the version of era they are defined in.
instance Crypto c => DecCBOR (ConwayGenesis c) where
  decCBOR = decode $ RecD ConwayGenesis <! From <! From <! From

instance Crypto c => EncCBOR (ConwayGenesis c) where
  encCBOR (ConwayGenesis pparams constitution committee) =
    encode $ Rec (ConwayGenesis @c) !> To pparams !> To constitution !> To committee

instance Crypto c => ToJSON (ConwayGenesis c) where
  toJSON = object . toConwayGenesisPairs
  toEncoding = pairs . mconcat . toConwayGenesisPairs

instance Crypto c => FromJSON (ConwayGenesis c) where
  parseJSON =
    withObject "ConwayGenesis" $ \obj -> do
      upgradeProtocolPParams <- parseJSON (Object obj)
      ConwayGenesis
        <$> pure upgradeProtocolPParams
        <*> obj .: "constitution"
        <*> obj .: "committee"

toConwayGenesisPairs :: (Crypto c, KeyValue e a) => ConwayGenesis c -> [a]
toConwayGenesisPairs cg@(ConwayGenesis _ _ _) =
  let ConwayGenesis {..} = cg
   in [ "constitution" .= cgConstitution
      , "committee" .= cgCommittee
      ]
        ++ toUpgradeConwayPParamsUpdatePairs cgUpgradePParams

instance Crypto c => Default (ConwayGenesis c) where
  def = ConwayGenesis def def def
