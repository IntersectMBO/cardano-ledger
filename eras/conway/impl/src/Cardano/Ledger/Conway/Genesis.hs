{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (UpgradeConwayPParams)
import Cardano.Ledger.Crypto (Crypto)
import Data.Aeson (
  FromJSON (..),
  KeyValue (..),
  ToJSON (..),
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
  decCBOR =
    ConwayGenesis
      <$> decCBOR
      <*> decCBOR
      <*> decCBOR

instance Crypto c => EncCBOR (ConwayGenesis c) where
  encCBOR (ConwayGenesis pparams constitution committee) =
    encCBOR pparams
      <> encCBOR constitution
      <> encCBOR committee

instance Crypto c => ToJSON (ConwayGenesis c) where
  toJSON = object . toConwayGenesisPairs
  toEncoding = pairs . mconcat . toConwayGenesisPairs

instance Crypto c => FromJSON (ConwayGenesis c) where
  parseJSON =
    withObject "ConwayGenesis" $ \obj ->
      ConwayGenesis
        <$> obj .: "upgradeProtocolParams"
        <*> obj .: "constitution"
        <*> obj .: "committee"

toConwayGenesisPairs :: (Crypto c, KeyValue a) => ConwayGenesis c -> [a]
toConwayGenesisPairs cg@(ConwayGenesis _ _ _) =
  let ConwayGenesis {..} = cg
   in [ "upgradeProtocolParams" .= cgUpgradePParams
      , "constitution" .= cgConstitution
      , "committee" .= cgCommittee
      ]

instance Crypto c => Default (ConwayGenesis c) where
  def = ConwayGenesis def def def
