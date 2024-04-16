{-# LANGUAGE DataKinds #-}
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
  cgDelegsL,
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
import Cardano.Ledger.Conway.TxCert (Delegatee)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DRep (DRepState)
import Cardano.Ledger.Keys (KeyRole (..))
import Data.Aeson (
  FromJSON (..),
  KeyValue (..),
  ToJSON (..),
  Value (..),
  object,
  pairs,
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Functor.Identity (Identity)
import Data.ListMap (ListMap)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

data ConwayGenesis c = ConwayGenesis
  { cgUpgradePParams :: !(UpgradeConwayPParams Identity)
  , cgConstitution :: !(Constitution (ConwayEra c))
  , cgCommittee :: !(Committee (ConwayEra c))
  , cgDelegs :: ListMap (Credential 'Staking c) (Delegatee c)
  , cgInitialDReps :: ListMap (Credential 'DRepRole c) (DRepState c)
  }
  deriving (Eq, Generic, Show)

cgDelegsL :: Lens' (ConwayGenesis c) (ListMap (Credential 'Staking c) (Delegatee c))
cgDelegsL = lens cgDelegs (\x y -> x {cgDelegs = y})

instance Crypto c => NoThunks (ConwayGenesis c)

-- | Genesis are always encoded with the version of era they are defined in.
instance Crypto c => DecCBOR (ConwayGenesis c) where
  decCBOR = decode $ RecD ConwayGenesis <! From <! From <! From <! From <! From

instance Crypto c => EncCBOR (ConwayGenesis c) where
  encCBOR (ConwayGenesis pparams constitution committee delegs initialDReps) =
    encode $
      Rec (ConwayGenesis @c)
        !> To pparams
        !> To constitution
        !> To committee
        !> To delegs
        !> To initialDReps

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
        <*> obj .:? "delegs" .!= mempty
        <*> obj .:? "initialDReps" .!= mempty

toConwayGenesisPairs :: (Crypto c, KeyValue e a) => ConwayGenesis c -> [a]
toConwayGenesisPairs cg@(ConwayGenesis _ _ _ _ _) =
  let ConwayGenesis {..} = cg
   in [ "constitution" .= cgConstitution
      , "committee" .= cgCommittee
      ]
        ++ ["delegs" .= cgDelegs | not (null cgDelegs)]
        ++ ["initialDReps" .= cgInitialDReps | not (null cgInitialDReps)]
        ++ toUpgradeConwayPParamsUpdatePairs cgUpgradePParams
