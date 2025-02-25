{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Genesis (
  ConwayGenesis (..),
  toConwayGenesisPairs,
  cgDelegsL,
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
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (UpgradeConwayPParams, toUpgradeConwayPParamsUpdatePairs)
import Cardano.Ledger.Conway.TxCert (Delegatee)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DRep (DRepState)
import Cardano.Ledger.Genesis (EraGenesis (..))
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

data ConwayGenesis = ConwayGenesis
  { cgUpgradePParams :: !(UpgradeConwayPParams Identity)
  , cgConstitution :: !(Constitution ConwayEra)
  , cgCommittee :: !(Committee ConwayEra)
  , cgDelegs :: ListMap (Credential 'Staking) Delegatee
  , cgInitialDReps :: ListMap (Credential 'DRepRole) DRepState
  }
  deriving (Eq, Generic, Show)

cgDelegsL :: Lens' ConwayGenesis (ListMap (Credential 'Staking) Delegatee)
cgDelegsL = lens cgDelegs (\x y -> x {cgDelegs = y})

instance EraGenesis ConwayEra where
  type Genesis ConwayEra = ConwayGenesis

instance NoThunks ConwayGenesis

-- | Genesis are always encoded with the version of era they are defined in.
instance FromCBOR ConwayGenesis where
  fromCBOR =
    eraDecoder @ConwayEra $
      decode $
        RecD ConwayGenesis <! From <! From <! From <! From <! From

instance ToCBOR ConwayGenesis where
  toCBOR x@(ConwayGenesis _ _ _ _ _) =
    let ConwayGenesis {..} = x
     in toEraCBOR @ConwayEra . encode $
          Rec ConwayGenesis
            !> To cgUpgradePParams
            !> To cgConstitution
            !> To cgCommittee
            !> To cgDelegs
            !> To cgInitialDReps

instance DecCBOR ConwayGenesis
instance EncCBOR ConwayGenesis

instance ToJSON ConwayGenesis where
  toJSON = object . toConwayGenesisPairs
  toEncoding = pairs . mconcat . toConwayGenesisPairs

instance FromJSON ConwayGenesis where
  parseJSON =
    withObject "ConwayGenesis" $ \obj -> do
      upgradeProtocolPParams <- parseJSON (Object obj)
      ConwayGenesis
        <$> pure upgradeProtocolPParams
        <*> obj .: "constitution"
        <*> obj .: "committee"
        <*> obj .:? "delegs" .!= mempty
        <*> obj .:? "initialDReps" .!= mempty

toConwayGenesisPairs :: KeyValue e a => ConwayGenesis -> [a]
toConwayGenesisPairs cg@(ConwayGenesis _ _ _ _ _) =
  let ConwayGenesis {..} = cg
   in [ "constitution" .= cgConstitution
      , "committee" .= cgCommittee
      ]
        ++ ["delegs" .= cgDelegs | not (null cgDelegs)]
        ++ ["initialDReps" .= cgInitialDReps | not (null cgInitialDReps)]
        ++ toUpgradeConwayPParamsUpdatePairs cgUpgradePParams
