{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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
) where

import Cardano.Ledger.BaseTypes (KeyValuePairs (..), ToKeyValuePairs (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (UpgradeConwayPParams)
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
  deriving (ToJSON) via KeyValuePairs ConwayGenesis

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

instance FromJSON ConwayGenesis where
  parseJSON =
    withObject "ConwayGenesis" $ \obj -> do
      cgUpgradePParams <- parseJSON (Object obj)
      cgConstitution <- obj .: "constitution"
      cgCommittee <- obj .: "committee"
      cgDelegs <- obj .:? "delegs" .!= mempty
      cgInitialDReps <- obj .:? "initialDReps" .!= mempty
      pure ConwayGenesis {..}

instance ToKeyValuePairs ConwayGenesis where
  toKeyValuePairs cg@(ConwayGenesis _ _ _ _ _) =
    let ConwayGenesis {..} = cg
     in [ "constitution" .= cgConstitution
        , "committee" .= cgCommittee
        ]
          ++ toKeyValuePairs cgUpgradePParams
          ++ ["delegs" .= cgDelegs | not (null cgDelegs)]
          ++ ["initialDReps" .= cgInitialDReps | not (null cgInitialDReps)]

toConwayGenesisPairs :: KeyValue e a => ConwayGenesis -> [a]
toConwayGenesisPairs = toKeyValuePairs
{-# DEPRECATED toConwayGenesisPairs "In favor of `toKeyValuePairs`" #-}
