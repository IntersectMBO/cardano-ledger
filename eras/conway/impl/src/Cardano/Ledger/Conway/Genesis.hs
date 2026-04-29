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
  ConwayExtraConfig (..),
  cgDelegsL,
  cgExtraConfigL,
) where

import Cardano.Ledger.BaseTypes (
  KeyValuePairs (..),
  StrictMaybe (..),
  ToKeyValuePairs (..),
  maybeToStrictMaybe,
 )
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
import Cardano.Ledger.Shelley.Genesis (InjectionData (..))
import Control.DeepSeq (NFData)
import Data.Aeson (
  FromJSON (..),
  KeyValue (..),
  ToJSON (..),
  Value (..),
  withObject,
  (.!=),
  (.:),
  (.:?),
  (.=),
 )
import Data.Default (Default (..))
import Data.Functor.Identity (Identity)
import Data.ListMap (ListMap)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

-- | Extra configuration for injecting Conway-specific Genesis data
data ConwayExtraConfig = ConwayExtraConfig
  { cecDelegs :: !(InjectionData (Credential Staking) Delegatee)
  -- ^ Delegations to DReps and stake pools.
  , cecInitialDReps :: !(InjectionData (Credential DRepRole) DRepState)
  -- ^ Initial DRep registrations.
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON) via KeyValuePairs ConwayExtraConfig

instance NFData ConwayExtraConfig

instance NoThunks ConwayExtraConfig

instance Default ConwayExtraConfig where
  def = ConwayExtraConfig NoInjection NoInjection

instance EncCBOR ConwayExtraConfig where
  encCBOR (ConwayExtraConfig delegs dreps) =
    encode $
      Rec ConwayExtraConfig
        !> To delegs
        !> To dreps

instance DecCBOR ConwayExtraConfig where
  decCBOR =
    decode $
      RecD ConwayExtraConfig
        <! From
        <! From

instance ToKeyValuePairs ConwayExtraConfig where
  toKeyValuePairs (ConwayExtraConfig delegs dreps) =
    [ "delegs" .= delegs
    , "initialDReps" .= dreps
    ]

instance FromJSON ConwayExtraConfig where
  parseJSON = withObject "ConwayExtraConfig" $ \obj ->
    ConwayExtraConfig
      <$> obj .:? "delegs" .!= NoInjection
      <*> obj .:? "initialDReps" .!= NoInjection

data ConwayGenesis = ConwayGenesis
  { cgUpgradePParams :: !(UpgradeConwayPParams Identity)
  , cgConstitution :: !(Constitution ConwayEra)
  , cgCommittee :: !(Committee ConwayEra)
  , cgDelegs :: ListMap (Credential Staking) Delegatee
  , cgInitialDReps :: ListMap (Credential DRepRole) DRepState
  , cgExtraConfig :: !(StrictMaybe ConwayExtraConfig)
  -- ^ Optional extra configuration for streaming injection.
  }
  deriving (Eq, Generic, Show)
  deriving (ToJSON) via KeyValuePairs ConwayGenesis

cgDelegsL :: Lens' ConwayGenesis (ListMap (Credential Staking) Delegatee)
cgDelegsL = lens cgDelegs (\x y -> x {cgDelegs = y})

cgExtraConfigL :: Lens' ConwayGenesis (StrictMaybe ConwayExtraConfig)
cgExtraConfigL = lens cgExtraConfig (\x y -> x {cgExtraConfig = y})

instance EraGenesis ConwayEra where
  type Genesis ConwayEra = ConwayGenesis

instance NoThunks ConwayGenesis

instance NFData ConwayGenesis

-- | Genesis are always encoded with the version of era they are defined in.
instance FromCBOR ConwayGenesis where
  fromCBOR = fromEraCBOR @ConwayEra
  {-# INLINE fromCBOR #-}

instance ToCBOR ConwayGenesis where
  toCBOR x@(ConwayGenesis _ _ _ _ _ _) =
    let ConwayGenesis {..} = x
     in toEraCBOR @ConwayEra . encode $
          Rec ConwayGenesis
            !> To cgUpgradePParams
            !> To cgConstitution
            !> To cgCommittee
            !> To cgDelegs
            !> To cgInitialDReps
            !> To cgExtraConfig

instance DecCBOR ConwayGenesis where
  decCBOR = decode (RecD ConwayGenesis <! From <! From <! From <! From <! From <! From)
  {-# INLINE decCBOR #-}

instance EncCBOR ConwayGenesis

instance FromJSON ConwayGenesis where
  parseJSON =
    withObject "ConwayGenesis" $ \obj -> do
      cgUpgradePParams <- parseJSON (Object obj)
      cgConstitution <- obj .: "constitution"
      cgCommittee <- obj .: "committee"
      cgDelegs <- obj .:? "delegs" .!= mempty
      cgInitialDReps <- obj .:? "initialDReps" .!= mempty
      cgExtraConfig <- maybeToStrictMaybe <$> obj .:? "extraConfig"
      pure ConwayGenesis {..}

instance ToKeyValuePairs ConwayGenesis where
  toKeyValuePairs cg@(ConwayGenesis _ _ _ _ _ _) =
    let ConwayGenesis {..} = cg
     in [ "constitution" .= cgConstitution
        , "committee" .= cgCommittee
        ]
          <> toKeyValuePairs cgUpgradePParams
          <> ["delegs" .= cgDelegs | not (null cgDelegs)]
          <> ["initialDReps" .= cgInitialDReps | not (null cgInitialDReps)]
          <> ["extraConfig" .= ec | SJust ec <- [cgExtraConfig]]
