{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.EntitiesStakePools.FutureParams.V0 (
  EntitiesStakePoolsFutureParamsIn (..),
  EntitiesStakePoolsFutureParamsOut (..),
  CanonicalStakePoolParams (..),
  mkCanonicalStakePoolParams,
  fromCanonicalStakePoolParams,
) where

import Cardano.Ledger.BaseTypes (StrictMaybe, UnitInterval)
import Cardano.Ledger.CanonicalState.BasicTypes (
  CanonicalCoin (..),
  decodeNamespacedField,
 )
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.Ledger.Compactible (Compactible (fromCompact), toCompactPartial)
import Cardano.Ledger.Core (
  AccountAddress,
  KeyHash,
  KeyRole (StakePool),
  KeyRoleVRF (StakePoolVRF),
  Staking,
  VRFVerKeyHash,
 )
import Cardano.Ledger.State (
  PoolMetadata,
  StakePoolParams (..),
  StakePoolRelay,
 )
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..), decodeMapLenCanonicalOf)
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..), encodeAsMap, mkEncodablePair)
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  NamespaceKeySize,
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import Data.MemPack (MemPack (packM, unpackM))
import Data.Proxy (Proxy (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

instance
  ( Era era
  , NamespaceEra "entities/stake_pools/future_params/v0" ~ era
  , ToCanonicalCBOR "entities/stake_pools/future_params/v0" CanonicalStakePoolParams
  , FromCanonicalCBOR "entities/stake_pools/future_params/v0" CanonicalStakePoolParams
  ) =>
  KnownNamespace "entities/stake_pools/future_params/v0"
  where
  type NamespaceKey "entities/stake_pools/future_params/v0" = EntitiesStakePoolsFutureParamsIn
  type NamespaceEntry "entities/stake_pools/future_params/v0" = EntitiesStakePoolsFutureParamsOut

instance
  ( Era era
  , NamespaceEra "entities/stake_pools/future_params/v0" ~ era
  , ToCanonicalCBOR "entities/stake_pools/future_params/v0" CanonicalStakePoolParams
  ) =>
  CanonicalCBOREntryEncoder "entities/stake_pools/future_params/v0" EntitiesStakePoolsFutureParamsOut
  where
  encodeEntry (EntitiesStakePoolsFutureParamsOut n) = toCanonicalCBOR (Proxy @"entities/stake_pools/future_params/v0") n

instance
  ( Era era
  , NamespaceEra "entities/stake_pools/future_params/v0" ~ era
  , FromCanonicalCBOR "entities/stake_pools/future_params/v0" CanonicalStakePoolParams
  ) =>
  CanonicalCBOREntryDecoder "entities/stake_pools/future_params/v0" EntitiesStakePoolsFutureParamsOut
  where
  decodeEntry = fmap EntitiesStakePoolsFutureParamsOut <$> fromCanonicalCBOR

newtype EntitiesStakePoolsFutureParamsIn = EntitiesStakePoolsFutureParamsIn (KeyHash StakePool)
  deriving (Eq, Ord, Show)

type instance NamespaceKeySize "entities/stake_pools/future_params/v0" = 28

instance IsKey EntitiesStakePoolsFutureParamsIn where
  keySize = namespaceKeySize @"entities/stake_pools/future_params/v0"
  packKeyM (EntitiesStakePoolsFutureParamsIn stakePoolKey) =
    packM stakePoolKey
  unpackKeyM =
    EntitiesStakePoolsFutureParamsIn <$> unpackM

newtype EntitiesStakePoolsFutureParamsOut
  = EntitiesStakePoolsFutureParamsOut CanonicalStakePoolParams
  deriving (Eq, Show, Generic)

deriving newtype instance
  ToCanonicalCBOR "entities/stake_pools/future_params/v0" CanonicalStakePoolParams =>
  ToCanonicalCBOR "entities/stake_pools/future_params/v0" EntitiesStakePoolsFutureParamsOut

deriving newtype instance
  FromCanonicalCBOR "entities/stake_pools/future_params/v0" CanonicalStakePoolParams =>
  FromCanonicalCBOR "entities/stake_pools/future_params/v0" EntitiesStakePoolsFutureParamsOut

data CanonicalStakePoolParams = CanonicalStakePoolParams
  { csppId :: !(KeyHash StakePool)
  , csppVrf :: !(VRFVerKeyHash StakePoolVRF)
  , csppPledge :: !CanonicalCoin
  , csppCost :: !CanonicalCoin
  , csppMargin :: !UnitInterval
  , csppAccountAddress :: !AccountAddress
  , csppOwners :: !(Set (KeyHash Staking))
  , csppRelays :: !(StrictSeq StakePoolRelay)
  , csppMetadata :: !(StrictMaybe PoolMetadata)
  }
  deriving (Show, Eq, Generic)

instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v CanonicalStakePoolParams where
  toCanonicalCBOR v CanonicalStakePoolParams {..} =
    encodeAsMap
      [ mkEncodablePair v ("id" :: Text) csppId
      , mkEncodablePair v ("vrf" :: Text) csppVrf
      , mkEncodablePair v ("cost" :: Text) csppCost
      , mkEncodablePair v ("margin" :: Text) csppMargin
      , mkEncodablePair v ("owners" :: Text) csppOwners
      , mkEncodablePair v ("pledge" :: Text) csppPledge
      , mkEncodablePair v ("relays" :: Text) csppRelays
      , mkEncodablePair v ("metadata" :: Text) csppMetadata
      , mkEncodablePair v ("account_address" :: Text) csppAccountAddress
      ]

instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v CanonicalStakePoolParams where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 9
    Versioned csppId <- decodeNamespacedField @v "id"
    Versioned csppVrf <- decodeNamespacedField @v "vrf"
    Versioned csppCost <- decodeNamespacedField @v "cost"
    Versioned csppMargin <- decodeNamespacedField @v "margin"
    Versioned csppOwners <- decodeNamespacedField @v "owners"
    Versioned csppPledge <- decodeNamespacedField @v "pledge"
    Versioned csppRelays <- decodeNamespacedField @v "relays"
    Versioned csppMetadata <- decodeNamespacedField @v "metadata"
    Versioned csppAccountAddress <- decodeNamespacedField @v "account_address"
    pure $ Versioned CanonicalStakePoolParams {..}

mkCanonicalStakePoolParams :: StakePoolParams -> CanonicalStakePoolParams
mkCanonicalStakePoolParams (StakePoolParams {..}) =
  CanonicalStakePoolParams
    { csppId = sppId
    , csppVrf = sppVrf
    , csppCost = CanonicalCoin (toCompactPartial sppCost)
    , csppMargin = sppMargin
    , csppPledge = CanonicalCoin (toCompactPartial sppPledge)
    , csppOwners = sppOwners
    , csppRelays = sppRelays
    , csppMetadata = sppMetadata
    , csppAccountAddress = sppAccountAddress
    }

fromCanonicalStakePoolParams :: CanonicalStakePoolParams -> StakePoolParams
fromCanonicalStakePoolParams (CanonicalStakePoolParams {..}) =
  StakePoolParams
    { sppId = csppId
    , sppVrf = csppVrf
    , sppCost = fromCompact $ unCoin csppCost
    , sppMargin = csppMargin
    , sppPledge = fromCompact $ unCoin csppPledge
    , sppOwners = csppOwners
    , sppRelays = csppRelays
    , sppMetadata = csppMetadata
    , sppAccountAddress = csppAccountAddress
    }
