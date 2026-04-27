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

module Cardano.Ledger.CanonicalState.Namespace.EntitiesStakePools.V0 (
  EntitiesStakePoolsIn (..),
  EntitiesStakePoolsOut (..),
  CanonicalStakePool (..),
  CanonicalStakePoolState (..),
  CanonicalStakePoolParams (..),
  mkCanonicalStakePoolState,
  fromCanonicalStakePoolState,
  mkCanonicalStakePoolParams,
  fromCanonicalStakePoolParams,
) where

import Cardano.Ledger.BaseTypes (EpochNo, StrictMaybe, UnitInterval)
import Cardano.Ledger.CanonicalState.BasicTypes (
  CanonicalCoin (..),
  decodeNamespacedField,
 )
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.Ledger.Compactible (Compactible (fromCompact), toCompactPartial)
import Cardano.Ledger.Core (
  AccountAddress,
  AccountId,
  KeyHash,
  KeyRole (StakePool),
  KeyRoleVRF (StakePoolVRF),
  Staking,
  VRFVerKeyHash,
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.State (
  PoolMetadata,
  StakePoolParams (..),
  StakePoolRelay,
  StakePoolState (..),
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
  , NamespaceEra "entities/stake_pools/v0" ~ era
  , ToCanonicalCBOR "entities/stake_pools/v0" CanonicalStakePool
  , FromCanonicalCBOR "entities/stake_pools/v0" CanonicalStakePool
  ) =>
  KnownNamespace "entities/stake_pools/v0"
  where
  type NamespaceKey "entities/stake_pools/v0" = EntitiesStakePoolsIn
  type NamespaceEntry "entities/stake_pools/v0" = EntitiesStakePoolsOut

instance
  ( Era era
  , NamespaceEra "entities/stake_pools/v0" ~ era
  , ToCanonicalCBOR "entities/stake_pools/v0" CanonicalStakePool
  ) =>
  CanonicalCBOREntryEncoder "entities/stake_pools/v0" EntitiesStakePoolsOut
  where
  encodeEntry (EntitiesStakePoolsOut n) = toCanonicalCBOR (Proxy @"entities/stake_pools/v0") n

instance
  ( Era era
  , NamespaceEra "entities/stake_pools/v0" ~ era
  , FromCanonicalCBOR "entities/stake_pools/v0" CanonicalStakePool
  ) =>
  CanonicalCBOREntryDecoder "entities/stake_pools/v0" EntitiesStakePoolsOut
  where
  decodeEntry = fmap EntitiesStakePoolsOut <$> fromCanonicalCBOR

newtype EntitiesStakePoolsIn = EntitiesStakePoolsIn (KeyHash StakePool)
  deriving (Eq, Ord, Show)

type instance NamespaceKeySize "entities/stake_pools/v0" = 28

instance IsKey EntitiesStakePoolsIn where
  keySize = namespaceKeySize @"entities/stake_pools/v0"
  packKeyM (EntitiesStakePoolsIn stakePoolKey) =
    packM stakePoolKey
  unpackKeyM =
    EntitiesStakePoolsIn <$> unpackM

newtype EntitiesStakePoolsOut
  = EntitiesStakePoolsOut CanonicalStakePool
  deriving (Eq, Show, Generic)

deriving newtype instance
  ToCanonicalCBOR "entities/stake_pools/v0" CanonicalStakePool =>
  ToCanonicalCBOR "entities/stake_pools/v0" EntitiesStakePoolsOut

deriving newtype instance
  FromCanonicalCBOR "entities/stake_pools/v0" CanonicalStakePool =>
  FromCanonicalCBOR "entities/stake_pools/v0" EntitiesStakePoolsOut

data CanonicalStakePool = CanonicalStakePool
  { cspStakePoolState :: !(StrictMaybe CanonicalStakePoolState)
  , cspFutureStakePoolParams :: !(StrictMaybe CanonicalStakePoolParams)
  , cspRetiringEpochNo :: !(StrictMaybe EpochNo)
  }
  deriving (Show, Eq, Generic)

instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v CanonicalStakePool where
  toCanonicalCBOR v CanonicalStakePool {..} =
    encodeAsMap
      [ mkEncodablePair v ("stake_pool_state" :: Text) cspStakePoolState
      , mkEncodablePair v ("retiring_epoch_no" :: Text) cspRetiringEpochNo
      , mkEncodablePair v ("future_stake_pool_params" :: Text) cspFutureStakePoolParams
      ]

instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v CanonicalStakePool where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 3
    Versioned cspStakePoolState <- decodeNamespacedField @v "stake_pool_state"
    Versioned cspRetiringEpochNo <- decodeNamespacedField @v "retiring_epoch_no"
    Versioned cspFutureStakePoolParams <- decodeNamespacedField @v "future_stake_pool_params"
    pure $ Versioned CanonicalStakePool {..}

data CanonicalStakePoolState = CanonicalStakePoolState
  { cspsVrf :: !(VRFVerKeyHash StakePoolVRF)
  , cspsPledge :: !CanonicalCoin
  , cspsCost :: !CanonicalCoin
  , cspsMargin :: !UnitInterval
  , cspsAccountId :: !AccountId
  , cspsOwners :: !(Set (KeyHash Staking))
  , cspsRelays :: !(StrictSeq StakePoolRelay)
  , cspsMetadata :: !(StrictMaybe PoolMetadata)
  , cspsDeposit :: !CanonicalCoin
  , cspsDelegators :: !(Set (Credential Staking))
  }
  deriving (Show, Eq, Generic)

instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v CanonicalStakePoolState where
  toCanonicalCBOR v CanonicalStakePoolState {..} =
    encodeAsMap
      [ mkEncodablePair v ("vrf" :: Text) cspsVrf
      , mkEncodablePair v ("cost" :: Text) cspsCost
      , mkEncodablePair v ("margin" :: Text) cspsMargin
      , mkEncodablePair v ("owners" :: Text) cspsOwners
      , mkEncodablePair v ("pledge" :: Text) cspsPledge
      , mkEncodablePair v ("relays" :: Text) cspsRelays
      , mkEncodablePair v ("deposit" :: Text) cspsDeposit
      , mkEncodablePair v ("metadata" :: Text) cspsMetadata
      , mkEncodablePair v ("account_id" :: Text) cspsAccountId
      , mkEncodablePair v ("delegators" :: Text) cspsDelegators
      ]

instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v CanonicalStakePoolState where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 10
    Versioned cspsVrf <- decodeNamespacedField @v "vrf"
    Versioned cspsCost <- decodeNamespacedField @v "cost"
    Versioned cspsMargin <- decodeNamespacedField @v "margin"
    Versioned cspsOwners <- decodeNamespacedField @v "owners"
    Versioned cspsPledge <- decodeNamespacedField @v "pledge"
    Versioned cspsRelays <- decodeNamespacedField @v "relays"
    Versioned cspsDeposit <- decodeNamespacedField @v "deposit"
    Versioned cspsMetadata <- decodeNamespacedField @v "metadata"
    Versioned cspsAccountId <- decodeNamespacedField @v "account_id"
    Versioned cspsDelegators <- decodeNamespacedField @v "delegators"
    pure $ Versioned CanonicalStakePoolState {..}

mkCanonicalStakePoolState :: StakePoolState -> CanonicalStakePoolState
mkCanonicalStakePoolState (StakePoolState {..}) =
  CanonicalStakePoolState
    { cspsVrf = spsVrf
    , cspsPledge = CanonicalCoin (toCompactPartial spsPledge)
    , cspsCost = CanonicalCoin (toCompactPartial spsCost)
    , cspsMargin = spsMargin
    , cspsAccountId = spsAccountId
    , cspsOwners = spsOwners
    , cspsRelays = spsRelays
    , cspsMetadata = spsMetadata
    , cspsDeposit = CanonicalCoin spsDeposit
    , cspsDelegators = spsDelegators
    }

fromCanonicalStakePoolState :: CanonicalStakePoolState -> StakePoolState
fromCanonicalStakePoolState (CanonicalStakePoolState {..}) =
  StakePoolState
    { spsVrf = cspsVrf
    , spsPledge = fromCompact $ unCoin cspsPledge
    , spsCost = fromCompact $ unCoin cspsCost
    , spsMargin = cspsMargin
    , spsAccountId = cspsAccountId
    , spsOwners = cspsOwners
    , spsRelays = cspsRelays
    , spsMetadata = cspsMetadata
    , spsDeposit = unCoin cspsDeposit
    , spsDelegators = cspsDelegators
    }

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
