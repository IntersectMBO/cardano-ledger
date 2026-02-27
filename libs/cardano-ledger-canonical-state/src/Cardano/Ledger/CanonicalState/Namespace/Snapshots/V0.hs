{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.Snapshots.V0 (
  SnapShotIn (..),
  SnapShotOut (..),
  SnapShotValueType (..),
  SnapshotStage (..),
  CanonicalStakePoolParams (..),
  mkCanonicalStakePoolParams,
  fromCanonicalStakePoolParams,
) where

import Cardano.Ledger.BaseTypes (UnitInterval, StrictMaybe)
import Cardano.Ledger.CanonicalState.LedgerCBOR
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Cardano.Ledger.CanonicalState.BasicTypes(
  CanonicalCoin (..),
  CanonicalRewardAccount (..),
  mkCanonicalRewardAccount,
  fromCanonicalVRFVerKeyHash,
  mkCanonicalVRFVerKeyHash,
  CanonicalVRFVerKeyHash (..),
  mkCanonicalRewardAccount,
  fromCanonicalRewardAccount,
  )
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.State
import Cardano.Ledger.Coin (compactCoinOrError)
import Cardano.SCLS.CBOR.Canonical (CanonicalDecoder)
import Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  decodeListLenCanonicalOf,
  decodeMapLenCanonicalOf,
 )
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..), encodeAsMap, mkEncodablePair)
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  namespaceKeySize,
 )
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.SCLS.Versioned (Versioned (..))
import Control.Monad (unless)
import Data.Foldable (toList)
import Data.MemPack (MemPack (..))
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)

data SnapshotStage = SnapshotStageMark | SnapShotStageSet | SnapshotStageGo
  deriving (Eq, Ord, Show, Typeable)

instance MemPack SnapshotStage where
  packedByteCount _ = 1
  packM SnapshotStageMark = packM (0 :: Word8)
  packM SnapShotStageSet = packM (1 :: Word8)
  packM SnapshotStageGo = packM (2 :: Word8)
  unpackM = do
    tag :: Word8 <- unpackM
    case tag of
      0 -> return SnapshotStageMark
      1 -> return SnapShotStageSet
      2 -> return SnapshotStageGo
      _ -> fail "Invalid SnapshotStage tag"

data SnapShotValueType
  = SnapShotValueCoin
  | SnapShotValueAddress
  | SnapShotValuePoolParams
  deriving (Eq, Ord, Show, Typeable)

instance MemPack SnapShotValueType where
  packedByteCount _ = 1
  packM SnapShotValueCoin = packM (0 :: Word8)
  packM SnapShotValueAddress = packM (1 :: Word8)
  packM SnapShotValuePoolParams = packM (2 :: Word8)
  unpackM = do
    tag :: Word8 <- unpackM
    case tag of
      0 -> return SnapShotValueCoin
      1 -> return SnapShotValueAddress
      2 -> return SnapShotValuePoolParams
      _ -> fail "Invalid SnapShotValueType tag"

data SnapShotIn where
  SnapShotInCred :: SnapshotStage -> Credential Staking -> SnapShotValueType -> SnapShotIn
  SnapShotInKey :: SnapshotStage -> KeyHash StakePool -> SnapShotValueType -> SnapShotIn
  deriving (Eq, Ord, Show, Typeable)

instance IsKey SnapShotIn where
  keySize = namespaceKeySize @"snapshots/v0"
  packKeyM (SnapShotInCred stage cred valueType) = do
    packM (0 :: Word8)
    packM stage
    packM cred
    packM valueType
  packKeyM (SnapShotInKey stage kh valueType) = do
    packM (1 :: Word8)
    packM stage
    packM kh
    packM (0 :: Word8) -- filler?
    packM valueType
  unpackKeyM = do
    tag :: Word8 <- unpackM
    stage <- unpackM
    case tag of
      0 -> do
        cred <- unpackM
        valueType <- unpackM
        return $ SnapShotInCred stage cred valueType
      1 -> do
        kh <- unpackM
        _filler :: Word8 <- unpackM
        valueType <- unpackM
        return $ SnapShotInKey stage kh valueType
      _ -> fail "Invalid SnapShotIn tag"

data SnapShotOut where
  SnapShotOutCoin :: CanonicalCoin -> SnapShotOut
  SnapShotOutAddress :: KeyHash StakePool -> SnapShotOut
  SnapShotOutPoolParams :: CanonicalStakePoolParams -> SnapShotOut
  deriving (Show)
  deriving (Eq)
  deriving (Generic)

instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v SnapShotOut where
  toCanonicalCBOR v (SnapShotOutCoin coin) = toCanonicalCBOR v (0 :: Word8, coin)
  toCanonicalCBOR v (SnapShotOutAddress kh) = toCanonicalCBOR v (1 :: Word8, kh)
  toCanonicalCBOR v (SnapShotOutPoolParams pp) = toCanonicalCBOR v (2 :: Word8, pp)

instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v SnapShotOut where
  fromCanonicalCBOR = do
    decodeListLenCanonicalOf 2
    Versioned (tag :: Word8) <- fromCanonicalCBOR
    case tag of
      0 -> fmap SnapShotOutCoin <$> fromCanonicalCBOR
      1 -> fmap SnapShotOutAddress <$> fromCanonicalCBOR
      2 -> fmap SnapShotOutPoolParams <$> fromCanonicalCBOR
      _ -> fail "Invalid SnapShotOut tag"

data CanonicalStakePoolParams = CanonicalStakePoolParams
  { sppCost :: !CanonicalCoin
  , sppPledge :: !CanonicalCoin
  , sppMargin :: !UnitInterval
  , sppRelays :: !(StrictSeq.StrictSeq StakePoolRelay)
  , sppId :: !(KeyHash StakePool)
  , sppOwners :: !(Set (KeyHash Staking))
  , sppVrf :: !(CanonicalVRFVerKeyHash StakePoolVRF)
  , sppMetadata :: !(StrictMaybe PoolMetadata)
  , sppRewardAccount :: !CanonicalRewardAccount
  }
  deriving (Eq, Show, Generic)

instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v CanonicalStakePoolParams where
  toCanonicalCBOR v CanonicalStakePoolParams {..} =
    encodeAsMap
      [ mkEncodablePair v ("cost" :: Text) sppCost
      , mkEncodablePair v ("pledge" :: Text) sppPledge
      , mkEncodablePair v ("margin" :: Text) sppMargin
      , mkEncodablePair v ("relays" :: Text) (toList sppRelays)
      , mkEncodablePair v ("operator" :: Text) sppId
      , mkEncodablePair v ("pool_owners" :: Text) sppOwners
      , mkEncodablePair v ("vrf_keyhash" :: Text) sppVrf
      , mkEncodablePair v ("pool_metadata" :: Text) sppMetadata
      , mkEncodablePair v ("reward_account" :: Text) sppRewardAccount
      ]

instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v CanonicalStakePoolParams where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 9
    Versioned sppCost <- decodeField @v "cost"
    Versioned sppMargin <- decodeField @v "margin"
    Versioned sppPledge <- decodeField @v "pledge"
    Versioned relaysList <- decodeField @v "relays"
    let sppRelays = StrictSeq.fromList relaysList
    Versioned sppId <- decodeField @v "operator"
    Versioned sppOwners <- decodeField @v "pool_owners"
    Versioned sppVrf <- decodeField @v "vrf_keyhash"
    Versioned sppMetadata <- decodeField @v "pool_metadata"
    Versioned sppRewardAccount <- decodeField @v "reward_account"
    pure $ Versioned CanonicalStakePoolParams {..}


decodeField :: forall v s a. FromCanonicalCBOR v a => T.Text -> CanonicalDecoder s (Versioned v a)
decodeField fieldName = do
  Versioned s <- fromCanonicalCBOR
  unless (s == fieldName) $
    fail $
      T.unpack $
        "Expected field name " <> fieldName <> " but got " <> s
  fromCanonicalCBOR

mkCanonicalStakePoolParams :: StakePoolParams -> CanonicalStakePoolParams
mkCanonicalStakePoolParams StakePoolParams {..} =
  CanonicalStakePoolParams
    { sppVrf = mkCanonicalVRFVerKeyHash sppVrf
    , sppMetadata = sppMetadata
    , sppRewardAccount = mkCanonicalRewardAccount sppAccountAddress
    , sppRelays = sppRelays
    , sppCost = CanonicalCoin (compactCoinOrError sppCost)
    , sppPledge = CanonicalCoin (compactCoinOrError sppPledge)
    , ..
    }

fromCanonicalStakePoolParams :: CanonicalStakePoolParams -> StakePoolParams
fromCanonicalStakePoolParams CanonicalStakePoolParams {..} =
  StakePoolParams
    { sppCost = fromCompact (unCoin sppCost)
    , sppPledge = fromCompact (unCoin sppPledge)
    , sppVrf = fromCanonicalVRFVerKeyHash sppVrf
    , sppAccountAddress = fromCanonicalRewardAccount sppRewardAccount
    , ..
    }

deriving via LedgerCBOR v PoolMetadata instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v PoolMetadata

deriving via LedgerCBOR v PoolMetadata instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v PoolMetadata

deriving via LedgerCBOR v StakePoolRelay instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v StakePoolRelay

deriving via LedgerCBOR v StakePoolRelay instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v StakePoolRelay

instance
  ( NamespaceEra "snapshots/v0" ~ era
  , Era era
  ) => KnownNamespace "snapshots/v0" where
  type NamespaceKey "snapshots/v0" = SnapShotIn
  type NamespaceEntry "snapshots/v0" = SnapShotOut

instance Era (NamespaceEra "snapshots/v0") => CanonicalCBOREntryEncoder "snapshots/v0" SnapShotOut where
  encodeEntry n = toCanonicalCBOR (Proxy @"snapshots/v0") n

instance Era (NamespaceEra "snapshots/v0") => CanonicalCBOREntryDecoder "snapshots/v0" SnapShotOut where
  decodeEntry = fromCanonicalCBOR

