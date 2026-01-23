{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.SCLS.Namespace.Snapshots.V0 (
  SnapShotIn (..),
  SnapShotOut (..),
  SnapShotValueType (..),
  SnapshotStage (..),
  CanonicalStakePoolParams (..),
  mkCanonicalStakePoolParams,
  fromCanonicalStakePoolParams,
  CanonicalPoolMetadata (..),
  mkCanonicalPoolMetadata,
  fromCanonicalPoolMetadata,
  CanonicalStakePoolRelay (..),
  mkCanonicalStakePoolRelay,
  fromCanonicalStakePoolRelay,
) where

import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Cardano.Ledger.SCLS.Common (
  CanonicalCoin (..),
  CanonicalRewardAccount (..),
  CanonicalVRFVerKeyHash (..),
  DnsName,
  IPv4,
  IPv6,
  IsCanonicalCoin (..),
  Port,
  StrictMaybe (..),
  UnitInterval,
  Url (..),
  fromCanonicalRewardAccount,
  fromCanonicalVRFVerKeyHash,
  mkCanonicalRewardAccount,
  mkCanonicalVRFVerKeyHash,
 )
import Cardano.Ledger.State (PoolMetadata (..), StakePoolParams (..), StakePoolRelay (..))
import Cardano.SCLS.CBOR.Canonical (CanonicalDecoder)
import Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  decodeListLenCanonical,
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
import qualified Data.Primitive.ByteArray as Prim (ByteArray (..))


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

instance ToCanonicalCBOR v SnapShotOut where
  toCanonicalCBOR v (SnapShotOutCoin coin) = toCanonicalCBOR v (0 :: Word8, coin)
  toCanonicalCBOR v (SnapShotOutAddress kh) = toCanonicalCBOR v (1 :: Word8, kh)
  toCanonicalCBOR v (SnapShotOutPoolParams pp) = toCanonicalCBOR v (2 :: Word8, pp)

instance FromCanonicalCBOR v SnapShotOut where
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
  , sppRelays :: !(StrictSeq.StrictSeq CanonicalStakePoolRelay)
  , sppId :: !(KeyHash StakePool)
  , sppOwners :: !(Set (KeyHash Staking))
  , sppVrf :: !(CanonicalVRFVerKeyHash StakePoolVRF)
  , sppMetadata :: !(StrictMaybe CanonicalPoolMetadata)
  , sppRewardAccount :: !CanonicalRewardAccount
  }
  deriving (Eq, Show, Generic)

instance ToCanonicalCBOR v CanonicalStakePoolParams where
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

instance FromCanonicalCBOR v CanonicalStakePoolParams where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 9
    Versioned sppCost <- decodeField "cost"
    Versioned sppMargin <- decodeField "margin"
    Versioned sppPledge <- decodeField "pledge"
    Versioned relaysList <- decodeField "relays"
    let sppRelays = StrictSeq.fromList relaysList
    Versioned sppId <- decodeField "operator"
    Versioned sppOwners <- decodeField "pool_owners"
    Versioned sppVrf <- decodeField "vrf_keyhash"
    Versioned sppMetadata <- decodeField "pool_metadata"
    Versioned sppRewardAccount <- decodeField "reward_account"
    pure $ Versioned CanonicalStakePoolParams {..}

mkCanonicalStakePoolParams :: StakePoolParams -> CanonicalStakePoolParams
mkCanonicalStakePoolParams StakePoolParams {..} =
  CanonicalStakePoolParams
    { sppVrf = mkCanonicalVRFVerKeyHash sppVrf
    , sppMetadata = fmap mkCanonicalPoolMetadata sppMetadata
    , sppRewardAccount = mkCanonicalRewardAccount sppAccountAddress
    , sppRelays = fmap mkCanonicalStakePoolRelay sppRelays
    , sppCost = mkCanonicalCoin sppCost
    , sppPledge = mkCanonicalCoin sppPledge
    , ..
    }

fromCanonicalStakePoolParams :: CanonicalStakePoolParams -> StakePoolParams
fromCanonicalStakePoolParams CanonicalStakePoolParams {..} =
  StakePoolParams
    { sppCost = fromCanonicalCoin sppCost
    , sppPledge = fromCanonicalCoin sppPledge
    , sppMargin = sppMargin
    , sppRelays = fmap fromCanonicalStakePoolRelay sppRelays
    , sppId = sppId
    , sppOwners = sppOwners
    , sppVrf = fromCanonicalVRFVerKeyHash sppVrf
    , sppMetadata = fmap (\CanonicalPoolMetadata {..} -> PoolMetadata pmUrl pmHash) sppMetadata
    , sppAccountAddress = fromCanonicalRewardAccount sppRewardAccount
    }

data CanonicalPoolMetadata = CanonicalPoolMetadata
  { pmUrl :: !Url
  , pmHash :: !Prim.ByteArray
  }
  deriving (Eq, Show, Generic)

instance ToCanonicalCBOR v CanonicalPoolMetadata where
  toCanonicalCBOR v CanonicalPoolMetadata {..} = toCanonicalCBOR v (pmUrl, pmHash)

instance FromCanonicalCBOR v CanonicalPoolMetadata where
  fromCanonicalCBOR = do
    Versioned (pmUrl, pmHash) <- fromCanonicalCBOR
    return $ Versioned CanonicalPoolMetadata {..}

mkCanonicalPoolMetadata :: PoolMetadata -> CanonicalPoolMetadata
mkCanonicalPoolMetadata PoolMetadata {..} = CanonicalPoolMetadata {..}

fromCanonicalPoolMetadata :: CanonicalPoolMetadata -> PoolMetadata
fromCanonicalPoolMetadata CanonicalPoolMetadata {..} = PoolMetadata {..}

decodeField :: forall s v a. FromCanonicalCBOR v a => T.Text -> CanonicalDecoder s (Versioned v a)
decodeField fieldName = do
  Versioned s <- fromCanonicalCBOR
  unless (s == fieldName) $
    fail $
      T.unpack $
        "Expected field name " <> fieldName <> " but got " <> s
  fromCanonicalCBOR

data CanonicalStakePoolRelay
  = CanonicalSingleHostAddr !(StrictMaybe Port) !(StrictMaybe IPv4) !(StrictMaybe IPv6)
  | CanonicalSingleHostName !(StrictMaybe Port) !DnsName
  | CanonicalMultiHostName !DnsName
  deriving (Eq, Show, Generic)

instance ToCanonicalCBOR v CanonicalStakePoolRelay where
  toCanonicalCBOR v (CanonicalSingleHostAddr port ipv4 ipv6) =
    toCanonicalCBOR v (0 :: Word8, port, ipv4, ipv6)
  toCanonicalCBOR v (CanonicalSingleHostName port dns) =
    toCanonicalCBOR v (1 :: Word8, port, dns)
  toCanonicalCBOR v (CanonicalMultiHostName dns) =
    toCanonicalCBOR v (2 :: Word8, dns)

instance FromCanonicalCBOR v CanonicalStakePoolRelay where
  fromCanonicalCBOR = do
    l <- decodeListLenCanonical
    Versioned (tag :: Word8) <- fromCanonicalCBOR
    case tag of
      0
        | l == 4 -> do
            Versioned port <- fromCanonicalCBOR
            Versioned ipv4 <- fromCanonicalCBOR
            Versioned ipv6 <- fromCanonicalCBOR
            return $ Versioned (CanonicalSingleHostAddr port ipv4 ipv6)
        | otherwise -> fail "CanonicalStakePoolRelay: unexpected list for SingleHostAddr"
      1 -> do
        Versioned port <- fromCanonicalCBOR
        Versioned dns <- fromCanonicalCBOR
        return $ Versioned (CanonicalSingleHostName port dns)
      2 -> do
        Versioned dns <- fromCanonicalCBOR
        return $ Versioned (CanonicalMultiHostName dns)
      _ -> fail "Invalid CanonicalStakePoolRelay tag"

mkCanonicalStakePoolRelay :: StakePoolRelay -> CanonicalStakePoolRelay
mkCanonicalStakePoolRelay (SingleHostAddr port ipv4 ipv6) = CanonicalSingleHostAddr port ipv4 ipv6
mkCanonicalStakePoolRelay (SingleHostName port dns) = CanonicalSingleHostName port dns
mkCanonicalStakePoolRelay (MultiHostName dns) = CanonicalMultiHostName dns

fromCanonicalStakePoolRelay :: CanonicalStakePoolRelay -> StakePoolRelay
fromCanonicalStakePoolRelay (CanonicalSingleHostAddr port ipv4 ipv6) = SingleHostAddr port ipv4 ipv6
fromCanonicalStakePoolRelay (CanonicalSingleHostName port dns) = SingleHostName port dns
fromCanonicalStakePoolRelay (CanonicalMultiHostName dns) = MultiHostName dns

instance ToCanonicalCBOR v PoolMetadata where
  toCanonicalCBOR v PoolMetadata {..} = toCanonicalCBOR v (pmUrl, pmHash)

instance KnownNamespace "snapshots/v0" where
  type NamespaceKey "snapshots/v0" = SnapShotIn
  type NamespaceEntry "snapshots/v0" = SnapShotOut

instance CanonicalCBOREntryEncoder "snapshots/v0" SnapShotOut where
  encodeEntry n = toCanonicalCBOR (Proxy @"snapshots/v0") n

instance CanonicalCBOREntryDecoder "snapshots/v0" SnapShotOut where
  decodeEntry = fromCanonicalCBOR
