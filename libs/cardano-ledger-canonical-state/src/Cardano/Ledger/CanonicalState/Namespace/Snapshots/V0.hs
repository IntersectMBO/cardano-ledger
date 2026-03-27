{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.Snapshots.V0 (
  SnapShotIn (..),
  SnapShotOut (..),
  SnapShotValueType (..),
  SnapshotStage (..),
) where

import Cardano.Ledger.CanonicalState.BasicTypes (
  CanonicalCoin (..),
 )
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  decodeListLenCanonicalOf,
 )
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import Data.MemPack (MemPack (..))
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)

data SnapshotStage = SnapshotMark | SnapshotSet | SnapshotGo

deriving instance Typeable SnapshotStage

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

data SnapShotIn (st :: SnapshotStage) where
  SnapShotInCred :: Credential Staking -> SnapShotValueType -> SnapShotIn st
  SnapShotInKey :: KeyHash StakePool -> SnapShotValueType -> SnapShotIn st
  deriving (Eq, Ord, Show, Typeable)

instance Typeable st => IsKey (SnapShotIn st) where
  keySize = namespaceKeySize @"snapshots/set/v0"
  packKeyM (SnapShotInCred cred valueType) = do
    packM (0 :: Word8)
    packM cred
    packM valueType
  packKeyM (SnapShotInKey kh valueType) = do
    packM (1 :: Word8)
    packM kh
    packM (0 :: Word8) -- filler?
    packM valueType
  unpackKeyM = do
    tag :: Word8 <- unpackM
    case tag of
      0 -> do
        cred <- unpackM
        valueType <- unpackM
        return $ SnapShotInCred cred valueType
      1 -> do
        kh <- unpackM
        _filler :: Word8 <- unpackM
        valueType <- unpackM
        return $ SnapShotInKey kh valueType
      _ -> fail "Invalid SnapShotIn tag"

data SnapShotOut (st :: SnapshotStage) where
  SnapShotOutCoin :: CanonicalCoin -> SnapShotOut st
  SnapShotOutAddress :: KeyHash StakePool -> SnapShotOut st
  deriving (Show)
  deriving (Eq)
  deriving (Generic)

instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v (SnapShotOut s) where
  toCanonicalCBOR v (SnapShotOutCoin coin) = toCanonicalCBOR v (0 :: Word8, coin)
  toCanonicalCBOR v (SnapShotOutAddress kh) = toCanonicalCBOR v (1 :: Word8, kh)

instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v (SnapShotOut st) where
  fromCanonicalCBOR = do
    decodeListLenCanonicalOf 2
    Versioned (tag :: Word8) <- fromCanonicalCBOR
    case tag of
      0 -> fmap SnapShotOutCoin <$> fromCanonicalCBOR
      1 -> fmap SnapShotOutAddress <$> fromCanonicalCBOR
      _ -> fail "Invalid SnapShotOut tag"

instance
  ( NamespaceEra "snapshots/mark/v0" ~ era
  , Era era
  ) =>
  KnownNamespace "snapshots/mark/v0"
  where
  type NamespaceKey "snapshots/mark/v0" = SnapShotIn SnapshotMark
  type NamespaceEntry "snapshots/mark/v0" = SnapShotOut SnapshotMark

instance
  Era (NamespaceEra "snapshots/mark/v0") =>
  CanonicalCBOREntryEncoder "snapshots/mark/v0" (SnapShotOut SnapshotMark)
  where
  encodeEntry n = toCanonicalCBOR (Proxy @"snapshots/mark/v0") n

instance
  Era (NamespaceEra "snapshots/mark/v0") =>
  CanonicalCBOREntryDecoder "snapshots/mark/v0" (SnapShotOut SnapshotMark)
  where
  decodeEntry = fromCanonicalCBOR

instance
  ( NamespaceEra "snapshots/set/v0" ~ era
  , Era era
  ) =>
  KnownNamespace "snapshots/set/v0"
  where
  type NamespaceKey "snapshots/set/v0" = SnapShotIn SnapshotSet
  type NamespaceEntry "snapshots/set/v0" = SnapShotOut SnapshotSet

instance
  Era (NamespaceEra "snapshots/set/v0") =>
  CanonicalCBOREntryEncoder "snapshots/set/v0" (SnapShotOut SnapshotSet)
  where
  encodeEntry n = toCanonicalCBOR (Proxy @"snapshots/set/v0") n

instance
  Era (NamespaceEra "snapshots/set/v0") =>
  CanonicalCBOREntryDecoder "snapshots/set/v0" (SnapShotOut SnapshotSet)
  where
  decodeEntry = fromCanonicalCBOR

instance
  ( NamespaceEra "snapshots/go/v0" ~ era
  , Era era
  ) =>
  KnownNamespace "snapshots/go/v0"
  where
  type NamespaceKey "snapshots/go/v0" = SnapShotIn SnapshotGo
  type NamespaceEntry "snapshots/go/v0" = SnapShotOut SnapshotGo

instance
  Era (NamespaceEra "snapshots/go/v0") =>
  CanonicalCBOREntryEncoder "snapshots/go/v0" (SnapShotOut SnapshotGo)
  where
  encodeEntry n = toCanonicalCBOR (Proxy @"snapshots/go/v0") n

instance
  Era (NamespaceEra "snapshots/go/v0") =>
  CanonicalCBOREntryDecoder "snapshots/go/v0" (SnapShotOut SnapshotGo)
  where
  decodeEntry = fromCanonicalCBOR
