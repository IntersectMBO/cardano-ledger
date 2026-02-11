{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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

module Cardano.Ledger.CanonicalState.Namespace.GovCommittee.V0 (
  GovCommitteeIn (..),
  GovCommitteeOut (..),
  CanonicalCommitteeState (..),
  CanonicalCommitteeAuthorization (..),
  mkCanonicalCommitteeAuthorization,
  fromCanonicalCommitteeAuthorization,
) where

import Cardano.Ledger.BaseTypes (Anchor (..), EpochNo (..), StrictMaybe (..))
import Cardano.Ledger.CanonicalState.BasicTypes (
  CanonicalCredential (..),
  fromCanonicalCredential,
  mkCanonicalCredential,
 )
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.Ledger.Keys (KeyRole (ColdCommitteeRole, HotCommitteeRole))
import Cardano.Ledger.State (CommitteeAuthorization (..))
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..), decodeListLenCanonicalOf)
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import qualified Data.Map.Strict as Map
import Data.MemPack.ByteOrdered (packWord64beM, unpackBigEndianM)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Generics (Generic)

instance (Era era, NamespaceEra "gov/committee/v0" ~ era) => KnownNamespace "gov/committee/v0" where
  type NamespaceKey "gov/committee/v0" = GovCommitteeIn
  type NamespaceEntry "gov/committee/v0" = GovCommitteeOut

instance
  (Era era, NamespaceEra "gov/committee/v0" ~ era) =>
  CanonicalCBOREntryEncoder "gov/committee/v0" GovCommitteeOut
  where
  encodeEntry (GovCommitteeOut n) = toCanonicalCBOR (Proxy @"gov/committee/v0") n

instance
  (Era era, NamespaceEra "gov/committee/v0" ~ era) =>
  CanonicalCBOREntryDecoder "gov/committee/v0" GovCommitteeOut
  where
  decodeEntry = fmap GovCommitteeOut <$> fromCanonicalCBOR

data GovCommitteeIn = GovCommitteeIn EpochNo
  deriving (Eq, Ord, Show)

newtype GovCommitteeOut = GovCommitteeOut CanonicalCommitteeState
  deriving (Eq, Show, Generic)

instance IsKey GovCommitteeIn where
  keySize = namespaceKeySize @"gov/committee/v0"
  packKeyM (GovCommitteeIn (EpochNo no)) = do
    packWord64beM no
  unpackKeyM = do
    no <- unpackBigEndianM
    return $ GovCommitteeIn (EpochNo no)

newtype CanonicalCommitteeState = CanonicalCommitteeState
  { csCommitteeCreds :: Map.Map (CanonicalCredential ColdCommitteeRole) CanonicalCommitteeAuthorization
  }
  deriving (Eq, Show, Generic)

instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v CanonicalCommitteeState where
  toCanonicalCBOR v CanonicalCommitteeState {..} = toCanonicalCBOR v csCommitteeCreds

instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v CanonicalCommitteeState where
  fromCanonicalCBOR = do
    st_ <- fromCanonicalCBOR
    return $ CanonicalCommitteeState <$> st_

instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v CanonicalCommitteeAuthorization where
  toCanonicalCBOR v (CanonicalCommitteeHotCredential cred) =
    toCanonicalCBOR v (0 :: Word8, cred)
  toCanonicalCBOR v (CanonicalCommitteeMemberResigned ma) =
    toCanonicalCBOR v (1 :: Word8, ma)

instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v CanonicalCommitteeAuthorization where
  fromCanonicalCBOR = do
    decodeListLenCanonicalOf 2
    Versioned (tag :: Word8) <- fromCanonicalCBOR
    case tag of
      0 -> fmap CanonicalCommitteeHotCredential <$> fromCanonicalCBOR
      1 -> fmap CanonicalCommitteeMemberResigned <$> fromCanonicalCBOR
      _ -> fail "Invalid CommitteeAuthorization tag"

data CanonicalCommitteeAuthorization
  = CanonicalCommitteeHotCredential (CanonicalCredential HotCommitteeRole)
  | CanonicalCommitteeMemberResigned (StrictMaybe Anchor)
  deriving (Eq, Show, Ord, Generic)

mkCanonicalCommitteeAuthorization :: CommitteeAuthorization -> CanonicalCommitteeAuthorization
mkCanonicalCommitteeAuthorization (CommitteeHotCredential credential) = CanonicalCommitteeHotCredential (mkCanonicalCredential credential)
mkCanonicalCommitteeAuthorization (CommitteeMemberResigned anchor) = CanonicalCommitteeMemberResigned anchor

fromCanonicalCommitteeAuthorization :: CanonicalCommitteeAuthorization -> CommitteeAuthorization
fromCanonicalCommitteeAuthorization (CanonicalCommitteeHotCredential credential) = CommitteeHotCredential (fromCanonicalCredential credential)
fromCanonicalCommitteeAuthorization (CanonicalCommitteeMemberResigned anchor) = CommitteeMemberResigned anchor
