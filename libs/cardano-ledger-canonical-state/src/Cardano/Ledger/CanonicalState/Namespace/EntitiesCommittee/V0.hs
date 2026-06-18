{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.EntitiesCommittee.V0 (
  EntitiesCommitteeIn (..),
  EntitiesCommitteeOut (..),
  CanonicalCommitteeState (..),
  CanonicalCommitteeAuthorization (..),
  mkCanonicalCommitteeAuthorization,
  fromCanonicalCommitteeAuthorization,
) where

import Cardano.Ledger.BaseTypes (Anchor (..), StrictMaybe (..))
import Cardano.Ledger.CanonicalState.BasicTypes ()
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.Ledger.Credential (Credential (..))
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
import Data.MemPack (MemPack (packM, unpackM))
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Generics (Generic)

instance (Era era, NamespaceEra "entities/committee/v0" ~ era) => KnownNamespace "entities/committee/v0" where
  type NamespaceKey "entities/committee/v0" = EntitiesCommitteeIn
  type NamespaceEntry "entities/committee/v0" = EntitiesCommitteeOut

instance
  (Era era, NamespaceEra "entities/committee/v0" ~ era) =>
  CanonicalCBOREntryEncoder "entities/committee/v0" EntitiesCommitteeOut
  where
  encodeEntry (EntitiesCommitteeOut n) = toCanonicalCBOR (Proxy @"entities/committee/v0") n

instance
  (Era era, NamespaceEra "entities/committee/v0" ~ era) =>
  CanonicalCBOREntryDecoder "entities/committee/v0" EntitiesCommitteeOut
  where
  decodeEntry = fmap EntitiesCommitteeOut <$> fromCanonicalCBOR

data EntitiesCommitteeIn = EntitiesCommitteeIn
  deriving (Eq, Ord, Show, Enum)

newtype EntitiesCommitteeOut = EntitiesCommitteeOut CanonicalCommitteeState
  deriving (Eq, Show, Generic)

instance IsKey EntitiesCommitteeIn where
  keySize = namespaceKeySize @"entities/committee/v0"
  packKeyM = do
    packM . fromIntegral @_ @Word8 . fromEnum
  unpackKeyM =
    toEnum . fromIntegral @Word8 <$> unpackM

newtype CanonicalCommitteeState = CanonicalCommitteeState
  { csCommitteeCreds :: Map.Map (Credential ColdCommitteeRole) CanonicalCommitteeAuthorization
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
      0 -> fmap CanonicalCommitteeHotCredential <$> fromCanonicalCBOR @v
      1 -> fmap CanonicalCommitteeMemberResigned <$> fromCanonicalCBOR @v
      _ -> fail "Invalid CommitteeAuthorization tag"

data CanonicalCommitteeAuthorization
  = CanonicalCommitteeHotCredential (Credential HotCommitteeRole)
  | CanonicalCommitteeMemberResigned (StrictMaybe Anchor)
  deriving (Eq, Show, Ord, Generic)

mkCanonicalCommitteeAuthorization :: CommitteeAuthorization -> CanonicalCommitteeAuthorization
mkCanonicalCommitteeAuthorization (CommitteeHotCredential credential) = CanonicalCommitteeHotCredential credential
mkCanonicalCommitteeAuthorization (CommitteeMemberResigned anchor) = CanonicalCommitteeMemberResigned anchor

fromCanonicalCommitteeAuthorization :: CanonicalCommitteeAuthorization -> CommitteeAuthorization
fromCanonicalCommitteeAuthorization (CanonicalCommitteeHotCredential credential) = CommitteeHotCredential credential
fromCanonicalCommitteeAuthorization (CanonicalCommitteeMemberResigned anchor) = CommitteeMemberResigned anchor
