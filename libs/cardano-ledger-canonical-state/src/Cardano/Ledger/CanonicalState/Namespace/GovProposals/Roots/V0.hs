{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.GovProposals.Roots.V0 (
  GovProposalsRootsIn (..),
  GovProposalsRootsOut (..),
) where

import Cardano.Ledger.CanonicalState.LedgerCBOR
import Cardano.Ledger.CanonicalState.Namespace (NamespaceEra)
import Cardano.Ledger.CanonicalState.Namespace.GovProposals.V0 (CanonicalGovActionId (..))
import Cardano.Ledger.Core (Era)
import Cardano.Ledger.TxIn
import Cardano.SCLS.CBOR.Canonical.Decoder as D
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CDDL ()
import Cardano.SCLS.Entry.IsKey
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (NamespaceEntry, NamespaceKey),
  NamespaceKeySize,
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import Data.Map (Map)
import Data.MapExtras (boundedEnumMap, lookupMapFail)
import Data.MemPack
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Generics (Generic)

instance (Era era, NamespaceEra "gov/proposals/roots/v0" ~ era) => KnownNamespace "gov/proposals/roots/v0" where
  type NamespaceKey "gov/proposals/roots/v0" = GovProposalsRootsIn
  type NamespaceEntry "gov/proposals/roots/v0" = GovProposalsRootsOut

data GovProposalsRootsIn
  = GovProposalsRootsInPParamUpdate
  | GovProposalsRootsInHardFork
  | GovProposalsRootsInCommittee
  | GovProposalsRootsInConstitution
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

keyGovProposalsRootsIn :: GovProposalsRootsIn -> Word8
keyGovProposalsRootsIn = fromIntegral . fromEnum

mapGovProposalsRootsIn :: Map Word8 GovProposalsRootsIn
mapGovProposalsRootsIn = boundedEnumMap keyGovProposalsRootsIn

instance IsKey GovProposalsRootsIn where
  keySize = namespaceKeySize @"gov/proposals/roots/v0"
  packKeyM = packM . keyGovProposalsRootsIn
  unpackKeyM = do
    tag :: Word8 <- unpackM
    lookupMapFail "GovProposalsRootsIn tag" mapGovProposalsRootsIn tag

newtype GovProposalsRootsOut = GovProposalsRootsOut CanonicalGovActionId
  deriving (Eq, Show, Generic)

deriving newtype instance
  ToCanonicalCBOR "gov/proposals/roots/v0" CanonicalGovActionId =>
  ToCanonicalCBOR "gov/proposals/roots/v0" GovProposalsRootsOut

deriving newtype instance
  FromCanonicalCBOR "gov/proposals/roots/v0" CanonicalGovActionId =>
  FromCanonicalCBOR "gov/proposals/roots/v0" GovProposalsRootsOut

instance
  (Era era, NamespaceEra "gov/proposals/roots/v0" ~ era) =>
  ToCanonicalCBOR "gov/proposals/roots/v0" CanonicalGovActionId
  where
  toCanonicalCBOR v (CanonicalGovActionId {..}) =
    toCanonicalCBOR v (gaidTxId, gaidGovActionIx)

instance
  (Era era, NamespaceEra "gov/proposals/roots/v0" ~ era) =>
  FromCanonicalCBOR "gov/proposals/roots/v0" CanonicalGovActionId
  where
  fromCanonicalCBOR = do
    Versioned (gaidTxId, gaidGovActionIx) <- fromCanonicalCBOR @"gov/proposals/roots/v0"
    return $ Versioned CanonicalGovActionId {..}

deriving via
  LedgerCBOR v TxId
  instance
    (Era era, NamespaceEra v ~ era) =>
    ToCanonicalCBOR v TxId

deriving via
  LedgerCBOR v TxId
  instance
    (Era era, NamespaceEra v ~ era) =>
    FromCanonicalCBOR v TxId

type instance NamespaceKeySize "gov/proposals/roots/v0" = 1

instance
  (Era era, NamespaceEra "gov/proposals/roots/v0" ~ era) =>
  CanonicalCBOREntryEncoder "gov/proposals/roots/v0" GovProposalsRootsOut
  where
  encodeEntry = toCanonicalCBOR (Proxy @"gov/proposals/roots/v0")

instance
  (Era era, NamespaceEra "gov/proposals/roots/v0" ~ era) =>
  CanonicalCBOREntryDecoder "gov/proposals/roots/v0" GovProposalsRootsOut
  where
  decodeEntry = fromCanonicalCBOR
