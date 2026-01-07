{-# LANGUAGE DataKinds #-}
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
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Namespace.GovConstitution (
  GovConstitutionIn (..),
  GovConstitutionOut (..),
) where

import Cardano.Ledger.BaseTypes (EpochNo (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (Constitution (..))
import Cardano.Ledger.Conway.SCLS.Common ()
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..))
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import Data.MemPack.ByteOrdered (packWord64beM, unpackBigEndianM)
import Data.Proxy (Proxy (..))

data GovConstitutionIn = GovConstitutionIn EpochNo
  deriving (Eq, Ord, Show)

instance IsKey GovConstitutionIn where
  keySize = namespaceKeySize @"gov/constitution/v0"
  packKeyM (GovConstitutionIn (EpochNo epochNo)) = do
    packWord64beM epochNo
  unpackKeyM = do
    epochNo <- unpackBigEndianM
    return $ GovConstitutionIn (EpochNo epochNo)

newtype GovConstitutionOut = GovConstitutionOut (Constitution ConwayEra)
  deriving (Eq, Show)

deriving newtype instance ToCanonicalCBOR v GovConstitutionOut

deriving newtype instance FromCanonicalCBOR v GovConstitutionOut

instance ToCanonicalCBOR v (Constitution ConwayEra) where
  toCanonicalCBOR v Constitution {..} =
    toCanonicalCBOR v (constitutionAnchor, constitutionGuardrailsScriptHash)

instance FromCanonicalCBOR v (Constitution ConwayEra) where
  fromCanonicalCBOR = do
    Versioned (anchor, script) <- fromCanonicalCBOR
    return $ Versioned $ Constitution anchor script

instance KnownNamespace "gov/constitution/v0" where
  type NamespaceKey "gov/constitution/v0" = GovConstitutionIn
  type NamespaceEntry "gov/constitution/v0" = GovConstitutionOut

instance CanonicalCBOREntryEncoder "gov/constitution/v0" GovConstitutionOut where
  encodeEntry n = toCanonicalCBOR (Proxy @"gov/constitution/v0") n

instance CanonicalCBOREntryDecoder "gov/constitution/v0" GovConstitutionOut where
  decodeEntry = fromCanonicalCBOR
