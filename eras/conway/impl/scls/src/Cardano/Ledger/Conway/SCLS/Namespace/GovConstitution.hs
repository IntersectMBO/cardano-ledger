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
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Namespace.GovConstitution (
  GovConstitutionIn (..),
  CanonicalConstitution (..),
  mkCanonicalConstitution,
  fromCanonicalConstitution,
) where

import Cardano.Ledger.BaseTypes (Anchor (..), EpochNo (..), StrictMaybe (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (Constitution (..))
import Cardano.Ledger.Conway.SCLS.Common ()
import Cardano.Ledger.Hashes (ScriptHash (..))
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
import GHC.Generics (Generic)

data GovConstitutionIn = GovConstitutionIn EpochNo
  deriving (Eq, Ord, Show)

instance IsKey GovConstitutionIn where
  keySize = namespaceKeySize @"gov/constitution/v0"
  packKeyM (GovConstitutionIn (EpochNo epochNo)) = do
    packWord64beM epochNo
  unpackKeyM = do
    epochNo <- unpackBigEndianM
    return $ GovConstitutionIn (EpochNo epochNo)

mkCanonicalConstitution :: Constitution ConwayEra -> CanonicalConstitution
mkCanonicalConstitution Constitution {..} = CanonicalConstitution {..}

fromCanonicalConstitution :: CanonicalConstitution -> Constitution ConwayEra
fromCanonicalConstitution CanonicalConstitution {..} = Constitution {..}

data CanonicalConstitution = CanonicalConstitution
  { constitutionAnchor :: !Anchor
  , constitutionGuardrailsScriptHash :: !(StrictMaybe ScriptHash)
  }
  deriving (Eq, Show, Generic)

instance ToCanonicalCBOR v CanonicalConstitution where
  toCanonicalCBOR v (CanonicalConstitution {..}) =
    toCanonicalCBOR v (constitutionAnchor, constitutionGuardrailsScriptHash)

instance FromCanonicalCBOR v CanonicalConstitution where
  fromCanonicalCBOR = do
    Versioned (anchor, script) <- fromCanonicalCBOR
    return $ Versioned $ CanonicalConstitution anchor script

instance KnownNamespace "gov/constitution/v0" where
  type NamespaceKey "gov/constitution/v0" = GovConstitutionIn
  type NamespaceEntry "gov/constitution/v0" = CanonicalConstitution

instance CanonicalCBOREntryEncoder "gov/constitution/v0" CanonicalConstitution where
  encodeEntry n = toCanonicalCBOR (Proxy @"gov/constitution/v0") n

instance CanonicalCBOREntryDecoder "gov/constitution/v0" CanonicalConstitution where
  decodeEntry = fromCanonicalCBOR
