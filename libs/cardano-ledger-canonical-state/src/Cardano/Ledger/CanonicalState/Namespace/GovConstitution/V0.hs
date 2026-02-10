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

module Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0 (
  CanonicalConstitution (..),
  GovConstitutionIn (..),
  GovConstitutionOut (..),
  IsCanonicalConstitution (..),
) where

import Cardano.Ledger.BaseTypes (Anchor (..), EpochNo (..), StrictMaybe (..))
import Cardano.Ledger.CanonicalState.BasicTypes ()
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
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

newtype GovConstitutionIn = GovConstitutionIn EpochNo
  deriving (Eq, Ord, Show)

newtype GovConstitutionOut = GovConstitutionOut CanonicalConstitution
  deriving (Eq, Show, Generic)

instance IsKey GovConstitutionIn where
  keySize = namespaceKeySize @"gov/constitution/v0"
  packKeyM (GovConstitutionIn (EpochNo epochNo)) = do
    packWord64beM epochNo
  unpackKeyM = do
    epochNo <- unpackBigEndianM
    return $ GovConstitutionIn (EpochNo epochNo)

data CanonicalConstitution = CanonicalConstitution
  { constitutionAnchor :: !Anchor
  , constitutionGuardrailsScriptHash :: !(StrictMaybe ScriptHash)
  }
  deriving (Eq, Show, Generic)

class IsCanonicalConstitution a where
  mkCanonicalConstitution :: a -> CanonicalConstitution
  fromCanonicalConstitution :: CanonicalConstitution -> a

instance (NamespaceEra v ~ era, Era era) => ToCanonicalCBOR v CanonicalConstitution where
  toCanonicalCBOR v (CanonicalConstitution {..}) =
    toCanonicalCBOR v (constitutionAnchor, constitutionGuardrailsScriptHash)

instance (NamespaceEra v ~ era, Era era) => FromCanonicalCBOR v CanonicalConstitution where
  fromCanonicalCBOR = do
    Versioned (anchor, script) <- fromCanonicalCBOR @v
    return $ Versioned $ CanonicalConstitution anchor script

instance (NamespaceEra "gov/constitution/v0" ~ era, Era era) => KnownNamespace "gov/constitution/v0" where
  type NamespaceKey "gov/constitution/v0" = GovConstitutionIn
  type NamespaceEntry "gov/constitution/v0" = GovConstitutionOut

instance
  (NamespaceEra "gov/constitution/v0" ~ era, Era era) =>
  CanonicalCBOREntryEncoder "gov/constitution/v0" GovConstitutionOut
  where
  encodeEntry (GovConstitutionOut n) = toCanonicalCBOR (Proxy @"gov/constitution/v0") n

instance
  (NamespaceEra "gov/constitution/v0" ~ era, Era era) =>
  CanonicalCBOREntryDecoder "gov/constitution/v0" GovConstitutionOut
  where
  decodeEntry = fmap GovConstitutionOut <$> fromCanonicalCBOR
