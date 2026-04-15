{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0 (
  CanonicalConstitution (..),
  GovConstitutionIn (..),
  GovConstitutionOut (..),
) where

import Cardano.Ledger.BaseTypes (Anchor (..), StrictMaybe (..))
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
import Data.MemPack (MemPack (packM, unpackM))
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Generics (Generic)

data GovConstitutionIn = GovConstitutionIn
  deriving (Eq, Ord, Show, Enum)

newtype GovConstitutionOut = GovConstitutionOut CanonicalConstitution
  deriving (Eq, Show, Generic)

instance IsKey GovConstitutionIn where
  keySize = namespaceKeySize @"gov/constitution/v0"
  packKeyM =
    packM . fromIntegral @_ @Word8 . fromEnum
  unpackKeyM =
    toEnum . fromIntegral @Word8 <$> unpackM

data CanonicalConstitution = CanonicalConstitution
  { constitutionAnchor :: !Anchor
  , constitutionGuardrailsScriptHash :: !(StrictMaybe ScriptHash)
  }
  deriving (Eq, Show, Generic)

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
