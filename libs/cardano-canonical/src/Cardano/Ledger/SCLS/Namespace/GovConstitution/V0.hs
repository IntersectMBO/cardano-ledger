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

module Cardano.Ledger.SCLS.Namespace.GovConstitution.V0 (
  CanonicalConstitution (..),
  GovConstitutionIn (..),
  GovConstitutionOut (..),
  IsCanonicalConstitution (..),
) where

import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.SCLS.BaseTypes (Anchor (..), EpochNo (..), StrictMaybe (..))
import Cardano.Ledger.SCLS.Common ()
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

instance ToCanonicalCBOR v CanonicalConstitution where
  toCanonicalCBOR v (CanonicalConstitution {..}) =
    toCanonicalCBOR v (constitutionAnchor, constitutionGuardrailsScriptHash)

instance FromCanonicalCBOR v CanonicalConstitution where
  fromCanonicalCBOR = do
    Versioned (anchor, script) <- fromCanonicalCBOR
    return $ Versioned $ CanonicalConstitution anchor script

instance KnownNamespace "gov/constitution/v0" where
  type NamespaceKey "gov/constitution/v0" = GovConstitutionIn
  type NamespaceEntry "gov/constitution/v0" = GovConstitutionOut

instance CanonicalCBOREntryEncoder "gov/constitution/v0" GovConstitutionOut where
  encodeEntry (GovConstitutionOut n) = toCanonicalCBOR (Proxy @"gov/constitution/v0") n

instance CanonicalCBOREntryDecoder "gov/constitution/v0" GovConstitutionOut where
  decodeEntry = fmap GovConstitutionOut <$> fromCanonicalCBOR
