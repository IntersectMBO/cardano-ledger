{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  CanonicalCommittee (..),
) where

import Cardano.Ledger.BaseTypes (EpochNo (..), StrictMaybe, UnitInterval)
import Cardano.Ledger.CanonicalState.BasicTypes ()
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyRole (ColdCommitteeRole))
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..))
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (Versioned))
import qualified Data.Map.Strict as Map
import Data.MemPack.ByteOrdered (packWord64beM, unpackBigEndianM)
import Data.Proxy (Proxy (..))
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

newtype GovCommitteeIn = GovCommitteeIn EpochNo
  deriving (Eq, Ord, Show)

newtype GovCommitteeOut = GovCommitteeOut (StrictMaybe CanonicalCommittee)
  deriving (Generic, Eq, Show)

deriving newtype instance
  ToCanonicalCBOR "gov/committee/v0" (StrictMaybe CanonicalCommittee) =>
  ToCanonicalCBOR "gov/committee/v0" GovCommitteeOut

deriving newtype instance
  FromCanonicalCBOR "gov/committee/v0" (StrictMaybe CanonicalCommittee) =>
  FromCanonicalCBOR "gov/committee/v0" GovCommitteeOut

instance IsKey GovCommitteeIn where
  keySize = namespaceKeySize @"gov/committee/v0"
  packKeyM (GovCommitteeIn (EpochNo no)) = packWord64beM no
  unpackKeyM = do
    GovCommitteeIn . EpochNo <$> unpackBigEndianM

data CanonicalCommittee = CanonicalCommittee
  { committeeMembers :: !(Map.Map (Credential ColdCommitteeRole) EpochNo)
  , committeeThreshold :: !UnitInterval
  }
  deriving (Eq, Show, Generic)

instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v CanonicalCommittee where
  toCanonicalCBOR v CanonicalCommittee {..} =
    toCanonicalCBOR v (committeeMembers, committeeThreshold)

instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v CanonicalCommittee where
  fromCanonicalCBOR = do
    Versioned (members, threshold) <- fromCanonicalCBOR @v
    return $
      Versioned $
        CanonicalCommittee {committeeMembers = members, committeeThreshold = threshold}
