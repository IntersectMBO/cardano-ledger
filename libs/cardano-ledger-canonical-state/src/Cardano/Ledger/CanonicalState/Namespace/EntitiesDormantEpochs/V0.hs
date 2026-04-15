{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
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

module Cardano.Ledger.CanonicalState.Namespace.EntitiesDormantEpochs.V0 (
  EntitiesDormantEpochsIn (..),
  EntitiesDormantEpochsOut (..),
) where

import Cardano.Ledger.BaseTypes (EpochNo)
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..))
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  NamespaceKeySize,
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import Data.MemPack (MemPack (packM, unpackM))
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Generics (Generic)

instance
  ( Era era
  , NamespaceEra "entities/dormant_epochs/v0" ~ era
  , ToCanonicalCBOR "entities/dormant_epochs/v0" EntitiesDormantEpochsOut
  , FromCanonicalCBOR "entities/dormant_epochs/v0" EntitiesDormantEpochsOut
  ) =>
  KnownNamespace "entities/dormant_epochs/v0"
  where
  type NamespaceKey "entities/dormant_epochs/v0" = EntitiesDormantEpochsIn
  type NamespaceEntry "entities/dormant_epochs/v0" = EntitiesDormantEpochsOut

instance
  ( Era era
  , NamespaceEra "entities/dormant_epochs/v0" ~ era
  , ToCanonicalCBOR "entities/dormant_epochs/v0" EntitiesDormantEpochsOut
  ) =>
  CanonicalCBOREntryEncoder "entities/dormant_epochs/v0" EntitiesDormantEpochsOut
  where
  encodeEntry = toCanonicalCBOR (Proxy @"entities/dormant_epochs/v0")

instance
  ( Era era
  , NamespaceEra "entities/dormant_epochs/v0" ~ era
  , FromCanonicalCBOR "entities/dormant_epochs/v0" EntitiesDormantEpochsOut
  ) =>
  CanonicalCBOREntryDecoder "entities/dormant_epochs/v0" EntitiesDormantEpochsOut
  where
  decodeEntry = fromCanonicalCBOR

data EntitiesDormantEpochsIn = EntitiesDormantEpochsIn
  deriving (Eq, Ord, Show, Enum, Generic)

type instance NamespaceKeySize "entities/dormant_epochs/v0" = 1

instance IsKey EntitiesDormantEpochsIn where
  keySize = namespaceKeySize @"entities/dormant_epochs/v0"
  packKeyM =
    packM . fromIntegral @_ @Word8 . fromEnum
  unpackKeyM =
    toEnum . fromIntegral @Word8 @Int <$> unpackM

newtype EntitiesDormantEpochsOut
  = EntitiesDormantEpochsOut EpochNo
  deriving (Eq, Show, Generic)

deriving newtype instance
  ToCanonicalCBOR "entities/dormant_epochs/v0" EpochNo =>
  ToCanonicalCBOR "entities/dormant_epochs/v0" EntitiesDormantEpochsOut

deriving newtype instance
  FromCanonicalCBOR "entities/dormant_epochs/v0" EpochNo =>
  FromCanonicalCBOR "entities/dormant_epochs/v0" EntitiesDormantEpochsOut
