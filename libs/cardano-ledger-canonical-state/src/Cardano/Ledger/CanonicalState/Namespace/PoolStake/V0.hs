{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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

module Cardano.Ledger.CanonicalState.Namespace.PoolStake.V0 (
  PoolStakeIn (..),
  PoolStakeOut (..),
) where

import Cardano.Ledger.CanonicalState.BasicTypes (CanonicalCoin (..), CanonicalVRFVerKeyHash (..))
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.Ledger.Keys (KeyHash, StakePool, StakePoolVRF)
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..), decodeMapLenCanonicalOf)
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..), encodeAsMap, mkEncodablePair)
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import Data.MemPack (MemPack (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)

newtype PoolStakeIn = PoolStakeIn (KeyHash StakePool)
  deriving (Eq, Ord, Show)

instance IsKey PoolStakeIn where
  keySize = namespaceKeySize @"pool_stake/v0"
  packKeyM (PoolStakeIn kh) = packM kh
  unpackKeyM = PoolStakeIn <$> unpackM

data PoolStakeOut = PoolStakeOut
  { total :: !CanonicalCoin
  , vrf :: !(CanonicalVRFVerKeyHash StakePoolVRF)
  }
  deriving (Eq, Show)
  deriving (Generic)

instance (NamespaceEra v ~ era, Era era) => ToCanonicalCBOR v PoolStakeOut where
  toCanonicalCBOR v (PoolStakeOut total vrf) =
    encodeAsMap
      [ mkEncodablePair v ("vrf" :: Text) vrf
      , mkEncodablePair v ("total" :: Text) total
      ]

instance (NamespaceEra v ~ era, Era era) => FromCanonicalCBOR v PoolStakeOut where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 2
    Versioned ("vrf" :: Text) <- fromCanonicalCBOR @v
    Versioned vrf <- fromCanonicalCBOR @v
    Versioned ("total" :: Text) <- fromCanonicalCBOR @v
    Versioned total <- fromCanonicalCBOR @v
    return $ Versioned PoolStakeOut {..}

instance (NamespaceEra "pool_stake/v0" ~ era, Era era) => KnownNamespace "pool_stake/v0" where
  type NamespaceKey "pool_stake/v0" = PoolStakeIn
  type NamespaceEntry "pool_stake/v0" = PoolStakeOut

instance
  (NamespaceEra "pool_stake/v0" ~ era, Era era) =>
  CanonicalCBOREntryEncoder "pool_stake/v0" PoolStakeOut
  where
  encodeEntry n = toCanonicalCBOR (Proxy @"pool_stake/v0") n

instance
  (NamespaceEra "pool_stake/v0" ~ era, Era era) =>
  CanonicalCBOREntryDecoder "pool_stake/v0" PoolStakeOut
  where
  decodeEntry = fromCanonicalCBOR
