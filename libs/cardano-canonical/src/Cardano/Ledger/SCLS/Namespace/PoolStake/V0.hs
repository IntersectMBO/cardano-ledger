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
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.SCLS.Namespace.PoolStake.V0 (
  PoolStakeIn (..),
  PoolStakeOut (..),
) where

import Cardano.Ledger.SCLS.Common (CanonicalVRFVerKeyHash (..), CanonicalCoin (..) )
import Cardano.Ledger.Keys (KeyHash, StakePool (..), StakePoolVRF (..))
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

instance ToCanonicalCBOR v PoolStakeOut where
  toCanonicalCBOR v (PoolStakeOut total vrf) =
    encodeAsMap
      [ mkEncodablePair v ("vrf" :: Text) vrf
      , mkEncodablePair v ("total" :: Text) total
      ]

instance FromCanonicalCBOR v PoolStakeOut where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 2
    Versioned ("vrf" :: Text) <- fromCanonicalCBOR
    Versioned vrf <- fromCanonicalCBOR
    Versioned ("total" :: Text) <- fromCanonicalCBOR
    Versioned total <- fromCanonicalCBOR
    return $ Versioned PoolStakeOut {..}

instance KnownNamespace "pool_stake/v0" where
  type NamespaceKey "pool_stake/v0" = PoolStakeIn
  type NamespaceEntry "pool_stake/v0" = PoolStakeOut

instance CanonicalCBOREntryEncoder "pool_stake/v0" PoolStakeOut where
  encodeEntry n = toCanonicalCBOR (Proxy @"pool_stake/v0") n

instance CanonicalCBOREntryDecoder "pool_stake/v0" PoolStakeOut where
  decodeEntry = fromCanonicalCBOR
