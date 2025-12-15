{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Namespace.PoolStake (
  PoolStakeIn (..),
  PoolStakeOut (..),
) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.SCLS.Common ()
import Cardano.Ledger.Keys
import Cardano.SCLS.CBOR.Canonical.Decoder
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.Entry.IsKey
import Cardano.SCLS.NamespaceCodec
import Cardano.SCLS.Versioned (Versioned (..))
import Data.MemPack
import Data.Proxy
import Data.Text (Text)
import GHC.Generics (Generic)

newtype PoolStakeIn = PoolStakeIn (KeyHash StakePool)
  deriving (Eq, Ord, Show)

instance IsKey PoolStakeIn where
  keySize = namespaceKeySize @"pool_stake/v0"
  packKeyM (PoolStakeIn kh) = packM kh
  unpackKeyM = PoolStakeIn <$> unpackM

data PoolStakeOut = PoolStakeOut
  { total :: !Coin
  , vrf :: !(VRFVerKeyHash StakePoolVRF)
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
