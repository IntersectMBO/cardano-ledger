{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.EntitiesStakePools.VRFKeyHashes.V0 (
  EntitiesStakePoolsVRFKeyHashesIn (..),
  EntitiesStakePoolsVRFKeyHashesOut (..),
) where

import Cardano.Ledger.BaseTypes (NonZero)
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.Ledger.Core (
  KeyRoleVRF (StakePoolVRF),
  VRFVerKeyHash,
  fromVRFVerKeyHash,
  toVRFVerKeyHash,
 )
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
import Data.Word (Word64)
import GHC.Generics (Generic)

instance
  ( Era era
  , NamespaceEra "entities/stake_pools/vrf_key_hashes/v0" ~ era
  , ToCanonicalCBOR "entities/stake_pools/vrf_key_hashes/v0" EntitiesStakePoolsVRFKeyHashesOut
  , FromCanonicalCBOR "entities/stake_pools/vrf_key_hashes/v0" EntitiesStakePoolsVRFKeyHashesOut
  ) =>
  KnownNamespace "entities/stake_pools/vrf_key_hashes/v0"
  where
  type NamespaceKey "entities/stake_pools/vrf_key_hashes/v0" = EntitiesStakePoolsVRFKeyHashesIn
  type NamespaceEntry "entities/stake_pools/vrf_key_hashes/v0" = EntitiesStakePoolsVRFKeyHashesOut

instance
  ( Era era
  , NamespaceEra "entities/stake_pools/vrf_key_hashes/v0" ~ era
  , ToCanonicalCBOR "entities/stake_pools/vrf_key_hashes/v0" EntitiesStakePoolsVRFKeyHashesOut
  ) =>
  CanonicalCBOREntryEncoder "entities/stake_pools/vrf_key_hashes/v0" EntitiesStakePoolsVRFKeyHashesOut
  where
  encodeEntry = toCanonicalCBOR (Proxy @"entities/stake_pools/vrf_key_hashes/v0")

instance
  ( Era era
  , NamespaceEra "entities/stake_pools/vrf_key_hashes/v0" ~ era
  , FromCanonicalCBOR "entities/stake_pools/vrf_key_hashes/v0" EntitiesStakePoolsVRFKeyHashesOut
  ) =>
  CanonicalCBOREntryDecoder "entities/stake_pools/vrf_key_hashes/v0" EntitiesStakePoolsVRFKeyHashesOut
  where
  decodeEntry = fromCanonicalCBOR

newtype EntitiesStakePoolsVRFKeyHashesIn = EntitiesStakePoolsVRFKeyHashesIn (VRFVerKeyHash StakePoolVRF)
  deriving (Eq, Ord, Show)

type instance NamespaceKeySize "entities/stake_pools/vrf_key_hashes/v0" = 32

instance IsKey EntitiesStakePoolsVRFKeyHashesIn where
  keySize = namespaceKeySize @"entities/stake_pools/vrf_key_hashes/v0"
  packKeyM (EntitiesStakePoolsVRFKeyHashesIn stakePoolVRF) =
    packM $ fromVRFVerKeyHash stakePoolVRF
  unpackKeyM =
    EntitiesStakePoolsVRFKeyHashesIn . toVRFVerKeyHash <$> unpackM

newtype EntitiesStakePoolsVRFKeyHashesOut
  = EntitiesStakePoolsVRFKeyHashesOut (NonZero Word64)
  deriving (Eq, Show, Generic)

deriving newtype instance
  ToCanonicalCBOR "entities/stake_pools/vrf_key_hashes/v0" (NonZero Word64) =>
  ToCanonicalCBOR "entities/stake_pools/vrf_key_hashes/v0" EntitiesStakePoolsVRFKeyHashesOut

deriving newtype instance
  FromCanonicalCBOR "entities/stake_pools/vrf_key_hashes/v0" (NonZero Word64) =>
  FromCanonicalCBOR "entities/stake_pools/vrf_key_hashes/v0" EntitiesStakePoolsVRFKeyHashesOut
