{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.Blocks.V0 (
  BlockIn (..),
  BlockOut (..),
) where

import Cardano.Ledger.BaseTypes (EpochNo (..))
import Cardano.Ledger.Keys (KeyHash, StakePool)
import Cardano.SCLS.CBOR.Canonical.Decoder as D
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Cardano.SCLS.CDDL ()
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  namespaceKeySize,
 )
import Data.MemPack (MemPack (..))
import Data.MemPack.ByteOrdered (packWord64beM, unpackBigEndianM)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import GHC.Num.Natural (Natural)

-- | Definition of the namespace.
instance KnownNamespace "blocks/v0" where
  type NamespaceKey "blocks/v0" = BlockIn
  type NamespaceEntry "blocks/v0" = BlockOut

data BlockIn = BlockIn
  { blockInStakePoolId :: !(KeyHash StakePool)
  , blockInEpochNo :: !EpochNo
  }
  deriving stock (Eq, Ord, Show, Generic)

instance IsKey BlockIn where
  keySize = namespaceKeySize @"blocks/v0"
  packKeyM (BlockIn kh (EpochNo epochNo)) = do
    packM kh
    packWord64beM epochNo
  unpackKeyM = do
    a <- unpackM
    epochNo <- unpackBigEndianM
    return $ BlockIn a (EpochNo epochNo)

-- Top level encoding/decoding

instance CanonicalCBOREntryEncoder "blocks/v0" BlockOut where
  encodeEntry (BlockOut n) = toCanonicalCBOR (Proxy @"blocks/v0") (BlockOut n)

instance CanonicalCBOREntryDecoder "blocks/v0" BlockOut where
  decodeEntry = fromCanonicalCBOR

newtype BlockOut = BlockOut Natural
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToCanonicalCBOR v, FromCanonicalCBOR v)
