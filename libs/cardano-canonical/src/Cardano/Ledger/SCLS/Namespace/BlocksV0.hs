{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Definitions for the "blocks/v0" SCLS namespace.
module Cardano.Ledger.SCLS.Namespace.BlocksV0 (
  BlockIn (..),
  BlockOut (..),
) where

import Cardano.Ledger.SCLS.BaseTypes (EpochNo (..))
import Cardano.Ledger.Keys (KeyHash, StakePool)
import Cardano.SCLS.CDDL ()
import Cardano.SCLS.CBOR.Canonical.Decoder as D
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
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
import GHC.Num.Natural (Natural)

instance KnownNamespace "blocks/v0" where
  type NamespaceKey "blocks/v0" = BlockIn
  type NamespaceEntry "blocks/v0" = BlockOut

instance CanonicalCBOREntryEncoder "blocks/v0" BlockOut where
  encodeEntry (BlockOut n) = toCanonicalCBOR (Proxy @"blocks/v0") (BlockOut n)

instance CanonicalCBOREntryDecoder "blocks/v0" BlockOut where
  decodeEntry = fromCanonicalCBOR

newtype BlockIn = BlockIn (KeyHash StakePool, EpochNo)
  deriving (Eq, Ord, Show)

instance IsKey BlockIn where
  keySize = namespaceKeySize @"blocks/v0"
  packKeyM (BlockIn (kh, EpochNo epochNo)) = do
    packM kh
    packWord64beM epochNo
  unpackKeyM = do
    a <- unpackM
    epochNo <- unpackBigEndianM
    return $ BlockIn (a, EpochNo epochNo)

newtype BlockOut = BlockOut Natural
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToCanonicalCBOR v, FromCanonicalCBOR v)
