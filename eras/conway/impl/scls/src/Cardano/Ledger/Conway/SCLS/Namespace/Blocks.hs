{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Namespace.Blocks (
  BlockIn (..),
  BlockOut (..),
) where

import Cardano.Ledger.BaseTypes (EpochNo (..))
import Cardano.Ledger.Conway.SCLS.Common ()
import Cardano.Ledger.Keys
import Cardano.SCLS.CBOR.Canonical.Decoder as D
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.Entry.IsKey
import Cardano.SCLS.NamespaceCodec
import Data.MemPack
import Data.MemPack.ByteOrdered
import Data.Proxy
import GHC.Num.Natural

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
  deriving (Eq, Ord, Show)

instance ToCanonicalCBOR v BlockOut where
  toCanonicalCBOR v (BlockOut n) = toCanonicalCBOR v (fromIntegral n :: Integer)

instance FromCanonicalCBOR v BlockOut where
  fromCanonicalCBOR = fmap (BlockOut . fromIntegral @Integer) <$> fromCanonicalCBOR
