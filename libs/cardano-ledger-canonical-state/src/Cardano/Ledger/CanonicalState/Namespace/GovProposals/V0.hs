{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.GovProposals.V0 (
  GovProposalIn (..),
  GovProposalOut (..),
  CanonicalGovActionId (..),
  CanonicalGovActionIx (..),
) where

import Cardano.Ledger.TxIn
import Cardano.SCLS.Entry.IsKey
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  NamespaceKeySize,
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import Data.MemPack
import Data.MemPack.ByteOrdered
import Data.Proxy (Proxy (..))
import Data.Word (Word16)
import GHC.Generics (Generic)
import Cardano.Ledger.CanonicalState.BasicTypes ()
import Cardano.SCLS.CBOR.Canonical.Decoder as D
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CDDL ()

newtype CanonicalGovActionIx = CanonicalGovActionIx Word16
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (ToCanonicalCBOR v)
  deriving newtype (FromCanonicalCBOR v)

data CanonicalGovActionId = CanonicalGovActionId
  { gaidTxId :: TxId
  , gaidGovActionIx :: {-# UNPACK #-} !CanonicalGovActionIx
  }
  deriving (Eq, Ord, Show, Generic)

data GovProposalIn = GovProposalIn CanonicalGovActionId
  deriving (Eq, Ord, Show)

instance MemPack CanonicalGovActionIx where
  packedByteCount _ = 2
  packM (CanonicalGovActionIx g) = do
    packWord16beM g
  unpackM = do
    g <- unpackBigEndianM
    return (CanonicalGovActionIx g)

instance IsKey GovProposalIn where
  keySize = namespaceKeySize @"gov/proposals/v0"
  packKeyM (GovProposalIn CanonicalGovActionId {..}) = do
    packM gaidTxId
    packM gaidGovActionIx
  unpackKeyM = do
    gaidTxId <- unpackM
    gaidGovActionIx <- unpackM
    return $ GovProposalIn CanonicalGovActionId {..}

-- | Canonical wrapper over gov action state. Because this is on-chain data
-- we create a wrapper for that.
newtype GovProposalOut v = GovProposalOut v
  deriving (Eq, Show, Generic)
  deriving newtype (ToCanonicalCBOR "gov/proposals/v0")
  deriving newtype (FromCanonicalCBOR "gov/proposals/v0")

type instance NamespaceKeySize "gov/proposals/v0" = 34

instance ToCanonicalCBOR "gov/proposals/v0" v => CanonicalCBOREntryEncoder "gov/proposals/v0" (GovProposalOut v) where
  encodeEntry (GovProposalOut n) = toCanonicalCBOR (Proxy @"gov/proposals/v0") n

instance
  (FromCanonicalCBOR "gov/proposals/v0" (GovProposalOut v)) =>
  CanonicalCBOREntryDecoder "gov/proposals/v0" (GovProposalOut v)
  where
  decodeEntry = fromCanonicalCBOR
