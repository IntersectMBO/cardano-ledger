{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.GovProposals.V0 (
  GovProposalIn (..),
  GovProposalOut (..),
  CanonicalGovActionId (..),
  CanonicalGovActionIx (..),
) where

import Cardano.Ledger.CanonicalState.BasicTypes ()
import Cardano.Ledger.TxIn
import Cardano.SCLS.CBOR.Canonical.Decoder as D
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CDDL ()
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
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)

newtype CanonicalGovActionIx = CanonicalGovActionIx Word16
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (ToCanonicalCBOR v)
  deriving newtype (FromCanonicalCBOR v)

data CanonicalGovActionId = CanonicalGovActionId
  { gaidTxId :: !TxId
  , gaidGovActionIx :: {-# UNPACK #-} !CanonicalGovActionIx
  }
  deriving (Eq, Ord, Show, Generic)

newtype GovProposalIn = GovProposalIn CanonicalGovActionId
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

-- | Canonical wrapper over proposal order and gov action state. Because this
-- is on-chain data we create a wrapper for that.
data GovProposalOut v = GovProposalOut
  { gpoProposalOrder :: !Word64
  , gpoProposal :: !v
  }
  deriving (Eq, Show, Generic)

type instance NamespaceKeySize "gov/proposals/v0" = 34

instance ToCanonicalCBOR v p => ToCanonicalCBOR v (GovProposalOut p) where
  toCanonicalCBOR v (GovProposalOut {..}) =
    toCanonicalCBOR v (gpoProposalOrder, gpoProposal)

instance FromCanonicalCBOR v p => FromCanonicalCBOR v (GovProposalOut p) where
  fromCanonicalCBOR = do
    Versioned (gpoProposalOrder, gpoProposal) <- fromCanonicalCBOR @v
    return $ Versioned $ GovProposalOut {..}

instance
  ToCanonicalCBOR "gov/proposals/v0" v =>
  CanonicalCBOREntryEncoder "gov/proposals/v0" (GovProposalOut v)
  where
  encodeEntry = toCanonicalCBOR (Proxy @"gov/proposals/v0")

instance
  FromCanonicalCBOR "gov/proposals/v0" v =>
  CanonicalCBOREntryDecoder "gov/proposals/v0" (GovProposalOut v)
  where
  decodeEntry = fromCanonicalCBOR
