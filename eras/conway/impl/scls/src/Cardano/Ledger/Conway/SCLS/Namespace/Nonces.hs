{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Namespace.Nonces (
  NoncesIn (..),
  NoncesOut (..),
  CanonicalWithOrigin (..),
  mkCanonicalWithOrigin,
  fromCanonicalWithOrigin
) where

import qualified Cardano.Crypto.Hash as Hash
-- import Cardano.Crypto.Hashing (AbstractHash, Hash, hashToBytes, unsafeHashFromBytes)

import Cardano.Ledger.BaseTypes (Nonce (..), SlotNo (..))
import Cardano.Ledger.Conway.SCLS.Common ()
import qualified Cardano.Ledger.Hashes as H
import Cardano.Ledger.Keys
import Cardano.SCLS.CBOR.Canonical
import Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  decodeListLenCanonical,
  decodeMapLenCanonicalOf,
 )
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..), encodeAsMap, mkEncodablePair)
import Cardano.SCLS.Entry.IsKey
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Monad (unless)
import Data.Map.Strict (Map)
import Data.MemPack (MemPack (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import GHC.Generics (Generic)

data NoncesIn = NoncesIn
  deriving (Eq, Ord, Show)

newtype NoncesOut = NoncesOut NoncesState
  deriving (Eq, Ord, Show)

data NoncesState = NoncesState
  { noncesStateLastSlot :: !(CanonicalWithOrigin SlotNo)
  , noncesStateOCertCounters :: !(Map (KeyHash BlockIssuer) Word64)
  , noncesStateEvolvingNonce :: !Nonce
  , noncesStateCandidateNonce :: !Nonce
  , noncesStateEpochNonce :: !Nonce
  , noncesStateLabNonce :: !Nonce
  , noncesStateLastEpochBlockNonce :: !Nonce
  }
  deriving (Generic, Show, Eq, Ord)

data CanonicalWithOrigin t = CanonicalOrigin | CanonicalAt t
  deriving (Generic, Show, Eq, Ord)

mkCanonicalWithOrigin :: WithOrigin t -> CanonicalWithOrigin t
mkCanonicalWithOrigin Origin = CanonicalOrigin
mkCanonicalWithOrigin (At t) = CanonicalAt t

fromCanonicalWithOrigin :: CanonicalWithOrigin t -> WithOrigin t
fromCanonicalWithOrigin CanonicalOrigin = Origin
fromCanonicalWithOrigin (CanonicalAt t) = At t

instance ToCanonicalCBOR v a => ToCanonicalCBOR v (CanonicalWithOrigin a) where
  toCanonicalCBOR v CanonicalOrigin = toCanonicalCBOR v [0 :: Word8]
  toCanonicalCBOR v (CanonicalAt t) = toCanonicalCBOR v (1 :: Word8, t)

instance FromCanonicalCBOR v a => FromCanonicalCBOR v (CanonicalWithOrigin a) where
  fromCanonicalCBOR = do
    n <- decodeListLenCanonical
    case n of
      1 -> do
        Versioned (0 :: Word8) <- fromCanonicalCBOR
        return (Versioned CanonicalOrigin)
      2 -> do
        Versioned (1 :: Word8) <- fromCanonicalCBOR
        t <- fromCanonicalCBOR
        return (CanonicalAt <$> t)
      _ -> fail "Invalid WithOrigin encoding"

instance ToCanonicalCBOR v Nonce where
  toCanonicalCBOR v (Nonce n) = toCanonicalCBOR v (1 :: Word8, n)
  toCanonicalCBOR v (NeutralNonce) = toCanonicalCBOR v [0 :: Word8]

instance FromCanonicalCBOR v Nonce where
  fromCanonicalCBOR = fmap Nonce <$> fromCanonicalCBOR

instance ToCanonicalCBOR v NoncesState where
  toCanonicalCBOR v NoncesState {..} =
    encodeAsMap
      [ mkEncodablePair v ("lastSlot" :: Text) noncesStateLastSlot
      , mkEncodablePair v ("oCertCounters" :: Text) noncesStateOCertCounters
      , mkEncodablePair v ("evolvingNonce" :: Text) noncesStateEvolvingNonce
      , mkEncodablePair v ("candidateNonce" :: Text) noncesStateCandidateNonce
      , mkEncodablePair v ("epochNonce" :: Text) noncesStateEpochNonce
      , mkEncodablePair v ("labNonce" :: Text) noncesStateLabNonce
      , mkEncodablePair v ("lastEpochBlockNonce" :: Text) noncesStateLastEpochBlockNonce
      ]

instance FromCanonicalCBOR v NoncesState where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 7
    Versioned noncesStateLastSlot <- decodeField "last_slot"
    Versioned noncesStateOCertCounters <- decodeField "cert_counters"
    Versioned noncesStateEvolvingNonce <- decodeField "evolving_nonce"
    Versioned noncesStateCandidateNonce <- decodeField "candidate_nonce"
    Versioned noncesStateEpochNonce <- decodeField "epoch_nonce"
    Versioned noncesStateLabNonce <- decodeField "lab_nonce"
    Versioned noncesStateLastEpochBlockNonce <- decodeField "last_epoch_block_nonce"
    pure $ Versioned NoncesState {..}

instance KnownNamespace "nonces/v0" where
  type NamespaceKey "nonces/v0" = NoncesIn
  type NamespaceEntry "nonces/v0" = NoncesOut

instance CanonicalCBOREntryEncoder "nonces/v0" NoncesOut where
  encodeEntry (NoncesOut n) = toCanonicalCBOR (Proxy @"nonces/v0") n

instance CanonicalCBOREntryDecoder "nonces/v0" NoncesOut where
  decodeEntry = fmap NoncesOut <$> fromCanonicalCBOR

instance IsKey NoncesIn where
  keySize = namespaceKeySize @"nonces/v0"
  packKeyM (NoncesIn) = do
    packM (0 :: Word8)
  unpackKeyM = do
    (0 :: Word8) <- unpackM
    return NoncesIn

decodeField :: forall s v a. FromCanonicalCBOR v a => T.Text -> CanonicalDecoder s (Versioned v a)
decodeField fieldName = do
  Versioned s <- fromCanonicalCBOR
  unless (s == fieldName) $
    fail $
      T.unpack $
        "Expected field name " <> fieldName <> " but got " <> s
  fromCanonicalCBOR

instance ToCanonicalCBOR v (H.Hash a b) where
  toCanonicalCBOR v h = toCanonicalCBOR v (Hash.hashToBytes h)

instance H.HashAlgorithm a => FromCanonicalCBOR v (H.Hash a b) where
  fromCanonicalCBOR = do
    Versioned bytes <- fromCanonicalCBOR
    case Hash.hashFromBytesShort bytes of
      Just h -> return (Versioned h)
      Nothing -> fail "Invalid hash bytes"
