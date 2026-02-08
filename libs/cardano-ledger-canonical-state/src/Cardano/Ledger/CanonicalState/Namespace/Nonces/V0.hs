{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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

module Cardano.Ledger.CanonicalState.Namespace.Nonces.V0 (
  NoncesIn (..),
  NoncesOut (..),
  NoncesState (..),
  CanonicalWithOrigin (..),
  mkCanonicalWithOrigin,
  fromCanonicalWithOrigin,
  CanonicalNonce (..),
  mkCanonicalNonce,
  fromCanonicalNonce,
) where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (Nonce (..), SlotNo (..))
import Cardano.Ledger.CanonicalState.BasicTypes ()
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
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
import qualified Codec.CBOR.Decoding as D
import Control.Monad (join, unless)
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
  , noncesStateEvolvingNonce :: !CanonicalNonce
  , noncesStateCandidateNonce :: !CanonicalNonce
  , noncesStateEpochNonce :: !CanonicalNonce
  , noncesStateLabNonce :: !CanonicalNonce
  , noncesStateLastEpochBlockNonce :: !CanonicalNonce
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

data CanonicalNonce
  = CanonicalNonce (H.Hash Hash.Blake2b_256 Nonce)
  | CanonicalNeutralNonce
  deriving (Generic, Show, Eq, Ord)

mkCanonicalNonce :: Nonce -> CanonicalNonce
mkCanonicalNonce (Nonce p) = CanonicalNonce p
mkCanonicalNonce NeutralNonce = CanonicalNeutralNonce

fromCanonicalNonce :: CanonicalNonce -> Nonce
fromCanonicalNonce CanonicalNeutralNonce = NeutralNonce
fromCanonicalNonce (CanonicalNonce p) = Nonce p

instance ToCanonicalCBOR v CanonicalNonce where
  toCanonicalCBOR v (CanonicalNonce n) = toCanonicalCBOR v (1 :: Word8, n)
  toCanonicalCBOR v (CanonicalNeutralNonce) = toCanonicalCBOR v [0 :: Word8]

instance FromCanonicalCBOR v CanonicalNonce where
  fromCanonicalCBOR = join $ assumeCanonicalDecoder $ do
    n <- D.decodeListLenCanonical
    case n of
      1 -> do
        D.decodeWordOf 0
        return (return $ Versioned CanonicalNeutralNonce)
      2 -> do
        D.decodeWordOf 1
        return (fmap CanonicalNonce <$> fromCanonicalCBOR)
      _ -> fail $ "CanonicalNonce: Unexpected tuple size " <> show n

instance Era (NamespaceEra v) => ToCanonicalCBOR v NoncesState where
  toCanonicalCBOR v NoncesState {..} =
    encodeAsMap
      [ mkEncodablePair v ("last_slot" :: Text) noncesStateLastSlot
      , mkEncodablePair v ("cert_counters" :: Text) noncesStateOCertCounters
      , mkEncodablePair v ("evolving_nonce" :: Text) noncesStateEvolvingNonce
      , mkEncodablePair v ("candidate_nonce" :: Text) noncesStateCandidateNonce
      , mkEncodablePair v ("epoch_nonce" :: Text) noncesStateEpochNonce
      , mkEncodablePair v ("lab_nonce" :: Text) noncesStateLabNonce
      , mkEncodablePair v ("last_epoch_block_nonce" :: Text) noncesStateLastEpochBlockNonce
      ]

instance (NamespaceEra v ~ era, Era era) => FromCanonicalCBOR v NoncesState where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 7
    Versioned noncesStateLabNonce <- decodeField @v "lab_nonce"
    Versioned noncesStateLastSlot <- decodeField @v "last_slot"
    Versioned noncesStateEpochNonce <- decodeField @v "epoch_nonce"
    Versioned noncesStateOCertCounters <- decodeField @v "cert_counters"
    Versioned noncesStateEvolvingNonce <- decodeField @v "evolving_nonce"
    Versioned noncesStateCandidateNonce <- decodeField @v "candidate_nonce"
    Versioned noncesStateLastEpochBlockNonce <- decodeField @v "last_epoch_block_nonce"
    pure $ Versioned NoncesState {..}

instance Era (NamespaceEra "nonces/v0") => KnownNamespace "nonces/v0" where
  type NamespaceKey "nonces/v0" = NoncesIn
  type NamespaceEntry "nonces/v0" = NoncesOut

instance Era (NamespaceEra "nonces/v0") => CanonicalCBOREntryEncoder "nonces/v0" NoncesOut where
  encodeEntry (NoncesOut n) = toCanonicalCBOR (Proxy @"nonces/v0") n

instance Era (NamespaceEra "nonces/v0") => CanonicalCBOREntryDecoder "nonces/v0" NoncesOut where
  decodeEntry = fmap NoncesOut <$> fromCanonicalCBOR @"nonces/v0"

instance IsKey NoncesIn where
  keySize = namespaceKeySize @"nonces/v0"
  packKeyM (NoncesIn) = do
    packM (0 :: Word8)
  unpackKeyM = do
    (0 :: Word8) <- unpackM
    return NoncesIn

decodeField :: forall v s a. FromCanonicalCBOR v a => T.Text -> CanonicalDecoder s (Versioned v a)
decodeField fieldName = do
  Versioned s <- fromCanonicalCBOR
  unless (s == fieldName) $
    fail $
      T.unpack $
        "Expected field name " <> fieldName <> " but got " <> s
  fromCanonicalCBOR
