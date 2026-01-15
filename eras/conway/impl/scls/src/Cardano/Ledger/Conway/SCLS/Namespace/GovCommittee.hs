{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Namespace.GovCommittee (
  GovCommitteeIn (..),
  CanonicalCommitteeState (..),
  mkCanonicalCommitteeState,
  fromCanonicalCommitteeState,
  CanonicalCommitteeAuthorization (..),
  mkCanonicalCommitteeAuthorization,
  fromCanonicalCommitteeAuthorization,
) where

import Cardano.Ledger.SCLS.BaseTypes (Anchor (..), EpochNo (..), StrictMaybe (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.SCLS.Common (
  CanonicalCredential (..),
  fromCanonicalCredential,
  mkCanonicalCredential,
 )
import Cardano.Ledger.Conway.SCLS.Namespace.GovConstitution ()
import Cardano.Ledger.Conway.SCLS.Namespace.GovPParams ()
import Cardano.Ledger.Conway.SCLS.Namespace.Snapshots ()
import Cardano.Ledger.Keys (KeyRole (ColdCommitteeRole, HotCommitteeRole))
import Cardano.Ledger.State (CommitteeAuthorization (..), CommitteeState (..))
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..), decodeListLenCanonicalOf)
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import qualified Data.Map.Strict as Map
import Data.MemPack.ByteOrdered (packWord64beM, unpackBigEndianM)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Generics (Generic)

instance KnownNamespace "gov/committee/v0" where
  type NamespaceKey "gov/committee/v0" = GovCommitteeIn
  type NamespaceEntry "gov/committee/v0" = CanonicalCommitteeState

instance CanonicalCBOREntryEncoder "gov/committee/v0" CanonicalCommitteeState where
  encodeEntry n = toCanonicalCBOR (Proxy @"gov/committee/v0") n

instance CanonicalCBOREntryDecoder "gov/committee/v0" CanonicalCommitteeState where
  decodeEntry = fromCanonicalCBOR

data GovCommitteeIn = GovCommitteeIn EpochNo
  deriving (Eq, Ord, Show)

instance IsKey GovCommitteeIn where
  keySize = namespaceKeySize @"gov/committee/v0"
  packKeyM (GovCommitteeIn (EpochNo no)) = do
    packWord64beM no
  unpackKeyM = do
    no <- unpackBigEndianM
    return $ GovCommitteeIn (EpochNo no)

newtype CanonicalCommitteeState = CanonicalCommitteeState
  { csCommitteeCreds :: Map.Map (CanonicalCredential ColdCommitteeRole) CanonicalCommitteeAuthorization
  }
  deriving (Eq, Show, Generic)

mkCanonicalCommitteeState :: CommitteeState ConwayEra -> CanonicalCommitteeState
mkCanonicalCommitteeState CommitteeState {..} =
  CanonicalCommitteeState
    ( Map.fromList
        [ (mkCanonicalCredential k, mkCanonicalCommitteeAuthorization v)
        | (k, v) <- Map.toList csCommitteeCreds
        ]
    )

fromCanonicalCommitteeState :: CanonicalCommitteeState -> CommitteeState ConwayEra
fromCanonicalCommitteeState CanonicalCommitteeState {..} =
  CommitteeState
    ( Map.fromList
        [ (fromCanonicalCredential k, fromCanonicalCommitteeAuthorization v)
        | (k, v) <- Map.toList csCommitteeCreds
        ]
    )

instance ToCanonicalCBOR v CanonicalCommitteeState where
  toCanonicalCBOR v CanonicalCommitteeState {..} = toCanonicalCBOR v csCommitteeCreds

instance FromCanonicalCBOR v CanonicalCommitteeState where
  fromCanonicalCBOR = do
    st_ <- fromCanonicalCBOR
    return $ CanonicalCommitteeState <$> st_

instance ToCanonicalCBOR v CanonicalCommitteeAuthorization where
  toCanonicalCBOR v (CanonicalCommitteeHotCredential cred) =
    toCanonicalCBOR v (0 :: Word8, cred)
  toCanonicalCBOR v (CanonicalCommitteeMemberResigned ma) =
    toCanonicalCBOR v (1 :: Word8, ma)

instance FromCanonicalCBOR v CanonicalCommitteeAuthorization where
  fromCanonicalCBOR = do
    decodeListLenCanonicalOf 2
    Versioned (tag :: Word8) <- fromCanonicalCBOR
    case tag of
      0 -> fmap CanonicalCommitteeHotCredential <$> fromCanonicalCBOR
      1 -> fmap CanonicalCommitteeMemberResigned <$> fromCanonicalCBOR
      _ -> fail "Invalid CommitteeAuthorization tag"

data CanonicalCommitteeAuthorization
  = CanonicalCommitteeHotCredential (CanonicalCredential HotCommitteeRole)
  | CanonicalCommitteeMemberResigned (StrictMaybe Anchor)
  deriving (Eq, Show, Ord, Generic)

mkCanonicalCommitteeAuthorization :: CommitteeAuthorization -> CanonicalCommitteeAuthorization
mkCanonicalCommitteeAuthorization (CommitteeHotCredential credential) = CanonicalCommitteeHotCredential (mkCanonicalCredential credential)
mkCanonicalCommitteeAuthorization (CommitteeMemberResigned anchor) = CanonicalCommitteeMemberResigned anchor

fromCanonicalCommitteeAuthorization :: CanonicalCommitteeAuthorization -> CommitteeAuthorization
fromCanonicalCommitteeAuthorization (CanonicalCommitteeHotCredential credential) = CommitteeHotCredential (fromCanonicalCredential credential)
fromCanonicalCommitteeAuthorization (CanonicalCommitteeMemberResigned anchor) = CommitteeMemberResigned anchor
