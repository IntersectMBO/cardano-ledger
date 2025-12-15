{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Namespace.GovProposals (
  GovProposalIn (..),
  GovProposalOut (..),
  GovActionState' (..),
  toWire,
) where

import Cardano.Ledger.BaseTypes (EpochNo (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovActionState (..),
  GovPurposeId (..),
  ProposalProcedure (..),
  Vote (..),
 )
import Cardano.Ledger.Conway.SCLS.Common ()
import Cardano.Ledger.Conway.SCLS.LedgerCBOR
import Cardano.Ledger.Conway.SCLS.Namespace.GovConstitution ()
import Cardano.Ledger.Conway.SCLS.Namespace.GovPParams ()
import Cardano.Ledger.Conway.SCLS.Namespace.Snapshots ()
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Cardano.SCLS.CBOR.Canonical
import Cardano.SCLS.CBOR.Canonical.Decoder
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.Entry.IsKey
import Cardano.SCLS.NamespaceCodec
import Cardano.SCLS.Versioned (Versioned (..))
import Control.Monad (unless)
import Data.Map (Map)
import Data.MemPack
import Data.MemPack.ByteOrdered
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)

data GovProposalIn = GovProposalIn GovActionId
  deriving (Eq, Ord, Show)

instance MemPack GovActionIx where
  packedByteCount _ = 2
  packM (GovActionIx g) = do
    packWord16beM g
  unpackM = do
    g <- unpackBigEndianM
    return (GovActionIx g)

instance IsKey GovProposalIn where
  keySize = namespaceKeySize @"gov/proposals/v0"
  packKeyM (GovProposalIn GovActionId {..}) = do
    packM gaidTxId
    packM gaidGovActionIx
  unpackKeyM = do
    gaidTxId <- unpackM
    gaidGovActionIx <- unpackM
    return $ GovProposalIn GovActionId {..}

newtype GovProposalOut = GovProposalOut (GovActionState')
  deriving (Eq, Show)

deriving newtype instance ToCanonicalCBOR v (GovProposalOut)

deriving newtype instance FromCanonicalCBOR v (GovProposalOut)

instance ToCanonicalCBOR v (GovActionState') where
  toCanonicalCBOR v GovActionState' {..} =
    encodeAsMap
      [ mkEncodablePair v ("drep_votes" :: Text) gasDRepVotes
      , mkEncodablePair v ("proposed_in" :: Text) gasProposedIn
      , mkEncodablePair v ("expires_after" :: Text) gasExpiresAfter
      , mkEncodablePair v ("committee_votes" :: Text) gasCommitteeVotes
      , mkEncodablePair v ("stake_pool_votes" :: Text) gasStakePoolVotes
      , mkEncodablePair v ("proposal_procedure" :: Text) gasProposalProcedure
      ]

decodeField :: forall s v a. FromCanonicalCBOR v a => T.Text -> CanonicalDecoder s (Versioned v a)
decodeField fieldName = do
  Versioned s <- fromCanonicalCBOR
  unless (s == fieldName) $
    fail $
      T.unpack $
        "Expected field name " <> fieldName <> " but got " <> s
  fromCanonicalCBOR

toWire :: GovActionState ConwayEra -> GovActionState'
toWire GovActionState {..} = GovActionState' {..}

data GovActionState' = GovActionState'
  { gasCommitteeVotes :: !(Map (Credential HotCommitteeRole) Vote)
  , gasDRepVotes :: !(Map (Credential DRepRole) Vote)
  , gasStakePoolVotes :: !(Map (KeyHash StakePool) Vote)
  , gasProposalProcedure :: !(ProposalProcedure ConwayEra)
  , gasProposedIn :: !EpochNo
  , gasExpiresAfter :: !EpochNo
  }
  deriving (Eq, Show)
  deriving (Generic)

instance FromCanonicalCBOR v (GovActionState') where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 6
    Versioned gasDRepVotes <- decodeField "drep_votes"
    Versioned gasProposedIn <- decodeField "proposed_in"
    Versioned gasExpiresAfter <- decodeField "expires_after"
    Versioned gasCommitteeVotes <- decodeField "committee_votes"
    Versioned gasStakePoolVotes <- decodeField "stake_pool_votes"
    Versioned gasProposalProcedure <- decodeField "proposal_procedure"
    pure $ Versioned GovActionState' {..}

instance ToCanonicalCBOR v (ProposalProcedure ConwayEra) where
  toCanonicalCBOR v ProposalProcedure {..} =
    encodeAsMap
      [ mkEncodablePair v ("anchor" :: Text) pProcAnchor
      , mkEncodablePair v ("deposit" :: Text) pProcDeposit
      , mkEncodablePair v ("gov_action" :: Text) pProcGovAction
      , mkEncodablePair v ("return_address" :: Text) pProcReturnAddr
      ]

instance FromCanonicalCBOR v (ProposalProcedure ConwayEra) where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 4
    Versioned pProcAnchor <- decodeField "anchor"
    Versioned pProcDeposit <- decodeField "deposit"
    Versioned pProcGovAction <- decodeField "gov_action"
    Versioned pProcReturnAddr <- decodeField "return_address"
    pure $ Versioned ProposalProcedure {..}

instance ToCanonicalCBOR v (GovAction ConwayEra) where
  toCanonicalCBOR v (ParameterChange purposeId pparamsUpdate mScriptHash) =
    toCanonicalCBOR v (0 :: Word8, purposeId, pparamsUpdate, mScriptHash)
  toCanonicalCBOR v (HardForkInitiation purposeId protVer) =
    toCanonicalCBOR v (1 :: Word8, purposeId, protVer)
  toCanonicalCBOR v (TreasuryWithdrawals withdrawals mScriptHash) =
    toCanonicalCBOR v (2 :: Word8, withdrawals, mScriptHash)
  toCanonicalCBOR v (NoConfidence purposeId) =
    toCanonicalCBOR v (3 :: Word8, purposeId)
  toCanonicalCBOR v (UpdateCommittee purposeId removedMembers addedMembers newThreshold) =
    toCanonicalCBOR v (4 :: Word8, purposeId, removedMembers, addedMembers, newThreshold)
  toCanonicalCBOR v (NewConstitution purposeId constitution) =
    toCanonicalCBOR v (5 :: Word8, purposeId, constitution)
  toCanonicalCBOR v (InfoAction) =
    toCanonicalCBOR v (6 :: Word8, ())

instance FromCanonicalCBOR v (GovAction ConwayEra) where
  fromCanonicalCBOR = do
    l <- decodeListLenCanonical
    tag <- decodeWord8Canonical
    case tag of
      0 | l == 4 -> do
        Versioned purposeId <- fromCanonicalCBOR
        Versioned pparamsUpdate <- fromCanonicalCBOR
        Versioned mScriptHash <- fromCanonicalCBOR
        pure $ Versioned $ ParameterChange purposeId pparamsUpdate mScriptHash
      1 | l == 3 -> do
        Versioned purposeId <- fromCanonicalCBOR
        Versioned protVer <- fromCanonicalCBOR
        pure $ Versioned $ HardForkInitiation purposeId protVer
      2 | l == 3 -> do
        Versioned withdrawals <- fromCanonicalCBOR
        Versioned mScriptHash <- fromCanonicalCBOR
        pure $ Versioned $ TreasuryWithdrawals withdrawals mScriptHash
      3 | l == 2 -> do
        Versioned purposeId <- fromCanonicalCBOR
        pure $ Versioned $ NoConfidence purposeId
      4 | l == 5 -> do
        Versioned purposeId <- fromCanonicalCBOR
        Versioned removedMembers <- fromCanonicalCBOR
        Versioned addedMembers <- fromCanonicalCBOR
        Versioned newThreshold <- fromCanonicalCBOR
        pure $ Versioned $ UpdateCommittee purposeId removedMembers addedMembers newThreshold
      5 | l == 3 -> do
        Versioned purposeId <- fromCanonicalCBOR
        Versioned constitution <- fromCanonicalCBOR
        pure $ Versioned $ NewConstitution purposeId constitution
      6 | l == 2 -> do
        Versioned () <- fromCanonicalCBOR
        pure $ Versioned InfoAction
      _ -> fail $ "Unknown GovAction tag: " ++ show tag

deriving via (LedgerCBOR v (GovPurposeId purpose)) instance ToCanonicalCBOR v (GovPurposeId purpose)

deriving via
  (LedgerCBOR v (GovPurposeId purpose))
  instance
    Typeable purpose => FromCanonicalCBOR v (GovPurposeId purpose)

deriving via (LedgerCBOR v (Vote)) instance ToCanonicalCBOR v (Vote)

deriving via (LedgerCBOR v (Vote)) instance FromCanonicalCBOR v (Vote)

type instance NamespaceKeySize "gov/proposals/v0" = 34

instance KnownNamespace "gov/proposals/v0" where
  type NamespaceKey "gov/proposals/v0" = GovProposalIn
  type NamespaceEntry "gov/proposals/v0" = GovProposalOut

instance CanonicalCBOREntryEncoder "gov/proposals/v0" GovProposalOut where
  encodeEntry n = toCanonicalCBOR (Proxy @"gov/proposals/v0") n

instance CanonicalCBOREntryDecoder "gov/proposals/v0" GovProposalOut where
  decodeEntry = fromCanonicalCBOR
