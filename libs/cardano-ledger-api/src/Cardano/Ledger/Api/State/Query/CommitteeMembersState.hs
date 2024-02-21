{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Api.State.Query.CommitteeMembersState (
  CommitteeMemberState (..),
  CommitteeMembersState (..),
  HotCredAuthStatus (..),
  MemberStatus (..),
  NextEpochChange (..),
) where

import Cardano.Ledger.BaseTypes (UnitInterval)
import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  EncCBOR (encCBOR),
  decodeEnumBounded,
  encodeEnum,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (
  KeyRole (..),
 )
import Cardano.Ledger.Slot (
  EpochNo (..),
 )
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Map.Strict (Map)
import GHC.Generics (Generic)

data MemberStatus
  = -- Votes of this member will count during ratification
    Active
  | Expired
  | -- | This can happen when a hot credential for an unknown cold credential exists.
    -- Such Committee member will be either removed from the state at the next
    -- epoch boundary or enacted as a new member.
    Unrecognized
  deriving (Show, Eq, Enum, Bounded, Generic, Ord, ToJSON)

instance EncCBOR MemberStatus where
  encCBOR = encodeEnum

instance DecCBOR MemberStatus where
  decCBOR = decodeEnumBounded

data HotCredAuthStatus c
  = MemberAuthorized (Credential 'HotCommitteeRole c)
  | -- | Member enacted, but no hot credential for voting has been registered
    MemberNotAuthorized
  | MemberResigned
  deriving (Show, Eq, Generic, Ord, ToJSON)

instance Crypto c => EncCBOR (HotCredAuthStatus c) where
  encCBOR =
    encode . \case
      MemberAuthorized cred -> Sum MemberAuthorized 0 !> To cred
      MemberNotAuthorized -> Sum MemberNotAuthorized 1
      MemberResigned -> Sum MemberResigned 2

instance Crypto c => DecCBOR (HotCredAuthStatus c) where
  decCBOR =
    decode $ Summands "HotCredAuthStatus" $ \case
      0 -> SumD MemberAuthorized <! From
      1 -> SumD MemberNotAuthorized
      2 -> SumD MemberResigned
      k -> Invalid k

data NextEpochChange
  = --- | Member not enacted yet, but will be at the next epoch
    ToBeEnacted
  | -- | Member will be removed
    ToBeRemoved
  | NoChangeExpected
  | ToBeExpired
  | TermAdjusted EpochNo
  deriving (Show, Eq, Generic, Ord, ToJSON)

instance EncCBOR NextEpochChange where
  encCBOR =
    encode . \case
      ToBeEnacted -> Sum ToBeEnacted 0
      ToBeRemoved -> Sum ToBeRemoved 1
      NoChangeExpected -> Sum NoChangeExpected 2
      ToBeExpired -> Sum ToBeExpired 3
      TermAdjusted e -> Sum TermAdjusted 4 !> To e

instance DecCBOR NextEpochChange where
  decCBOR =
    decode $ Summands "NextEpochChange" $ \case
      0 -> SumD ToBeEnacted
      1 -> SumD ToBeRemoved
      2 -> SumD NoChangeExpected
      3 -> SumD ToBeExpired
      4 -> SumD TermAdjusted <! From
      k -> Invalid k

data CommitteeMemberState c = CommitteeMemberState
  { cmsHotCredAuthStatus :: !(HotCredAuthStatus c)
  , cmsStatus :: !MemberStatus
  , cmsExpiration :: !(Maybe EpochNo)
  -- ^ Absolute epoch number when the member expires
  , cmsNextEpochChange :: !NextEpochChange
  -- ^ Changes to the member at the next epoch
  }
  deriving (Show, Eq, Generic)

deriving instance Ord (CommitteeMemberState c)

instance Crypto c => EncCBOR (CommitteeMemberState c) where
  encCBOR (CommitteeMemberState cStatus mStatus ex nec) =
    encode $
      Rec (CommitteeMemberState @c)
        !> To cStatus
        !> To mStatus
        !> To ex
        !> To nec

instance Crypto c => DecCBOR (CommitteeMemberState c) where
  decCBOR =
    decode $
      RecD CommitteeMemberState
        <! From
        <! From
        <! From
        <! From

instance Crypto c => ToJSON (CommitteeMemberState c) where
  toJSON = object . toCommitteeMemberStatePairs
  toEncoding = pairs . mconcat . toCommitteeMemberStatePairs

toCommitteeMemberStatePairs :: (Crypto c, KeyValue e a) => CommitteeMemberState c -> [a]
toCommitteeMemberStatePairs c@(CommitteeMemberState _ _ _ _) =
  let CommitteeMemberState {..} = c
   in [ "hotCredsAuthStatus" .= cmsHotCredAuthStatus
      , "status" .= cmsStatus
      , "expiration" .= cmsExpiration
      , "nextEpochChange" .= cmsNextEpochChange
      ]

data CommitteeMembersState c = CommitteeMembersState
  { csCommittee :: !(Map (Credential 'ColdCommitteeRole c) (CommitteeMemberState c))
  , csThreshold :: !(Maybe UnitInterval)
  , csEpochNo :: !EpochNo
  -- ^ Current epoch number. This is necessary to interpret committee member states
  }
  deriving (Eq, Show, Generic)

deriving instance Ord (CommitteeMembersState c)

instance Crypto c => EncCBOR (CommitteeMembersState c) where
  encCBOR c@(CommitteeMembersState _ _ _) =
    let CommitteeMembersState {..} = c
     in encode $
          Rec (CommitteeMembersState @c)
            !> To csCommittee
            !> To csThreshold
            !> To csEpochNo

instance Crypto c => DecCBOR (CommitteeMembersState c) where
  decCBOR =
    decode $
      RecD CommitteeMembersState
        <! From
        <! From
        <! From

instance Crypto c => ToJSON (CommitteeMembersState c) where
  toJSON = object . toCommitteeMembersStatePairs
  toEncoding = pairs . mconcat . toCommitteeMembersStatePairs

toCommitteeMembersStatePairs :: (KeyValue e a, Crypto c) => CommitteeMembersState c -> [a]
toCommitteeMembersStatePairs c@(CommitteeMembersState _ _ _) =
  let CommitteeMembersState {..} = c
   in [ "committee" .= csCommittee
      , "threshold" .= csThreshold
      , "epoch" .= csEpochNo
      ]
