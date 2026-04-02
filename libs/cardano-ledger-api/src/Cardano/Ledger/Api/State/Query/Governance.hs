{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Ledger.Api.State.Query.Governance (
  -- * Committee members state
  QueryResultCommitteeMemberState (..),
  QueryResultCommitteeMembersState (..),
  HotCredAuthStatus (..),
  MemberStatus (..),
  NextEpochChange (..),
) where

import Cardano.Ledger.BaseTypes (Anchor, KeyValuePairs (..), ToKeyValuePairs (..), UnitInterval)
import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  EncCBOR (encCBOR),
  decodeEnumBounded,
  encodeEnum,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), (.=))
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

data MemberStatus
  = -- | Votes of this member will count during ratification
    Active
  | Expired
  | -- | This can happen when a hot credential for an unknown cold credential
    -- exists. Such Committee member will be either removed from the state at
    -- the next epoch boundary or enacted as a new member.
    Unrecognized
  deriving (Show, Eq, Enum, Bounded, Generic, Ord, ToJSON)

instance NFData MemberStatus

instance NoThunks MemberStatus

instance EncCBOR MemberStatus where
  encCBOR = encodeEnum

instance DecCBOR MemberStatus where
  decCBOR = decodeEnumBounded

data HotCredAuthStatus
  = MemberAuthorized (Credential HotCommitteeRole)
  | -- | Member enacted, but no hot credential for voting has been registered
    MemberNotAuthorized
  | MemberResigned (Maybe Anchor)
  deriving (Show, Eq, Generic, Ord, ToJSON)

instance NFData HotCredAuthStatus

instance NoThunks HotCredAuthStatus

instance EncCBOR HotCredAuthStatus where
  encCBOR =
    encode . \case
      MemberAuthorized cred -> Sum MemberAuthorized 0 !> To cred
      MemberNotAuthorized -> Sum MemberNotAuthorized 1
      MemberResigned anchor -> Sum MemberResigned 2 !> To anchor

instance DecCBOR HotCredAuthStatus where
  decCBOR =
    decode $ Summands "HotCredAuthStatus" $ \case
      0 -> SumD MemberAuthorized <! From
      1 -> SumD MemberNotAuthorized
      2 -> SumD MemberResigned <! From
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

instance NFData NextEpochChange

instance NoThunks NextEpochChange

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

-- | Per-member committee state including authorization, member status,
-- and expiration.
data QueryResultCommitteeMemberState = QueryResultCommitteeMemberState
  { qrcmsHotCredAuthStatus :: !HotCredAuthStatus
  , qrcmsStatus :: !MemberStatus
  , qrcmsExpiration :: !(Maybe EpochNo)
  -- ^ Absolute epoch number when the member expires
  , qrcmsNextEpochChange :: !NextEpochChange
  -- ^ Changes to the member at the next epoch
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via KeyValuePairs QueryResultCommitteeMemberState

deriving instance Ord QueryResultCommitteeMemberState

instance NFData QueryResultCommitteeMemberState

instance NoThunks QueryResultCommitteeMemberState

instance EncCBOR QueryResultCommitteeMemberState where
  encCBOR (QueryResultCommitteeMemberState hotCredAuth status expiration nextEpoch) =
    encode $
      Rec QueryResultCommitteeMemberState
        !> To hotCredAuth
        !> To status
        !> To expiration
        !> To nextEpoch

instance DecCBOR QueryResultCommitteeMemberState where
  decCBOR =
    decode $
      RecD QueryResultCommitteeMemberState
        <! From
        <! From
        <! From
        <! From

instance ToKeyValuePairs QueryResultCommitteeMemberState where
  toKeyValuePairs (QueryResultCommitteeMemberState hotCredAuth status expiration nextEpoch) =
    [ "hotCredsAuthStatus" .= hotCredAuth
    , "status" .= status
    , "expiration" .= expiration
    , "nextEpochChange" .= nextEpoch
    ]

-- | Committee members state with the committee map, threshold, and
-- current epoch.
data QueryResultCommitteeMembersState = QueryResultCommitteeMembersState
  { qrcmsCommittee :: !(Map (Credential ColdCommitteeRole) QueryResultCommitteeMemberState)
  , qrcmsThreshold :: !(Maybe UnitInterval)
  , qrcmsEpochNo :: !EpochNo
  -- ^ Current epoch number. This is necessary to interpret committee
  -- member states
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON) via KeyValuePairs QueryResultCommitteeMembersState

deriving instance Ord QueryResultCommitteeMembersState

instance NFData QueryResultCommitteeMembersState

instance NoThunks QueryResultCommitteeMembersState

instance EncCBOR QueryResultCommitteeMembersState where
  encCBOR (QueryResultCommitteeMembersState committee threshold epoch) =
    encode $
      Rec QueryResultCommitteeMembersState
        !> To committee
        !> To threshold
        !> To epoch

instance DecCBOR QueryResultCommitteeMembersState where
  decCBOR =
    decode $
      RecD QueryResultCommitteeMembersState
        <! From
        <! From
        <! From

instance ToKeyValuePairs QueryResultCommitteeMembersState where
  toKeyValuePairs (QueryResultCommitteeMembersState committee threshold epoch) =
    [ "committee" .= committee
    , "threshold" .= threshold
    , "epoch" .= epoch
    ]
