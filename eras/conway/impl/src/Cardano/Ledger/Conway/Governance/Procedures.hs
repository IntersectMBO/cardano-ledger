{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Governance.Procedures (
  GovProcedures (..),
  VotingProcedures (..),
  VotingProcedure (..),
  ProposalProcedure (..),
  Anchor (..),
  AnchorData (..),
  Vote (..),
  Voter (..),
  Committee (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  PrevGovActionId (..),
  GovActionPurpose (..),
  govActionIdToText,
  indexedGovProps,
  -- Lenses
  pProcDepositL,
  committeeMembersL,
  committeeQuorumL,
  insertVote,
  unionLVotingProcedures,
  emptyVotingProcedures,
  unionLVotingProceduresEither,
  singletonVotingProcedures,
) where

import Cardano.Crypto.Hash (hashToTextAsHex)
import Cardano.Ledger.Address (RewardAcnt)
import Cardano.Ledger.BaseTypes (
  Anchor (..),
  AnchorData (..),
  ProtVer,
  UnitInterval,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeEnumBounded,
  decodeMapByKey,
  decodeNullStrictMaybe,
  encodeEnum,
  encodeListLen,
  encodeNullStrictMaybe,
  encodeWord8,
  invalidKey,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  decodeRecordSum,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (Era (..), EraPParams (..), PParamsUpdate)
import Cardano.Ledger.Credential (Credential (..), credToText)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.SafeHash (extractHash)
import Cardano.Ledger.Shelley.Governance (Constitution)
import Cardano.Ledger.Shelley.RewardProvenance ()
import Cardano.Ledger.TreeDiff (ToExpr)
import Cardano.Ledger.TxIn (TxId (..))
import Cardano.Slotting.Slot (EpochNo)
import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Data.Aeson (
  FromJSON (..),
  KeyValue (..),
  ToJSON (..),
  ToJSONKey (..),
  object,
  pairs,
  withObject,
  (.:),
 )
import Data.Aeson.Types (toJSONKeyText)
import Data.Default.Class
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence (Seq (..))
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Unit.Strict (forceElemsToWHNF)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

newtype GovActionIx = GovActionIx Word32
  deriving
    ( Generic
    , Eq
    , Ord
    , Show
    , NFData
    , NoThunks
    , EncCBOR
    , DecCBOR
    , ToJSON
    )

instance ToExpr GovActionIx

data GovActionId c = GovActionId
  { gaidTxId :: !(TxId c)
  , gaidGovActionIx :: !GovActionIx
  }
  deriving (Generic, Eq, Ord, Show)

instance ToExpr (GovActionId c)

instance Crypto c => DecCBOR (GovActionId c) where
  decCBOR =
    decode $
      RecD GovActionId
        <! From
        <! From

instance Crypto c => EncCBOR (GovActionId c) where
  encCBOR GovActionId {..} =
    encode $
      Rec GovActionId
        !> To gaidTxId
        !> To gaidGovActionIx

instance NoThunks (GovActionId c)

instance Crypto c => NFData (GovActionId c)

instance Crypto c => ToJSON (GovActionId c) where
  toJSON = object . toGovActionIdPairs
  toEncoding = pairs . mconcat . toGovActionIdPairs

toGovActionIdPairs :: (KeyValue a, Crypto c) => GovActionId c -> [a]
toGovActionIdPairs gaid@(GovActionId _ _) =
  let GovActionId {..} = gaid
   in [ "txId" .= gaidTxId
      , "govActionIx" .= gaidGovActionIx
      ]

instance Crypto c => ToJSONKey (GovActionId c) where
  toJSONKey = toJSONKeyText govActionIdToText

govActionIdToText :: GovActionId c -> Text.Text
govActionIdToText (GovActionId (TxId txidHash) (GovActionIx ix)) =
  hashToTextAsHex (extractHash txidHash)
    <> Text.pack "#"
    <> Text.pack (show ix)

data Voter c
  = CommitteeVoter !(Credential 'HotCommitteeRole c)
  | DRepVoter !(Credential 'DRepRole c)
  | StakePoolVoter !(KeyHash 'StakePool c)
  deriving (Generic, Eq, Ord, Show)

instance Crypto c => ToJSON (Voter c)

instance Crypto c => ToJSONKey (Voter c) where
  toJSONKey = toJSONKeyText $ \case
    CommitteeVoter cred ->
      "committee-" <> credToText cred
    DRepVoter cred ->
      "drep-" <> credToText cred
    StakePoolVoter kh ->
      "stakepool-" <> credToText (KeyHashObj kh)

instance Crypto c => DecCBOR (Voter c) where
  decCBOR = decodeRecordSum "Voter" $ \case
    0 -> (2,) . CommitteeVoter . KeyHashObj <$> decCBOR
    1 -> (2,) . CommitteeVoter . ScriptHashObj <$> decCBOR
    2 -> (2,) . DRepVoter . KeyHashObj <$> decCBOR
    3 -> (2,) . DRepVoter . ScriptHashObj <$> decCBOR
    4 -> (2,) . StakePoolVoter <$> decCBOR
    5 -> fail "Script objects are not allowed for StakePool votes"
    t -> invalidKey t

instance Crypto c => EncCBOR (Voter c) where
  encCBOR = \case
    CommitteeVoter (KeyHashObj keyHash) ->
      encodeListLen 2 <> encodeWord8 0 <> encCBOR keyHash
    CommitteeVoter (ScriptHashObj scriptHash) ->
      encodeListLen 2 <> encodeWord8 1 <> encCBOR scriptHash
    DRepVoter (KeyHashObj keyHash) ->
      encodeListLen 2 <> encodeWord8 2 <> encCBOR keyHash
    DRepVoter (ScriptHashObj scriptHash) ->
      encodeListLen 2 <> encodeWord8 3 <> encCBOR scriptHash
    StakePoolVoter keyHash ->
      encodeListLen 2 <> encodeWord8 4 <> encCBOR keyHash

instance NoThunks (Voter c)

instance NFData (Voter c)

data Vote
  = VoteNo
  | VoteYes
  | Abstain
  deriving (Generic, Eq, Show, Enum, Bounded)

instance ToExpr Vote

instance ToJSON Vote

instance NoThunks Vote

instance NFData Vote

instance DecCBOR Vote where
  decCBOR = decodeEnumBounded

instance EncCBOR Vote where
  encCBOR = encodeEnum

newtype VotingProcedures era = VotingProcedures
  { unVotingProcedures ::
      Map (Voter (EraCrypto era)) (Map (GovActionId (EraCrypto era)) (VotingProcedure era))
  }
  deriving stock (Generic, Eq, Show)
  deriving newtype (NoThunks, EncCBOR, ToJSON)

deriving newtype instance Era era => NFData (VotingProcedures era)

instance Era era => DecCBOR (VotingProcedures era) where
  decCBOR =
    fmap VotingProcedures $ decodeMapByKey decCBOR $ \voter -> do
      subMap <- decCBOR
      when (null subMap) $
        fail $
          "VotingProcedures require votes, but Voter: " <> show voter <> " didn't have any"
      pure subMap
  {-# INLINE decCBOR #-}

-- | Construct an empty `VotingProcedures`
emptyVotingProcedures :: VotingProcedures era
emptyVotingProcedures = VotingProcedures Map.empty

-- | Construct a singleton `VotingProcedures`
singletonVotingProcedures ::
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
  VotingProcedure era ->
  VotingProcedures era
singletonVotingProcedures v g p = VotingProcedures $ Map.singleton v $ Map.singleton g p

-- | Insert a vote into `VotingProcedures`. If a `GovActionId` entry is already present in the existing VotingProcedures
-- for the given voter, the associated `VotingProcedure` is overwritten, as expected from an insert operation.
-- A safer version is `unionLVotingProceduresEither` with a `singletonVotingProcedures`.
insertVote ::
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
  VotingProcedure era ->
  VotingProcedures era ->
  VotingProcedures era
insertVote voter gai vp (VotingProcedures vps) =
  VotingProcedures $
    Map.insertWith
      (\_newGaiVp oldGaiVp -> Map.insert gai vp oldGaiVp)
      voter
      (Map.singleton gai vp)
      vps

-- | Left-biased union of `VotingProcedures`, similar to `Map.union`
unionLVotingProcedures :: VotingProcedures era -> VotingProcedures era -> VotingProcedures era
unionLVotingProcedures (VotingProcedures l) (VotingProcedures r) = VotingProcedures $ Map.unionWith Map.union l r

-- | Same as `unionLVotingProcedures`, but returns an `Either (VotingProcedures era) (VotingProcedures era)`
-- where a `Left` returns all the conflicts.
unionLVotingProceduresEither ::
  VotingProcedures era ->
  VotingProcedures era ->
  Either (VotingProcedures era) (VotingProcedures era)
unionLVotingProceduresEither l@(VotingProcedures l') r@(VotingProcedures r') =
  let intersection = Map.intersectionWith Map.intersection l' r'
   in if Map.null intersection
        then Right $ unionLVotingProcedures l r
        else Left $ VotingProcedures intersection

data VotingProcedure era = VotingProcedure
  { vProcVote :: !Vote
  , vProcAnchor :: !(StrictMaybe (Anchor (EraCrypto era)))
  }
  deriving (Generic, Eq, Show)

instance NoThunks (VotingProcedure era)

instance Crypto (EraCrypto era) => NFData (VotingProcedure era)

instance Era era => DecCBOR (VotingProcedure era) where
  decCBOR =
    decode $
      RecD VotingProcedure
        <! From
        <! D (decodeNullStrictMaybe decCBOR)
  {-# INLINE decCBOR #-}

instance Era era => EncCBOR (VotingProcedure era) where
  encCBOR VotingProcedure {..} =
    encode $
      Rec (VotingProcedure @era)
        !> To vProcVote
        !> E (encodeNullStrictMaybe encCBOR) vProcAnchor

instance EraPParams era => ToJSON (VotingProcedure era) where
  toJSON = object . toVotingProcedurePairs
  toEncoding = pairs . mconcat . toVotingProcedurePairs

toVotingProcedurePairs :: (KeyValue a, EraPParams era) => VotingProcedure era -> [a]
toVotingProcedurePairs vProc@(VotingProcedure _ _) =
  let VotingProcedure {..} = vProc
   in [ "anchor" .= vProcAnchor
      , "decision" .= vProcVote
      ]

data GovProcedures era = GovProcedures
  { gpVotingProcedures :: !(VotingProcedures era)
  , gpProposalProcedures :: !(Seq (ProposalProcedure era))
  }
  deriving (Eq, Generic)

-- | Attaches indices to a sequence of proposal procedures. The indices grow
-- from left to right.
indexedGovProps ::
  Seq (ProposalProcedure era) ->
  Seq (GovActionIx, ProposalProcedure era)
indexedGovProps = enumerateProps 0
  where
    enumerateProps _ Empty = Empty
    enumerateProps !n (x :<| xs) = (GovActionIx n, x) :<| enumerateProps (succ n) xs

instance EraPParams era => NoThunks (GovProcedures era)

instance EraPParams era => NFData (GovProcedures era)

deriving instance EraPParams era => Show (GovProcedures era)

data ProposalProcedure era = ProposalProcedure
  { pProcDeposit :: !Coin
  , pProcReturnAddr :: !(RewardAcnt (EraCrypto era))
  , pProcGovAction :: !(GovAction era)
  , pProcAnchor :: !(Anchor (EraCrypto era))
  }
  deriving (Generic, Eq, Show)

pProcDepositL :: Lens' (ProposalProcedure era) Coin
pProcDepositL = lens pProcDeposit (\p x -> p {pProcDeposit = x})

instance EraPParams era => NoThunks (ProposalProcedure era)

instance EraPParams era => NFData (ProposalProcedure era)

instance EraPParams era => DecCBOR (ProposalProcedure era) where
  decCBOR =
    decode $
      RecD ProposalProcedure
        <! From
        <! From
        <! From
        <! From
  {-# INLINE decCBOR #-}

instance EraPParams era => EncCBOR (ProposalProcedure era) where
  encCBOR ProposalProcedure {..} =
    encode $
      Rec (ProposalProcedure @era)
        !> To pProcDeposit
        !> To pProcReturnAddr
        !> To pProcGovAction
        !> To pProcAnchor

data Committee era = Committee
  { committeeMembers :: !(Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
  -- ^ Committee members with epoch number when each of them expires
  , committeeQuorum :: !UnitInterval
  -- ^ Quorum of the committee that is necessary for a successful vote
  }
  deriving (Eq, Show, Generic)

instance ToExpr (Committee era)

instance Era era => NoThunks (Committee era)

instance Era era => NFData (Committee era)

instance Default (Committee era) where
  def = Committee mempty minBound

committeeMembersL :: Lens' (Committee era) (Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
committeeMembersL = lens committeeMembers (\c m -> c {committeeMembers = m})

committeeQuorumL :: Lens' (Committee era) UnitInterval
committeeQuorumL = lens committeeQuorum (\c q -> c {committeeQuorum = q})

instance Era era => DecCBOR (Committee era) where
  decCBOR =
    decode $
      RecD Committee
        <! From
        <! From
  {-# INLINE decCBOR #-}

instance Era era => EncCBOR (Committee era) where
  encCBOR Committee {committeeMembers, committeeQuorum} =
    encode $
      Rec (Committee @era)
        !> To committeeMembers
        !> To committeeQuorum

instance EraPParams era => ToJSON (Committee era) where
  toJSON = object . toCommitteePairs
  toEncoding = pairs . mconcat . toCommitteePairs

instance Era era => FromJSON (Committee era) where
  parseJSON = withObject "Committee" parseCommittee
    where
      parseCommittee o =
        Committee
          <$> (forceElemsToWHNF <$> o .: "members")
          <*> o .: "quorum"

toCommitteePairs :: (KeyValue a, EraPParams era) => Committee era -> [a]
toCommitteePairs committee@(Committee _ _) =
  let Committee {..} = committee
   in [ "members" .= committeeMembers
      , "quorum" .= committeeQuorum
      ]

data GovActionPurpose
  = PParamUpdatePurpose
  | HardForkPurpose
  | CommitteePurpose
  | ConstitutionPurpose
  deriving (Eq, Show)

newtype PrevGovActionId (r :: GovActionPurpose) c = PrevGovActionId (GovActionId c)
  deriving (Eq, Show, Generic, EncCBOR, DecCBOR, NoThunks, NFData, ToJSON, ToExpr)

type role PrevGovActionId nominal nominal

-- | Note that the previous governance action id is only optional for the very first
-- governance action of the same purpose.
data GovAction era
  = ParameterChange
      -- | Previous governance action id of `ParameterChange` type, which corresponds to
      -- `PParamUpdatePurpose`.
      !(StrictMaybe (PrevGovActionId 'PParamUpdatePurpose (EraCrypto era)))
      -- | Proposed changes to PParams
      !(PParamsUpdate era)
  | HardForkInitiation
      -- | Previous governance action id of `HardForkInitiation` type, which corresponds
      -- to `HardForkPurpose`
      !(StrictMaybe (PrevGovActionId 'HardForkPurpose (EraCrypto era)))
      -- | Proposed new protocol version
      !ProtVer
  | TreasuryWithdrawals
      -- | Proposed treasury withdrawals
      !(Map (RewardAcnt (EraCrypto era)) Coin)
  | NoConfidence
      -- | Previous governance action id of `NoConfidence` or `NewCommittee` type, which
      -- corresponds to `CommitteePurpose`
      !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
  | NewCommittee
      -- | Previous governance action id of `NewCommittee` or `NoConfidence` type, which
      -- corresponds to `CommitteePurpose`
      !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
      -- | Old committee
      !(Set (Credential 'ColdCommitteeRole (EraCrypto era)))
      -- | New Committee
      !(Committee era)
  | NewConstitution
      -- | Previous governance action id of `NewConstitution` type, which corresponds to
      -- `ConstitutionPurpose`
      !(StrictMaybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
      !(Constitution era)
  | InfoAction
  deriving (Generic)

instance ToExpr (PParamsHKD StrictMaybe era) => ToExpr (GovAction era)

deriving instance EraPParams era => Eq (GovAction era)

deriving instance EraPParams era => Show (GovAction era)

instance EraPParams era => NoThunks (GovAction era)

instance EraPParams era => NFData (GovAction era)

instance EraPParams era => ToJSON (GovAction era)

instance EraPParams era => DecCBOR (GovAction era) where
  decCBOR =
    decode $ Summands "GovAction" $ \case
      0 -> SumD ParameterChange <! D (decodeNullStrictMaybe decCBOR) <! From
      1 -> SumD HardForkInitiation <! D (decodeNullStrictMaybe decCBOR) <! From
      2 -> SumD TreasuryWithdrawals <! From
      3 -> SumD NoConfidence <! D (decodeNullStrictMaybe decCBOR)
      4 -> SumD NewCommittee <! D (decodeNullStrictMaybe decCBOR) <! From <! From
      5 -> SumD NewConstitution <! D (decodeNullStrictMaybe decCBOR) <! From
      6 -> SumD InfoAction
      k -> Invalid k
  {-# INLINE decCBOR #-}

instance EraPParams era => EncCBOR (GovAction era) where
  encCBOR =
    encode . \case
      ParameterChange gid ppup ->
        Sum ParameterChange 0 !> E (encodeNullStrictMaybe encCBOR) gid !> To ppup
      HardForkInitiation gid pv ->
        Sum HardForkInitiation 1 !> E (encodeNullStrictMaybe encCBOR) gid !> To pv
      TreasuryWithdrawals ws ->
        Sum TreasuryWithdrawals 2 !> To ws
      NoConfidence gid ->
        Sum NoConfidence 3 !> E (encodeNullStrictMaybe encCBOR) gid
      NewCommittee gid old new ->
        Sum NewCommittee 4 !> E (encodeNullStrictMaybe encCBOR) gid !> To old !> To new
      NewConstitution gid c ->
        Sum NewConstitution 5 !> E (encodeNullStrictMaybe encCBOR) gid !> To c
      InfoAction ->
        Sum InfoAction 6
