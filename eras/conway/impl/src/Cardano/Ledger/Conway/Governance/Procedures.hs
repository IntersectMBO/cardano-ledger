{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
  GovActionState (..),
  govActionIdToText,
  indexedGovProps,
  -- Lenses
  pProcDepositL,
  committeeMembersL,
  committeeQuorumL,
  gasDRepVotesL,
  gasStakePoolVotesL,
  gasCommitteeVotesL,
  gasExpiresAfterL,
  govProceduresProposalsL,
  pProcGovActionL,
  gasActionL,
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
  DecShareCBOR (..),
  EncCBOR (..),
  decNoShareCBOR,
  decodeEnumBounded,
  decodeMapByKey,
  decodeNullStrictMaybe,
  encodeEnum,
  encodeListLen,
  encodeNullStrictMaybe,
  encodeWord8,
  invalidKey,
  FromCBOR (..),
  toPlainDecoder,
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
import Cardano.Ledger.Core (Era (..), EraPParams (..), PParamsUpdate, eraProtVerLow)
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
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.OMap.Strict as OMap
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence as Seq
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

toGovActionIdPairs :: (KeyValue e a, Crypto c) => GovActionId c -> [a]
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

data GovActionState era = GovActionState
  { gasId :: !(GovActionId (EraCrypto era))
  , gasCommitteeVotes :: !(Map (Credential 'HotCommitteeRole (EraCrypto era)) Vote)
  , gasDRepVotes :: !(Map (Credential 'DRepRole (EraCrypto era)) Vote)
  , gasStakePoolVotes :: !(Map (KeyHash 'StakePool (EraCrypto era)) Vote)
  , gasDeposit :: !Coin
  , gasReturnAddr :: !(RewardAcnt (EraCrypto era))
  , gasAction :: !(GovAction era)
  , gasProposedIn :: !EpochNo
  , gasExpiresAfter :: !EpochNo
  }
  deriving (Generic)

gasCommitteeVotesL :: Lens' (GovActionState era) (Map (Credential 'HotCommitteeRole (EraCrypto era)) Vote)
gasCommitteeVotesL = lens gasCommitteeVotes (\x y -> x {gasCommitteeVotes = y})

gasDRepVotesL :: Lens' (GovActionState era) (Map (Credential 'DRepRole (EraCrypto era)) Vote)
gasDRepVotesL = lens gasDRepVotes (\x y -> x {gasDRepVotes = y})

gasStakePoolVotesL :: Lens' (GovActionState era) (Map (KeyHash 'StakePool (EraCrypto era)) Vote)
gasStakePoolVotesL = lens gasStakePoolVotes (\x y -> x {gasStakePoolVotes = y})

gasExpiresAfterL :: Lens' (GovActionState era) EpochNo
gasExpiresAfterL = lens gasExpiresAfter $ \x y -> x {gasExpiresAfter = y}

gasActionL :: Lens' (GovActionState era) (GovAction era)
gasActionL = lens gasAction $ \x y -> x {gasAction = y}

instance EraPParams era => ToExpr (GovActionState era)

instance EraPParams era => ToJSON (GovActionState era) where
  toJSON = object . toGovActionStatePairs
  toEncoding = pairs . mconcat . toGovActionStatePairs

toGovActionStatePairs :: (KeyValue e a, EraPParams era) => GovActionState era -> [a]
toGovActionStatePairs gas@(GovActionState _ _ _ _ _ _ _ _ _) =
  let GovActionState {..} = gas
   in [ "actionId" .= gasId
      , "committeeVotes" .= gasCommitteeVotes
      , "dRepVotes" .= gasDRepVotes
      , "stakePoolVotes" .= gasStakePoolVotes
      , "deposit" .= gasDeposit
      , "returnAddr" .= gasReturnAddr
      , "action" .= gasAction
      , "proposedIn" .= gasProposedIn
      , "expiresAfter" .= gasExpiresAfter
      ]

deriving instance EraPParams era => Eq (GovActionState era)

deriving instance EraPParams era => Show (GovActionState era)

instance EraPParams era => NoThunks (GovActionState era)

instance EraPParams era => NFData (GovActionState era)

-- TODO: Implement Sharing: https://github.com/input-output-hk/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (GovActionState era) where
  decShareCBOR _ =
    decode $
      RecD GovActionState
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance EraPParams era => DecCBOR (GovActionState era) where
  decCBOR = decNoShareCBOR

instance EraPParams era => EncCBOR (GovActionState era) where
  encCBOR GovActionState {..} =
    encode $
      Rec GovActionState
        !> To gasId
        !> To gasCommitteeVotes
        !> To gasDRepVotes
        !> To gasStakePoolVotes
        !> To gasDeposit
        !> To gasReturnAddr
        !> To gasAction
        !> To gasProposedIn
        !> To gasExpiresAfter

-- Ref: https://gitlab.haskell.org/ghc/ghc/-/issues/14046
instance
  c ~ EraCrypto era =>
  OMap.HasOKey (GovActionId c) (GovActionState era)
  where
  okeyL = lens gasId $ \gas gi -> gas {gasId = gi}

data Voter c
  = CommitteeVoter !(Credential 'HotCommitteeRole c)
  | DRepVoter !(Credential 'DRepRole c)
  | StakePoolVoter !(KeyHash 'StakePool c)
  deriving (Generic, Eq, Ord, Show)

instance Crypto c => ToJSON (Voter c)

instance Crypto c => ToExpr (Voter c)

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

instance Era era => ToExpr (VotingProcedures era)

instance Era era => DecCBOR (VotingProcedures era) where
  decCBOR =
    fmap VotingProcedures $ decodeMapByKey decCBOR $ \voter -> do
      subMap <- decCBOR
      when (null subMap) $
        fail $
          "VotingProcedures require votes, but Voter: " <> show voter <> " didn't have any"
      pure subMap
  {-# INLINE decCBOR #-}

data VotingProcedure era = VotingProcedure
  { vProcVote :: !Vote
  , vProcAnchor :: !(StrictMaybe (Anchor (EraCrypto era)))
  }
  deriving (Generic, Eq, Show)

instance NoThunks (VotingProcedure era)
instance ToExpr (VotingProcedure era)

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

toVotingProcedurePairs :: (KeyValue e a, EraPParams era) => VotingProcedure era -> [a]
toVotingProcedurePairs vProc@(VotingProcedure _ _) =
  let VotingProcedure {..} = vProc
   in [ "anchor" .= vProcAnchor
      , "decision" .= vProcVote
      ]

data GovProcedures era = GovProcedures
  { gpVotingProcedures :: !(VotingProcedures era)
  , gpProposalProcedures :: !(OSet.OSet (ProposalProcedure era))
  }
  deriving (Eq, Generic)

govProceduresProposalsL :: Lens' (GovProcedures era) (OSet.OSet (ProposalProcedure era))
govProceduresProposalsL = lens gpProposalProcedures $ \x y -> x {gpProposalProcedures = y}

-- | Attaches indices to a sequence of proposal procedures. The indices grow
-- from left to right.
indexedGovProps ::
  Seq.Seq (ProposalProcedure era) ->
  Seq.Seq (GovActionIx, ProposalProcedure era)
indexedGovProps = enumerateProps 0
  where
    enumerateProps _ Seq.Empty = Seq.Empty
    enumerateProps !n (x Seq.:<| xs) = (GovActionIx n, x) Seq.:<| enumerateProps (succ n) xs

instance EraPParams era => NoThunks (GovProcedures era)

instance EraPParams era => NFData (GovProcedures era)

deriving instance EraPParams era => Show (GovProcedures era)

data ProposalProcedure era = ProposalProcedure
  { pProcDeposit :: !Coin
  , pProcReturnAddr :: !(RewardAcnt (EraCrypto era))
  , pProcGovAction :: !(GovAction era)
  , pProcAnchor :: !(Anchor (EraCrypto era))
  }
  deriving (Generic, Eq, Show, Ord)

pProcDepositL :: Lens' (ProposalProcedure era) Coin
pProcDepositL = lens pProcDeposit (\p x -> p {pProcDeposit = x})

pProcGovActionL :: Lens' (ProposalProcedure era) (GovAction era)
pProcGovActionL = lens pProcGovAction $ \x y -> x {pProcGovAction = y}

instance EraPParams era => NoThunks (ProposalProcedure era)

instance EraPParams era => NFData (ProposalProcedure era)

instance EraPParams era => ToExpr (ProposalProcedure era)

instance EraPParams era => DecCBOR (ProposalProcedure era) where
  decCBOR =
    decode $
      RecD ProposalProcedure
        <! From
        <! From
        <! From
        <! From
  {-# INLINE decCBOR #-}

instance EraPParams era => FromCBOR (ProposalProcedure era) where
  fromCBOR = toPlainDecoder (eraProtVerLow @era) decCBOR

instance EraPParams era => EncCBOR (ProposalProcedure era) where
  encCBOR ProposalProcedure {..} =
    encode $
      Rec (ProposalProcedure @era)
        !> To pProcDeposit
        !> To pProcReturnAddr
        !> To pProcGovAction
        !> To pProcAnchor

instance EraPParams era => ToJSON (ProposalProcedure era) where
  toJSON = object . toProposalProcedurePairs
  toEncoding = pairs . mconcat . toProposalProcedurePairs

toProposalProcedurePairs :: (KeyValue e a, EraPParams era) => ProposalProcedure era -> [a]
toProposalProcedurePairs proposalProcedure@(ProposalProcedure _ _ _ _) =
  let ProposalProcedure {..} = proposalProcedure
   in [ "deposit" .= pProcDeposit
      , "returnAddr" .= pProcReturnAddr
      , "govAction" .= pProcGovAction
      , "anchor" .= pProcAnchor
      ]

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

toCommitteePairs :: (KeyValue e a, EraPParams era) => Committee era -> [a]
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

newtype PrevGovActionId (r :: GovActionPurpose) c = PrevGovActionId {unPrevGovActionId :: GovActionId c}
  deriving (Eq, Show, Generic, EncCBOR, DecCBOR, NoThunks, NFData, ToJSON, ToExpr, Ord)

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
      -- | Previous governance action id of `NoConfidence` or `UpdateCommittee` type, which
      -- corresponds to `CommitteePurpose`
      !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
  | UpdateCommittee
      -- | Previous governance action id of `UpdateCommittee` or `NoConfidence` type, which
      -- corresponds to `CommitteePurpose`
      !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
      -- | Constitutional Committe members to be removed
      !(Set (Credential 'ColdCommitteeRole (EraCrypto era)))
      -- | Constitutional committee members to be added
      !(Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
      -- New quorum
      !UnitInterval
  | NewConstitution
      -- | Previous governance action id of `NewConstitution` type, which corresponds to
      -- `ConstitutionPurpose`
      !(StrictMaybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
      !(Constitution era)
  | InfoAction
  deriving (Generic, Ord)

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
      4 -> SumD UpdateCommittee <! D (decodeNullStrictMaybe decCBOR) <! From <! From <! From
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
      UpdateCommittee gid old new q ->
        Sum UpdateCommittee 4 !> E (encodeNullStrictMaybe encCBOR) gid !> To old !> To new !> To q
      NewConstitution gid c ->
        Sum NewConstitution 5 !> E (encodeNullStrictMaybe encCBOR) gid !> To c
      InfoAction ->
        Sum InfoAction 6
