{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Governance.Procedures (
  VotingProcedures (..),
  VotingProcedure (..),
  foldlVotingProcedures,
  foldrVotingProcedures,
  ProposalProcedure (..),
  Anchor (..),
  AnchorData (..),
  Vote (..),
  Voter (..),
  Committee (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovPurposeId (..),
  GovActionPurpose (..),
  ToGovActionPurpose,
  isGovActionWithPurpose,
  GovRelation (..),
  grPParamUpdateL,
  grHardForkL,
  grCommitteeL,
  grConstitutionL,
  hoistGovRelation,
  withGovActionParent,
  GovActionState (..),
  govActionIdToText,
  indexedGovProps,
  Constitution (..),
  constitutionAnchorL,
  constitutionScriptL,
  showGovActionType,
  -- Lenses
  pProcDepositL,
  pProcGovActionL,
  pProcReturnAddrL,
  pProcAnchorL,
  committeeMembersL,
  committeeThresholdL,
  gasDRepVotesL,
  gasStakePoolVotesL,
  gasCommitteeVotesL,
  gasExpiresAfterL,
  gasProposalProcedureL,
  gasActionL,
  gasReturnAddrL,
  gasProposedInL,
  gasIdL,
  gasDepositL,
  gasDeposit,
  gasAction,
  gasReturnAddr,
) where

import Cardano.Crypto.Hash (hashToTextAsHex)
import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.Alonzo.TxBody (Indexable (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (
  Anchor (..),
  AnchorData (..),
  KeyValuePairs (..),
  ProtVer,
  ToKeyValuePairs (..),
  UnitInterval,
  maybeToStrictMaybe,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  FromCBOR (fromCBOR),
  Interns,
  ToCBOR (toCBOR),
  decNoShareCBOR,
  decodeEnumBounded,
  decodeMapByKey,
  decodeNullStrictMaybe,
  decodeRecordNamed,
  decodeRecordNamedT,
  encodeEnum,
  encodeListLen,
  encodeNullStrictMaybe,
  encodeWord8,
  internsFromMap,
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
import Cardano.Ledger.Credential (Credential (..), credToText)
import Cardano.Ledger.Shelley.RewardProvenance ()
import Cardano.Ledger.TxIn (TxId (..))
import Cardano.Slotting.Slot (EpochNo)
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict (get, put)
import Data.Aeson (
  FromJSON (..),
  KeyValue (..),
  ToJSON (..),
  ToJSONKey (..),
  withObject,
  (.:),
  (.:?),
 )
import Data.Aeson.Types (toJSONKeyText)
import Data.Data (Typeable)
import Data.Default
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Unit.Strict (forceElemsToWHNF)
import Data.Word (Word16)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.))
import NoThunks.Class (NoThunks)

newtype GovActionIx = GovActionIx {unGovActionIx :: Word16}
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

data GovActionId = GovActionId
  { gaidTxId :: !TxId
  , gaidGovActionIx :: !GovActionIx
  }
  deriving (Generic, Show, Eq, Ord)
  deriving (ToJSON) via KeyValuePairs GovActionId

instance DecCBOR GovActionId where
  decCBOR =
    decode $
      RecD GovActionId
        <! From
        <! From

instance EncCBOR GovActionId where
  encCBOR GovActionId {..} =
    encode $
      Rec GovActionId
        !> To gaidTxId
        !> To gaidGovActionIx

instance NoThunks GovActionId

instance NFData GovActionId

instance ToKeyValuePairs GovActionId where
  toKeyValuePairs gaid@(GovActionId _ _) =
    let GovActionId {..} = gaid
     in [ "txId" .= gaidTxId
        , "govActionIx" .= gaidGovActionIx
        ]

instance ToJSONKey GovActionId where
  toJSONKey = toJSONKeyText govActionIdToText

govActionIdToText :: GovActionId -> Text.Text
govActionIdToText (GovActionId (TxId txidHash) (GovActionIx ix)) =
  hashToTextAsHex (extractHash txidHash)
    <> Text.pack "#"
    <> Text.pack (show ix)

data GovActionState era = GovActionState
  { gasId :: !GovActionId
  , gasCommitteeVotes :: !(Map (Credential 'HotCommitteeRole) Vote)
  , gasDRepVotes :: !(Map (Credential 'DRepRole) Vote)
  , gasStakePoolVotes :: !(Map (KeyHash 'StakePool) Vote)
  , gasProposalProcedure :: !(ProposalProcedure era)
  , gasProposedIn :: !EpochNo
  , gasExpiresAfter :: !EpochNo
  }
  deriving (Ord, Generic)

gasIdL :: Lens' (GovActionState era) GovActionId
gasIdL = lens gasId $ \x y -> x {gasId = y}

gasCommitteeVotesL ::
  Lens' (GovActionState era) (Map (Credential 'HotCommitteeRole) Vote)
gasCommitteeVotesL = lens gasCommitteeVotes (\x y -> x {gasCommitteeVotes = y})

gasDRepVotesL :: Lens' (GovActionState era) (Map (Credential 'DRepRole) Vote)
gasDRepVotesL = lens gasDRepVotes (\x y -> x {gasDRepVotes = y})

gasStakePoolVotesL :: Lens' (GovActionState era) (Map (KeyHash 'StakePool) Vote)
gasStakePoolVotesL = lens gasStakePoolVotes (\x y -> x {gasStakePoolVotes = y})

gasProposalProcedureL :: Lens' (GovActionState era) (ProposalProcedure era)
gasProposalProcedureL = lens gasProposalProcedure (\x y -> x {gasProposalProcedure = y})

gasDepositL :: Lens' (GovActionState era) Coin
gasDepositL = gasProposalProcedureL . pProcDepositL

gasDeposit :: GovActionState era -> Coin
gasDeposit = pProcDeposit . gasProposalProcedure

gasReturnAddrL :: Lens' (GovActionState era) RewardAccount
gasReturnAddrL = gasProposalProcedureL . pProcReturnAddrL

gasReturnAddr :: GovActionState era -> RewardAccount
gasReturnAddr = pProcReturnAddr . gasProposalProcedure

gasActionL :: Lens' (GovActionState era) (GovAction era)
gasActionL = gasProposalProcedureL . pProcGovActionL

gasAction :: GovActionState era -> GovAction era
gasAction = pProcGovAction . gasProposalProcedure

gasProposedInL :: Lens' (GovActionState era) EpochNo
gasProposedInL = lens gasProposedIn $ \x y -> x {gasProposedIn = y}

gasExpiresAfterL :: Lens' (GovActionState era) EpochNo
gasExpiresAfterL = lens gasExpiresAfter $ \x y -> x {gasExpiresAfter = y}

deriving via
  KeyValuePairs (GovActionState era)
  instance
    EraPParams era => ToJSON (GovActionState era)

instance EraPParams era => ToKeyValuePairs (GovActionState era) where
  toKeyValuePairs gas@(GovActionState _ _ _ _ _ _ _) =
    let GovActionState {..} = gas
     in [ "actionId" .= gasId
        , "committeeVotes" .= gasCommitteeVotes
        , "dRepVotes" .= gasDRepVotes
        , "stakePoolVotes" .= gasStakePoolVotes
        , "proposalProcedure" .= gasProposalProcedure
        , "proposedIn" .= gasProposedIn
        , "expiresAfter" .= gasExpiresAfter
        ]

deriving instance EraPParams era => Eq (GovActionState era)

deriving instance EraPParams era => Show (GovActionState era)

instance EraPParams era => NoThunks (GovActionState era)

instance EraPParams era => NFData (GovActionState era)

instance EraPParams era => DecShareCBOR (GovActionState era) where
  type
    Share (GovActionState era) =
      ( Interns (Credential 'Staking)
      , Interns (KeyHash 'StakePool)
      , Interns (Credential 'DRepRole)
      , Interns (Credential 'HotCommitteeRole)
      )
  decSharePlusCBOR =
    decodeRecordNamedT "GovActionState" (const 7) $ do
      gasId <- lift decCBOR

      (cs, ks, cd, ch) <- get
      gasCommitteeVotes <- lift $ decShareCBOR (ch, mempty)
      gasDRepVotes <- lift $ decShareCBOR (cd, mempty)
      gasStakePoolVotes <- lift $ decShareCBOR (ks, mempty)

      -- DRep votes do not contain any new credentials, thus only additon of interns for SPOs and CCs
      put (cs, ks <> internsFromMap gasStakePoolVotes, cd, ch <> internsFromMap gasCommitteeVotes)

      gasProposalProcedure <- lift decCBOR
      gasProposedIn <- lift decCBOR
      gasExpiresAfter <- lift decCBOR
      pure GovActionState {..}

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
        !> To gasProposalProcedure
        !> To gasProposedIn
        !> To gasExpiresAfter

instance OMap.HasOKey GovActionId (GovActionState era) where
  toOKey = gasId

data Voter
  = CommitteeVoter !(Credential 'HotCommitteeRole)
  | DRepVoter !(Credential 'DRepRole)
  | StakePoolVoter !(KeyHash 'StakePool)
  deriving (Generic, Eq, Ord, Show)

instance ToJSON Voter

instance ToJSONKey Voter where
  toJSONKey = toJSONKeyText $ \case
    CommitteeVoter cred ->
      "committee-" <> credToText cred
    DRepVoter cred ->
      "drep-" <> credToText cred
    StakePoolVoter kh ->
      "stakepool-" <> credToText (KeyHashObj kh)

instance DecCBOR Voter where
  decCBOR = decodeRecordSum "Voter" $ \case
    0 -> (2,) . CommitteeVoter . KeyHashObj <$> decCBOR
    1 -> (2,) . CommitteeVoter . ScriptHashObj <$> decCBOR
    2 -> (2,) . DRepVoter . KeyHashObj <$> decCBOR
    3 -> (2,) . DRepVoter . ScriptHashObj <$> decCBOR
    4 -> (2,) . StakePoolVoter <$> decCBOR
    5 -> fail "Script objects are not allowed for StakePool votes"
    t -> invalidKey t

instance EncCBOR Voter where
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

instance NoThunks Voter

instance NFData Voter

data Vote
  = VoteNo
  | VoteYes
  | Abstain
  deriving (Ord, Generic, Eq, Show, Enum, Bounded)

instance ToJSON Vote

instance NoThunks Vote

instance NFData Vote

instance DecCBOR Vote where
  decCBOR = decodeEnumBounded

instance EncCBOR Vote where
  encCBOR = encodeEnum

newtype VotingProcedures era = VotingProcedures
  { unVotingProcedures :: Map Voter (Map GovActionId (VotingProcedure era))
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

foldlVotingProcedures ::
  -- | Accumulating function
  (c -> Voter -> GovActionId -> VotingProcedure era -> c) ->
  -- | Initial accumulator
  c ->
  -- | Procedures to fold over
  VotingProcedures era ->
  c
foldlVotingProcedures f initAcc =
  let fVotes initVotesAcc voter =
        Map.foldlWithKey' (\acc -> f acc voter) initVotesAcc
   in Map.foldlWithKey' fVotes initAcc . unVotingProcedures

foldrVotingProcedures ::
  -- | Accumulating function
  (Voter -> GovActionId -> VotingProcedure era -> c -> c) ->
  -- | Initial accumulator
  c ->
  -- | Procedures to fold over
  VotingProcedures era ->
  c
foldrVotingProcedures f initAcc =
  let fVotes voter votes acc =
        Map.foldrWithKey' (f voter) acc votes
   in Map.foldrWithKey' fVotes initAcc . unVotingProcedures

deriving instance Indexable Voter (VotingProcedures era)

data VotingProcedure era = VotingProcedure
  { vProcVote :: !Vote
  , vProcAnchor :: !(StrictMaybe Anchor)
  }
  deriving (Generic, Eq, Show)

instance NoThunks (VotingProcedure era)

instance NFData (VotingProcedure era)

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

deriving via
  KeyValuePairs (VotingProcedure era)
  instance
    EraPParams era => ToJSON (VotingProcedure era)

instance EraPParams era => ToKeyValuePairs (VotingProcedure era) where
  toKeyValuePairs vProc@(VotingProcedure _ _) =
    let VotingProcedure {..} = vProc
     in [ "anchor" .= vProcAnchor
        , "decision" .= vProcVote
        ]

-- | Attaches indices to a sequence of proposal procedures. The indices grow
-- from left to right.
indexedGovProps ::
  Seq.Seq (ProposalProcedure era) ->
  Seq.Seq (GovActionIx, ProposalProcedure era)
indexedGovProps = enumerateProps 0
  where
    enumerateProps _ Seq.Empty = Seq.Empty
    enumerateProps !n (x Seq.:<| xs) = (GovActionIx n, x) Seq.:<| enumerateProps (succ n) xs

data ProposalProcedure era = ProposalProcedure
  { pProcDeposit :: !Coin
  , pProcReturnAddr :: !RewardAccount
  , pProcGovAction :: !(GovAction era)
  , pProcAnchor :: !Anchor
  }
  deriving (Generic, Eq, Show, Ord)

pProcDepositL :: Lens' (ProposalProcedure era) Coin
pProcDepositL = lens pProcDeposit (\p x -> p {pProcDeposit = x})

pProcReturnAddrL :: Lens' (ProposalProcedure era) RewardAccount
pProcReturnAddrL = lens pProcReturnAddr (\p x -> p {pProcReturnAddr = x})

pProcGovActionL :: Lens' (ProposalProcedure era) (GovAction era)
pProcGovActionL = lens pProcGovAction $ \x y -> x {pProcGovAction = y}

pProcAnchorL :: Lens' (ProposalProcedure era) Anchor
pProcAnchorL = lens pProcAnchor $ \x y -> x {pProcAnchor = y}

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

deriving via
  KeyValuePairs (ProposalProcedure era)
  instance
    EraPParams era => ToJSON (ProposalProcedure era)

instance EraPParams era => ToKeyValuePairs (ProposalProcedure era) where
  toKeyValuePairs proposalProcedure@(ProposalProcedure _ _ _ _) =
    let ProposalProcedure {..} = proposalProcedure
     in [ "deposit" .= pProcDeposit
        , "returnAddr" .= pProcReturnAddr
        , "govAction" .= pProcGovAction
        , "anchor" .= pProcAnchor
        ]

data Committee era = Committee
  { committeeMembers :: !(Map (Credential 'ColdCommitteeRole) EpochNo)
  -- ^ Committee members with epoch number when each of them expires
  , committeeThreshold :: !UnitInterval
  -- ^ Threshold of the committee that is necessary for a successful vote
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON) via KeyValuePairs (Committee era)

instance Era era => NoThunks (Committee era)

instance Era era => NFData (Committee era)

instance Default (Committee era) where
  def = Committee mempty minBound

committeeMembersL ::
  Lens' (Committee era) (Map (Credential 'ColdCommitteeRole) EpochNo)
committeeMembersL = lens committeeMembers (\c m -> c {committeeMembers = m})

committeeThresholdL :: Lens' (Committee era) UnitInterval
committeeThresholdL = lens committeeThreshold (\c q -> c {committeeThreshold = q})

instance Era era => DecCBOR (Committee era) where
  decCBOR =
    decode $
      RecD Committee
        <! From
        <! From
  {-# INLINE decCBOR #-}

instance Era era => EncCBOR (Committee era) where
  encCBOR Committee {committeeMembers, committeeThreshold} =
    encode $
      Rec (Committee @era)
        !> To committeeMembers
        !> To committeeThreshold

instance Era era => FromJSON (Committee era) where
  parseJSON = withObject "Committee" parseCommittee
    where
      parseCommittee o =
        Committee
          <$> (forceElemsToWHNF <$> o .: "members")
          <*> o .: "threshold"

instance ToKeyValuePairs (Committee era) where
  toKeyValuePairs committee@(Committee _ _) =
    let Committee {..} = committee
     in [ "members" .= committeeMembers
        , "threshold" .= committeeThreshold
        ]

data GovActionPurpose
  = PParamUpdatePurpose
  | HardForkPurpose
  | CommitteePurpose
  | ConstitutionPurpose
  deriving (Eq, Show, Generic)

class ToGovActionPurpose (p :: GovActionPurpose) where
  toGovActionPurpose :: GovActionPurpose

instance ToGovActionPurpose 'PParamUpdatePurpose where
  toGovActionPurpose = PParamUpdatePurpose

instance ToGovActionPurpose 'HardForkPurpose where
  toGovActionPurpose = HardForkPurpose

instance ToGovActionPurpose 'CommitteePurpose where
  toGovActionPurpose = CommitteePurpose

instance ToGovActionPurpose 'ConstitutionPurpose where
  toGovActionPurpose = ConstitutionPurpose

isGovActionWithPurpose :: forall p era. ToGovActionPurpose p => GovAction era -> Bool
isGovActionWithPurpose govAction =
  case govAction of
    ParameterChange {} -> toGovActionPurpose @p == PParamUpdatePurpose
    HardForkInitiation {} -> toGovActionPurpose @p == HardForkPurpose
    TreasuryWithdrawals {} -> False
    NoConfidence {} -> toGovActionPurpose @p == CommitteePurpose
    UpdateCommittee {} -> toGovActionPurpose @p == CommitteePurpose
    NewConstitution {} -> toGovActionPurpose @p == ConstitutionPurpose
    InfoAction -> False

newtype GovPurposeId (p :: GovActionPurpose) = GovPurposeId
  { unGovPurposeId :: GovActionId
  }
  deriving (Eq, Ord, Generic)

type role GovPurposeId nominal

deriving newtype instance EncCBOR (GovPurposeId (p :: GovActionPurpose))

deriving newtype instance
  Typeable p => DecCBOR (GovPurposeId (p :: GovActionPurpose))

deriving newtype instance NoThunks (GovPurposeId (p :: GovActionPurpose))

deriving newtype instance NFData (GovPurposeId (p :: GovActionPurpose))

deriving newtype instance ToJSONKey (GovPurposeId (p :: GovActionPurpose))

deriving newtype instance ToJSON (GovPurposeId (p :: GovActionPurpose))

deriving newtype instance Show (GovPurposeId (p :: GovActionPurpose))

-- | Abstract data type for representing relationship of governance action with the same purpose
data GovRelation (f :: Type -> Type) = GovRelation
  { grPParamUpdate :: !(f (GovPurposeId 'PParamUpdatePurpose))
  , grHardFork :: !(f (GovPurposeId 'HardForkPurpose))
  , grCommittee :: !(f (GovPurposeId 'CommitteePurpose))
  , grConstitution :: !(f (GovPurposeId 'ConstitutionPurpose))
  }
  deriving (Generic)

deriving instance
  (forall p. Eq (f (GovPurposeId (p :: GovActionPurpose)))) =>
  Eq (GovRelation f)

deriving instance
  (forall p. Show (f (GovPurposeId (p :: GovActionPurpose)))) =>
  Show (GovRelation f)

instance
  (forall p. NoThunks (f (GovPurposeId (p :: GovActionPurpose)))) =>
  NoThunks (GovRelation f)

instance
  (forall p. Default (f (GovPurposeId (p :: GovActionPurpose)))) =>
  Default (GovRelation f)

instance
  (forall p. NFData (f (GovPurposeId (p :: GovActionPurpose)))) =>
  NFData (GovRelation f)
  where
  rnf (GovRelation a b c d) = a `deepseq` b `deepseq` c `deepseq` rnf d

instance
  (forall p. Semigroup (f (GovPurposeId (p :: GovActionPurpose)))) =>
  Semigroup (GovRelation f)
  where
  (<>) p1 p2 =
    GovRelation
      { grPParamUpdate = grPParamUpdate p1 <> grPParamUpdate p2
      , grHardFork = grHardFork p1 <> grHardFork p2
      , grCommittee = grCommittee p1 <> grCommittee p2
      , grConstitution = grConstitution p1 <> grConstitution p2
      }

instance
  (forall p. Monoid (f (GovPurposeId (p :: GovActionPurpose)))) =>
  Monoid (GovRelation f)
  where
  mempty =
    GovRelation
      { grPParamUpdate = mempty
      , grHardFork = mempty
      , grCommittee = mempty
      , grConstitution = mempty
      }

instance
  ( Typeable f
  , (forall p. Typeable p => DecCBOR (f (GovPurposeId (p :: GovActionPurpose))))
  ) =>
  DecCBOR (GovRelation f)
  where
  decCBOR =
    decodeRecordNamed
      "GovRelation"
      (const 4)
      (GovRelation <$> decCBOR <*> decCBOR <*> decCBOR <*> decCBOR)

instance
  (forall p. Typeable p => EncCBOR (f (GovPurposeId (p :: GovActionPurpose)))) =>
  EncCBOR (GovRelation f)
  where
  encCBOR govPurpose@(GovRelation _ _ _ _) =
    let GovRelation {..} = govPurpose
     in encodeListLen 4
          <> encCBOR grPParamUpdate
          <> encCBOR grHardFork
          <> encCBOR grCommittee
          <> encCBOR grConstitution

instance
  (forall p. ToJSON (f (GovPurposeId (p :: GovActionPurpose)))) =>
  ToKeyValuePairs (GovRelation f)
  where
  toKeyValuePairs govPurpose@(GovRelation _ _ _ _) =
    let GovRelation {..} = govPurpose
     in [ "PParamUpdate" .= grPParamUpdate
        , "HardFork" .= grHardFork
        , "Committee" .= grCommittee
        , "Constitution" .= grConstitution
        ]

deriving via
  KeyValuePairs (GovRelation f)
  instance
    (forall p. ToJSON (f (GovPurposeId (p :: GovActionPurpose)))) => ToJSON (GovRelation f)

grPParamUpdateL :: Lens' (GovRelation f) (f (GovPurposeId 'PParamUpdatePurpose))
grPParamUpdateL = lens grPParamUpdate $ \x y -> x {grPParamUpdate = y}

grHardForkL :: Lens' (GovRelation f) (f (GovPurposeId 'HardForkPurpose))
grHardForkL = lens grHardFork $ \x y -> x {grHardFork = y}

grCommitteeL :: Lens' (GovRelation f) (f (GovPurposeId 'CommitteePurpose))
grCommitteeL = lens grCommittee $ \x y -> x {grCommittee = y}

grConstitutionL :: Lens' (GovRelation f) (f (GovPurposeId 'ConstitutionPurpose))
grConstitutionL = lens grConstitution $ \x y -> x {grConstitution = y}

hoistGovRelation :: (forall a. f a -> g a) -> GovRelation f -> GovRelation g
hoistGovRelation f gr =
  GovRelation
    { grPParamUpdate = f (grPParamUpdate gr)
    , grHardFork = f (grHardFork gr)
    , grCommittee = f (grCommittee gr)
    , grConstitution = f (grConstitution gr)
    }

-- | Apply a function to a GovAction that can have a parent.
withGovActionParent ::
  GovActionState era ->
  -- | The result to be used for governance actions that can't have a parent
  a ->
  -- | Function that will be applied to a lens and a parent
  ( forall p.
    (forall f. Lens' (GovRelation f) (f (GovPurposeId p))) ->
    StrictMaybe (GovPurposeId p) -> -- GovAction Parent
    GovPurposeId p ->
    a
  ) ->
  a
withGovActionParent gas noParent f =
  case gas ^. gasActionL of
    ParameterChange parent _ _ -> f grPParamUpdateL parent (GovPurposeId (gas ^. gasIdL))
    HardForkInitiation parent _ -> f grHardForkL parent (GovPurposeId (gas ^. gasIdL))
    TreasuryWithdrawals _ _ -> noParent
    NoConfidence parent -> f grCommitteeL parent (GovPurposeId (gas ^. gasIdL))
    UpdateCommittee parent _ _ _ -> f grCommitteeL parent (GovPurposeId (gas ^. gasIdL))
    NewConstitution parent _ -> f grConstitutionL parent (GovPurposeId (gas ^. gasIdL))
    InfoAction -> noParent

-- | Note that the previous governance action id is only optional for the very first
-- governance action of the same purpose.
data GovAction era
  = ParameterChange
      -- | Previous governance action id of `ParameterChange` type, which corresponds to
      -- `PParamUpdatePurpose`.
      !(StrictMaybe (GovPurposeId 'PParamUpdatePurpose))
      -- | Proposed changes to PParams
      !(PParamsUpdate era)
      -- | Policy hash protection
      !(StrictMaybe ScriptHash)
  | HardForkInitiation
      -- | Previous governance action id of `HardForkInitiation` type, which corresponds
      -- to `HardForkPurpose`
      !(StrictMaybe (GovPurposeId 'HardForkPurpose))
      -- | Proposed new protocol version
      !ProtVer
  | TreasuryWithdrawals
      -- | Proposed treasury withdrawals
      !(Map RewardAccount Coin)
      -- | Policy hash protection
      !(StrictMaybe ScriptHash)
  | NoConfidence
      -- | Previous governance action id of `NoConfidence` or `UpdateCommittee` type, which
      -- corresponds to `CommitteePurpose`
      !(StrictMaybe (GovPurposeId 'CommitteePurpose))
  | UpdateCommittee
      -- | Previous governance action id of `UpdateCommittee` or `NoConfidence` type, which
      -- corresponds to `CommitteePurpose`
      !(StrictMaybe (GovPurposeId 'CommitteePurpose))
      -- | Constitutional Committe members to be removed
      !(Set (Credential 'ColdCommitteeRole))
      -- | Constitutional committee members to be added
      !(Map (Credential 'ColdCommitteeRole) EpochNo)
      -- | New Threshold
      !UnitInterval
  | NewConstitution
      -- | Previous governance action id of `NewConstitution` type, which corresponds to
      -- `ConstitutionPurpose`
      !(StrictMaybe (GovPurposeId 'ConstitutionPurpose))
      !(Constitution era)
  | InfoAction
  deriving (Generic, Ord)

showGovActionType :: GovAction era -> String
showGovActionType NewConstitution {} = "NewConstitution"
showGovActionType ParameterChange {} = "ParameterChange"
showGovActionType HardForkInitiation {} = "HardForkInitiation"
showGovActionType TreasuryWithdrawals {} = "TreasuryWithdrawals"
showGovActionType NoConfidence {} = "NoConfidence"
showGovActionType UpdateCommittee {} = "UpdateCommittee"
showGovActionType InfoAction {} = "InfoAction"

deriving instance EraPParams era => Show (GovAction era)

deriving instance EraPParams era => Eq (GovAction era)

instance EraPParams era => NoThunks (GovAction era)

instance EraPParams era => NFData (GovAction era)

instance EraPParams era => ToJSON (GovAction era)

instance EraPParams era => DecCBOR (GovAction era) where
  decCBOR =
    decode $ Summands "GovAction" $ \case
      0 ->
        SumD ParameterChange
          <! D (decodeNullStrictMaybe decCBOR)
          <! From
          <! D (decodeNullStrictMaybe decCBOR)
      1 -> SumD HardForkInitiation <! D (decodeNullStrictMaybe decCBOR) <! From
      2 -> SumD TreasuryWithdrawals <! From <! D (decodeNullStrictMaybe decCBOR)
      3 -> SumD NoConfidence <! D (decodeNullStrictMaybe decCBOR)
      4 -> SumD UpdateCommittee <! D (decodeNullStrictMaybe decCBOR) <! From <! From <! From
      5 -> SumD NewConstitution <! D (decodeNullStrictMaybe decCBOR) <! From
      6 -> SumD InfoAction
      k -> Invalid k
  {-# INLINE decCBOR #-}

instance EraPParams era => EncCBOR (GovAction era) where
  encCBOR =
    encode . \case
      ParameterChange gid ppup pol ->
        Sum ParameterChange 0
          !> E (encodeNullStrictMaybe encCBOR) gid
          !> To ppup
          !> E (encodeNullStrictMaybe encCBOR) pol
      HardForkInitiation gid pv ->
        Sum HardForkInitiation 1 !> E (encodeNullStrictMaybe encCBOR) gid !> To pv
      TreasuryWithdrawals ws pol ->
        Sum TreasuryWithdrawals 2 !> To ws !> E (encodeNullStrictMaybe encCBOR) pol
      NoConfidence gid ->
        Sum NoConfidence 3 !> E (encodeNullStrictMaybe encCBOR) gid
      UpdateCommittee gid old new q ->
        Sum UpdateCommittee 4 !> E (encodeNullStrictMaybe encCBOR) gid !> To old !> To new !> To q
      NewConstitution gid c ->
        Sum NewConstitution 5 !> E (encodeNullStrictMaybe encCBOR) gid !> To c
      InfoAction ->
        Sum InfoAction 6

data Constitution era = Constitution
  { constitutionAnchor :: !Anchor
  , constitutionScript :: !(StrictMaybe ScriptHash)
  }
  deriving (Generic, Ord)
  deriving (ToJSON) via KeyValuePairs (Constitution era)

constitutionAnchorL :: Lens' (Constitution era) Anchor
constitutionAnchorL = lens constitutionAnchor (\x y -> x {constitutionAnchor = y})

constitutionScriptL :: Lens' (Constitution era) (StrictMaybe ScriptHash)
constitutionScriptL = lens constitutionScript (\x y -> x {constitutionScript = y})

instance Era era => FromJSON (Constitution era) where
  parseJSON = withObject "Constitution" $ \o ->
    Constitution
      <$> o .: "anchor"
      <*> (maybeToStrictMaybe <$> (o .:? "script"))

instance ToKeyValuePairs (Constitution era) where
  toKeyValuePairs c@(Constitution _ _) =
    let Constitution {..} = c
     in ["anchor" .= constitutionAnchor]
          <> ["script" .= cScript | SJust cScript <- [constitutionScript]]

deriving instance Eq (Constitution era)

deriving instance Show (Constitution era)

instance Era era => Default (Constitution era) where
  def = Constitution def def

instance Era era => DecCBOR (Constitution era) where
  decCBOR =
    decode $
      RecD Constitution
        <! From
        <! D (decodeNullStrictMaybe decCBOR)

instance Era era => EncCBOR (Constitution era) where
  encCBOR Constitution {..} =
    encode $
      Rec (Constitution @era)
        !> To constitutionAnchor
        !> E (encodeNullStrictMaybe encCBOR) constitutionScript

instance Era era => ToCBOR (Constitution era) where
  toCBOR = toEraCBOR @era

instance Era era => FromCBOR (Constitution era) where
  fromCBOR = fromEraCBOR @era

instance Era era => NFData (Constitution era)

instance Era era => NoThunks (Constitution era)
