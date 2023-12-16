{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Governance.Proposals (
  Proposals,
  proposalsIds,
  proposalsAddVote,
  proposalsAddProposal,
  proposalsActions,
  proposalsRemoveIds,
  proposalsLookupId,
  fromGovActionStateSeq,
  proposalsGovActionStates,
  PrevGovActionIds (..),
  PrevGovActionIdsChildren (..),
  pgacPParamUpdateL,
  pgacHardForkL,
  pgacCommitteeL,
  pgacConstitutionL,
  proposalsRemoveDescendentIds,
  -- Testing
  isConsistent_,
) where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (DecCBOR (..), DecShareCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Conway.Governance.Procedures (
  GovAction (..),
  GovActionId,
  GovActionPurpose (..),
  GovActionState (..),
  PrevGovActionId (PrevGovActionId),
  Vote,
  Voter (..),
  gasActionL,
  gasChildrenL,
  gasCommitteeVotesL,
  gasDRepVotesL,
  gasIdL,
  gasStakePoolVotesL,
 )
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default.Class (Default (..))
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import qualified Data.OMap.Strict as OMap
import Data.Sequence.Strict (StrictSeq (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

newtype Proposals era
  = Proposals
      (OMap.OMap (GovActionId (EraCrypto era)) (GovActionState era))
  deriving newtype (Show, Eq)
  deriving stock (Generic)

instance EraPParams era => ToJSON (Proposals era)

instance EraPParams era => NFData (Proposals era)

instance EraPParams era => NoThunks (Proposals era)

instance Default (Proposals era) where
  def = Proposals def

instance EraPParams era => EncCBOR (Proposals era) where
  encCBOR = encCBOR . proposalsActions

instance EraPParams era => DecCBOR (Proposals era) where
  decCBOR = fromGovActionStateSeq <$> decCBOR

-- TODO: Implement Sharing: https://github.com/intersectmbo/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (Proposals era) where
  decShareCBOR _ = fromGovActionStateSeq <$> decCBOR

-- | Get the sequence of `GovActionState`s
proposalsActions ::
  Proposals era ->
  StrictSeq (GovActionState era)
proposalsActions (Proposals omap) = OMap.toStrictSeq omap

-- | Get the sequence of `GovActionId`s
proposalsIds ::
  Proposals era ->
  StrictSeq (GovActionId (EraCrypto era))
proposalsIds (Proposals omap) = OMap.toStrictSeqOKeys omap

-- | Get the unordered map of `GovActionId`s and `GovActionState`s
proposalsGovActionStates ::
  Proposals era ->
  Map (GovActionId (EraCrypto era)) (GovActionState era)
proposalsGovActionStates (Proposals omap) = OMap.toMap omap

-- | Add a vote to an existing `GovActionState` This is a no-op if the .
-- provided `GovActionId` does not already exist                       .
proposalsAddVote ::
  Voter (EraCrypto era) ->
  Vote ->
  GovActionId (EraCrypto era) ->
  Proposals era ->
  Proposals era
proposalsAddVote voter vote gai (Proposals omap) =
  Proposals $ OMap.adjust updateVote gai omap
  where
    insertVote ::
      Ord k =>
      Lens' (GovActionState era) (Map k Vote) ->
      k ->
      GovActionState era ->
      GovActionState era
    insertVote l k = l %~ Map.insert k vote
    updateVote = case voter of
      DRepVoter c -> insertVote gasDRepVotesL c
      StakePoolVoter kh -> insertVote gasStakePoolVotesL kh
      CommitteeVoter c -> insertVote gasCommitteeVotesL c

-- | Extract `GovActionState`s for the given set of `GovActionId`s from the `Proposals`
proposalsRemoveIds ::
  Set (GovActionId (EraCrypto era)) ->
  Proposals era ->
  (Proposals era, Map.Map (GovActionId (EraCrypto era)) (GovActionState era))
proposalsRemoveIds gais (Proposals omap) =
  let (retained, removed) = OMap.extractKeys gais omap
   in (Proposals retained, removed)

-- | Extract `GovActionState`s for all the descendents of a set of
-- `GovActionId`s from the `Proposals`
proposalsRemoveDescendentIds ::
  Set (GovActionId (EraCrypto era)) ->
  Proposals era ->
  (Proposals era, Map (GovActionId (EraCrypto era)) (GovActionState era))
proposalsRemoveDescendentIds gais ps@(Proposals omap) =
  proposalsRemoveIds (gais <> foldMap (getAllDescendents omap) gais) ps

getAllDescendents ::
  OMap.OMap (GovActionId (EraCrypto era)) (GovActionState era) ->
  GovActionId (EraCrypto era) ->
  Set (GovActionId (EraCrypto era))
getAllDescendents omap gaid =
  case OMap.lookup gaid omap of
    Nothing -> mempty
    Just gas ->
      let children = gasChildren gas
       in children <> foldMap (getAllDescendents omap) children

proposalsLookupId ::
  GovActionId (EraCrypto era) ->
  Proposals era ->
  Maybe (GovActionState era)
proposalsLookupId gai (Proposals omap) = OMap.lookup gai omap

-- | Converts a sequence of `GovActionState`s to a `Proposals`.
--
-- /Warning/ - This function expects `GovActionState`'s to have unique
-- `GovActionId`s, because duplicate Ids will result in `GovActionStates`
-- to be dropped.
fromGovActionStateSeq :: StrictSeq (GovActionState era) -> Proposals era
fromGovActionStateSeq = Proposals . OMap.fromFoldable

-- | Internal function for checking if the invariants are maintained
isConsistent_ :: Proposals era -> Bool
isConsistent_ (Proposals omap) = OMap.invariantHolds' omap

pgacPParamUpdateL ::
  Lens' (PrevGovActionIdsChildren era) (Set (PrevGovActionId 'PParamUpdatePurpose (EraCrypto era)))
pgacPParamUpdateL = lens pgacPParamUpdate $ \x y -> x {pgacPParamUpdate = y}

pgacHardForkL ::
  Lens' (PrevGovActionIdsChildren era) (Set (PrevGovActionId 'HardForkPurpose (EraCrypto era)))
pgacHardForkL = lens pgacHardFork $ \x y -> x {pgacHardFork = y}

pgacCommitteeL ::
  Lens' (PrevGovActionIdsChildren era) (Set (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
pgacCommitteeL = lens pgacCommittee $ \x y -> x {pgacCommittee = y}

pgacConstitutionL ::
  Lens' (PrevGovActionIdsChildren era) (Set (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
pgacConstitutionL = lens pgacConstitution $ \x y -> x {pgacConstitution = y}

data PrevGovActionIdsChildren era = PrevGovActionIdsChildren
  { pgacPParamUpdate :: !(Set (PrevGovActionId 'PParamUpdatePurpose (EraCrypto era)))
  , pgacHardFork :: !(Set (PrevGovActionId 'HardForkPurpose (EraCrypto era)))
  , pgacCommittee :: !(Set (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
  , pgacConstitution :: !(Set (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
  }
  deriving (Show, Eq, Generic)

instance NoThunks (PrevGovActionIdsChildren era)
instance Era era => NFData (PrevGovActionIdsChildren era)
instance Default (PrevGovActionIdsChildren era)

instance Era era => DecCBOR (PrevGovActionIdsChildren era) where
  decCBOR =
    decode $
      RecD PrevGovActionIdsChildren
        <! From
        <! From
        <! From
        <! From

instance Era era => EncCBOR (PrevGovActionIdsChildren era) where
  encCBOR PrevGovActionIdsChildren {..} =
    encode $
      Rec (PrevGovActionIdsChildren @era)
        !> To pgacPParamUpdate
        !> To pgacHardFork
        !> To pgacCommittee
        !> To pgacConstitution

toPrevGovActionIdsChildrenPairs ::
  (KeyValue e a, Era era) => PrevGovActionIdsChildren era -> [a]
toPrevGovActionIdsChildrenPairs pga@(PrevGovActionIdsChildren _ _ _ _) =
  let PrevGovActionIdsChildren {..} = pga
   in [ "pgacPParamUpdate" .= pgacPParamUpdate
      , "pgacHardFork" .= pgacHardFork
      , "pgacCommittee" .= pgacCommittee
      , "pgacConstitution" .= pgacConstitution
      ]

instance Era era => ToJSON (PrevGovActionIdsChildren era) where
  toJSON = object . toPrevGovActionIdsChildrenPairs
  toEncoding = pairs . mconcat . toPrevGovActionIdsChildrenPairs

data PrevGovActionIds era = PrevGovActionIds
  { pgaPParamUpdate :: !(StrictMaybe (PrevGovActionId 'PParamUpdatePurpose (EraCrypto era)))
  -- ^ The last enacted GovActionId for a protocol parameter update
  , pgaHardFork :: !(StrictMaybe (PrevGovActionId 'HardForkPurpose (EraCrypto era)))
  -- ^ The last enacted GovActionId for a hard fork
  , pgaCommittee :: !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
  -- ^ The last enacted GovActionId for a committee change or no confidence vote
  , pgaConstitution :: !(StrictMaybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
  -- ^ The last enacted GovActionId for a new constitution
  }
  deriving (Eq, Show, Generic)

instance NoThunks (PrevGovActionIds era)
instance Era era => NFData (PrevGovActionIds era)
instance Default (PrevGovActionIds era)

instance Era era => DecCBOR (PrevGovActionIds era) where
  decCBOR =
    decode $
      RecD PrevGovActionIds
        <! From
        <! From
        <! From
        <! From

instance Era era => EncCBOR (PrevGovActionIds era) where
  encCBOR PrevGovActionIds {..} =
    encode $
      Rec (PrevGovActionIds @era)
        !> To pgaPParamUpdate
        !> To pgaHardFork
        !> To pgaCommittee
        !> To pgaConstitution

toPrevGovActionIdsPairs :: (KeyValue e a, Era era) => PrevGovActionIds era -> [a]
toPrevGovActionIdsPairs pga@(PrevGovActionIds _ _ _ _) =
  let PrevGovActionIds {..} = pga
   in [ "pgaPParamUpdate" .= pgaPParamUpdate
      , "pgaHardFork" .= pgaHardFork
      , "pgaCommittee" .= pgaCommittee
      , "pgaConstitution" .= pgaConstitution
      ]

instance Era era => ToJSON (PrevGovActionIds era) where
  toJSON = object . toPrevGovActionIdsPairs
  toEncoding = pairs . mconcat . toPrevGovActionIdsPairs

proposalsAddProposal ::
  GovActionState era ->
  PrevGovActionIds era ->
  PrevGovActionIdsChildren era ->
  Proposals era ->
  Maybe (PrevGovActionIdsChildren era, Proposals era)
proposalsAddProposal gas pgai pgac ps@(Proposals omap) =
  let
    parentIsEnacted = addToEnactedPrevGovActionIdsChildren gas pgai pgac
    parentIsAnotherProposal = proposalsAddChild gas ps
   in
    case parentIsEnacted of
      Just updatedChildren ->
        -- Parent is an enacted action
        Just (updatedChildren, Proposals $ omap OMap.||> gas)
      Nothing ->
        case parentIsAnotherProposal of
          -- Parent not found, cannot accept this proposal
          Nothing -> Nothing
          -- Parent is another accepted proposal
          Just (Proposals updatedOMap) ->
            Just (pgac, Proposals $ updatedOMap OMap.||> gas)

-- | Add a child to the already enacted actions. Returns a @`Just`
-- (`PrevGovActionIdsChildren` era)@ with the child added, or `Nothing` if
-- the `PrevGovActionId` is not found.
--
-- NOTE:
-- If both the already enacted action as well as the previous Id
-- referenced by the action being added are `SNothing` then checks pass
addToEnactedPrevGovActionIdsChildren ::
  forall era.
  GovActionState era ->
  PrevGovActionIds era ->
  PrevGovActionIdsChildren era ->
  Maybe (PrevGovActionIdsChildren era)
addToEnactedPrevGovActionIdsChildren gas PrevGovActionIds {..} children =
  case gas ^. gasActionL of
    ParameterChange prev _ ->
      guard (pgaPParamUpdate == prev) $> update pgacPParamUpdateL
    HardForkInitiation prev _ ->
      guard (pgaHardFork == prev) $> update pgacHardForkL
    TreasuryWithdrawals _ -> Just children
    NoConfidence prev ->
      guard (pgaCommittee == prev) $> update pgacCommitteeL
    UpdateCommittee prev _ _ _ ->
      guard (pgaCommittee == prev) $> update pgacCommitteeL
    NewConstitution prev _ ->
      guard (pgaConstitution == prev) $> update pgacConstitutionL
    InfoAction -> Just children
  where
    update ::
      Lens' (PrevGovActionIdsChildren era) (Set.Set (PrevGovActionId p (EraCrypto era))) ->
      PrevGovActionIdsChildren era
    update pgacL =
      let actionId = PrevGovActionId $ gas ^. gasIdL
       in children & pgacL %~ Set.insert actionId

-- | Add a child to an accepted proposal. Return a @`Just (Proposals
-- era)`@ with the child added, or `Nothing` if the `PrevGovActionId` is
-- not found.
proposalsAddChild ::
  forall era.
  GovActionState era ->
  Proposals era ->
  Maybe (Proposals era)
proposalsAddChild gas proposals@(Proposals omap) =
  case gas ^. gasActionL of
    ParameterChange prev _ ->
      updateProposals prev $ \case
        ParameterChange {} -> True
        _ -> False
    HardForkInitiation prev _ ->
      updateProposals prev $ \case
        HardForkInitiation {} -> True
        _ -> False
    TreasuryWithdrawals _ -> Just proposals
    NoConfidence prev ->
      updateProposals prev $ \case
        NoConfidence {} -> True
        UpdateCommittee {} -> True
        _ -> False
    UpdateCommittee prev _ _ _ ->
      updateProposals prev $ \case
        NoConfidence {} -> True
        UpdateCommittee {} -> True
        _ -> False
    NewConstitution prev _ ->
      updateProposals prev $ \case
        NewConstitution {} -> True
        _ -> False
    InfoAction -> Just proposals
  where
    updateProposals ::
      StrictMaybe (PrevGovActionId p (EraCrypto era)) ->
      (GovAction era -> Bool) ->
      Maybe (Proposals era)
    updateProposals mprev cond = do
      SJust (PrevGovActionId prev) <- Just mprev
      parent <- OMap.lookup prev omap
      guard (cond $ parent ^. gasActionL)
        $> ( Proposals $
              OMap.adjust
                (gasChildrenL %~ Set.insert (gas ^. gasIdL))
                prev
                omap
           )
