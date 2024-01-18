{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module isolates all the types and functionality around
-- Governance Proposals.
--
-- It is important to note that there are two sets of state that we
-- maintain around proposals and their enactment. One is processed
-- with transactions (@`Proposals`@) and another at the epoch boundary
-- (@`PrevGovActionIds`@, @`DRepPulser`@). These two sets work together:
-- the incoming proposals and votes are collected continuously in the
-- state that lives with transaction processing, and at each epoch
-- boundary a snapshot of this state is taken to perform pulsing
-- computations on - after it has been adjusted (the proposals are
-- enacted or expired) based on the result of the previous pulsing
-- computaiion.
--
-- Below is a typical timeline of the processing of these states.
--
-- 1. Epoch n: Proposals and votes are continuously collected from
-- incoming transactions into @`Proposals`@
--
-- 2. Epoch n boundary: The @`DRepPulser`@ contains all proposals and
-- votes from epoch (n - 1). Its calculation is completed, ratified
-- and enacted or expired. Ratification and enactment do not affect
-- @`Proposals`@ directly. They only update the @`PrevGovActionIds`@
-- directly and return the sequence of enacted action-ids and the set
-- of expired action-ids that inform us of the changes pending on
-- @`Proposals`@.
--
--   2.1. We take this sequence of enacted action-ids and set of expired
--   action-ids and apply them to the @`Proposals`@ in the ledger
--   state that now includes all the newly collected proposals and
--   votes from epoch n and epoch (n - 1), as this is a superset of
--   the pulsed set of proposals and votes. We do not expect this
--   operation to fail, since all invariants are expected to hold and
--   only an implementation bug could cause this operation to fail.
--   After applying this operation we expect the @`Proposals`@ to be in
--   a state, where (i) all expired actions and their descendents have
--   been pruned, and (ii) the sequence of enacted action-ids have been
--   promoted to be the root of the respective tree and their competing
--   or sibling action-ids and their descendents have been pruned from
--   the @`Proposals`@ tree.
--
--   2.2. The resultant @`Proposals`@ forest has all the latest
--   proposals and votes collected and with enactments and expirations
--   applied to existing ones, so we take a new snapshot to perform
--   pulsing computations on and start the new pulser (@`DRepPulser`@),
--   before entering the new epoch to collect more proposals and votes
--   in @`Proposals`@. Here we trust that the pulser accounts correctly
--   for newly collected votes on proposals from previous epochs that
--   haven't been ratified yet.
--
-- 3. Epoch (n + 1): New proposals and votes are collected from incoming
-- transactions into @`Proposals`@.
--
-- 4. Epoch (n + 1) boundary: The @`DRepPulser`@ now contains all
-- unratified proposals and votes from epoch n. Its calculation
-- is completed, ratified and enacted or expired. This updates
-- @`PrevGovActionIds`@ and gives us a new sequence of enacted
-- action-ids and set of expired actions-ids to apply to the
-- @`Proposals`@, which have been collecting even newer proposals and
-- votes to be a superset of our set of pulsed proposals and votes. And
-- so on...
module Cardano.Ledger.Conway.Governance.Proposals (
  -- * Intended interface to be used for all implementation
  Proposals,
  proposalsIds,
  proposalsActions,
  proposalsSize,
  proposalsAddAction,
  proposalsApplyEnactment,
  proposalsRemoveDescendentIds,
  proposalsAddVote,
  proposalsLookupId,
  proposalsActionsMap,
  PrevGovActionIds (..),
  prevGovActionIdsL,

  -- * To be used only for testing
  pPropsL,
  pRootsL,
  pGraphL,
  mkProposals,
  PForest (..),
  pfPParamUpdateL,
  pfHardForkL,
  pfCommitteeL,
  pfConstitutionL,
  PRoot (..),
  prRootL,
  prChildrenL,
  PNode (..),
  pnChildrenL,
  PGraph (..),
  pGraphNodesL,
  fromPrevGovActionIds,
  toPrevGovActionIds,
  proposalsAreConsistent,
) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  decodeRecordNamed,
  encodeListLen,
 )
import Cardano.Ledger.Conway.Governance.Procedures
import Cardano.Ledger.Core
import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Control.Monad (unless)
import Data.Aeson (KeyValue ((.=)), ToJSON (..), object, pairs)
import Data.Default.Class (Default (..))
import Data.Foldable (foldl')
import Data.Kind (Type)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import qualified Data.OMap.Strict as OMap
import Data.Pulse (foldlM')
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

-- | The root of a single `Proposals` tree. `prRoot` is always expected
-- to be equal to the respective `PrevGovActionId` at the end of every
-- epoch boundary
data PRoot a = PRoot
  { prRoot :: !(StrictMaybe a)
  , prChildren :: !(Set a)
  }
  deriving (Show, Eq, Ord, Generic, NoThunks, NFData, Default)

prRootL :: Lens' (PRoot a) (StrictMaybe a)
prRootL = lens prRoot $ \x y -> x {prRoot = y}

prChildrenL :: Lens' (PRoot a) (Set a)
prChildrenL = lens prChildren $ \x y -> x {prChildren = y}

-- | A non-root node in a `Proposals` tree. `prParent` is expected to be
-- a `Nothing` only at the begining when no governance actions has been
-- enacted yet.
data PNode a = PNode
  { -- This field lacks its lens only because we didn't need one yet
    -- because, in all operations implemented so far, we only traverse
    -- down the tree and not up.
    pnParent :: !(StrictMaybe a)
  , pnChildren :: !(Set a)
  }
  deriving (Show, Eq, Ord, Generic, NoThunks, NFData, Default)

pnChildrenL :: Lens' (PNode a) (Set a)
pnChildrenL = lens pnChildren $ \x y -> x {pnChildren = y}

-- | A single proposal-tree. This map represents all the action-ids that
-- form a tree.
newtype PGraph a = PGraph
  { unPGraph :: Map a (PNode a)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (NoThunks, NFData, Default)

pGraphNodesL :: Lens' (PGraph a) (Map a (PNode a))
pGraphNodesL = lens unPGraph $ \x y -> x {unPGraph = y}

-- | A smart data-type that encapsulates the essence of having 4
-- instance fields of the same parameterized data-structure. This is
-- used to make the 4 proposals-forest roots, the 4 proposals-forest
-- hierarchies and 4 `PrevGovActionIds`.
data PForest (f :: Type -> Type) era = PForest
  { pfPParamUpdate :: !(f (GovPurposeId 'PParamUpdatePurpose era))
  , pfHardFork :: !(f (GovPurposeId 'HardForkPurpose era))
  , pfCommittee :: !(f (GovPurposeId 'CommitteePurpose era))
  , pfConstitution :: !(f (GovPurposeId 'ConstitutionPurpose era))
  }
  deriving (Generic)

deriving instance
  ( Show (f (GovPurposeId 'PParamUpdatePurpose era))
  , Show (f (GovPurposeId 'HardForkPurpose era))
  , Show (f (GovPurposeId 'CommitteePurpose era))
  , Show (f (GovPurposeId 'ConstitutionPurpose era))
  ) =>
  Show (PForest f era)

pfPParamUpdateL :: Lens' (PForest f era) (f (GovPurposeId 'PParamUpdatePurpose era))
pfPParamUpdateL = lens pfPParamUpdate $ \x y -> x {pfPParamUpdate = y}

pfHardForkL :: Lens' (PForest f era) (f (GovPurposeId 'HardForkPurpose era))
pfHardForkL = lens pfHardFork $ \x y -> x {pfHardFork = y}

pfCommitteeL :: Lens' (PForest f era) (f (GovPurposeId 'CommitteePurpose era))
pfCommitteeL = lens pfCommittee $ \x y -> x {pfCommittee = y}

pfConstitutionL :: Lens' (PForest f era) (f (GovPurposeId 'ConstitutionPurpose era))
pfConstitutionL = lens pfConstitution $ \x y -> x {pfConstitution = y}

deriving instance
  (forall p. Eq (f (GovPurposeId (p :: GovActionPurpose) era))) =>
  Eq (PForest f era)
deriving instance
  (forall p. Ord (f (GovPurposeId (p :: GovActionPurpose) era))) =>
  Ord (PForest f era)
deriving instance
  (forall p. NoThunks (f (GovPurposeId (p :: GovActionPurpose) era))) =>
  NoThunks (PForest f era)
deriving instance
  (forall p. NFData (f (GovPurposeId (p :: GovActionPurpose) era))) =>
  NFData (PForest f era)
deriving instance
  (forall p. Default (f (GovPurposeId (p :: GovActionPurpose) era))) =>
  Default (PForest f era)

-- | Self-contained representation of all 4 proposals trees. This forest
-- is made up of only action-ids for nodes - full `GovActionState`s are
-- stored only once in the `OMap`. All functions in this module prefixed
-- with the string @proposals-@ operate on this data-type keeping it
-- consistent.
data Proposals era = Proposals
  { pProps :: !(OMap.OMap (GovActionId (EraCrypto era)) (GovActionState era))
  , pRoots :: !(PForest PRoot era)
  , pGraph :: !(PForest PGraph era)
  }
  deriving (Show, Eq, Generic, NoThunks, NFData, Default)

pPropsL :: Lens' (Proposals era) (OMap.OMap (GovActionId (EraCrypto era)) (GovActionState era))
pPropsL = lens pProps $ \x y -> x {pProps = y}

pRootsL :: Lens' (Proposals era) (PForest PRoot era)
pRootsL = lens pRoots $ \x y -> x {pRoots = y}

pGraphL :: Lens' (Proposals era) (PForest PGraph era)
pGraphL = lens pGraph $ \x y -> x {pGraph = y}

instance EraPParams era => ToJSON (Proposals era) where
  toJSON = toJSON . pProps
  toEncoding = toEncoding . pProps

-- | Add a single @`GovActionState`@ to the @`Proposals`@ forest.
-- The tree to which it is added is picked according to its
-- @`GovActionPurpose`@. Returns `Nothing` when the operation cannot
-- succeed.
proposalsAddAction :: forall era. GovActionState era -> Proposals era -> Maybe (Proposals era)
proposalsAddAction gas ps =
  case gas ^. gasActionL of
    ParameterChange parent _ _ ->
      update pfPParamUpdateL parent
    HardForkInitiation parent _ ->
      update pfHardForkL parent
    TreasuryWithdrawals _ _ -> Just $ ps & pPropsL %~ (OMap.||> gas)
    NoConfidence parent ->
      update pfCommitteeL parent
    UpdateCommittee parent _ _ _ ->
      update pfCommitteeL parent
    NewConstitution parent _ ->
      update pfConstitutionL parent
    InfoAction -> Just $ ps & pPropsL %~ (OMap.||> gas)
  where
    update ::
      (forall f. Lens' (PForest f era) (f (GovPurposeId p era))) ->
      StrictMaybe (GovPurposeId p era) ->
      Maybe (Proposals era)
    update forestL prev
      | prev == ps ^. pRootsL . forestL . prRootL =
          Just $
            ps
              & pPropsL %~ (OMap.||> gas)
              & pRootsL . forestL . prChildrenL %~ Set.insert newId
              & pGraphL . forestL . pGraphNodesL %~ Map.insert newId (newNode prev)
      | SJust parentId <- prev
      , Map.member parentId $ ps ^. pGraphL . forestL . pGraphNodesL =
          Just $
            ps
              & pPropsL %~ (OMap.||> gas)
              & pGraphL . forestL . pGraphNodesL
                %~ ( Map.insert newId (newNode $ SJust parentId)
                      . Map.adjust (pnChildrenL %~ Set.insert newId) parentId
                   )
      | otherwise = Nothing
      where
        newId = GovPurposeId $ gas ^. gasIdL
        newNode parent = PNode parent Set.empty

-- | Internal function for checking if the invariants for @`Proposals`@
-- are maintained
proposalsAreConsistent :: Proposals era -> Bool
proposalsAreConsistent (Proposals omap roots hierarchy) =
  -- OMap internal invariant
  assert (OMap.invariantHolds' omap)
    $ assert
      -- The hierarchies are disjoint by the pigeon-hole principle
      ( let sizeOfUnions = Set.size unionGraph
            sumOfSizes =
              sum
                [ Map.size pparamUpdateH
                , Map.size hardForkH
                , Map.size committeeH
                , Map.size constitutionH
                ]
         in sizeOfUnions == sumOfSizes && sumOfSizes == OMap.size omap
      )
    $ assert
      -- The root-children are disjoint by the pigeon-hole principle
      ( let sizeOfUnions =
              Set.size $
                Set.unions
                  [ Set.map unGovPurposeId pparamUpdateRC
                  , Set.map unGovPurposeId hardForkRC
                  , Set.map unGovPurposeId committeeRC
                  , Set.map unGovPurposeId constitutionRC
                  ]
            sumOfSizes =
              sum
                [ Set.size pparamUpdateRC
                , Set.size hardForkRC
                , Set.size committeeRC
                , Set.size constitutionRC
                ]
         in sizeOfUnions == sumOfSizes
      )
    $ assert
      -- The union of the four hierarchies should be equal to the OMap
      (Map.keysSet (OMap.toMap omap) == unionGraph)
    $ assert
      -- Root-children should be subsets of the hierarchy
      ( and
          [ pparamUpdateRC `Set.isSubsetOf` Map.keysSet pparamUpdateH
          , hardForkRC `Set.isSubsetOf` Map.keysSet hardForkH
          , committeeRC `Set.isSubsetOf` Map.keysSet committeeH
          , constitutionRC `Set.isSubsetOf` Map.keysSet constitutionH
          ]
      )
    $ assert
      -- Children of nodes should be subsets of the hierarchy
      ( and
          [ isSubset pparamUpdateH
          , isSubset hardForkH
          , isSubset committeeH
          , isSubset constitutionH
          ]
      )
      True
  where
    pparamUpdateH = hierarchy ^. pfPParamUpdateL . pGraphNodesL
    hardForkH = hierarchy ^. pfHardForkL . pGraphNodesL
    committeeH = hierarchy ^. pfCommitteeL . pGraphNodesL
    constitutionH = hierarchy ^. pfConstitutionL . pGraphNodesL
    unionGraph =
      Set.unions
        [ Set.map unGovPurposeId $ Map.keysSet pparamUpdateH
        , Set.map unGovPurposeId $ Map.keysSet hardForkH
        , Set.map unGovPurposeId $ Map.keysSet committeeH
        , Set.map unGovPurposeId $ Map.keysSet constitutionH
        ]
    pparamUpdateRC = roots ^. pfPParamUpdateL . prChildrenL
    hardForkRC = roots ^. pfHardForkL . prChildrenL
    committeeRC = roots ^. pfCommitteeL . prChildrenL
    constitutionRC = roots ^. pfConstitutionL . prChildrenL
    isSubset h = and $ flip Set.isSubsetOf (Map.keysSet h) . pnChildren <$> Map.elems h

-- | Reconstruct the @`Proposals`@ forest from an @`OMap`@ of
-- @`GovActionState`@s and the 4 roots (@`PrevGovActionIds`@)
mkProposals ::
  (EraPParams era, MonadFail m) =>
  PrevGovActionIds era ->
  OMap.OMap (GovActionId (EraCrypto era)) (GovActionState era) ->
  m (Proposals era)
mkProposals pgais omap = do
  ps@(Proposals omap' _roots _hierarchy) <-
    foldl'
      ( \mprops gas ->
          maybe (fail "mkProposals: Could not add a proposal") pure
            . proposalsAddAction gas
            =<< mprops
      )
      initialProposals
      omap
  unless (omap == omap') $ fail "mkProposals: OMap is malformed"
  unless (proposalsAreConsistent ps) $ fail "mkProposals: Proposals structure is inconsistent"
  pure ps
  where
    initialProposals = pure $ def & pRootsL .~ fromPrevGovActionIds pgais

instance EraPParams era => EncCBOR (Proposals era) where
  encCBOR ps =
    let roots = toPrevGovActionIds $ ps ^. pRootsL
        props = ps ^. pPropsL
     in encCBOR (roots, props)

instance EraPParams era => DecCBOR (Proposals era) where
  decCBOR = decCBOR >>= uncurry mkProposals

-- TODO: Implement Sharing: https://github.com/intersectmbo/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (Proposals era) where
  decShareCBOR _ = decCBOR

-- | Add a vote to an existing `GovActionState`. This is a no-op if the
-- provided `GovActionId` does not already exist
proposalsAddVote ::
  Voter (EraCrypto era) ->
  Vote ->
  GovActionId (EraCrypto era) ->
  Proposals era ->
  Proposals era
proposalsAddVote voter vote gai (Proposals omap roots hierarchy) =
  Proposals (OMap.adjust updateVote gai omap) roots hierarchy
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

-- | For each action-id in the given set. attempt to remove it from
-- the @`Proposals`@ forest based on its purpose. Although the removal
-- operations are applied to parts of the forest without any checks, we
-- cover them in property-tests
proposalsRemoveIds ::
  forall era.
  Set (GovActionId (EraCrypto era)) ->
  Proposals era ->
  (Proposals era, Map.Map (GovActionId (EraCrypto era)) (GovActionState era))
proposalsRemoveIds gais ps =
  let (retainedOMap, removedFromOMap) = OMap.extractKeys gais $ ps ^. pPropsL
      (roots, hierarchy) = foldl' removeEach (ps ^. pRootsL, ps ^. pGraphL) gais
   in (Proposals retainedOMap roots hierarchy, removedFromOMap)
  where
    removeEach accum@(!roots, !hierarchy) gai =
      case OMap.lookup gai $ ps ^. pPropsL of
        Nothing -> accum
        Just gas ->
          case gas ^. gasActionL of
            ParameterChange parent _ _ -> remove pfPParamUpdateL parent
            HardForkInitiation parent _ -> remove pfHardForkL parent
            TreasuryWithdrawals _ _ -> accum
            NoConfidence parent -> remove pfCommitteeL parent
            UpdateCommittee parent _ _ _ -> remove pfCommitteeL parent
            NewConstitution parent _ -> remove pfConstitutionL parent
            InfoAction -> accum
      where
        remove ::
          (forall f. Lens' (PForest f era) (f (GovPurposeId p era))) ->
          StrictMaybe (GovPurposeId p era) ->
          (PForest PRoot era, PForest PGraph era)
        remove forestL parent =
          let gpi = GovPurposeId gai
           in ( roots & forestL . prChildrenL %~ Set.delete gpi
              , hierarchy
                  & forestL . pGraphNodesL %~ Map.delete gpi
                  & case parent of
                    SNothing -> id
                    SJust parentGpi ->
                      forestL . pGraphNodesL %~ Map.adjust (pnChildrenL %~ Set.delete gpi) parentGpi
              )

-- | Get all the descendents of an action-id from the @`Proposals`@ forest
getAllDescendents ::
  forall era.
  Proposals era ->
  GovActionId (EraCrypto era) ->
  Set (GovActionId (EraCrypto era))
getAllDescendents (Proposals omap _roots hierarchy) gai = case OMap.lookup gai omap of
  Nothing -> Set.empty
  Just gas -> case gas ^. gasActionL of
    ParameterChange {} -> collectDescendents pfPParamUpdateL
    HardForkInitiation {} -> collectDescendents pfHardForkL
    TreasuryWithdrawals {} -> Set.empty
    NoConfidence {} -> collectDescendents pfCommitteeL
    UpdateCommittee {} -> collectDescendents pfCommitteeL
    NewConstitution {} -> collectDescendents pfConstitutionL
    InfoAction -> Set.empty
  where
    collectDescendents ::
      Lens' (PForest PGraph era) (PGraph (GovPurposeId p era)) ->
      Set (GovActionId (EraCrypto era))
    collectDescendents graphL = Set.map unGovPurposeId $ go graphL $ GovPurposeId gai
    go ::
      forall p.
      Lens' (PForest PGraph era) (PGraph (GovPurposeId p era)) ->
      GovPurposeId p era ->
      Set (GovPurposeId p era)
    go graphL gpi =
      case Map.lookup gpi $ hierarchy ^. graphL . pGraphNodesL of
        -- Impossible! getAllDescendents: GovPurposeId not found
        Nothing -> assert False mempty
        Just (PNode _parent children) -> children <> foldMap (go graphL) children

-- | Remove the set of given action-ids with their descendents from the
-- @`Proposals`@ forest
proposalsRemoveDescendentIds ::
  Set (GovActionId (EraCrypto era)) ->
  Proposals era ->
  (Proposals era, Map (GovActionId (EraCrypto era)) (GovActionState era))
proposalsRemoveDescendentIds gais ps =
  proposalsRemoveIds (gais <> foldMap (getAllDescendents ps) gais) ps

-- | For use in the @`EPOCH`@ rule. Apply the result of
-- @`extractDRepPulsingState`@ to the @`Proposals`@ forest, so that:
--   i. all the expired action-ids and their descendents are removed,
--   and
--   ii. the sequence of enacted action-ids is promoted to the root,
--   removing competing/sibling action-ids and their descendents at each
--   step
proposalsApplyEnactment ::
  forall era.
  Seq (GovActionId (EraCrypto era)) ->
  Set (GovActionId (EraCrypto era)) ->
  Proposals era ->
  ( Proposals era
  , Map (GovActionId (EraCrypto era)) (GovActionState era) -- Removed due to enactment
  , Map (GovActionId (EraCrypto era)) (GovActionState era) -- Removed due to expiry
  )
proposalsApplyEnactment enactedGais expiredGais props =
  let (unexpiredProposals, expiredRemoved) = proposalsRemoveDescendentIds expiredGais props
      (enactedProposalsState, enactedRemoved) =
        case foldlM' enact (unexpiredProposals, Map.empty) enactedGais of
          Nothing -> assert False (props, mempty)
          Just result -> result
   in (enactedProposalsState, enactedRemoved, expiredRemoved)
  where
    enact (!ps, !removed) gai = do
      gas <- proposalsLookupId gai ps
      case gas ^. gasActionL of
        ParameterChange parent _ _ -> enactFromRoot pfPParamUpdateL parent
        HardForkInitiation parent _ -> enactFromRoot pfHardForkL parent
        TreasuryWithdrawals _ _ -> Just enactWithoutRoot
        NoConfidence parent -> enactFromRoot pfCommitteeL parent
        UpdateCommittee parent _ _ _ -> enactFromRoot pfCommitteeL parent
        NewConstitution parent _ -> enactFromRoot pfConstitutionL parent
        InfoAction -> Just enactWithoutRoot
      where
        enactWithoutRoot ::
          ( Proposals era
          , Map (GovActionId (EraCrypto era)) (GovActionState era)
          )
        enactWithoutRoot =
          let (newOMap, removedActions) = OMap.extractKeys (Set.singleton gai) $ ps ^. pPropsL
           in (ps & pPropsL .~ newOMap, removed `Map.union` removedActions)
        enactFromRoot ::
          (forall f. Lens' (PForest f era) (f (GovPurposeId p era))) ->
          StrictMaybe (GovPurposeId p era) ->
          Maybe
            ( Proposals era
            , Map (GovActionId (EraCrypto era)) (GovActionState era)
            )
        enactFromRoot forestL parent = do
          let gpi = GovPurposeId gai
              siblings = Set.delete gai $ Set.map unGovPurposeId (ps ^. pRootsL . forestL . prChildrenL)
              (withoutSiblings, removedActions) = proposalsRemoveDescendentIds siblings ps
              newGraph = Map.delete gpi $ withoutSiblings ^. pGraphL . forestL . pGraphNodesL
              (newOMap, enactedAction) = OMap.extractKeys (Set.singleton gai) $ withoutSiblings ^. pPropsL
          newRootNode <- Map.lookup gpi $ ps ^. pGraphL . forestL . pGraphNodesL
          assert
            (ps ^. pRootsL . forestL . prRootL == parent)
            Just
            ( withoutSiblings
                & pGraphL . forestL . pGraphNodesL .~ newGraph
                & pRootsL . forestL . prRootL .~ SJust gpi -- Set the new root
                & pRootsL . forestL . prChildrenL .~ pnChildren newRootNode -- Set the new root children
                & pPropsL .~ newOMap
            , removed `Map.union` removedActions `Map.union` enactedAction
            )

-- | Get the sequence of `GovActionState`s
proposalsActions ::
  Proposals era ->
  StrictSeq (GovActionState era)
proposalsActions (Proposals omap _ _) = OMap.toStrictSeq omap

-- | Get the sequence of `GovActionId`s
proposalsIds ::
  Proposals era ->
  StrictSeq (GovActionId (EraCrypto era))
proposalsIds (Proposals omap _ _) = OMap.toStrictSeqOKeys omap

-- | Get the unordered map of `GovActionId`s and `GovActionState`s
proposalsActionsMap ::
  Proposals era ->
  Map (GovActionId (EraCrypto era)) (GovActionState era)
proposalsActionsMap (Proposals omap _ _) = OMap.toMap omap

proposalsSize :: Proposals era -> Int
proposalsSize (Proposals omap _ _) = OMap.size omap

proposalsLookupId ::
  GovActionId (EraCrypto era) ->
  Proposals era ->
  Maybe (GovActionState era)
proposalsLookupId gai (Proposals omap _ _) = OMap.lookup gai omap

-- | Represents the @`GovActionId`@ of the last actions enacted on the
-- previous epoch boundary in the @`EnactState`@
newtype PrevGovActionIds era = PrevGovActionIds
  { unPrevGovActionIds :: PForest StrictMaybe era
  }
  deriving (Show, Eq, Generic)

prevGovActionIdsL :: Lens' (PrevGovActionIds era) (PForest StrictMaybe era)
prevGovActionIdsL = lens unPrevGovActionIds $ \_x y -> PrevGovActionIds y

instance Era era => NoThunks (PrevGovActionIds era)
instance Era era => NFData (PrevGovActionIds era)
instance Era era => Default (PrevGovActionIds era)

instance Era era => DecCBOR (PrevGovActionIds era) where
  decCBOR =
    PrevGovActionIds
      <$> decodeRecordNamed
        "PrevGovActionIds"
        (const 4)
        (PForest <$> decCBOR <*> decCBOR <*> decCBOR <*> decCBOR)

instance Era era => EncCBOR (PrevGovActionIds era) where
  encCBOR (PrevGovActionIds pforest@(PForest _ _ _ _)) =
    let PForest {..} = pforest
     in encodeListLen 4
          <> encCBOR pfPParamUpdate
          <> encCBOR pfHardFork
          <> encCBOR pfCommittee
          <> encCBOR pfConstitution

toPrevGovActionIdsPairs :: (Era era, KeyValue e a) => PrevGovActionIds era -> [a]
toPrevGovActionIdsPairs (PrevGovActionIds pforest@(PForest _ _ _ _)) =
  let PForest {..} = pforest
   in [ "EnactedPParamUpdate" .= pfPParamUpdate
      , "EnactedHardFork" .= pfHardFork
      , "EnactedCommittee" .= pfCommittee
      , "EnactedConstitution" .= pfConstitution
      ]

instance Era era => ToJSON (PrevGovActionIds era) where
  toJSON = object . toPrevGovActionIdsPairs
  toEncoding = pairs . mconcat . toPrevGovActionIdsPairs

toPrevGovActionIds :: Era era => PForest PRoot era -> PrevGovActionIds era
toPrevGovActionIds pforest@(PForest _ _ _ _) =
  let PForest {..} = pforest
   in def
        & prevGovActionIdsL . pfPParamUpdateL .~ (pfPParamUpdate ^. prRootL)
        & prevGovActionIdsL . pfHardForkL .~ (pfHardFork ^. prRootL)
        & prevGovActionIdsL . pfCommitteeL .~ (pfCommittee ^. prRootL)
        & prevGovActionIdsL . pfConstitutionL .~ (pfConstitution ^. prRootL)

fromPrevGovActionIds :: PrevGovActionIds era -> PForest PRoot era
fromPrevGovActionIds (PrevGovActionIds pforest@(PForest _ _ _ _)) =
  let PForest {..} = pforest
   in def
        & pfPParamUpdateL . prRootL .~ pfPParamUpdate
        & pfHardForkL . prRootL .~ pfHardFork
        & pfCommitteeL . prRootL .~ pfCommittee
        & pfConstitutionL . prRootL .~ pfConstitution
