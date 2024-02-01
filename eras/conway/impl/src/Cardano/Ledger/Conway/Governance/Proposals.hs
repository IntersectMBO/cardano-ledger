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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
--   a state, where (i) all expired actions and their descendants have
--   been pruned, and (ii) the sequence of enacted action-ids have been
--   promoted to be the root of the respective tree and their competing
--   or sibling action-ids and their descendants have been pruned from
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
  proposalsAddVote,
  proposalsLookupId,
  proposalsActionsMap,
  toPrevGovActionIds,
  fromPrevGovActionIds,

  -- * To be used only for testing
  proposalsRemoveWithDescendants,
  TreeMaybe (..),
  toGovRelationTree,
  toGovRelationTreeEither,
  pPropsL,
  pRootsL,
  pGraphL,
  mkProposals,
  PRoot (..),
  prRootL,
  prChildrenL,
  PEdges (..),
  peChildrenL,
  PGraph (..),
  pGraphNodesL,
) where

import Cardano.Ledger.BaseTypes (
  StrictMaybe (..),
  isSJust,
  isSNothing,
  strictMaybe,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Conway.Governance.Procedures
import Cardano.Ledger.Core
import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Control.Monad (unless)
import Data.Aeson (ToJSON (..))
import Data.Default.Class (Default (..))
import Data.Either (partitionEithers)
import Data.Foldable (foldl', foldrM, toList)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import qualified Data.OMap.Strict as OMap
import qualified Data.OSet.Strict as OSet
import Data.Pulse (foldlM')
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq (..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Tree
import GHC.Generics (Generic)
import GHC.Stack
import Lens.Micro
import NoThunks.Class (NoThunks)

-- | The root of a single `Proposals` tree. `prRoot` is always expected
-- to be equal to the respective `PrevGovActionId` at the end of every
-- epoch boundary
data PRoot a = PRoot
  { prRoot :: !(StrictMaybe a)
  , prChildren :: !(Set a)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks, NFData, Default)

-- | A non-root edges in a `Proposals` tree. `peParent` is expected to be
-- a `SNothing` only at the begining when no governance actions has been
-- enacted yet.
data PEdges a = PEdges
  { peParent :: !(StrictMaybe a)
  , peChildren :: !(Set a)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks, NFData, Default)

-- | A single proposal-tree. This map represents all the action-ids that
-- form a tree.
newtype PGraph a = PGraph
  { unPGraph :: Map a (PEdges a)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (NoThunks, NFData, Default)

prRootL :: Lens' (PRoot a) (StrictMaybe a)
prRootL = lens prRoot $ \x y -> x {prRoot = y}

prChildrenL :: Lens' (PRoot a) (Set a)
prChildrenL = lens prChildren $ \x y -> x {prChildren = y}

peChildrenL :: Lens' (PEdges a) (Set a)
peChildrenL = lens peChildren $ \x y -> x {peChildren = y}

pGraphNodesL :: Lens' (PGraph a) (Map a (PEdges a))
pGraphNodesL = lens unPGraph $ \x y -> x {unPGraph = y}

-- | Self-contained representation of all 4 proposals trees. This forest
-- is made up of only action-ids for nodes - full `GovActionState`s are
-- stored only once in the `OMap`. All functions in this module prefixed
-- with the string @proposals-@ operate on this data-type keeping it
-- consistent.
--
-- NOTE: The correct way to think about this data-structure is similar
-- to 4 of the following, one for each @`GovActionPurpose`@
--
-- @
--   data Tree a = Node (StrictMaybe a) [Tree a]
-- @
--
-- but because this does not allow us to look-up a node's edges in
-- predictable time, we use a map from nodes to their edges (parent and
-- children) to capture the graph (@`PGraph`@). We also need to always
-- know the roots of the 4 trees, and those we store in the @`PRoot`@
--
-- NOTE: At the end of an epoch boundary, we expect @`pRoots`@ to be the same
-- as the @`PrevGovActionIds`@ from the @`EnactState`@
data Proposals era = Proposals
  { pProps :: !(OMap.OMap (GovActionId (EraCrypto era)) (GovActionState era))
  , pRoots :: !(GovRelation PRoot era)
  , pGraph :: !(GovRelation PGraph era)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoThunks, NFData, Default)

pPropsL :: Lens' (Proposals era) (OMap.OMap (GovActionId (EraCrypto era)) (GovActionState era))
pPropsL = lens pProps $ \x y -> x {pProps = y}

pRootsL :: Lens' (Proposals era) (GovRelation PRoot era)
pRootsL = lens pRoots $ \x y -> x {pRoots = y}

pGraphL :: Lens' (Proposals era) (GovRelation PGraph era)
pGraphL = lens pGraph $ \x y -> x {pGraph = y}

instance EraPParams era => ToJSON (Proposals era) where
  toJSON = toJSON . pProps
  toEncoding = toEncoding . pProps

-- | Add a single @`GovActionState`@ to the @`Proposals`@ forest.
-- The tree to which it is added is picked according to its
-- @`GovActionPurpose`@. Returns `Nothing` when the operation cannot
-- succeed.
proposalsAddAction ::
  forall era.
  (EraPParams era, HasCallStack) =>
  GovActionState era ->
  Proposals era ->
  Maybe (Proposals era)
proposalsAddAction gas ps = withGovActionParent gas (Just psWithGas) update
  where
    psWithGas = ps & pPropsL %~ (OMap.||> gas)
    -- Append a new GovActionState to the Proposals and then add it to the set of children
    -- for its parent as well as initiate an empty lineage for this new child.
    update ::
      HasCallStack =>
      (forall f. Lens' (GovRelation f era) (f (GovPurposeId p era))) ->
      StrictMaybe (GovPurposeId p era) ->
      GovPurposeId p era ->
      Maybe (Proposals era)
    update govRelationL parent newId
      | parent == ps ^. pRootsL . govRelationL . prRootL =
          Just $
            checkInvariantAfterAddition gas ps $
              psWithGas
                & pRootsL . govRelationL . prChildrenL %~ Set.insert newId
                & pGraphL . govRelationL . pGraphNodesL %~ Map.insert newId (PEdges parent Set.empty)
      | SJust parentId <- parent
      , Map.member parentId $ ps ^. pGraphL . govRelationL . pGraphNodesL =
          Just $
            checkInvariantAfterAddition gas ps $
              psWithGas
                & pGraphL . govRelationL . pGraphNodesL
                  %~ ( Map.insert newId (PEdges (SJust parentId) Set.empty)
                        . Map.adjust (peChildrenL %~ Set.insert newId) parentId
                     )
      | otherwise = Nothing

-- | Reconstruct the @`Proposals`@ forest from an @`OMap`@ of
-- @`GovActionState`@s and the 4 roots (@`PrevGovActionIds`@)
mkProposals ::
  (EraPParams era, MonadFail m) =>
  GovRelation StrictMaybe era ->
  OMap.OMap (GovActionId (EraCrypto era)) (GovActionState era) ->
  m (Proposals era)
mkProposals pgais omap = do
  ps@(Proposals omap' _roots _hierarchy) <-
    foldlM'
      ( \props gas ->
          case proposalsAddAction gas props of
            Nothing -> fail $ "mkProposals: Could not add a proposal" <> show (gas ^. gasIdL)
            Just props' -> pure props'
      )
      initialProposals
      omap
  unless (omap == omap') $ fail "mkProposals: OMap is malformed"
  pure ps
  where
    initialProposals = def & pRootsL .~ fromPrevGovActionIds pgais

instance EraPParams era => EncCBOR (Proposals era) where
  encCBOR ps =
    let roots = toPrevGovActionIds $ ps ^. pRootsL
     in encCBOR (roots, ps ^. pPropsL)

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
  EraPParams era =>
  Set (GovActionId (EraCrypto era)) ->
  Proposals era ->
  (Proposals era, Map.Map (GovActionId (EraCrypto era)) (GovActionState era))
proposalsRemoveIds gais ps =
  let (retainedOMap, removedFromOMap) = OMap.extractKeys gais $ ps ^. pPropsL
      (roots, hierarchy) = foldl' removeEach (ps ^. pRootsL, ps ^. pGraphL) removedFromOMap
   in (checkInvariantAfterDeletion gais ps $ Proposals retainedOMap roots hierarchy, removedFromOMap)
  where
    removeEach accum@(!roots, !graph) gas =
      withGovActionParent gas accum $ \govRelationL parent gpi ->
        if parent == roots ^. govRelationL . prRootL
          then
            ( roots & govRelationL . prChildrenL %~ Set.delete gpi
            , graph & govRelationL . pGraphNodesL %~ Map.delete gpi
            )
          else
            ( roots
            , graph
                & govRelationL . pGraphNodesL %~ Map.delete gpi
                & case parent of
                  SNothing -> assert False id
                  SJust parentGpi ->
                    govRelationL . pGraphNodesL %~ Map.adjust (peChildrenL %~ Set.delete gpi) parentGpi
            )

-- | Remove the set of given action-ids with their descendants from the
-- @`Proposals`@ forest. Cannot be used for removing enacted GovActionIds (i.e. roots)
proposalsRemoveWithDescendants ::
  EraPParams era =>
  Set (GovActionId (EraCrypto era)) ->
  Proposals era ->
  (Proposals era, Map (GovActionId (EraCrypto era)) (GovActionState era))
proposalsRemoveWithDescendants gais ps@(Proposals omap _roots graph) =
  proposalsRemoveIds (gais <> foldMap getAllDescendants gais) ps
  where
    -- Recursively aet all of the descendants for those actions that have lineage
    getAllDescendants gai =
      case OMap.lookup gai omap of
        Nothing -> assert False mempty
        Just gas -> withGovActionParent gas mempty $ \govRelationL _ ->
          let go acc gpi =
                case Map.lookup gpi $ graph ^. govRelationL . pGraphNodesL of
                  Nothing -> assert False acc
                  Just (PEdges _parent children) ->
                    foldl' go (Set.map unGovPurposeId children <> acc) children
           in go mempty

-- | For use in the @`EPOCH`@ rule. Apply the result of
-- @`extractDRepPulsingState`@ to the @`Proposals`@ forest, so that:
--   i. all the expired action-ids and their descendants are removed,
--   and
--   ii. the sequence of enacted action-ids is promoted to the root,
--   removing competing/sibling action-ids and their descendants at each
--   step
proposalsApplyEnactment ::
  forall era.
  EraPParams era =>
  Seq (GovActionState era) ->
  Set (GovActionId (EraCrypto era)) ->
  Proposals era ->
  ( Proposals era
  , Map (GovActionId (EraCrypto era)) (GovActionState era) -- Removed due to enactment
  , Map (GovActionId (EraCrypto era)) (GovActionState era) -- Removed due to expiry
  )
proposalsApplyEnactment enactedGass expiredGais props =
  let (unexpiredProposals, expiredRemoved) = proposalsRemoveWithDescendants expiredGais props
      (enactedProposalsState, enactedRemoved) =
        foldl' enact (unexpiredProposals, Map.empty) enactedGass
   in (enactedProposalsState, enactedRemoved, expiredRemoved)
  where
    enact (!ps, !removed) gas = withGovActionParent gas enactWithoutRoot enactFromRoot
      where
        gai = gas ^. gasIdL
        enactWithoutRoot ::
          ( Proposals era
          , Map (GovActionId (EraCrypto era)) (GovActionState era)
          )
        enactWithoutRoot =
          let (newOMap, removedActions) = OMap.extractKeys (Set.singleton gai) $ ps ^. pPropsL
           in (ps & pPropsL .~ newOMap, removed `Map.union` removedActions)
        enactFromRoot ::
          (forall f. Lens' (GovRelation f era) (f (GovPurposeId p era))) ->
          StrictMaybe (GovPurposeId p era) ->
          GovPurposeId p era ->
          ( Proposals era
          , Map (GovActionId (EraCrypto era)) (GovActionState era)
          )
        enactFromRoot govRelationL parent gpi =
          let siblings =
                Set.delete gai $
                  Set.map unGovPurposeId (ps ^. pRootsL . govRelationL . prChildrenL)
              newRootChildren =
                case Map.lookup gpi $ ps ^. pGraphL . govRelationL . pGraphNodesL of
                  Nothing -> assert False Set.empty
                  Just pe -> peChildren pe
              (withoutSiblings, removedActions) = proposalsRemoveWithDescendants siblings ps
              newGraph = Map.delete gpi $ withoutSiblings ^. pGraphL . govRelationL . pGraphNodesL
              (newOMap, enactedAction) =
                OMap.extractKeys (Set.singleton gai) $ withoutSiblings ^. pPropsL
              newProposals =
                withoutSiblings
                  & pRootsL . govRelationL . prRootL .~ SJust gpi -- Set the new root
                  & pRootsL . govRelationL . prChildrenL .~ newRootChildren
                  & pGraphL . govRelationL . pGraphNodesL .~ newGraph
                  & pPropsL .~ newOMap
           in assert
                (ps ^. pRootsL . govRelationL . prRootL == parent)
                ( checkInvariantAfterDeletion (Set.singleton gai) withoutSiblings newProposals
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

toPrevGovActionIds :: GovRelation PRoot era -> GovRelation StrictMaybe era
toPrevGovActionIds = hoistGovRelation prRoot

fromPrevGovActionIds :: GovRelation StrictMaybe era -> GovRelation PRoot era
fromPrevGovActionIds = hoistGovRelation (`PRoot` Set.empty)

---------------------
-- Debugging tools --
---------------------

-- | Wraper type, which serves as a composition of @`Tree` . `StrictMaybe`@
--
-- Also its Show instance will print a nice tree structure.
data TreeMaybe a = TreeMaybe {unTreeMaybe :: Tree (StrictMaybe a)}
  deriving (Eq)

instance Show (TreeMaybe (GovPurposeId p era)) where
  show = ("\n" <>) . drawTree . fmap showGovPurposeId . unTreeMaybe
    where
      showGovPurposeId = \case
        SNothing -> "x"
        SJust (GovPurposeId govActionId) -> T.unpack (govActionIdToText govActionId)

-- | Partial version of `toGovRelationTreeEither`
toGovRelationTree :: (Era era, HasCallStack) => Proposals era -> GovRelation TreeMaybe era
toGovRelationTree = either error id . toGovRelationTreeEither

-- | Convert `Proposals` into a valid `Tree`
toGovRelationTreeEither :: Era era => Proposals era -> Either String (GovRelation TreeMaybe era)
toGovRelationTreeEither Proposals {pProps, pRoots, pGraph} = do
  unless (OMap.invariantHolds' pProps) $ Left "OMap invariant is violated"

  let propsMap = OMap.toMap pProps
      childParentRelation = toChildParentRelation propsMap

  (grPParamUpdate, nodesPParamUpdate) <-
    toPTree (grPParamUpdate childParentRelation) (grPParamUpdate pRoots) (grPParamUpdate pGraph)
  (grHardFork, nodesHardFork) <-
    toPTree (grHardFork childParentRelation) (grHardFork pRoots) (grHardFork pGraph)
  (grCommittee, nodesCommittee) <-
    toPTree (grCommittee childParentRelation) (grCommittee pRoots) (grCommittee pGraph)
  (grConstitution, nodesConstitution) <-
    toPTree (grConstitution childParentRelation) (grConstitution pRoots) (grConstitution pGraph)

  let allNodes =
        Set.unions
          [ Set.map unGovPurposeId nodesPParamUpdate
          , Set.map unGovPurposeId nodesHardFork
          , Set.map unGovPurposeId nodesCommittee
          , Set.map unGovPurposeId nodesConstitution
          ]
      guardUnknown = do
        let unknown = allNodes Set.\\ Map.keysSet propsMap
        unless (null unknown) $ do
          Left $ "Discovered unrecognized nodes: " ++ show unknown
      guardUnique = do
        let sumSizes =
              sum
                [ Set.size nodesPParamUpdate
                , Set.size nodesHardFork
                , Set.size nodesCommittee
                , Set.size nodesConstitution
                ]
        unless (Set.size allNodes == sumSizes) $ do
          Left $
            "Duplicate govActionIds found between different purposes: "
              ++ show (sumSizes - Set.size allNodes)
  guardUnknown
  guardUnique
  pure GovRelation {grPParamUpdate, grHardFork, grCommittee, grConstitution}

-- | Mapping from a child to a parent that was specified in the GovAction.
newtype ChildParent a = ChildParent (Map a (StrictMaybe a))
  deriving stock (Show)
  deriving newtype (Eq, Semigroup, Monoid)

toChildParentRelation ::
  Foldable f =>
  f (GovActionState era) ->
  GovRelation ChildParent era
toChildParentRelation = foldMap toChildParent
  where
    toChildParent gas =
      withGovActionParent gas mempty $ \govRelationL parent _ ->
        mempty & govRelationL .~ ChildParent (Map.singleton (GovPurposeId (gas ^. gasIdL)) parent)

toPTree ::
  (Ord a, Show a) =>
  ChildParent a ->
  PRoot a ->
  PGraph a ->
  Either String (TreeMaybe a, Set a)
toPTree (ChildParent childParent) root (PGraph fullGraph) = do
  (_, tree) <- nodeToTree (prRoot root) (prChildren root) fullGraph
  nodesList <-
    case partitionEithers $ map (strictMaybe (Left ()) Right) $ toList tree of
      (roots, nodes)
        | isSNothing (prRoot root) && null roots ->
            Left $ "Expected an empty root, but it was not found in the Tree"
        | isSJust (prRoot root) && not (null roots) ->
            Left $ "Expected a full root, but got " ++ show (length roots) ++ " Nothing cases"
        | otherwise -> pure nodes
  let nodes = Set.fromList nodesList
      nodesWithoutRoot = strictMaybe nodes (`Set.delete` nodes) (prRoot root)
      unreachable = Map.withoutKeys fullGraph nodesWithoutRoot
  unless (Set.size nodes == length nodesList) $ do
    Left $ "Detected duplicate nodes: " ++ show (fst $ OSet.fromFoldableDuplicates nodesList)
  unless (Map.null unreachable) $ do
    Left $ "Discovered unreachable nodes in the graph: " ++ show unreachable
  pure (TreeMaybe tree, nodesWithoutRoot)
  where
    nodeToTree node children graph = do
      (graph', subTrees) <- foldrM (childToTree node) (graph, []) children
      pure (graph', Node node subTrees)
    childToTree parent child (!graph, !acc) =
      case Map.lookup child graph of
        Nothing -> Left $ "Cannot find the node: " ++ show child
        Just edges -> do
          unless (peParent edges == parent) $
            Left $
              "Incorrect parent: "
                ++ show (peParent edges)
                ++ " listed for the node: "
                ++ show child
          case Map.lookup child childParent of
            Nothing -> Left $ "Node is not found in the governance states map: " ++ show child
            Just trueParent
              | trueParent /= parent ->
                  Left $
                    "Parent of "
                      ++ show child
                      ++ " specified in the GovAction: "
                      ++ show trueParent
                      ++ " does not match the one on the Graph: "
                      ++ show parent
              | otherwise -> pure ()
          (graph', !subTree) <-
            -- Deleting the child from the graph ensures that every node except the root
            -- appears exactly once in the graph.
            nodeToTree (SJust child) (peChildren edges) (Map.delete child graph)
          pure (graph', subTree : acc)

-- | Verify invariant after addition of GovActionState to Proposals. Will print the state
-- before the invariant is violated.
--
-- /Note/ - runs only when assertions are turned on.
checkInvariantAfterAddition ::
  (EraPParams era, HasCallStack) =>
  -- | GovAction that was added
  GovActionState era ->
  -- | Proposals before adding the GovActionState
  Proposals era ->
  -- | Proposals after adding the GovActionState
  Proposals era ->
  Proposals era
checkInvariantAfterAddition gas psPre ps = assert check ps
  where
    check =
      case toGovRelationTreeEither ps of
        Left err -> error $ "Addition error: " ++ err ++ "\n" ++ show gas ++ "\n" ++ show psPre
        Right _ -> True

-- | Verify invariant after deletion of GovActionState to Proposals. Will print the state
-- before the invariant is violated.
--
-- /Note/ - runs only when assertions are turned on.
checkInvariantAfterDeletion ::
  (EraPParams era, HasCallStack) =>
  -- | GovAction that was added
  Set (GovActionId (EraCrypto era)) ->
  -- | Proposals before adding the GovActionState
  Proposals era ->
  -- | Proposals after adding the GovActionState
  Proposals era ->
  Proposals era
checkInvariantAfterDeletion gais psPre ps = assert check ps
  where
    check =
      case toGovRelationTreeEither ps of
        Left err -> error $ "Deletion error: " ++ err ++ "\n" ++ show gais ++ "\n" ++ show psPre
        Right _ -> True
