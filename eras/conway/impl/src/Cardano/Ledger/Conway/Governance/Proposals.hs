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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- FIXME: @aniketd: Document this entire module very clearly

-- | This module isolates all the types and functionality around
-- Governance Proposals.
--
-- It is important to note that there are two sets of state that we
-- maintain around proposals and their enactment. One is processed
-- with transactions (@`Proposals`@) and another at the epoch boundary
-- (@`PrevGovActionIds`@, @`DRepPulser`@, @`Proposals`@).
--
-- Below is an outline of a typical timeline of the processing of these
-- states.
--
-- 1. Epoch n: Proposals and votes are continuously collected from
-- incoming transactions into @`Proposals`@
--
-- 2. Epoch n boundary: The @`DRepPulser`@ contains all proposals and
-- votes from epoch (n - 1). Its calculation is completed, ratified
-- and enacted. Ratification and enactment do not affect @`Proposals`@
-- directly. They only update the @`PrevGovActionIds`@ directly and
-- return the sequence of enacted action-ids and the set of expired
-- action-ids that inform us of the changes pending to @`Proposals`@
--
-- 3. FIXME: @aniketd: complete this task
module Cardano.Ledger.Conway.Governance.Proposals (
  Proposals,
  pPropsL,
  pRootsL,
  pHierarchyL,
  proposalsIds,
  proposalsActions,
  proposalsSize,
  proposalsAddAction,
  proposalsApplyEnactment,
  proposalsRemoveDescendentIds,
  proposalsShowDebug,
  proposalsAddVote,
  proposalsLookupId,
  proposalsGovActionStates,
  mkProposals,
  PForest (..),
  pfPParamUpdateL,
  pfHardForkL,
  pfCommitteeL,
  pfConstitutionL,
  pfrPParamUpdateL,
  pfrHardForkL,
  pfrCommitteeL,
  pfrConstitutionL,
  pfhPParamUpdateL,
  pfhHardForkL,
  pfhCommitteeL,
  pfhConstitutionL,
  PRoot (..),
  prRootL,
  prChildrenL,
  PNode (..),
  pnChildrenL,
  PHierarchy (..),
  pHierarchyNTL,
  PrevGovActionIds (..),
  prevGovActionIdsL,
  fromPrevGovActionIds,
  toPrevGovActionIds,
  -- Testing
  isConsistent_,
) where

import Cardano.Ledger.BaseTypes (
  StrictMaybe (..),
  maybeToStrictMaybe,
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  decodeRecordNamed,
  encodeListLen,
 )
import Cardano.Ledger.Binary.Coders (Decode (From, RecD), Encode (Rec, To), decode, encode, (!>), (<!))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance.Procedures
import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Control.Monad (guard)
import Data.Aeson (KeyValue ((.=)), ToJSON (..), object, pairs)
import Data.Default.Class (Default (..))
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.Kind (Type)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe (isNothing)
import qualified Data.OMap.Strict as OMap
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

data PRoot a = PRoot
  { prRoot :: !(Maybe a)
  , prChildren :: !(Set a)
  }
  deriving (Show, Eq, Ord, Generic, NoThunks, NFData, Default)

prRootL :: Lens' (PRoot a) (Maybe a)
prRootL = lens prRoot $ \x y -> x {prRoot = y}

prChildrenL :: Lens' (PRoot a) (Set a)
prChildrenL = lens prChildren $ \x y -> x {prChildren = y}

data PNode a = PNode
  { pnParent :: !(Maybe a)
  , pnChildren :: !(Set a)
  }
  deriving (Show, Eq, Ord, Generic, NoThunks, NFData, Default)

pnChildrenL :: Lens' (PNode a) (Set a)
pnChildrenL = lens pnChildren $ \x y -> x {pnChildren = y}

newtype PHierarchy a = PHierarchy
  { unPHierarchy :: Map a (PNode a)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (NoThunks, NFData, Default)

pHierarchyNTL :: Lens' (PHierarchy a) (Map a (PNode a)) -- FIXME: what a terrible name!
pHierarchyNTL = lens unPHierarchy $ \x y -> x {unPHierarchy = y}

data PForest (f :: Type -> Type) era = PForest
  { pfPParamUpdate :: !(f (GovPurposeId 'PParamUpdatePurpose era))
  , pfHardFork :: !(f (GovPurposeId 'HardForkPurpose era))
  , pfCommittee :: !(f (GovPurposeId 'CommitteePurpose era))
  , pfConstitution :: !(f (GovPurposeId 'ConstitutionPurpose era))
  }

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

pfrPParamUpdateL :: Lens' (PForest PRoot era) (PRoot (GovPurposeId 'PParamUpdatePurpose era))
pfrPParamUpdateL = lens pfPParamUpdate $ \x y -> x {pfPParamUpdate = y}

pfrHardForkL :: Lens' (PForest PRoot era) (PRoot (GovPurposeId 'HardForkPurpose era))
pfrHardForkL = lens pfHardFork $ \x y -> x {pfHardFork = y}

pfrCommitteeL :: Lens' (PForest PRoot era) (PRoot (GovPurposeId 'CommitteePurpose era))
pfrCommitteeL = lens pfCommittee $ \x y -> x {pfCommittee = y}

pfrConstitutionL :: Lens' (PForest PRoot era) (PRoot (GovPurposeId 'ConstitutionPurpose era))
pfrConstitutionL = lens pfConstitution $ \x y -> x {pfConstitution = y}

pfhPParamUpdateL :: Lens' (PForest PHierarchy era) (PHierarchy (GovPurposeId 'PParamUpdatePurpose era))
pfhPParamUpdateL = lens pfPParamUpdate $ \x y -> x {pfPParamUpdate = y}

pfhHardForkL :: Lens' (PForest PHierarchy era) (PHierarchy (GovPurposeId 'HardForkPurpose era))
pfhHardForkL = lens pfHardFork $ \x y -> x {pfHardFork = y}

pfhCommitteeL :: Lens' (PForest PHierarchy era) (PHierarchy (GovPurposeId 'CommitteePurpose era))
pfhCommitteeL = lens pfCommittee $ \x y -> x {pfCommittee = y}

pfhConstitutionL :: Lens' (PForest PHierarchy era) (PHierarchy (GovPurposeId 'ConstitutionPurpose era))
pfhConstitutionL = lens pfConstitution $ \x y -> x {pfConstitution = y}

deriving instance Era era => Eq (PForest PRoot era)
deriving instance Era era => Ord (PForest PRoot era)
deriving instance Era era => Generic (PForest PRoot era)
deriving instance Era era => NoThunks (PForest PRoot era)
deriving instance Era era => NFData (PForest PRoot era)
deriving instance Era era => Default (PForest PRoot era)

deriving instance Era era => Eq (PForest PHierarchy era)
deriving instance Era era => Ord (PForest PHierarchy era)
deriving instance Era era => Generic (PForest PHierarchy era)
deriving instance Era era => NoThunks (PForest PHierarchy era)
deriving instance Era era => NFData (PForest PHierarchy era)
deriving instance Era era => Default (PForest PHierarchy era)

data Proposals era = Proposals
  { pProps :: !(OMap.OMap (GovActionId (EraCrypto era)) (GovActionState era))
  , pRoots :: !(PForest PRoot era)
  , pHierarchy :: !(PForest PHierarchy era)
  }
  deriving (Show, Eq, Generic, NoThunks, NFData, Default)

pPropsL :: Lens' (Proposals era) (OMap.OMap (GovActionId (EraCrypto era)) (GovActionState era))
pPropsL = lens pProps $ \x y -> x {pProps = y}

pRootsL :: Lens' (Proposals era) (PForest PRoot era)
pRootsL = lens pRoots $ \x y -> x {pRoots = y}

pHierarchyL :: Lens' (Proposals era) (PForest PHierarchy era)
pHierarchyL = lens pHierarchy $ \x y -> x {pHierarchy = y}

instance EraPParams era => ToJSON (Proposals era) where
  toJSON = toJSON . pProps
  toEncoding = toEncoding . pProps

proposalsAddAction :: forall era. GovActionState era -> Proposals era -> Maybe (Proposals era)
proposalsAddAction gas ps =
  case gas ^. gasActionL of
    ParameterChange parent _ ->
      update pfrPParamUpdateL pfhPParamUpdateL parent
    HardForkInitiation parent _ ->
      update pfrHardForkL pfhHardForkL parent
    TreasuryWithdrawals _ -> Just $ ps & pPropsL %~ (OMap.||> gas)
    NoConfidence parent ->
      update pfrCommitteeL pfhCommitteeL parent
    UpdateCommittee parent _ _ _ ->
      update pfrCommitteeL pfhCommitteeL parent
    NewConstitution parent _ ->
      update pfrConstitutionL pfhConstitutionL parent
    InfoAction -> Just $ ps & pPropsL %~ (OMap.||> gas)
  where
    update ::
      Lens' (PForest PRoot era) (PRoot (GovPurposeId p era)) ->
      Lens' (PForest PHierarchy era) (PHierarchy (GovPurposeId p era)) ->
      StrictMaybe (GovPurposeId p era) ->
      Maybe (Proposals era)
    update lenzR lenzH prev =
      let newId = GovPurposeId $ gas ^. gasIdL
          newNode parent = PNode parent Set.empty
       in case prev of
            SNothing ->
              -- We only accept actions with parent `SNothing` when
              -- there are no proposals enacted yet, which means that
              -- the root has to be `Nothing`
              if isNothing $ ps ^. pRootsL . lenzR . prRootL
                then
                  Just $
                    ps
                      & pPropsL %~ (OMap.||> gas)
                      & pRootsL . lenzR . prChildrenL %~ Set.insert newId
                      & pHierarchyL . lenzH . pHierarchyNTL %~ Map.insert newId (newNode Nothing)
                else Nothing
            SJust parentId ->
              if Just parentId == ps ^. pRootsL . lenzR . prRootL
                then
                  Just $
                    ps
                      & pPropsL %~ (OMap.||> gas)
                      & pRootsL . lenzR . prChildrenL %~ Set.insert newId
                      & pHierarchyL . lenzH . pHierarchyNTL %~ Map.insert newId (newNode $ Just parentId)
                else
                  if Map.member parentId $ ps ^. pHierarchyL . lenzH . pHierarchyNTL
                    then
                      Just $
                        ps
                          & pPropsL %~ (OMap.||> gas)
                          & pHierarchyL . lenzH . pHierarchyNTL %~ Map.adjust (pnChildrenL %~ Set.insert newId) parentId
                          & pHierarchyL . lenzH . pHierarchyNTL %~ Map.insert newId (newNode $ Just parentId)
                    else Nothing

-- | Internal function for checking if the invariants are maintained
isConsistent_ :: Proposals era -> Bool
isConsistent_ (Proposals omap roots hierarchy) =
  -- OMap internal invariant
  assert (OMap.invariantHolds' omap)
    $ assert
      -- The hierarchies are disjoint by pigeon-hole principle
      ( let sizeOfUnions = Set.size unionHierarchy
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
      -- The root-children are disjoint by pigeon-hole principle
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
      (Map.keysSet (OMap.toMap omap) == unionHierarchy)
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
    pparamUpdateH = hierarchy ^. pfhPParamUpdateL . pHierarchyNTL
    hardForkH = hierarchy ^. pfhHardForkL . pHierarchyNTL
    committeeH = hierarchy ^. pfhCommitteeL . pHierarchyNTL
    constitutionH = hierarchy ^. pfhConstitutionL . pHierarchyNTL
    unionHierarchy =
      Set.unions
        [ Set.map unGovPurposeId $ Map.keysSet pparamUpdateH
        , Set.map unGovPurposeId $ Map.keysSet hardForkH
        , Set.map unGovPurposeId $ Map.keysSet committeeH
        , Set.map unGovPurposeId $ Map.keysSet constitutionH
        ]
    pparamUpdateRC = roots ^. pfrPParamUpdateL . prChildrenL
    hardForkRC = roots ^. pfrHardForkL . prChildrenL
    committeeRC = roots ^. pfrCommitteeL . prChildrenL
    constitutionRC = roots ^. pfrConstitutionL . prChildrenL
    isSubset h = and $ flip Set.isSubsetOf (Map.keysSet h) . pnChildren <$> Map.elems h

mkProposals ::
  forall era.
  EraPParams era =>
  OMap.OMap (GovActionId (EraCrypto era)) (GovActionState era) ->
  PrevGovActionIds era ->
  Maybe (Proposals era)
mkProposals omap pgais = do
  ps@(Proposals omap' _roots _hierarchy) <-
    foldl'
      (\mprops gas -> proposalsAddAction gas =<< mprops)
      initialProposals
      (OMap.toStrictSeq omap)
  guard (omap == omap' && isConsistent_ ps) $> ps
  where
    initialProposals = Just $ def & pRootsL .~ fromPrevGovActionIds pgais

data ProposalsSerializable era
  = ProposalsSerializable
      (PrevGovActionIds era)
      (OMap.OMap (GovActionId (EraCrypto era)) (GovActionState era))

instance EraPParams era => EncCBOR (ProposalsSerializable era) where
  encCBOR (ProposalsSerializable roots props) =
    encode $ Rec (ProposalsSerializable @era) !> To roots !> To props

instance EraPParams era => DecCBOR (ProposalsSerializable era) where
  decCBOR = decode $ RecD ProposalsSerializable <! From <! From

instance EraPParams era => EncCBOR (Proposals era) where
  encCBOR ps =
    let roots = toPrevGovActionIds $ ps ^. pRootsL
        props = ps ^. pPropsL
     in encCBOR $ ProposalsSerializable roots props

instance EraPParams era => DecCBOR (Proposals era) where
  decCBOR =
    decCBOR >>= \(ProposalsSerializable roots props) ->
      case mkProposals props roots of
        Nothing -> fail "Could not decode Proposals"
        Just ps -> pure ps

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

proposalsRemoveIds ::
  forall era.
  Set (GovActionId (EraCrypto era)) ->
  Proposals era ->
  (Proposals era, Map.Map (GovActionId (EraCrypto era)) (GovActionState era))
proposalsRemoveIds gais ps =
  let (retainedOMap, removedFromOMap) = OMap.extractKeys gais $ ps ^. pPropsL
      (roots, hierarchy) = foldl' removeEach (ps ^. pRootsL, ps ^. pHierarchyL) gais
   in (Proposals retainedOMap roots hierarchy, removedFromOMap)
  where
    removeEach accum@(roots, hierarchy) gai =
      case OMap.lookup gai $ ps ^. pPropsL of
        Nothing -> accum
        Just gas ->
          case gas ^. gasActionL of
            ParameterChange parent _ ->
              remove roots hierarchy pfrPParamUpdateL pfhPParamUpdateL parent
            HardForkInitiation parent _ ->
              remove roots hierarchy pfrHardForkL pfhHardForkL parent
            TreasuryWithdrawals _ -> accum
            NoConfidence parent ->
              remove roots hierarchy pfrCommitteeL pfhCommitteeL parent
            UpdateCommittee parent _ _ _ ->
              remove roots hierarchy pfrCommitteeL pfhCommitteeL parent
            NewConstitution parent _ ->
              remove roots hierarchy pfrConstitutionL pfhConstitutionL parent
            InfoAction -> accum
      where
        remove ::
          PForest PRoot era ->
          PForest PHierarchy era ->
          Lens' (PForest PRoot era) (PRoot (GovPurposeId p era)) ->
          Lens' (PForest PHierarchy era) (PHierarchy (GovPurposeId p era)) ->
          StrictMaybe (GovPurposeId p era) ->
          (PForest PRoot era, PForest PHierarchy era)
        remove rs hy lenzR lenzH parent =
          let gpi = GovPurposeId gai
           in ( rs & lenzR . prChildrenL %~ Set.delete gpi
              , hy
                  & lenzH . pHierarchyNTL %~ Map.delete gpi
                  & case parent of
                    SNothing -> id
                    SJust parentGpi ->
                      lenzH . pHierarchyNTL %~ Map.adjust (pnChildrenL %~ Set.delete gpi) parentGpi
              )

getAllDescendents ::
  forall era.
  Proposals era ->
  GovActionId (EraCrypto era) ->
  Set (GovActionId (EraCrypto era))
getAllDescendents (Proposals omap _roots hierarchy) gai = case OMap.lookup gai omap of
  Nothing -> Set.empty
  Just gas -> case gas ^. gasActionL of
    ParameterChange _parent _ -> collectDescendents pfhPParamUpdateL
    HardForkInitiation _parent _ -> collectDescendents pfhHardForkL
    TreasuryWithdrawals _ -> Set.empty
    NoConfidence _parent -> collectDescendents pfhCommitteeL
    UpdateCommittee _parent _ _ _ -> collectDescendents pfhCommitteeL
    NewConstitution _parent _ -> collectDescendents pfhConstitutionL
    InfoAction -> Set.empty
  where
    collectDescendents ::
      Lens' (PForest PHierarchy era) (PHierarchy (GovPurposeId p era)) ->
      Set (GovActionId (EraCrypto era))
    collectDescendents lenz = Set.map unGovPurposeId $ go lenz $ GovPurposeId gai
    go ::
      forall p.
      Lens' (PForest PHierarchy era) (PHierarchy (GovPurposeId p era)) ->
      GovPurposeId p era ->
      Set (GovPurposeId p era)
    go lenz gpi =
      case Map.lookup gpi $ hierarchy ^. lenz . pHierarchyNTL of
        Nothing -> error "Impossible! getAllDescendents: GovPurposeId not found"
        Just (PNode _parent children) -> children <> foldMap (go lenz) children

proposalsRemoveDescendentIds ::
  Set (GovActionId (EraCrypto era)) ->
  Proposals era ->
  (Proposals era, Map (GovActionId (EraCrypto era)) (GovActionState era))
proposalsRemoveDescendentIds gais ps =
  proposalsRemoveIds (gais <> foldMap (getAllDescendents ps) gais) ps

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
      (enactedProposalsState, enactedRemoved) = foldl' enact (unexpiredProposals, Map.empty) enactedGais
   in (enactedProposalsState, enactedRemoved, expiredRemoved)
  where
    enact (ps, removed) gai = case proposalsLookupId gai ps of
      Nothing ->
        error $ -- QUESTION: Is using `error` the best in this function? (Used atleast twice)
          unlines
            [ "Impossible! Enacted GovActionId:"
            , "\t\t" <> show gai
            , "\t" <> "not found in Proposals OMap"
            ]
      Just gas -> case gas ^. gasActionL of
        ParameterChange parent _ -> enactFromRoot pfrPParamUpdateL pfhPParamUpdateL parent
        HardForkInitiation parent _ -> enactFromRoot pfrHardForkL pfhHardForkL parent
        TreasuryWithdrawals _ -> enactWithoutRoot
        NoConfidence parent -> enactFromRoot pfrCommitteeL pfhCommitteeL parent
        UpdateCommittee parent _ _ _ -> enactFromRoot pfrCommitteeL pfhCommitteeL parent
        NewConstitution parent _ -> enactFromRoot pfrConstitutionL pfhConstitutionL parent
        InfoAction -> enactWithoutRoot
      where
        enactWithoutRoot ::
          ( Proposals era
          , Map (GovActionId (EraCrypto era)) (GovActionState era)
          )
        enactWithoutRoot =
          let (newOMap, removedActions) = OMap.extractKeys (Set.singleton gai) $ ps ^. pPropsL
           in (ps & pPropsL .~ newOMap, removed `Map.union` removedActions)
        enactFromRoot ::
          Lens' (PForest PRoot era) (PRoot (GovPurposeId p era)) ->
          Lens' (PForest PHierarchy era) (PHierarchy (GovPurposeId p era)) ->
          StrictMaybe (GovPurposeId p era) ->
          ( Proposals era
          , Map (GovActionId (EraCrypto era)) (GovActionState era)
          )
        enactFromRoot lenzR lenzH parent =
          let gpi = GovPurposeId gai
              siblings = Set.delete gai $ Set.map unGovPurposeId (ps ^. pRootsL . lenzR . prChildrenL)
              (withoutSiblings, removedActions) = proposalsRemoveDescendentIds siblings ps
              newRootNode = case Map.lookup gpi $ ps ^. pHierarchyL . lenzH . pHierarchyNTL of
                Nothing ->
                  error $
                    unlines
                      [ "Impossible! Enacted GovActionId:"
                      , "\t\t" <> show gai
                      , "\t" <> "not found in Proposals Hierarchy"
                      ]
                Just node -> node
              newHierarchy = Map.delete gpi $ withoutSiblings ^. pHierarchyL . lenzH . pHierarchyNTL
              (newOMap, enactedAction) = OMap.extractKeys (Set.singleton gai) $ withoutSiblings ^. pPropsL
           in assert
                (ps ^. pRootsL . lenzR . prRootL == strictMaybeToMaybe parent)
                ( withoutSiblings
                    & pHierarchyL . lenzH . pHierarchyNTL .~ newHierarchy
                    & pRootsL . lenzR . prRootL ?~ gpi -- Set the new root
                    & pRootsL . lenzR . prChildrenL .~ pnChildren newRootNode -- Set the new root children
                    & pPropsL .~ newOMap
                , removed `Map.union` removedActions `Map.union` enactedAction
                )

-- getLenses :: -- TODO: Get this to type-check and use it everywhere!
--   forall era.
--   GovAction era ->
--   forall (p :: GovActionPurpose).
--   Maybe
--     ( Lens' (PForest PRoot era) (PRoot (GovPurposeId p era))
--     , Lens' (PForest PHierarchy era) (PHierarchy (GovPurposeId p era))
--     , StrictMaybe (GovPurposeId p era)
--     )
-- getLenses = \case
--   ParameterChange parent _ -> Just (pfrPParamUpdateL, pfhPParamUpdateL, parent)
--   HardForkInitiation parent _ -> Just (pfrHardForkL, pfhHardForkL, parent)
--   TreasuryWithdrawals _ -> Nothing
--   NoConfidence parent -> Just (pfrCommitteeL, pfhCommitteeL, parent)
--   UpdateCommittee parent _ _ _ -> Just (pfrCommitteeL, pfhConstitutionL, parent)
--   NewConstitution parent _ -> Just (pfrConstitutionL, pfhConstitutionL, parent)
--   InfoAction -> Nothing

proposalsShowDebug :: Era era => Proposals era -> Bool -> String
proposalsShowDebug ps showRoots =
  unlines $
    [ ""
    , "----- Proposals -----"
    , "Size"
    , show $ proposalsSize ps
    , "OMap"
    , show $ proposalsIds ps
    , ""
    , "Roots"
    , "> PParamUpdate"
    , show $ ps ^. pRootsL . pfrPParamUpdateL
    , "> HardFork"
    , show $ ps ^. pRootsL . pfrHardForkL
    , "> Committee"
    , show $ ps ^. pRootsL . pfrCommitteeL
    , "> Constitution"
    , show $ ps ^. pRootsL . pfrConstitutionL
    ]
      <> ( if showRoots
            then
              [ "Heirarchy"
              , ">> PParamUpdate"
              , show $ ps ^. pHierarchyL . pfhPParamUpdateL . pHierarchyNTL
              , ">> HardFork"
              , show $ ps ^. pHierarchyL . pfhHardForkL . pHierarchyNTL
              , ">> Committee"
              , show $ ps ^. pHierarchyL . pfhCommitteeL . pHierarchyNTL
              , ">> Constitution"
              , show $ ps ^. pHierarchyL . pfhConstitutionL . pHierarchyNTL
              ]
            else mempty
         )
      <> ["----- Proposals End -----"]

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
proposalsGovActionStates ::
  Proposals era ->
  Map (GovActionId (EraCrypto era)) (GovActionState era)
proposalsGovActionStates (Proposals omap _ _) = OMap.toMap omap

proposalsSize :: Proposals era -> Int
proposalsSize (Proposals omap _ _) = OMap.size omap

proposalsLookupId ::
  GovActionId (EraCrypto era) ->
  Proposals era ->
  Maybe (GovActionState era)
proposalsLookupId gai (Proposals omap _ _) = OMap.lookup gai omap

deriving instance Era era => Eq (PForest StrictMaybe era)
deriving instance Era era => Ord (PForest StrictMaybe era)
deriving instance Era era => Generic (PForest StrictMaybe era)
deriving instance Era era => NoThunks (PForest StrictMaybe era)
deriving instance Era era => NFData (PForest StrictMaybe era)
deriving instance Era era => ToJSON (PForest StrictMaybe era)
deriving instance Era era => Default (PForest StrictMaybe era)

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
        & prevGovActionIdsL . pfPParamUpdateL .~ maybeToStrictMaybe (pfPParamUpdate ^. prRootL)
        & prevGovActionIdsL . pfHardForkL .~ maybeToStrictMaybe (pfHardFork ^. prRootL)
        & prevGovActionIdsL . pfCommitteeL .~ maybeToStrictMaybe (pfCommittee ^. prRootL)
        & prevGovActionIdsL . pfConstitutionL .~ maybeToStrictMaybe (pfConstitution ^. prRootL)

fromPrevGovActionIds :: Era era => PrevGovActionIds era -> PForest PRoot era
fromPrevGovActionIds (PrevGovActionIds pforest@(PForest _ _ _ _)) =
  let PForest {..} = pforest
   in def
        & pfrPParamUpdateL . prRootL .~ strictMaybeToMaybe pfPParamUpdate
        & pfrHardForkL . prRootL .~ strictMaybeToMaybe pfHardFork
        & pfrCommitteeL . prRootL .~ strictMaybeToMaybe pfCommittee
        & pfrConstitutionL . prRootL .~ strictMaybeToMaybe pfConstitution
