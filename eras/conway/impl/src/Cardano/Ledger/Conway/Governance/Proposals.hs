{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Cardano.Ledger.Conway.Governance.Proposals (
  Proposals,
  proposalsIds,
  proposalsAddVote,
  proposalsInsertGovAction,
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
  pgaPParamUpdateL,
  pgaHardForkL,
  pgaCommitteeL,
  pgaConstitutionL,
  -- Testing
  isConsistent_,
) where

import Cardano.Ledger.Binary (DecCBOR (..), DecShareCBOR (..), EncCBOR (..))
import Cardano.Ledger.Conway.Core (Era (..), EraPParams)
import Cardano.Ledger.Conway.Governance.Procedures (
  GovActionId,
  GovActionState (..),
  Vote,
  Voter (..),
  gasCommitteeVotesL,
  gasDRepVotesL,
  gasStakePoolVotesL, GovAction (..), PrevGovActionId (..), GovActionPurpose (..), gasActionL,
 )
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), KeyValue (..), object, pairs)
import Data.Default.Class (Default (..))
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import qualified Data.OMap.Strict as OMap
import Data.Sequence.Strict (StrictSeq (..))
import Data.Set (Set)
import GHC.Generics (Generic)
import Lens.Micro (Lens', (%~), lens, (^.))
import NoThunks.Class (NoThunks)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import Cardano.Ledger.Binary.Coders (decode, Decode (..), (<!), encode, Encode (..), (!>))
import Control.Monad (guard)
import Data.Functor (($>))

newtype Proposals era
  = Proposals
      (OMap.OMap (GovActionId (EraCrypto era)) (GovActionState era))
  deriving newtype (Show, Eq)
  deriving stock (Generic)

instance EraPParams era => ToExpr (Proposals era)

instance EraPParams era => ToJSON (Proposals era)

instance EraPParams era => NFData (Proposals era)

instance EraPParams era => NoThunks (Proposals era)

instance Default (Proposals era) where
  def = Proposals def

instance EraPParams era => EncCBOR (Proposals era) where
  encCBOR = encCBOR . proposalsActions

instance EraPParams era => DecCBOR (Proposals era) where
  decCBOR = fromGovActionStateSeq <$> decCBOR

-- TODO: Implement Sharing: https://github.com/input-output-hk/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (Proposals era) where
  decShareCBOR _ = fromGovActionStateSeq <$> decCBOR

-- | Insert a `GovActionState`, overwriting an entry of it if the
-- corresponding `GovActionId` already exists.
proposalsInsertGovAction ::
  GovActionState era ->
  Proposals era ->
  Proposals era
proposalsInsertGovAction gas (Proposals omap) =
  Proposals (omap OMap.||> gas)

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

pgaPParamUpdateL :: Lens' (PrevGovActionIds era) (StrictMaybe (PrevGovActionId 'PParamUpdatePurpose (EraCrypto era)))
pgaPParamUpdateL = lens pgaPParamUpdate (\x y -> x {pgaPParamUpdate = y})

pgaHardForkL :: Lens' (PrevGovActionIds era) (StrictMaybe (PrevGovActionId 'HardForkPurpose (EraCrypto era)))
pgaHardForkL = lens pgaHardFork (\x y -> x {pgaHardFork = y})

pgaCommitteeL :: Lens' (PrevGovActionIds era) (StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
pgaCommitteeL = lens pgaCommittee (\x y -> x {pgaCommittee = y})

pgaConstitutionL :: Lens' (PrevGovActionIds era) (StrictMaybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
pgaConstitutionL = lens pgaConstitution (\x y -> x {pgaConstitution = y})

instance NoThunks (PrevGovActionIds era)
instance Era era => NFData (PrevGovActionIds era)
instance Default (PrevGovActionIds era)
instance ToExpr (PrevGovActionIds era)

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
