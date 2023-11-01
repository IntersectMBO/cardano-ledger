{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.Governance.Snapshots (
  ProposalsSnapshot,
  snapshotIds,
  snapshotAddVote,
  snapshotInsertGovAction,
  snapshotActions,
  snapshotRemoveIds,
  snapshotLookupId,
  fromGovActionStateSeq,
  snapshotGovActionStates,
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
  gasStakePoolVotesL,
 )
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Default.Class (Default (..))
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import qualified Data.OMap.Strict as OMap
import Data.Sequence.Strict (StrictSeq (..))
import Data.Set (Set)
import GHC.Generics (Generic)
import Lens.Micro (Lens', (%~))
import NoThunks.Class (NoThunks)

newtype ProposalsSnapshot era
  = ProposalsSnapshot
      (OMap.OMap (GovActionId (EraCrypto era)) (GovActionState era))
  deriving newtype (Show, Eq)
  deriving stock (Generic)

instance EraPParams era => ToExpr (ProposalsSnapshot era)

instance EraPParams era => ToJSON (ProposalsSnapshot era)

instance EraPParams era => NFData (ProposalsSnapshot era)

instance EraPParams era => NoThunks (ProposalsSnapshot era)

instance Default (ProposalsSnapshot era) where
  def = ProposalsSnapshot def

instance EraPParams era => EncCBOR (ProposalsSnapshot era) where
  encCBOR = encCBOR . snapshotActions

instance EraPParams era => DecCBOR (ProposalsSnapshot era) where
  decCBOR = fromGovActionStateSeq <$> decCBOR

-- TODO: Implement Sharing: https://github.com/input-output-hk/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (ProposalsSnapshot era) where
  decShareCBOR _ = fromGovActionStateSeq <$> decCBOR

-- | Insert a `GovActionState`, overwriting an entry of it if the
-- corresponding `GovActionId` already exists.
snapshotInsertGovAction ::
  GovActionState era ->
  ProposalsSnapshot era ->
  ProposalsSnapshot era
snapshotInsertGovAction gas (ProposalsSnapshot omap) =
  ProposalsSnapshot (omap OMap.||> gas)

-- | Get the sequence of `GovActionState`s
snapshotActions ::
  ProposalsSnapshot era ->
  StrictSeq (GovActionState era)
snapshotActions (ProposalsSnapshot omap) = OMap.toStrictSeq omap

-- | Get the sequence of `GovActionId`s
snapshotIds ::
  ProposalsSnapshot era ->
  StrictSeq (GovActionId (EraCrypto era))
snapshotIds (ProposalsSnapshot omap) = OMap.toStrictSeqOKeys omap

-- | Get the unordered map of `GovActionId`s and `GovActionState`s
snapshotGovActionStates ::
  ProposalsSnapshot era ->
  Map (GovActionId (EraCrypto era)) (GovActionState era)
snapshotGovActionStates (ProposalsSnapshot omap) = OMap.toMap omap

-- | Add a vote to an existing `GovActionState` This is a no-op if the .
-- provided `GovActionId` does not already exist                       .
snapshotAddVote ::
  Voter (EraCrypto era) ->
  Vote ->
  GovActionId (EraCrypto era) ->
  ProposalsSnapshot era ->
  ProposalsSnapshot era
snapshotAddVote voter vote gai (ProposalsSnapshot omap) =
  ProposalsSnapshot $ OMap.adjust updateVote gai omap
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
snapshotRemoveIds ::
  Set (GovActionId (EraCrypto era)) ->
  ProposalsSnapshot era ->
  (ProposalsSnapshot era, Map.Map (GovActionId (EraCrypto era)) (GovActionState era))
snapshotRemoveIds gais (ProposalsSnapshot omap) =
  let (retained, removed) = OMap.extractKeys gais omap
   in (ProposalsSnapshot retained, removed)

snapshotLookupId ::
  GovActionId (EraCrypto era) ->
  ProposalsSnapshot era ->
  Maybe (GovActionState era)
snapshotLookupId gai (ProposalsSnapshot omap) = OMap.lookup gai omap

-- | Converts a sequence of `GovActionState`s to a `ProposalsSnapshot`.
--
-- /Warning/ - This function expects `GovActionState`'s to have unique
-- `GovActionId`s, because duplicate Ids will result in `GovActionStates`
-- to be dropped.
fromGovActionStateSeq :: StrictSeq (GovActionState era) -> ProposalsSnapshot era
fromGovActionStateSeq = ProposalsSnapshot . OMap.fromFoldable

-- | Internal function for checking if the invariants are maintained
isConsistent_ :: ProposalsSnapshot era -> Bool
isConsistent_ (ProposalsSnapshot omap) = OMap.invariantHolds' omap
