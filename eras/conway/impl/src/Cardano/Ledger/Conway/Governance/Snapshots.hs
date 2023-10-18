{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

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
  snapshotGovActionStates,
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
import Data.Foldable (Foldable (..))
import Data.List (sort)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Sequence.Strict (StrictSeq (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro (Lens', (%~))
import NoThunks.Class (NoThunks)

data ProposalsSnapshot era = ProposalsSnapshot
  { psGovActionStates :: !(Map (GovActionId (EraCrypto era)) (GovActionState era))
  , psProposalOrder :: !(StrictSeq (GovActionId (EraCrypto era)))
  -- ^ Newer actions are near the end
  }
  deriving (Generic, Eq, Show)

instance EraPParams era => ToExpr (ProposalsSnapshot era)

instance EraPParams era => ToJSON (ProposalsSnapshot era)

instance EraPParams era => NFData (ProposalsSnapshot era)

instance EraPParams era => NoThunks (ProposalsSnapshot era)

instance Default (ProposalsSnapshot era) where
  def = ProposalsSnapshot def def

instance EraPParams era => EncCBOR (ProposalsSnapshot era) where
  encCBOR = encCBOR . snapshotActions

instance EraPParams era => DecCBOR (ProposalsSnapshot era) where
  decCBOR = fromGovActionStateSeq <$> decCBOR

-- TODO: Implement Sharing: https://github.com/input-output-hk/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (ProposalsSnapshot era) where
  decShareCBOR _ = fromGovActionStateSeq <$> decCBOR

snapshotInsertGovAction ::
  GovActionState era ->
  ProposalsSnapshot era ->
  ProposalsSnapshot era
snapshotInsertGovAction gas@GovActionState {gasId} ps@ProposalsSnapshot {..}
  | Map.member gasId psGovActionStates =
      ps {psGovActionStates = Map.insert gasId gas psGovActionStates}
  | otherwise =
      ProposalsSnapshot
        { psGovActionStates = Map.insert gasId gas psGovActionStates
        , psProposalOrder = psProposalOrder :|> gasId
        }

snapshotActions ::
  ProposalsSnapshot era ->
  StrictSeq (GovActionState era)
snapshotActions ProposalsSnapshot {..} = toGovAction <$> psProposalOrder
  where
    toGovAction gaId =
      fromMaybe
        (error $ "Impossible: ProposalsSnapshot invariant is not maintained: " <> show gaId)
        (Map.lookup gaId psGovActionStates)

snapshotIds ::
  ProposalsSnapshot era ->
  StrictSeq (GovActionId (EraCrypto era))
snapshotIds = psProposalOrder

snapshotGovActionStates ::
  ProposalsSnapshot era ->
  Map (GovActionId (EraCrypto era)) (GovActionState era)
snapshotGovActionStates = psGovActionStates

snapshotAddVote ::
  Voter (EraCrypto era) ->
  Vote ->
  GovActionId (EraCrypto era) ->
  ProposalsSnapshot era ->
  ProposalsSnapshot era
snapshotAddVote voter vote gId ps@ProposalsSnapshot {..} =
  ps
    { psGovActionStates = Map.update (Just . updateVote) gId psGovActionStates
    }
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

snapshotRemoveIds ::
  Set (GovActionId (EraCrypto era)) ->
  ProposalsSnapshot era ->
  ProposalsSnapshot era
snapshotRemoveIds gIds (ProposalsSnapshot {..}) =
  ProposalsSnapshot
    { psGovActionStates = psGovActionStates `Map.withoutKeys` gIds
    , psProposalOrder = foldl' (\s x -> if x `Set.member` gIds then s else x :<| s) mempty psProposalOrder
    }

snapshotLookupId ::
  GovActionId (EraCrypto era) ->
  ProposalsSnapshot era ->
  Maybe (GovActionState era)
snapshotLookupId gId ProposalsSnapshot {psGovActionStates} =
  Map.lookup gId psGovActionStates

-- | Converts a sequence of `GovActionState`s to a `ProposalsSnapshot`.
--
-- /Warning/ - This function expects `GovActionState`'s to have unique
-- `GovActionId`s, because duplicate Ids will result in `GovActionStates`
-- to be dropped.
fromGovActionStateSeq ::
  StrictSeq (GovActionState era) ->
  ProposalsSnapshot era
fromGovActionStateSeq = foldl' (flip snapshotInsertGovAction) def

-- | Internal function for checking if the invariants are maintained
isConsistent_ :: ProposalsSnapshot era -> Bool
isConsistent_ (ProposalsSnapshot {psGovActionStates, psProposalOrder}) =
  Map.keys psGovActionStates == sort (toList psProposalOrder)
    && all (\(k, GovActionState {gasId}) -> k == gasId) (Map.toList psGovActionStates)
