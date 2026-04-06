module Cardano.Ledger.Api.State.Query.Debug (
  -- * Debug queries
  queryDebugEpochState,
  queryDebugNewEpochState,

  -- * Deprecated queries
  queryProposedPParamsUpdates,
) where

import Cardano.Ledger.Shelley.LedgerState (
  EpochState,
  NewEpochState (nesEs),
 )
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates, emptyPPPUpdates)

-- | Query the full 'EpochState' for debugging.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:424
--   answerPureBlockQuery case for DebugEpochState
--
-- __Warning:__ This returns internal, era-parameterized state that is not
-- covered by CBOR stability guarantees. Use only for debugging and tooling.
queryDebugEpochState :: NewEpochState era -> EpochState era
queryDebugEpochState = nesEs

-- | Query the full 'NewEpochState' for debugging.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:437
--   answerPureBlockQuery case for DebugNewEpochState
--
-- __Warning:__ This returns internal, era-parameterized state that is not
-- covered by CBOR stability guarantees. Use only for debugging and tooling.
queryDebugNewEpochState :: NewEpochState era -> NewEpochState era
queryDebugNewEpochState = id

-- | Query proposed protocol parameter updates.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:420
--   answerPureBlockQuery case for GetProposedPParamsUpdates
queryProposedPParamsUpdates :: NewEpochState era -> ProposedPPUpdates era
queryProposedPParamsUpdates _ = emptyPPPUpdates
{-# DEPRECATED
  queryProposedPParamsUpdates
  "Shelley-era PParams update mechanism was replaced by Conway governance."
  #-}
