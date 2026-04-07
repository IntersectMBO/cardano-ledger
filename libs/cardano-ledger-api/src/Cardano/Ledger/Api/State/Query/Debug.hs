module Cardano.Ledger.Api.State.Query.Debug (
  -- * Debug queries
  queryDebugEpochState,
  queryDebugNewEpochState,
) where

import Cardano.Ledger.Shelley.LedgerState (
  EpochState,
  NewEpochState (nesEs),
 )

-- | Query the full 'EpochState' for debugging.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:424
--   answerPureBlockQuery case for DebugEpochState
--
-- __Warning:__ This returns internal, era-parameterized state that is not
-- covered by CBOR stability guarantees. Use only for debugging and tooling.
--
-- /O(1)/
queryDebugEpochState :: NewEpochState era -> EpochState era
queryDebugEpochState = nesEs

-- | Query the full 'NewEpochState' for debugging.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:437
--   answerPureBlockQuery case for DebugNewEpochState
--
-- __Warning:__ This returns internal, era-parameterized state that is not
-- covered by CBOR stability guarantees. Use only for debugging and tooling.
--
-- /O(1)/
queryDebugNewEpochState :: NewEpochState era -> NewEpochState era
queryDebugNewEpochState = id
