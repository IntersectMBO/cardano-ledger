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
--
-- __Warning:__ This returns internal, era-parameterized state that is
-- not covered by CBOR stability guarantees. Use only for debugging and
-- tooling.
queryDebugEpochState :: NewEpochState era -> EpochState era
queryDebugEpochState = nesEs

-- | Query the full 'NewEpochState' for debugging.
--
-- __Warning:__ This returns internal, era-parameterized state that is
-- not covered by CBOR stability guarantees. Use only for debugging and
-- tooling.
queryDebugNewEpochState :: NewEpochState era -> NewEpochState era
queryDebugNewEpochState = id
