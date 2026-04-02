module Cardano.Ledger.Api.State.Query.Epoch (
  queryChainAccountState,
) where

import Cardano.Ledger.Shelley.LedgerState (NewEpochState)
import Cardano.Ledger.State (ChainAccountState, chainAccountStateL)
import Lens.Micro ((^.))

-- | Query chain account state (treasury and reserves).
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:475
--   answerPureBlockQuery case for GetAccountState
-- Also: cardano-api:cardano-api/src/Cardano/Api/Query/Internal/Expr.hs:484
--   queryAccountState cardano-api wrapper (CLI via Convenience.hs:171)
queryChainAccountState ::
  NewEpochState era ->
  ChainAccountState
queryChainAccountState nes = nes ^. chainAccountStateL
