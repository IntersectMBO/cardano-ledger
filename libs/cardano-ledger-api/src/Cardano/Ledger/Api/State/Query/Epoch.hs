module Cardano.Ledger.Api.State.Query.Epoch (
  queryChainAccountState,
) where

import Cardano.Ledger.Shelley.LedgerState (NewEpochState)
import Cardano.Ledger.State (ChainAccountState, chainAccountStateL)
import Lens.Micro ((^.))

-- | Query chain account state (treasury and reserves).
queryChainAccountState ::
  NewEpochState era ->
  ChainAccountState
queryChainAccountState nes = nes ^. chainAccountStateL
