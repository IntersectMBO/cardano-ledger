module Cardano.Ledger.Api.State.Query.Epoch (
  queryEpochNo,
  queryChainAccountState,
) where

import Cardano.Ledger.Shelley.LedgerState (NewEpochState (nesEL))
import Cardano.Ledger.Slot (EpochNo)
import Cardano.Ledger.State (ChainAccountState, chainAccountStateL)
import Lens.Micro ((^.))

-- | Query the current epoch number.
queryEpochNo :: NewEpochState era -> EpochNo
queryEpochNo = nesEL

-- | Query chain account state (treasury and reserves).
queryChainAccountState ::
  NewEpochState era ->
  ChainAccountState
queryChainAccountState nes = nes ^. chainAccountStateL
