{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Ledger state queries consolidated from @ouroboros-consensus@, @cardano-api@,
-- @cardano-cli@, and @cardano-db-sync@.
--
-- Each query function includes @Source:@ comments pointing to the original
-- call sites in these downstream packages. These are intended to guide
-- downstream propagation of these queries. Once a downstream call site has been
-- successfully migrated, the corresponding comment may be removed.
module Cardano.Ledger.Api.State.Query (
  -- * @GetChainAccountState@
  module Cardano.Ledger.Api.State.Query.Epoch,

  -- * Governance queries
  module Cardano.Ledger.Api.State.Query.Governance,

  -- * @GetCurrentPParams@ / @GetFuturePParams@
  module Cardano.Ledger.Api.State.Query.PParams,

  -- * Pool queries
  module Cardano.Ledger.Api.State.Query.Pool,

  -- * Reward queries
  module Cardano.Ledger.Api.State.Query.Reward,

  -- * @GetStakeSnapshots@
  module Cardano.Ledger.Api.State.Query.Snapshot,

  -- * @GetFilteredDelegationsAndRewardAccounts@
  module Cardano.Ledger.Api.State.Query.StakeDelegation,

  -- * UTxO queries
  module Cardano.Ledger.Api.State.Query.UTxO,

  -- * Debug queries
  module Cardano.Ledger.Api.State.Query.Debug,
) where

import Cardano.Ledger.Api.State.Query.Debug
import Cardano.Ledger.Api.State.Query.Epoch
import Cardano.Ledger.Api.State.Query.Governance
import Cardano.Ledger.Api.State.Query.PParams
import Cardano.Ledger.Api.State.Query.Pool
import Cardano.Ledger.Api.State.Query.Reward
import Cardano.Ledger.Api.State.Query.Snapshot
import Cardano.Ledger.Api.State.Query.StakeDelegation
import Cardano.Ledger.Api.State.Query.UTxO
