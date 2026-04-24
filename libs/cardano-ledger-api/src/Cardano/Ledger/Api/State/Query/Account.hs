module Cardano.Ledger.Api.State.Query.Account (
  -- * @GetStakeDelegDeposits@
  queryAccountsDeposits,

  -- * @GetFilteredVoteDelegatees@
  queryDRepDelegatees,
) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.State (ConwayEraAccounts, dRepDelegationAccountStateL)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (NewEpochState, esLStateL, lsCertStateL, nesEsL)
import Cardano.Ledger.State (DRep, EraAccounts (..), EraCertState (..), accountsL)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro

-- | Query staking delegation deposits.
--
-- Returns the deposit for each given credential that is currently
-- registered. Empty 'Set' returns all registered credentials.
queryAccountsDeposits ::
  EraCertState era =>
  NewEpochState era ->
  Set.Set (Credential Staking) ->
  Map.Map (Credential Staking) Coin
queryAccountsDeposits nes creds =
  let accountsMap = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL
      selected
        | Set.null creds = accountsMap
        | otherwise = accountsMap `Map.restrictKeys` creds
   in Map.map (fromCompact . (^. depositAccountStateL)) selected

-- | Query the DRep delegatee for each given staking credential.
--
-- Returns the DRep each credential has delegated to. Credentials with
-- no DRep delegation are omitted from the result. Empty 'Set' returns
-- all.
queryDRepDelegatees ::
  (EraCertState era, ConwayEraAccounts era) =>
  NewEpochState era ->
  Set.Set (Credential Staking) ->
  Map.Map (Credential Staking) DRep
queryDRepDelegatees nes creds =
  let accountsMap = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL
      selected
        | Set.null creds = accountsMap
        | otherwise = accountsMap `Map.restrictKeys` creds
   in Map.mapMaybe (^. dRepDelegationAccountStateL) selected
