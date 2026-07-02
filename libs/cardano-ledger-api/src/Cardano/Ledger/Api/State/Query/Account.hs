{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Ledger.Api.State.Query.Account (
  -- * @GetStakeDelegDeposits@
  queryAccountsDeposits,

  -- * @GetFilteredVoteDelegatees@
  queryDRepDelegatees,

  -- * Delegations and reward accounts
  queryDelegationsAndRewards,
  QueryResultDelegationsAndRewards (..),
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway.State (ConwayEraAccounts, dRepDelegationAccountStateL)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (NewEpochState, esLStateL, lsCertStateL, nesEsL)
import Cardano.Ledger.State (DRep, EraAccounts (..), EraCertState (..), accountsL)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
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

-- | Stake-pool delegation and reward balance for each registered staking credential.
newtype QueryResultDelegationsAndRewards = QueryResultDelegationsAndRewards
  { unQueryResultDelegationsAndRewards :: Map.Map (Credential Staking) (Maybe (KeyHash StakePool), Coin)
  }
  deriving (Eq, Show, Generic)
  deriving newtype (NFData, ToJSON, EncCBOR, DecCBOR)

-- | Stake-pool delegations and reward balances for the given registered credentials.
-- An empty 'Set' selects all registered credentials.
queryDelegationsAndRewards ::
  EraCertState era =>
  NewEpochState era ->
  Set.Set (Credential Staking) ->
  QueryResultDelegationsAndRewards
queryDelegationsAndRewards nes creds =
  let accountsMap = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL
      accountsMapFiltered
        | Set.null creds = accountsMap
        | otherwise = accountsMap `Map.restrictKeys` creds
      delegationAndReward account =
        ( account ^. stakePoolDelegationAccountStateL
        , fromCompact $ account ^. balanceAccountStateL
        )
   in QueryResultDelegationsAndRewards $ Map.map delegationAndReward accountsMapFiltered
