{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- some GHC bug wrongfully complains about CanGetInstantStake constraint being redundant.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.State.Account (
  CanGetAccounts (..),
  CanSetAccounts (..),
  EraAccounts (..),
  lookupAccountState,
  updateLookupAccountState,
  isAccountRegistered,
  adjustAccountState,
  lookupStakePoolDelegation,
  sumBalancesAccounts,
  sumDepositsAccounts,
  addToBalanceAccounts,
  withdrawalsThatDoNotDrainAccounts,
  drainAccounts,
  removeStakePoolDelegations,
) where

import Cardano.Ledger.Address (RewardAccount (..), Withdrawals (..))
import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Data.Aeson (ToJSON)
import Data.Default (Default)
import Data.Foldable (foldMap')
import Data.Kind (Type)
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro
import NoThunks.Class (NoThunks)

class CanGetAccounts t where
  accountsG :: SimpleGetter (t era) (Accounts era)
  default accountsG :: CanSetAccounts t => SimpleGetter (t era) (Accounts era)
  accountsG = accountsL
  {-# INLINE accountsG #-}

class CanGetAccounts t => CanSetAccounts t where
  accountsL :: Lens' (t era) (Accounts era)

class
  ( Era era
  , Eq (Accounts era)
  , Show (Accounts era)
  , Default (Accounts era)
  , EncCBOR (Accounts era)
  , DecShareCBOR (Accounts era)
  , Share (Accounts era)
      ~ ( Interns (Credential Staking)
        , Interns (KeyHash StakePool)
        , Interns (Credential DRepRole)
        )
  , ToJSON (Accounts era)
  , NFData (Accounts era)
  , NoThunks (Accounts era)
  , Eq (AccountState era)
  , Show (AccountState era)
  , NFData (AccountState era)
  , NoThunks (AccountState era)
  ) =>
  EraAccounts era
  where
  type AccountState era = (r :: Type) | r -> era
  type Accounts era = (r :: Type) | r -> era

  -- | Add `AccountState` to `Accounts`. There are no checks whether account is already registered
  -- or not.
  addAccountState :: Credential 'Staking -> AccountState era -> Accounts era -> Accounts era

  accountsMapL :: Lens' (Accounts era) (Map (Credential 'Staking) (AccountState era))

  balanceAccountStateL :: Lens' (AccountState era) (CompactForm Coin)

  depositAccountStateL :: Lens' (AccountState era) (CompactForm Coin)

  stakePoolDelegationAccountStateL :: Lens' (AccountState era) (Maybe (KeyHash 'StakePool))

  -- | Remove the account from the state. Note that it is not capable of affecting state for DReps
  -- and StakePools, those have to be handled separately.
  --
  -- There is no counterpart for registering an account, because different eras require different
  -- information. However for testing purposed there is
  -- `Test.Cardano.Ledger.Era.registerTestAccount` that can be used for all eras.
  unregisterAccount ::
    -- | Credential to unregister
    Credential 'Staking ->
    -- | `Accounts` to remove the account state from
    Accounts era ->
    -- | Returns `Just` whenever account was registered and `Nothing` otherwise. Produced `Accounts`
    -- will have the account state removed, if it was present there to begin with.
    (Maybe (AccountState era), Accounts era)

sumBalancesAccounts :: EraAccounts era => Accounts era -> Coin
sumBalancesAccounts accounts =
  fromCompact $ foldMap' (^. balanceAccountStateL) $ accounts ^. accountsMapL

sumDepositsAccounts :: EraAccounts era => Accounts era -> Coin
sumDepositsAccounts accounts =
  fromCompact $ foldMap' (^. depositAccountStateL) $ accounts ^. accountsMapL

-- | Top up balance in accounts
--
-- /Warning/ - it is an error to try to increase a balance of an account that is not present in
-- `Accounts`
addToBalanceAccounts ::
  EraAccounts era =>
  -- | Map containing amounts that the balance in the account should be increased by. It is
  -- important to ensure that all of the credentials in this Map are actually registered.
  Map (Credential 'Staking) (CompactForm Coin) ->
  -- | Accounts that will have their balance increased.
  Accounts era ->
  Accounts era
addToBalanceAccounts addBalanceMap accounts =
  accounts
    & accountsMapL
      %~ Map.merge
        -- We have an assert here, since this should never be the case that we try to add to a
        -- balance of a non-registered account
        (Map.mapMaybeMissing (\_ _ -> assert False Nothing))
        Map.preserveMissing
        (Map.zipWithMatched (\_ balanceToAdd -> balanceAccountStateL <>~ balanceToAdd))
        addBalanceMap

-- | Lookup an account state by its credential. Returns Nothing if such account is not registrered
lookupAccountState ::
  EraAccounts era => Credential 'Staking -> Accounts era -> Maybe (AccountState era)
lookupAccountState cred accounts = Map.lookup cred (accounts ^. accountsMapL)

-- | Update account state. Returns Nothing if the value is not present and modified value otherwise
updateLookupAccountState ::
  EraAccounts era =>
  (AccountState era -> AccountState era) ->
  Credential 'Staking ->
  Accounts era ->
  (Maybe (AccountState era), Accounts era)
updateLookupAccountState f cred accounts =
  case Map.updateLookupWithKey (\_ -> Just . f) cred (accounts ^. accountsMapL) of
    (res, accountsMap) -> (res, accounts & accountsMapL .~ accountsMap)

-- | Check whether account for this staking credential is registered
isAccountRegistered :: EraAccounts era => Credential 'Staking -> Accounts era -> Bool
isAccountRegistered cred accounts = Map.member cred (accounts ^. accountsMapL)

adjustAccountState ::
  EraAccounts era =>
  (AccountState era -> AccountState era) -> Credential 'Staking -> Accounts era -> Accounts era
adjustAccountState cred f = accountsMapL %~ Map.adjust cred f

-- | In case when account state is registered and it is delegated to a stake pool this function
-- will return that delegation.
lookupStakePoolDelegation ::
  EraAccounts era =>
  Credential 'Staking ->
  Accounts era ->
  Maybe (KeyHash 'StakePool)
lookupStakePoolDelegation cred accounts =
  lookupAccountState cred accounts
    >>= (^. stakePoolDelegationAccountStateL)

-- | This function returns `Nothing` iff all of the accounts that withdrawals are trying to drain are
-- indeed registered and all of the amounts in the withdrawals match the respective balances exactly.
withdrawalsThatDoNotDrainAccounts ::
  EraAccounts era =>
  Withdrawals ->
  Network ->
  Accounts era ->
  Maybe Withdrawals
withdrawalsThatDoNotDrainAccounts (Withdrawals withdrawalsMap) networkId accounts
  -- @withdrawalsMap@ is small and @accountsMap@ big, better to traverse the former than the latter.
  | Map.foldrWithKey checkAllValidWithdrawals True withdrawalsMap =
      Nothing
  | otherwise =
      Just $ Withdrawals $ Map.foldrWithKey collectInvalidWithdrawals Map.empty withdrawalsMap
  where
    accountsMap = accounts ^. accountsMapL
    checkAllValidWithdrawals rewardAccount withdrawalAmount noBadWithdrawals =
      noBadWithdrawals && isValidWithdrawal rewardAccount withdrawalAmount
    collectInvalidWithdrawals rewardAccount withdrawalAmount badWithdrawals
      | isValidWithdrawal rewardAccount withdrawalAmount = badWithdrawals
      | otherwise = Map.insert rewardAccount withdrawalAmount badWithdrawals
    isValidWithdrawal RewardAccount {raCredential, raNetwork} withdrawalAmount
      | raNetwork == networkId
      , Just accountState <- Map.lookup raCredential accountsMap
      , withdrawalAmount == fromCompact (accountState ^. balanceAccountStateL) =
          True
      | otherwise =
          False

-- | Reset balances to zero for all accounts that are specified in the supplied `Withdrawals`.
--
-- /Note/ - There are no checks that withdrawals mention only registered accounts with correct
-- `NetworkId`. Nor there are any checks that amounts in withdrawals match up the balance in the
-- corresponding accounts. Use `withdrawalsThatDoNotDrainAccounts` to verify that calling
-- `drainAccounts` is actually safe on the supplied arguments
drainAccounts ::
  EraAccounts era =>
  Withdrawals ->
  Accounts era ->
  Accounts era
drainAccounts (Withdrawals withdrawalsMap) accounts =
  accounts
    & accountsMapL %~ \accountsMap ->
      Map.foldrWithKey'
        (\ra _withdrawalAmount -> Map.adjust (balanceAccountStateL .~ mempty) (raCredential ra))
        accountsMap
        withdrawalsMap

-- TODO: This is an expensive operation, since it iterates over the whole accountsMap. We need to
-- start keeping track of all delegations to the stake pool in its state, then we would be able to
-- switch from `Set (KeyHash 'StakePool)` to `Map (KeyHash 'StakePool) (Set (Credential Staking))`
-- and drastically speed up this operation.

-- | Remove delegations for the supplied Stake
removeStakePoolDelegations ::
  EraAccounts era => Set (KeyHash 'StakePool) -> Accounts era -> Accounts era
removeStakePoolDelegations stakeDelegationsToRemove accounts =
  accounts & accountsMapL %~ Map.map clearAccountStateDelegation
  where
    clearAccountStateDelegation =
      stakePoolDelegationAccountStateL %~ \case
        Just poolId | poolId `Set.member` stakeDelegationsToRemove -> Nothing
        delegation -> delegation
