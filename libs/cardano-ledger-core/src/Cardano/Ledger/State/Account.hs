{-# LANGUAGE BangPatterns #-}
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
  lookupAccountStateIntern,
  updateLookupAccountState,
  isAccountRegistered,
  adjustAccountState,
  lookupStakePoolDelegation,
  sumBalancesAccounts,
  sumDepositsAccounts,
  addToBalanceAccounts,
  withdrawalsThatDoNotDrainAccounts,
  withdrawalsThatExceedAccountBalance,
  drainAccounts,
  applyWithdrawals,
  applyDirectDeposits,
  directDepositsMissingAccounts,
  removeStakePoolDelegations,
) where

import Cardano.Ledger.Address (DirectDeposits (..))
import Cardano.Ledger.BaseTypes (Mismatch (..), Network, Relation (..))
import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Data.Aeson (ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.Default (Default)
import Data.Foldable (foldMap')
import Data.Kind (Type)
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MapExtras (lookupInternMap)
import Data.Set (Set)
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
  addAccountState :: Credential Staking -> AccountState era -> Accounts era -> Accounts era

  accountsMapL :: Lens' (Accounts era) (Map (Credential Staking) (AccountState era))

  balanceAccountStateL :: Lens' (AccountState era) (CompactForm Coin)

  depositAccountStateL :: Lens' (AccountState era) (CompactForm Coin)

  stakePoolDelegationAccountStateL :: Lens' (AccountState era) (Maybe (KeyHash StakePool))

  -- | Remove the account from the state. Note that it is not capable of affecting state for DReps
  -- and StakePools, those have to be handled separately.
  --
  -- There is no counterpart for registering an account, because different eras require different
  -- information. However for testing purposed there is
  -- `Test.Cardano.Ledger.Era.registerTestAccount` that can be used for all eras.
  unregisterAccount ::
    -- | Credential to unregister
    Credential Staking ->
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
  Map (Credential Staking) (CompactForm Coin) ->
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
  EraAccounts era => Credential Staking -> Accounts era -> Maybe (AccountState era)
lookupAccountState cred accounts = Map.lookup cred (accounts ^. accountsMapL)

lookupAccountStateIntern ::
  EraAccounts era =>
  Credential Staking -> Accounts era -> Maybe (Credential Staking, AccountState era)
lookupAccountStateIntern cred accounts =
  lookupInternMap cred (accounts ^. accountsMapL)

-- | Update account state. Returns Nothing if the value is not present and modified value otherwise
updateLookupAccountState ::
  EraAccounts era =>
  (AccountState era -> AccountState era) ->
  Credential Staking ->
  Accounts era ->
  (Maybe (AccountState era), Accounts era)
updateLookupAccountState f cred accounts =
  case Map.updateLookupWithKey (\_ -> Just . f) cred (accounts ^. accountsMapL) of
    (res, accountsMap) -> (res, accounts & accountsMapL .~ accountsMap)

-- | Check whether account for this staking credential is registered
isAccountRegistered :: EraAccounts era => Credential Staking -> Accounts era -> Bool
isAccountRegistered cred accounts = Map.member cred (accounts ^. accountsMapL)

adjustAccountState ::
  EraAccounts era =>
  (AccountState era -> AccountState era) -> Credential Staking -> Accounts era -> Accounts era
adjustAccountState f cred = accountsMapL %~ Map.adjust f cred

-- | In case when account state is registered and it is delegated to a stake pool this function
-- will return that delegation.
lookupStakePoolDelegation ::
  EraAccounts era =>
  Credential Staking ->
  Accounts era ->
  Maybe (KeyHash StakePool)
lookupStakePoolDelegation cred accounts =
  lookupAccountState cred accounts
    >>= (^. stakePoolDelegationAccountStateL)

-- | This function returns `Nothing` iff all of the accounts that withdrawals
-- are trying to drain are indeed registered and all of the amounts in the
-- withdrawals match the respective balances exactly. It returns a 2-tuple where
-- the `fst` is withdrawals with missing account addresses or the wrong network,
-- and `snd` is incomplete withdrawals.
withdrawalsThatDoNotDrainAccounts ::
  EraAccounts era =>
  Withdrawals ->
  Network ->
  Accounts era ->
  -- | invalid withdrawal = that which does not have an account address or is in
  -- the wrong network.
  -- incomplete withdrawal = that which does not withdraw the exact account
  -- balance.
  Maybe (Withdrawals, Map AccountAddress (Mismatch RelEQ Coin))
withdrawalsThatDoNotDrainAccounts =
  categorizeWithdrawals
    ( \withdrawalAmount account ->
        withdrawalAmount == fromCompact (account ^. balanceAccountStateL)
    )

withdrawalsThatExceedAccountBalance ::
  EraAccounts era =>
  Withdrawals ->
  Network ->
  Accounts era ->
  Maybe (Withdrawals, Map AccountAddress (Mismatch RelLTEQ Coin))
withdrawalsThatExceedAccountBalance =
  categorizeWithdrawals
    ( \withdrawalAmount account ->
        withdrawalAmount <= fromCompact (account ^. balanceAccountStateL)
    )

categorizeWithdrawals ::
  EraAccounts era =>
  (Coin -> AccountState era -> Bool) ->
  Withdrawals ->
  Network ->
  Accounts era ->
  Maybe (Withdrawals, Map AccountAddress (Mismatch r Coin))
categorizeWithdrawals amountAcceptable (Withdrawals withdrawals) networkId accounts
  -- We simply `checkBadWithdrawals` to avoid allocating new variables for the most likely case
  | Map.foldrWithKey' checkBadWithdrawals True withdrawals = Nothing
  | otherwise =
      Just $
        first Withdrawals $
          Map.foldrWithKey' collectBadWithdrawals (Map.empty, Map.empty) withdrawals
  where
    checkBadWithdrawals accountAddress withdrawalAmount noBadWithdrawals =
      noBadWithdrawals && isGoodWithdrawal accountAddress withdrawalAmount
    isGoodWithdrawal accountAddress withdrawalAmount =
      maybe False (amountAcceptable withdrawalAmount) (lookupAccount accountAddress)
    collectBadWithdrawals accountAddress withdrawalAmount accum@(!_, !_) =
      case lookupAccount accountAddress of
        Nothing -> first (Map.insert accountAddress withdrawalAmount) accum
        Just account
          | amountAcceptable withdrawalAmount account -> accum
          | otherwise ->
              second
                ( Map.insert
                    accountAddress
                    $ Mismatch withdrawalAmount (fromCompact $ account ^. balanceAccountStateL)
                )
                accum
    lookupAccount (AccountAddress aaNetworkId (AccountId credential))
      | aaNetworkId == networkId = lookupAccountState credential accounts
      | otherwise = Nothing
{-# INLINE categorizeWithdrawals #-}

-- | Reset balances to zero for all accounts that are specified in the supplied `Withdrawals`.
--
-- /Note/ - There are no checks that withdrawals mention only registered accounts with correct
-- `NetworkId`. Nor there are any checks that amounts in withdrawals match up the balance in the
-- corresponding accounts.
-- Verify that it's safe to call on the supplied arguments before calling it.
drainAccounts ::
  EraAccounts era =>
  Withdrawals ->
  Accounts era ->
  Accounts era
drainAccounts (Withdrawals wdrls) = updateAccountBalances (\_ _ -> mempty) wdrls

-- | Subtract each withdrawal amount from the matching account balance.
--
-- /Note/ - No checks on the accounts or amount being withdrawn are made in this function.
-- Verify that it's safe to call on the supplied arguments before calling it.
applyWithdrawals ::
  EraAccounts era =>
  Withdrawals ->
  Accounts era ->
  Accounts era
applyWithdrawals (Withdrawals wdrls) =
  updateAccountBalances
    (\amount account -> subtractCompactCoin amount (account ^. balanceAccountStateL))
    wdrls

-- | Add each direct-deposit amount to the matching account balance.
--
-- /Note/ - There are no checks that direct deposits mention only registered accounts.
applyDirectDeposits ::
  EraAccounts era =>
  DirectDeposits ->
  Accounts era ->
  Accounts era
applyDirectDeposits (DirectDeposits dd) =
  updateAccountBalances
    (\amount account -> addCompactCoin amount (account ^. balanceAccountStateL))
    dd

-- | Fold over a `Map AccountAddress Coin`, applying the supplied balance update to each registered
-- account. Skips entries whose credential is not present in `Accounts`.
--
-- /Note/ - There are no checks that entries mention only registered accounts with correct
-- `NetworkId`. Callers must pre-validate before calling this function.
updateAccountBalances ::
  EraAccounts era =>
  -- | Balance update: given a coin amount and the account, returns the new balance.
  (CompactForm Coin -> AccountState era -> CompactForm Coin) ->
  Map AccountAddress Coin ->
  Accounts era ->
  Accounts era
updateAccountBalances updateBalance balanceMap =
  accountsMapL %~ \accountsMap ->
    Map.foldrWithKey'
      ( \(AccountAddress _ (AccountId credential)) amount ->
          Map.adjust
            ( \account ->
                account & balanceAccountStateL .~ updateBalance (compactCoinOrError amount) account
            )
            credential
      )
      accountsMap
      balanceMap
{-# INLINE updateAccountBalances #-}

-- | Returns `Nothing` iff every credential targeted by the supplied
-- `DirectDeposits` is a registered account. Otherwise it returns the subset of
-- direct deposits whose target credential is not registered.
directDepositsMissingAccounts ::
  EraAccounts era =>
  DirectDeposits ->
  Accounts era ->
  Maybe DirectDeposits
directDepositsMissingAccounts (DirectDeposits dds) accounts
  | Map.foldrWithKey' checkRegistered True dds = Nothing
  | otherwise = Just $ DirectDeposits $ Map.foldrWithKey' collectMissing Map.empty dds
  where
    isRegistered (AccountAddress _ (AccountId credential)) =
      isAccountRegistered credential accounts
    checkRegistered addr _ acc = acc && isRegistered addr
    collectMissing addr amount acc
      | isRegistered addr = acc
      | otherwise = Map.insert addr amount acc

-- | Remove delegations of supplied credentials
removeStakePoolDelegations ::
  EraAccounts era => Set (Credential Staking) -> Accounts era -> Accounts era
removeStakePoolDelegations creds accounts =
  accounts
    & accountsMapL
      %~ ( \accountsMap ->
             foldr
               (Map.adjust (stakePoolDelegationAccountStateL .~ Nothing))
               accountsMap
               creds
         )
