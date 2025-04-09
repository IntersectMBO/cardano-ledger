{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- some GHC bug wrongfully complains about CanGetInstantStake constraint being redundant.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.State.Account (
  CanGetAccounts (..),
  CanSetAccounts (..),
  EraAccounts (..),
  isAccountRegistered,
  sumBalancesAccounts,
  sumDepositsAccounts,
  addToBalanceAccounts,
)
where

import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Data.Default (Default)
import Data.Foldable (foldMap')
import Data.Kind (Type)
import qualified Data.Map.Merge.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
  , NFData (Accounts era)
  , NoThunks (Accounts era)
  ) =>
  EraAccounts era
  where
  type AccountState era = (r :: Type) | r -> era
  type Accounts era = (r :: Type) | r -> era

  accountsMapL :: Lens' (Accounts era) (Map (Credential 'Staking) (AccountState era))

  balanceAccountStateL :: Lens' (AccountState era) (CompactForm Coin)

  depositAccountStateL :: Lens' (AccountState era) (CompactForm Coin)

  stakePoolDelegationAccountStateL :: Lens' (AccountState era) (Maybe (KeyHash 'StakePool))

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

-- | Check whether account for this staking credential is registered
isAccountRegistered :: EraAccounts era => Credential 'Staking -> Accounts era -> Bool
isAccountRegistered cred accounts = Map.member cred (accounts ^. accountsMapL)

-- @withdrawalsMap@ is small and @accountsMap@ big, better to transform the former
-- than the latter to have the same keys, so we can call 'Map.isSubmapOfBy'.

-- | This function returns `True` iff all of the accounts that withdrawals are trying to drain are
-- indeed registered and all of the amounts in the withdrawals match the respective balances exactly.
--
-- /Note/ - `NetworkId` in the withdrawals are ignored.
doWithdrawalsDrainAccounts ::
  EraAccounts era =>
  Withdrawals ->
  Accounts era ->
  Bool
doWithdrawalsDrainAccounts (Withdrawals withdrawalsMap) accounts =
  Map.isSubmapOfBy checkBalance (Map.mapKeys raCredential withdrawalsMap) (accounts ^. accountsMapL)
  where
    checkBalance :: Coin -> AccountState era -> Bool
    checkBalance withdrawalAmount accountState =
      withdrawalAmount == fromCompact (accountState balanceAccountStateL)

-- | Reset balances to zero for all accounts that are specified in the supplied `Withdrawals`.
--
-- /Note/ - There are no checks that withdrawals mention only registered accounts. There are also no
-- checks that amounts in withdrawals match up the balance in the corresponding accounts. Use
-- `doWithdrawalsDrainAccounts` to verify that calling `drainAccounts` is actually safe.
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
