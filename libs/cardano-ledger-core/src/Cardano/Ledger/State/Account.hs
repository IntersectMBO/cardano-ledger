{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- some GHC bug wrongfully complains about CanGetInstantStake constraint being redundant.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.State.Account (
  CanGetAccountsState (..),
  CanSetAccountsState (..),
  EraAccountsState (..),
  sumDepositsAcountsState,
)
where

import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Control.DeepSeq (NFData)
import Data.Default (Default)
import Data.Foldable (foldMap')
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Lens.Micro
import NoThunks.Class (NoThunks)

class CanGetAccountsState t where
  accountsStateG :: SimpleGetter (t era) (AccountsState era)
  default accountsStateG :: CanSetAccountsState t => SimpleGetter (t era) (AccountsState era)
  accountsStateG = accountsStateL
  {-# INLINE accountsStateG #-}

class CanGetAccountsState t => CanSetAccountsState t where
  accountsStateL :: Lens' (t era) (AccountsState era)

class
  ( Era era
  , Eq (AccountsState era)
  , Show (AccountsState era)
  , Default (AccountsState era)
  , EncCBOR (AccountsState era)
  , DecShareCBOR (AccountsState era)
  , Share (AccountsState era)
      ~ ( Interns (Credential Staking)
        , Interns (KeyHash StakePool)
        , Interns (Credential DRepRole)
        )
  , NFData (AccountsState era)
  , NoThunks (AccountsState era)
  ) =>
  EraAccountsState era
  where
  type AccountState era = (r :: Type) | r -> era
  type AccountsState era = (r :: Type) | r -> era

  accountsStateMapL :: Lens' (AccountsState era) (Map (Credential 'Staking) (AccountState era))

  balanceAccountStateL :: Lens' (AccountState era) (CompactForm Coin)

  depositAccountStateL :: Lens' (AccountState era) (CompactForm Coin)

  stakePoolDelegationAccountStateL :: Lens' (AccountState era) (Maybe (KeyHash 'StakePool))

sumDepositsAcountsState :: EraAccountsState era => AccountsState era -> Coin
sumDepositsAcountsState accountsState =
  fromCompact $ foldMap' (^. depositAccountStateL) $ accountsState ^. accountsStateMapL
