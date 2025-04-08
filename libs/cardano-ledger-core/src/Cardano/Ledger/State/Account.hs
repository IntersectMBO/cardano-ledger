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

sumDepositsAcountsState :: EraAccounts era => Accounts era -> Coin
sumDepositsAcountsState accounts =
  fromCompact $ foldMap' (^. depositAccountStateL) $ accounts ^. accountsMapL
