{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.State.Account where

import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Control.DeepSeq (NFData)
import Data.Default (Default)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Lens.Micro
import NoThunks.Class (NoThunks)

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
