{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Cardano.Ledger.State.Account where

import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Hashes
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Lens.Micro

class EraAccountState era where
  type AccountState era = (r :: Type) | r -> era
  type AccountStates era = (r :: Type) | r -> era

  accountStatesMapL :: Lens' (AccountStates era) (Map (Credential 'Staking) (AccountState era))

  balanceAccountStateL :: Lens' (AccountState era) (CompactForm Coin)

  depositAccountStateL :: Lens' (AccountState era) (CompactForm Coin)

  stakePoolDelegationAccountStateL :: Lens' (AccountState era) (Maybe (KeyHash 'StakePool))
