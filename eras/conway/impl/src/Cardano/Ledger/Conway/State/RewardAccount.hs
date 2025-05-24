{-# LANGUAGE DataKinds #-}

module Cardano.Ledger.Conway.State.Account where

import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.DRep
import Cardano.Ledger.Hashes
import Data.Map.Strict (Map)

data Delegation
  = NoDelegation
  | DelegateStakePool !(KeyHash 'StakePool)
  | DelegateDRep !DRep
  | DelegateStakePoolDRep !(KeyHash 'StakePool) !DRep

data ConwayAccountState
  = ConwayAccountState
  { casBalance :: {-# UNPACK #-} !(CompactForm Coin)
  , casDeposit :: {-# UNPACK #-} !(CompactForm Coin)
  , casDelegation :: !Delegation
  }

newtype AccountsStates era = AccountsStates
  { unAccountsStates :: Map (Credential 'Staking) ConwayAccountState
  }
  deriving (Show, Eq)
