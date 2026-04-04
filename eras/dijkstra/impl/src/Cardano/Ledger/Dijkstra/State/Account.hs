{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.State.Account () where

import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era
import qualified Data.Map.Strict as Map
import Lens.Micro

instance EraAccounts DijkstraEra where
  type AccountState DijkstraEra = ConwayAccountState DijkstraEra
  type Accounts DijkstraEra = ConwayAccounts DijkstraEra

  addAccountState cred accountState = accountsMapL %~ Map.insert cred accountState

  accountsMapL = lens caStates $ \cas asMap -> cas {caStates = asMap}

  balanceAccountStateL = balanceConwayAccountStateL
  {-# INLINE balanceAccountStateL #-}

  depositAccountStateL = depositConwayAccountStateL
  {-# INLINE depositAccountStateL #-}

  stakePoolDelegationAccountStateL = stakePoolDelegationConwayAccountStateL
  {-# INLINE stakePoolDelegationAccountStateL #-}

  unregisterAccount = unregisterConwayAccount

instance ConwayEraAccounts DijkstraEra where
  dRepDelegationAccountStateL = dRepDelegationConwayAccountStateL
  {-# INLINE dRepDelegationAccountStateL #-}
