{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.State.Account () where

import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Shelley.State
import Lens.Micro

instance EraAccounts BabbageEra where
  type AccountState BabbageEra = ShelleyAccountState BabbageEra
  type Accounts BabbageEra = ShelleyAccounts BabbageEra

  addAccountState = shelleyAddAccountState

  accountsMapL = lens saStates $ \sas asMap -> sas {saStates = asMap}

  balanceAccountStateL = lens sasBalance $ \sas b -> sas {sasBalance = b}

  depositAccountStateL = lens sasDeposit $ \sas d -> sas {sasDeposit = d}

  stakePoolDelegationAccountStateL =
    lens (strictMaybeToMaybe . sasStakePoolDelegation) $ \sas d ->
      sas {sasStakePoolDelegation = maybeToStrictMaybe d}

  unregisterAccount = unregisterShelleyAccount

instance ShelleyEraAccounts BabbageEra
