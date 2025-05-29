{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.State.Account () where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Mary.Era
import Cardano.Ledger.Shelley.State
import Lens.Micro

instance EraAccounts MaryEra where
  type AccountState MaryEra = ShelleyAccountState MaryEra
  type Accounts MaryEra = ShelleyAccounts MaryEra

  addAccountState = shelleyAddAccountState

  accountsMapL = lens saStates $ \sas asMap -> sas {saStates = asMap}

  balanceAccountStateL = lens sasBalance $ \sas b -> sas {sasBalance = b}

  depositAccountStateL = lens sasDeposit $ \sas d -> sas {sasDeposit = d}

  stakePoolDelegationAccountStateL =
    lens (strictMaybeToMaybe . sasStakePoolDelegation) $ \sas d ->
      sas {sasStakePoolDelegation = maybeToStrictMaybe d}

  unregisterAccount = unregisterShelleyAccount

instance ShelleyEraAccounts MaryEra
