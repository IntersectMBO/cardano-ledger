{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.State.Account () where

import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Shelley.State
import Lens.Micro

instance EraAccounts AlonzoEra where
  type AccountState AlonzoEra = ShelleyAccountState AlonzoEra
  type Accounts AlonzoEra = ShelleyAccounts AlonzoEra

  addAccountState = shelleyAddAccountState

  accountsMapL = lens saStates $ \sas asMap -> sas {saStates = asMap}

  balanceAccountStateL = lens sasBalance $ \sas b -> sas {sasBalance = b}

  depositAccountStateL = lens sasDeposit $ \sas d -> sas {sasDeposit = d}

  stakePoolDelegationAccountStateL =
    lens (strictMaybeToMaybe . sasStakePoolDelegation) $ \sas d ->
      sas {sasStakePoolDelegation = maybeToStrictMaybe d}

  unregisterAccount = unregisterShelleyAccount

instance ShelleyEraAccounts AlonzoEra
