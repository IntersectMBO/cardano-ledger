{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.State.Account () where

import Cardano.Ledger.Allegra.Era
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Shelley.State
import Lens.Micro

instance EraAccounts AllegraEra where
  type AccountState AllegraEra = ShelleyAccountState AllegraEra
  type Accounts AllegraEra = ShelleyAccounts AllegraEra

  addAccountState = shelleyAddAccountState

  accountsMapL = lens saStates $ \sas asMap -> sas {saStates = asMap}

  balanceAccountStateL = lens sasBalance $ \sas b -> sas {sasBalance = b}

  depositAccountStateL = lens sasDeposit $ \sas d -> sas {sasDeposit = d}

  stakePoolDelegationAccountStateL =
    lens (strictMaybeToMaybe . sasStakePoolDelegation) $ \sas d ->
      sas {sasStakePoolDelegation = maybeToStrictMaybe d}

  unregisterAccount = unregisterShelleyAccount

instance ShelleyEraAccounts AllegraEra
