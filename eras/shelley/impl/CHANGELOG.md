# Version history for `cardano-ledger-shelley`

## 1.1.0.0

* Add `ToJSON (GovernanceState era)` superclass constraint for `EraCovernance`
* Add `ToJSON` instance for:
  * `ShelleyTxOut`
  * `AlonzoPParams StrictMaybe`
  * `ProposedPPUpdates` and `ShelleyPPUPState`
  * `AccountState`, `EpochState`, `UTxOState`, `IncrementalStake` and `LedgerState`
  * `Likelihood` and `NonMyopic`

## 1.0.0.0

* First properly versioned release.
