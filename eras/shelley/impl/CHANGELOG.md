# Version history for `cardano-ledger-shelley`

## 1.1.0.0

* Added a default implementation for `emptyGovernanceState`
* Added lenses:
  * `esAccountStateL`
  * `esSnapshotsL`
  * `esLStateL`
  * `esPrevPpL`
  * `esPpL`
  * `esNonMyopicL`
  * `lsUTxOState`
  * `lsDPState`
  * `utxosUtxo`
  * `utxosDeposited`
  * `utxosFees`
  * `utxosGovernance`
  * `utxosStakeDistr`
* Added `ToJSON` instance for `ShelleyTxOut`
* Added `ToJSON` instance for `AlonzoPParams StrictMaybe`
* Added `ToJSON (GovernanceState era)` superclass constraint for `EraGovernance`
* Added `ToJSON` instance for:
  * `ShelleyTxOut`
  * `AlonzoPParams StrictMaybe`
  * `ProposedPPUpdates` and `ShelleyPPUPState`
  * `AccountState`, `EpochState`, `UTxOState`, `IncrementalStake` and `LedgerState`
  * `Likelihood` and `NonMyopic`
  * `RewardUpdate` and `PulsingRewUpdate`
* Added of `ToJSON`/`FromJSON` instances for `LogWeight`
* Change `totalCertsDeposits` to accept a function that checks for registered pools,
  rather than the `DPState`. Use `totalCertsDepositsDPState` for the previous behavior
* Added `getProducedValue` and `totalCertsDepositsDPState`.
* Deprecate `evaluateTransactionBalance`

### `testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
