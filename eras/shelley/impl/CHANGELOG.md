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
* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285
* Added `ToJSON` instance for `ShelleyTxOut`
* Added `ToJSON` instance for `AlonzoPParams StrictMaybe`

## 1.0.0.0

* First properly versioned release.
