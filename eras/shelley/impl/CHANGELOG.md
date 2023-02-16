# Version history for `cardano-ledger-shelley`

## 1.0.1.0

Added:
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

Changed:
* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
