# Version history for `cardano-ledger-shelley`

## 1.2.0.0

* Disable `TICKF` rule optimization: [#3375](https://github.com/input-output-hk/cardano-ledger/pull/3375)
* Replace `DPState c` with `CertState era`
* Parametrize `DState` and `PState` by era
* Rename `obligationDPState` to `obligationCertState`
* Rename `keyCertsRefundsDPState` to `keyCertsRefundsCertState`
* Rename `totalCertsDepositsDPState` to `totalCertsDepositsCertState`
* Added new functions to `DELEGS` rule
  * `drainWithdrawals`
  * `validateZeroRewards`
  * `validateDelegationRegistered`
* Deprecated `poolSpec` function
* Deprecated `Cardano.Ledger.Shelley.Delegation.Certificates` and
  `Cardano.Ledger.Shelley.Delegation.PoolParams` modules
* Added `Cardano.Ledger.Shelley.Delegation` module
* Make `DCert` parametrized on `era` instead of `c`rypto
* Make `DCert` into a type family with pattern synonyms and rename the actual type into
  `ShelleyDCert`
* Remove `certsTxBodyL` and `certsTxBodyG` from `ShelleyEraTxBody`
* Add `shelleyDCertDecoder`
* Rename `ShelleyDCertDeleg` to `ShelleyDCertDelegCert`

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
* Change types in `StakePoolRetirementWrongEpochPOOL` from `Word64` to `EpochNo`

### `testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
