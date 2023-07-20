# Version history for `cardano-ledger-shelley`

## 1.4.2.0

* Add implementation for `spendableInputsTxBodyL`
* Fix an issue with where `witsVKeyNeededNoGovernance` and `witsVKeyNeeded` required
  witnesses for `allInputsTxL`, which affected reference inputs in Babbage.

## 1.4.1.0

* Add `getConstitutionHash` to `EraGovernance` #3506
  * Also add `nesEpochStateL` to `LedgerState.Types`

## 1.4.0.0

* Deprecated `updateTxBodyG`
* Changed the argument types of `witsVKeyNeeded`
* Changed the signature of `validateNeededWitnesses`
* Added `witsVKeyNeededGovernance` and `witsVKeyNeededNoGovernance`
* Added protocol version bound to
  * `STS (ShelleyUTXOW era)` instance
  * `transitionRulesUTXOW`
  * `witsVKeyNeeded`
* Prevent using `getMirTxCert` from being used in eras after Babbage. This also affects
  all functions that use it
* Prevent using `mkGenesisDelegTxCert`, `getGenesisDelegTxCert` and `GenesisDelegTxCert`
  from being used in eras after Babbage. This also affects all functions that use those
  functions and patern synonym
* Remove `WrongCertificateTypePOOL` as an impossible case.

## 1.3.0.0

* Deprecated `poolSpec` function
* Deprecated `Cardano.Ledger.Shelley.Delegation.Certificates` and
  `Cardano.Ledger.Shelley.Delegation.PoolParams` modules
* Added `Cardano.Ledger.Shelley.TxCert` module
* Make `DCert` parameterized on `era` instead of `c`rypto and rename it as `ShelleyTxCert`:
  * `DCertDelegCert` -> `ShelleyTxCertDeleg`
  * `DCertPool` -> `ShelleyTxCertPool`
  * `DCertGenesis` -> `ShelleyTxCertGenesis`
  * `DCertMir` -> `ShelleyTxCertMir`
* Introduce `TxCert` type family with pattern synonyms and rename the actual `DCert` type into
  `ShelleyTxCert`
* Introduce `ShelleyEraTxCert` type class.
* Add `EraTxCert` and `ShelleyEraTxCert` instances to `ShelleyEra`
* Remove `certsTxBodyL` and `certsTxBodyG` from `ShelleyEraTxBody`. Former migrated to `EraTxBody`.
* Add helper functions `shelleyTxCertDelegDecoder`, `commonTxCertDecoder`, `encodeShelleyDelegCert`,
  `encodePoolCert` and `encodeConstitutionalCert`
* Deprecate:
  * `RegKey` in favor of `ShelleyRegCert`
  * `DeRegKey` in favor of `ShelleyUnRegCert`
  * `Delegate` in favor of `ShelleyDelegCert`
* Addition of `getVKeyWitnessShelleyTxCert` and `getScriptWitnessShelleyTxCert`
* Deprecate:
  * `extractKeyHashWitnessSet` in favor of `credKeyHashWitness`
  * `scriptCred` in favor of `credScriptHash`
  * `scriptStakeCred` in favor of `getScriptWitnessTxCert`
  * `requiresVKeyWitness` in favor of `getVKeyWitnessTxCert`
  * `delegCWitness` - no longer used.
  * `propWits` - will become an internal function in the future version
* `validateNeededWitnesses` no longer accepts `witsVKeyNeeded` as an argument.
* Move `ConstitutionalDelegCert` from `cardano-ledger-core` as `GenesisDelegCert`.
* Fixed `NoThunks (ShelleyGenesis c)` instance, as it was incorrectly disallowing thunks in its `sgInitialFunds` and `sgStaking` fields

## 1.2.0.0

* Replace `DPState c` with `CertState era`
* Parametrize `DState` and `PState` by era
* Rename `obligationDPState` to `obligationCertState`
* Rename `keyCertsRefundsDPState` to `keyCertsRefundsCertState`
* Rename `totalCertsDepositsDPState` to `totalCertsDepositsCertState`
* Added new functions to `DELEGS` rule
  * `drainWithdrawals`
  * `validateZeroRewards`
  * `validateDelegationRegistered`

## 1.1.1.0

* Disable `TICKF` rule optimization: [#3375](https://github.com/input-output-hk/cardano-ledger/pull/3375)

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
