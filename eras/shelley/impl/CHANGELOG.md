# Version history for `cardano-ledger-shelley`

## 1.10.0.1

*

## 1.10.0.0

* Remove the `PParams` param from `validateMissingScripts`
* Remove the `missingScriptsSymmetricDifference` function
* Add `NFData` instance for `AdaPots`, `ShelleyDelegEvent`
* Add `Generic`, `Eq` and `NFData` instances for:
  * `ShelleyDelegsEvent`
  * `ShelleyDelplEvent`
  * `ShelleyEpochEvent`
  * `ShelleyLedgerEvent`
  * `ShelleyMirEvent`
  * `ShelleyNewEpochEvent`
  * `PoolEvent`
  * `PoolReap`
  * `PpupEvent`
  * `RupdEvent`
  * `SnapEvent`
  * `ShelleyTickEvent`
  * `UtxoEvent`
  * `ShelleyUtxowEvent`
* Rename `NewEpoch` constructor of `ShelleyDelegEvent` to `DelegNewEpoch`
* Rename `ShelleyGovState` fields:
  * `proposals` to `sgsCurProposals`
  * `futureProposals` to `sgsFutureProposals`
  * `sgovPp` to `sgsCurPParams`
  * `sgovPrevPp` to `sgsPrevPParams`
* Change `UPEC` environment to `LedgerState`
* Rename `currentPp` to `usCurPParams` and `ppupState` to `usGovState`
* Change `ApplyTxError`, `TickTransitionError` and `BlockTransitionError`
  to use `NonEmpty (PredicateFailure _)` instead of `[PredicateFailure _]`
* Removed `prettyWitnessSetParts`
* Re-implemented `ShelleyTxWits` with `MemoBytes`
* Re-implemented `ShelleyTxAuxData` with `MemoBytes`
* Remove `getNextEpochCommitteeMembers` from `EraGov`
* Deprecated `PPUPPredFailure`
* Add instances for `InjectRuleFailure` and switch to using `injectFailure`
* Remove `poolCertTransition`
* Remove `getDRepDistr`, `getConstitution` and `getCommitteeMembers` from `EraGov` #4033
  * Move `Constitution` to `Conway.Governance.Procedures`
* Deprecated `keyBy`
* Add `getShelleyMinFeeTxUtxo`
* Add implementation for `getMinFeeTxUtxo`
* Remove deprecated functions from `Wallet`: `evaluateMinFee` and `evaluateTransactionFee`
* Replace `depositsAdaPot` in `AdaPots` with `obligationsPot`
* Add `sumAdaPots`
* Deprecate `delPlAcnt` in favor of `delPlAccount`
* Rename `RewardAccount` fields `getRwdNetwork` and `getRwdCred` to `raNetwork` and `raCredential` respectively
* Deprecate `prAcnt` in favor of `prAccountState`
* Deprecate `RewardAcnt` in favor of `RewardAccount`
* Remove `registerInitialFunds` and `registerInitialStaking`
* Add `registerInitialFundsThenStaking`
* Modify `PParams` JSON instances to match `cardano-api`
* Add wrapper to `PParams` in `ShelleyGenesis` to preserve the legacy behaviour of JSON instances

### `testlib`

* Add `ToExpr` instances for:
  * `ShelleyDelegsEvent`
  * `ShelleyDelplEvent`
  * `ShelleyEpochEvent`
  * `ShelleyLedgerEvent`
  * `ShelleyMirEvent`
  * `ShelleyNewEpochEvent`
  * `PoolEvent`
  * `PoolReap`
  * `PpupEvent`
  * `RupdEvent`
  * `SnapEvent`
  * `ShelleyTickEvent`
  * `UtxoEvent`
  * `ShelleyUtxowEvent`
* Add `SomeSTSEvent`
* Replaced `small-steps-test` dependency with `small-steps:testlib`
* Change `submitFailingTx`, `tryRunImpRule` and `trySubmitTx`
  to use `NonEmpty (PredicateFailure _)` instead of `[PredicateFailure _]`
* Added `ToExpr` instance for `ShelleyTxWitsRaw`
* Add `RuleListEra` instance for Shelley
* Add argument to `impSatisfyNativeScript` that accounts for already satisifed witnesses
* Remove `impFreshIdxL`
* Remove root coin argument from `initImpNES`, `initShelleyImpNES`
* Adjust `sendCointTo` and `sendValueTo` to return `TxIn`
* Added `ToExpr` instance for `ShelleyTxAuxDataRaw`
* Add:
  * `PlutusArgs`
  * `ScriptTestContext`
  * `shelleyFixupTx`
  * `impGetScriptTestContext`
  * `impGetScriptContextMaybe`
  * `updateAddrTxWits`
  * `addNativeScriptTxWits`
  * `addRootTxIn`
  * `fixupFees`
  * `logFeeMismatch`
  * `impScriptsL`
  * `impNativeScriptsG`
* Add:
  * `expectRegisteredRewardAddress`
  * `expectNotRegisteredRewardAddress`
  * `expectTreasury`
* Add `ToExpr` instances for `AdaPots` and `Obligations`

## 1.9.0.0

* Add `getShelleyGenesisKeyHashCountTxBody`
* Change `PPUP` signal to `StrictMaybe` from `Maybe`
* Deprecate `txup` function.
* Prevent instances for `SahelleyEraTxBody` starting with protocol version 9.
* Add `ToJSON` instance for `ShelleyTxCert`, `ShelleyDelegCert`, `MIRCert`, `MIRTarget`,
  `MIRPot` and `GenesisDelegCert`
* Deprecate `evaluateTransactionFee` in favor of `Cardano.Ledger.Api.Tx.estimateMinFeeTx`
* Change the type signature and the logic in `validateNeededWitnesses`
* Add `getShelleyWitsVKeyNeeded` and `getShelleyWitsVKeyNeededNoGov`
* Deprecate `witsVKeyNeededNoGov` and `shelleyWitsVKeyNeeded`
* Get rid of `ueGenDelegs` and `ueGenDelegsL` as unnecessary.
* Change the signature of `protectMainnet` to make it work with any
  `TransitionConfig`
* Add `protectMainnetLens`
* Add `tcNetworkIDG`
* Add `sgInitialFundsL` and `sgStakingL`
* Stop exporting all of the internal `hkd*` functions and `PParamsHKD` from
  `Cardano.Ledger.Shelley.Core`.
* Deprecated unused `hashMultiSigScript`, `txwitsScript`
* Delete deprecated modules:
  * `Cardano.Ledger.Shelley.Orphans`
  * `Cardano.Ledger.Shelley.Delegation.PoolParams`
  * `Cardano.Ledger.Shelley.LedgerState.RefundsAndDeposits`
* Remove deprecated functions: `updateTxBodyG`, `totalCertsDeposits`,
  `totalCertsDepositsCertState`, `totalTxRefundsShelley`, `keyCertsRefunds`,
  `keyCertsRefundsCertState`, `totalTxDeposits`, `totalTxDepositsShelley` `minfee`,
  `extractKeyHashWitnessSet`, `witKeyHash`, `wvkBytes`, `validateNativeMultiSigScript`,
  `evalNativeMultiSigScript`, `diffWitHashes`, `nullWitHashes`, `unWitHashes` and
  `emptyDState`
* Remove deprecated type: `WitHashes`
* Stop exporting `ShelleyTxBody`, `ShelleyTxOut`, `TxIn`, `TxId` from
  `Cardano.Ledger.Shelley.Tx`
* Stop exporting `GenesisDelegCert`, `MIRCert`, `MIRPot`, `MIRTarget`, `ShelleyDelegCert`,
  `PoolCert`, `PoolMetadata`, `PoolParams`, `Ptr`, `StakePoolRelay`, `TxOut`,
  `ShelleyTxOut`, `Url`, `SizeOfPoolRelays`, `SizeOfPoolOwners`, `Delegation`,
  `addrEitherShelleyTxOutL`, `valueEitherShelleyTxOutL`, `nativeMultiSigTag` and
  `Cardano.Ledger.Keys.WitVKey` module from `Cardano.Ledger.Shelley.TxBody`
* Export `RegTxCert`, `UnRegTxCert`, `DelegStakeTxCert` and `GenesisDelegTxCert` from
  `Cardano.Ledger.Shelley.Core`.
* Change the type of `ppEMaxL`
* Change the type of `pvCanFollow` to make sure new protocol version is not optional
* Add `hasLegalProtVerUpdate`

### `testlib`

* Add `freshKeyHashVRF`
* Add `registerPool`
* Add `sendValueTo` and `sendCoinTo`
* Add `dispenseIndex`
* Add `fixupTx`, `impAddNativeScript` and `logToExpr`
* Remove `mkTxWits`
* Add `impNESG`, `impLastTickG`, `impKeyPairsG` and `impScriptsG`. Stop exporting
  `impNESL` and `impLastTickL`
* Add `impScripts` and `impScriptsG`
* Add `impSatisfyNativeScript` and `impShelleySatisfyNativeScript`
* Rename `emptyImpNES` to `initImpNES`
* Rename `emptyShelleyImpNES` to `initShelleyImpNES`
* Add `lookupImpRootTxOut` and remove `impRootTxCoin`
* Add `submitTx_`, `submitTxAnn` and `submitTxAnn_`.
* Change type signature for `submitTx` and `trySubmitTx`.
* Add `tryRunImpRule`
* Extract `impWitsVKeyNeeded` from the `ShelleyEraImp` type class and make it into an
  `ImpTestM` action
* Add `fixupFees` `impLastTickL` `withNoFixup` `ImpTestEnv` `iteDoFixupL`
* Add `runImpTestM`, `runImpTestM_`, `evalImpTestM` and `execImpTestM`
* Add instance `Example (a -> ImpTestM era ())`, which allows use of `Arbitrary`
* Add `getNextEpochCommitteeMembers` to `EraGov` typeclass, with a default empty implementation
* Add `TxUTxODiff (UTxO era) (UTxO era)` inhabitant to the `UtxoEvent era` data type.

## 1.8.0.0

* Add `shelleyTotalDepositsTxCerts` and `shelleyTotalRefundsTxCerts`
* Remove dead functions: `depositPoolChange` and `reapRewards`
* Move `getTotalDepositsTxBody` and `getTotalRefundsTxBody` into
  `cardano-ledger-core:Cardano.Ledger.Core.EraTxBody` from `ShelleyEraTxBody`
* Change type signature and behavior of `updateUTxOState`
* Deprecate `totalCertsDeposits`, `totalCertsDepositsCertState`, `totalTxDepositsShelley`,
  `keyCertsRefundsCertState`, `keyCertsRefunds` and `totalTxRefundsShelley`

### `testlib`

* Provide CDDL spec files with `readBabbageCddlFileNames` and `readBabbageCddlFiles` from
  `Test.Cardano.Ledger.Babbage.Binary.Cddl`
* Remove `impExpectFailure` in favor of `expectLeft`
* Remove `impExpectSuccess` in favor of `expectRightDeep`
* Remove `itM` in favor of `it`
* Remove `impIOMsg` in favor of new `impAnn`
* Remove `impIO` and `predicateFailureShouldBe` as no longer needed
* Add `withImpState`
* Expose type `ImpTestState`
* Add `Example` instance for `ImpTestM`

## 1.7.0.0

* Change the return type of `getCommitteeMembers`
* Remove `validateDelegationRegistered` in favor of a new and more specific validating
  function `validateStakePoolDelegateeRegistered`.
* Add `ToExpr` instances for:
  * `ShelleyDelegPredFailure`
  * `ShelleyDelegsPredFailure`
  * `ShelleyDelplPredFailure`
  * `ShelleyEpochPredFailure`
  * `ShelleyLedgerPredFailure`
  * `ShelleyPpupPredFailure`
  * `ShelleyUtxoPredFailure`
  * `VotingPeriod`
* Add `NFData` instances for:
  * `ShelleyMirPredFailure`
  * `ShelleyNewEpochPredFailure`
  * `ShelleyNewppPredFailure`
  * `ShelleyPoolreapPredFailure`
  * `ShelleySnapPredFailure`
  * `ShelleyUpecPredFailure`
* Add `epochStateGovStateL`
* Add `shelleyCommonPParamsHKDPairsV8`
* Add `ToExpr` instances for:
  * `ShelleyPoolPredFailure`
  * `ShelleyUtxowPredFailure`
* Add `NFData` instance for:
  * `ShelleyRupdPredFailure`
  * `ShelleyTickPredFailure`
* Rename `validateFailedScripts` to `validateFailedNativeScripts` and change its
  arguments.
* Change type of `validateMissingScripts`

### `testlib`

* Add `Test.Cardano.Ledger.Shelley.ImpTest`
* Add `Test.Cardano.Ledger.Shelley.ImpTestSpec`
* Add `EraImpTest` instance for `ShelleyEra`

## 1.6.1.0

* Add `ToExpr` instance for:
  * `Update`
  * `ShelleyTx`
  * `ShelleyTxBody`
  * `ShelleyTxWits`
* Add `Generic` instance for `ShelleyTx`
* Add `Memoized` instance for `ShelleyTx`
* Introduce `Cardano.Ledger.Shelley.Transition` module with `EraTransition` interface.
* Deprecated `CanStartFromGenesis` interface in favor of `EraTransition`
* Add `Semigroup` and `Monoid` instances for `ShelleyGenesisStaking`
* Add `FromJSON` instance for `Constitution`

### `testlib`

* Add `Arbitrary` instance for `Constitution`
* `Test.Cardano.Ledger.Shelley.Binary.RoundTrip` module with:
  * `roundTripShelleyCommonSpec`
  * `roundTripStateEraTypesSpec`

## 1.6.1.0

* Add lens `epochStateTreasuryL` #3748

## 1.6.0.0

* Deprecate `isRegKey` and `isDeRegKey` in favor of `isRegStakeTxCert` and `isUnRegStakeTxCert` #3700
* Add lenses for `UTxOEnv` #3688
* Add `getTotalTxDepositsBody` to `ShelleyEraTxBody`
* Add `obligationGovState` to `EraGov`
* Add `ToExpr` instance to `Constitution`
* Replace `obligationCertState` with `totalObligation`
* Deprecated `totalTxDeposits`
* Add `potEqualsObligation`
* Add `shelleyLedgerAssertions`
* Add `totalTxDepositsShelley`
* Add `eqMultiSigRaw`, `shelleyEqTxRaw` and `shelleyEqTxWitsRaw`
* Add `EqRaw` instance for `MultiSig`, `ShelleyTxWits`, `ShelleyTxAuxData`, `TxBody` and `Tx`
* Add `ToExpr` instance for `GenesisDelegCert`, `MIRPot`, `MirTarget`, `MIRCert`,
	`ShelleyTxCert`, `ShelleyDelegCert`, `MultiSig` and `MultiSigRaw`

## 1.5.1.0

* Add new lens `epochStateIncrStakeDistrL`, that points to the `credMap` field of
  `IncrementalStake` from `EpochState`

## 1.5.0.0

* Introduce `ShelleyPoolreapEnv` and replace `Environment` for `ShelleyPOOLREAP`
* Remove redundant reimplementation of lens function `%~`: `updateWithLens`.
* Change `getConstituitionHash` to `getConstitution`
* Replace `constitutionHash` with `constitutionAnchor`
* Add `upgradeShelleyTxCert`
* Rename `*governance*` to `*gov*` #3607
  * `EraGovernance` to `EraGov`
  * `GovernanceState` to `GovState`
  * `witsVKeyNeededNoGovernance` to `witsVKeyNeededNoGov`
  * `witsVKeyNeededGovernance` to `witsVKeyNeededGov`
  * `utxosGovernance` to `utxosGovState`
* Filter out zero valued `TxOut`'s on Byron/Shelley boundary.
* Rename `getProducedValue` to `shelleyProducedValue`
* Change the constraints on `produced` and `evaluateTransactionBalance`
* Add `lsCertStateL`
* Make new `Constitution` datatype #3556
  * Adopt some Default instances for example SafeHash
* Add new methods to `EraGovernance`:
  * `curPParamsGovStateL`
  * `prevPParamsGovStateL`
* Rename `ShelleyPPUPState` to `ShelleyGovState`
* Add new fields to `ShelleyGovState`:
  * `sgovPp`
  * `sgovPrevPp`
* Add lenses:
  * `proposalsL`
  * `futureProposalsL`
  * `esAccountStateL`
  * `esSnapshotsL`
  * `esNonMyopicL`
* Remove `esPrevPp` and `esPp` from `EpochState`
* Rename `esPrevPpL` to `prevPParamsEpochStateL`
* Rename `esPpL` to `curPParamsEpochStateL`
* Swap the order of `esSnapshots` and `esLState` in `EpochState`
* Add lenses:
  * `lsCertStateL`
  * `utxosStakeDistrL`
  * `utxosDonationL`
  * `utxosUtxoL`
  * `utxosDepositedL`
  * `esAccountStateL`
  * `asTreasuryL`
  * `asReservesL`

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

* Disable `TICKF` rule optimization: [#3375](https://github.com/intersectmbo/cardano-ledger/pull/3375)

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
