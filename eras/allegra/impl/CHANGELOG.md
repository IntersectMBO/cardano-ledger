# Version history for `cardano-ledger-allegra`

## 1.9.0.0

* Add `invalidBeforeL`, `invalidHereAfterL`
* Add `basicAllegraTxBody`
* Add `TxLevel` argument to `Tx` and `TxBody`
* Add `HasEraTxLevel` instances for `Tx` and `TxBody`
* Add `EraTxLevel` instance
* Remove deprecated `timelockScriptsTxAuxDataL`

### `testlib`

* Add `impSatisfyMNativeScripts`
* Add `impSatisfySignature`

## 1.8.0.0

* Replace `timelockScriptsTxAuxDataL` with `nativeScriptsTxAuxDataL`
* Replace `timelockScriptsAllegraTxAuxDataL` with `nativeScriptsAllegraTxAuxDataL`
* Changed `MaxTxSizeUTxO` to use `Word32`
* Remove `TriesToForgeADA`
* Change the type of `actualSize` and `PParameterMaxValue` fields in `OutputTooBigUTxO` to `Int`
* Added `COMPLETE` pragma for `TxCert AllegraEra`
* Added `COMPLETE` pragma for `NativeScript AllegraEra`
* Move to `testlib` `DecCBOR` instances for: `TxBody AllegraEra`, `AllegraTxAuxDataRaw`, `AllegraTxAuxData`, `TimelockRaw`, `Timelock`
* Remove `AllegraTxBody`
* Removed `era` parameter from `AllegraTxBodyRaw`
* Expose access to `AllegraTxBodyRaw`, `AllegraTxAuxData` and `TimelockRaw`
* Expose constructor `MkAllegraTxBody`, `MkTxAuxData` and `MkTimelock`
* Deprecate `TimelockConstr`
* Rename `atbrTxFee` to `atbrFee` for consistency

### `testlib`

* Added `EraSpecificSpec AllegraEra` instance
* Added `Examples` module with: `ledgerExamples`, `exampleAllegraTxBody`, `exampleAllegraTxAuxData`
* Added `Arbitrary` instance for `TransitionConfig AllegraEra`
* Added `Era` module with `AllegraEraTest` class

## 1.7.0.0

* Add `DecCBOR` instances for:
  - `Timelock`
  - `AllegraTxAuxData`
  - `AllegraTxBody`
* Converted `CertState` to a type family
* Made the fields of predicate failures and environments lazy
* Add `Era era` constraint to `NoThunks` instance for `TimeLock`
* Remove `Era era` constraint from:
  - `getRequireSignatureTimelock`
  - `getRequireAllOfTimelock`
  - `getRequireAnyOfTimelock`
  - `getRequireMOfTimelock`
  - `getTimeStartTimelock`
  - `getTimeExpireTimelock`
* Add `MemPack` instance for `Timelock`
* Remove deprecated `AuxiliaryData` type synonym
* Deprecate `Allegra` type synonym
* Remove crypto parametrization from `AllegraEra`

## 1.6.1.0

* Use `Mismatch` to clarify predicate failures. #4711

### `testlib`

* Switch to using `ImpSpec` package

## 1.6.0.1

*

## 1.6.0.0

* Change instance of `TranslationContext` to `NoGenesis`

## 1.5.0.0

* Replace patterns within `Timelock` with `AllegraEraScript`- constrained ones:
  - `RequireTimeExpire`
  - `RequireTimeStart`
* Remove `Timelock` patterns:
  - `RequireSignature`
  - `RequireAllOf`
  - `RequireAnyOf`
  - `RequireMOf`
* Introduce `AllegraEraScript` class
* Add `AllegraEraScript` and `ShelleyEraScript` instances for `AllegraEra`
* Change signatures of `evalTimelock` and `validateTimelock`:
  - replace `Era` constraint with `AllegraEraScript`
  - replace `Timelock` with `NativeScript`

### testlib

* Change signatures of `Arbitrary` instances for `Timelock` and `AllegraTxAuxData era`:
  - replace `Era` constraint with `AllegraEraScript`
  - add `NativeScript era ~ Timelock era` constraint

## 1.4.1.0

* Add a `ToJSON` instance for `ValidityInterval`
* Add `metadataAllegraTxAuxDataL`
* Add `AllegraEraTxAuxData` with `timelockScriptsTxAuxDataL` and helper
  `timelockScriptsAllegraTxAuxDataL`

## 1.4.0.0

* Add type `EraRuleEvent` instances for `LEDGER` and `TICK` events
* Add `Eq` and `NFData` instances for `AllegraUtxoEvent`
* Add instances for `InjectRuleFailure` and switch to using `injectFailure`
* Add `shelleyToAllegraUtxoPredFailure`
* Add `NFData` instance for `AllegraUtxoPredFailure`
* Add implementation for `getMinFeeTxUtxo`

### `testlib`

* Add `ToExpr` instance for `AllegraUtxoEvent`
* Add `RuleListEra` instance for Allegra

## 1.3.0.0

* Remove `ShelleyEraTxBody` superclass constraint on `AllegraEraTxBody`
* Stop exporting all of the internal `hkd*` functions and `PParamsHKD` from
  `Cardano.Ledger.Allegra.Core`.
* Export `ValidityInterval` from `Cardano.Ledger.Allegra.Core`
* Moved `ToExpr` instances out of the main library and into the testlib.
* Add `TxUTxODiff (UTxO era) (UTxO era)` inhabitant to the `AllegraUtxoEvent era` data type.

### `testlib`

* Add `impAllegraSatisfyNativeScript`

## 1.2.5.1

* Update `cardano-ledger-core` bounds

## 1.2.5.0

### `testlib`

* Provide CDDL spec files with `readBabbageCddlFileNames` and `readBabbageCddlFiles` from
  `Test.Cardano.Ledger.Babbage.Binary.Cddl`

## 1.2.4.0

* Add `NFData` instance for `AllegraUtxoPredFailure`
* Implement `getScriptsProvided`
* Flip arguments on `validateTimelock` (breaking change, but mistakenly only minor version
  was bumped)

### `testlib`

* Add `Test.Cardano.Ledger.Allegra.ImpTest`
* Add `EraImpTest` instance for `AllegraEra`

## 1.2.3.0

* Add `ToExpr` instance for `AllegraTxBody`
* Add `EraTransition` instance.

## 1.2.2.0

* Add `EqRaw` instance for `Timelock`, `AllegraTxAuxData` and `AllegraTxBody`
* Add `ToExpr` instance for `AllegraTxAuxData`

## 1.2.1.1

*

## 1.2.1.0

* Add implementation for `spendableInputsTxBodyL`

## 1.2.0.3

*

## 1.2.0.2

*

## 1.2.0.1

*

## 1.2.0.0

* Introduction of `TxCert` and `EraTxCert`
* Add `EraTxCert` and `ShelleyEraTxCert` instances to `AllegraEra`
* Fix an issue where `TotalDeposits` didn't appear on Allegra and Mary era

## 1.1.1.0

* Add `TranslateEra` instances for:
  - `DState`
  - `PState`
  - `VState`

## 1.1.0.0

* Remove redundant pattern synonym `AllegraTxAuxData'`
* Hide internal `AllegraTxAuxDataRaw` constructor with `atadrMetadata` and `atadrTimelock`
  record fields.

### `testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
