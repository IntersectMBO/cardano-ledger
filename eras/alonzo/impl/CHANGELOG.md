# Version history for `cardano-ledger-alonzo`

## 1.5.1.0

* Moved `Cardano.Ledger.Alonzo.Scripts.Data` to `Cardano.Ledger.Plutus.Data`
* Deprecated `Cardano.Ledger.Alonzo.Language`

### `testlib`

* Provide CDDL spec files with `readAlonzoCddlFileNames` and `readAlonzoCddlFiles` from
  `Test.Cardano.Ledger.Alonzo.Binary.Cddl`

## 1.5.0.0

* Add `Test.Cardano.Ledger.Alonzo.CostModel`
* Add `updateCostModels`
* Add `AlonzoEraUTxO` and use in the rules.
* Deprecate `getDatum` in favor of `getSpendingDatum`
* Deprecate `getDatumAlonzo` in favor of `getAlonzoSpendingDatum`
* Deprecate `getSpendingTxIn` in favor of `getAlonzoSpendingTxIn`
* Deprecate `getAllowedSupplimentalDataHashes` in favor of `getSupplementalDataHashes`
* Rename `NonOutputSupplimentaryDatums` to `NotAllowedSupplementalDatums`
* Remove unused `isTwoPhaseScriptAddress`
* Change arguments to `hasExactSetOfRedeemers`

### `testlib`

* Add `emptyAlonzoImpNES`
* Add `Test.Cardano.Ledger.Alonzo.ImpTest`
* Add `EraImpTest` instance for `AlonzoEra`
* Add `ToExpr` instances for:
  * `CollectError`
  * `AlonzoUtxoPredFailure`
  * `FailureDescription`
  * `TagMismatchDescription`
  * `AlonzoUtxosPredFailure`
  * `AlonzoUtxowPredFailure`
  * `BinaryData`
  * `Datum`
  * `IsValid`
  * `ScriptPurpose`
  * `TxOutSource`
  * `TranslationError`
  * `Addr28Extra`
  * `DataHash32`
  * `RdmrPtr`
* Expose `genValidCostModel` in `Arbitrary`

## 1.4.2.0

* Add `ToExpr` instance for:
  * `PlutusData`
  * `Data`
  * `BinaryData`
  * `Datum`
  * `AlonzoTx`
  * `AlonzoTxBody`
  * `AlonzoTxOut`
  * `AlozoTxWits`
  * `IsValid`
  * `Addr28Extra`
  * `DataHash32`
  * `RdmrPtr`
  * `Redeemers`
* Add `Generic` instance for :
  * `AlonzoTxBody`
  * `Redeemers`
  * `TxDats`
* Add `upgradeData`, `upgradeRedeemers` and `upgradeTxDats`
* Add `TxUpgradeError` type to `EraTx`
* Add `AlonzoTxBodyUpgradeError`, `AlonzoTxUpgradeError`
* Add `toAlonzoTransitionConfigPairs` and `EraTransition` instance.
* Rename `alonzoGenesisAesonPairs` -> `toAlonzoGenesisPairs` for consistency.

### `testlib`

* Add `Test.Cardano.Ledger.Alonzo.Binary.RoundTrip` module with:
  * `roundTripAlonzoCommonSpec`
  * `roundTripAlonzoEraTypesSpec`

## 1.4.1.0

* Made `isPlutusScript` more general.
* Add `alonzoEqTxRaw` and `alonzoEqTxWitsRaw`
* Add `EqRaw` instance for `AlonzoScript`, `AlonzoTxWits`, `AlonzoTxAuxData`,
  `AlonzoTxBody` and `AlonzoTx`

## 1.4.0.0

* Add `translateAlonzoTxAuxData`, `translateAlonzoScript` and `translateDatum`
* Deprecated `translateTxOut` in favor of `upgradeTxOut`
* Deprecated `transStakeCred` in favor of `transCred`
* Rename `transShelleyTxCert` to `alonzoTransTxCert`
* Change type of `Plutus` to use `BinaryPlutus` instead of `ShortByteString`
* Deprecate `runPLCScript` in favor of `runPlutusScript`
* Addition of `PlutusWithContext`
* Deprecation of `collectTwoPhaseScriptInputs` in favor of `collectPlutusScriptsWithContext`
* Deprecation of `evalScripts` in favor of `evalPlutusScripts`
* Deprecation of `runPLCScript` in favor of `runPlutusScript`
* Deprecation of `explainPlutusFailure` in favor of `explainPlutusEvaluationError`
* Move `BinaryPlutus` into `cardano-ledger-core` package in `Cardano.Ledger.Language` module
* Change `PlutusScript` constructor of the `AlonzoScript` type. It now accepts the new
  `Plutus` type instead of a `Language` and `ShortByteString`
* Rename `pdSBS` field to `pdPlutusScript` in the `PlutusDebugLang` data type

## 1.3.3.0

* Export `transMintValue`

## 1.3.2.0

* Add implementation for `spendableInputsTxBodyL`

## 1.3.1.1

*

## 1.3.1.0

* Added `Semigroup` and `Monoid` instances to `AlonzoScriptsNeeded`

## 1.3.0.0

* Introduction of `TxCert` and `EraTxCert`
* Add `EraTxCert` and `ShelleyEraTxCert` instances to `AlonzoEra`
* Add `PlutusTxCert`
* Add `unTxCertV1`, `unTxCertV2`, `unTxCertV3`
* Add `EraPlutusContext`
* Add `EraPlutusContext 'PlutusV1` instance to `AlonzoEra`
* Rename `transTxCert` to `transShelleyTxCert`
* Remove `witsVKeyNeeded`, in favor of the one from `cardano-ledger-shelley`
* Fix an issue where `TotalDeposits` didn't appear on Alonzo era

## 1.2.1.0

* Fix `PParams AlonzoEra` serialization. [#3440](https://github.com/input-output-hk/cardano-ledger/pull/3440)

## 1.2.0.0

* Replace `DPState c` with `CertState era`
* Add `TranslateEra` instances for:
  * `DState`
  * `PState`
  * `VState`
* Added support for Plutus V3 in the types and functions that use `Language`.
  (Note that the Alonzo ledger era rules do not allow V3 scripts, however.).
* Fix a bug of converting a mint field to the plutus context: [#3398](https://github.com/input-output-hk/cardano-ledger/pull/3398)
* Change parametrization of `ScriptPurpose`, `CollectError` and `TransactionScriptFailure`
  from `c`rypto to `era`

## 1.1.0.0

* Add `ToJSON` instance for `AlonzoTxOut`, `AlonzoScript` and `Datum`
* Add `ToJSON` instance for `AlonzoPParams StrictMaybe`
* Stop exporting an internal function `decodeBinaryData`
* Remove redundant `Redeemers'` pattern synonym.
* Move `Cardano.Ledger.Alonzo.Tools` module into `cardano-ledger-api:Cardano.Ledger.Api.Scripts`
* Add helper lens `hashDataTxWitsL`
* Rename `smMap` to `cmValues`
* Remove redundant pattern synonym `AlonzoTxAuxData'{atadMetadata',atadTimelock',atadPlutus'}`
* Addition of `costModelToMap`, `costModelFromMap` and `costModelParamNames`
* Made it possible for `FromJSON` to decode `CostModels` both as the new approach:
  1. as a list of cost models values,
  2. and the old approach of mapping from the parameter name to the cost model value

### `testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
