# Version history for `cardano-ledger-babbage`

## 1.5.1.1

*

## 1.5.1.0

### `testlib`

* Provide CDDL spec files with `readBabbageCddlFileNames` and `readBabbageCddlFiles` from
  `Test.Cardano.Ledger.Babbage.Binary.Cddl`

## 1.5.0.0

* Add `Generic` instance for:
  * `BabbageUtxoPredFailure`
  * `BabbageUtxowPredFailure`
* Add `ToExpr` instance for:
  * `BabbageUtxoPredFailure`
  * `BabbageUtxowPredFailure`
  * `BabbageTxOut`
* Switch to using `AlonzoEraUTxO` in rules
* Deprecate `getDatumBabbage` in favor of `getBabbageSpendingDatum`
* Add `getBabbageSupplementalDataHashes`
* Remove unused `isTwoPhaseScriptAddress`
* Deprecate `babbageTxScripts` and `refScripts`

### `testlib`

* Add `Test.Cardano.Ledger.Babbage.ImpTest`
* Add `EraImpTest` instance for `BabbageEra`

## 1.4.5.0

* Add `ToExpr` instance for:
  * `BabbageTxBody`
  * `BabbageTxOut`
* Add `Generic` instance for `BabbageTxBody`
* Add `BabbageTxUpgradeError` and `BabbageTxBodyUpgradeError`
* Add `EraTransition` instance.

## 1.4.4.0

* Add `EqRaw` instance for `BabbageTxBody`

## 1.4.3.0

* Deprecate `translateTxOut`
* Added `babbagePParamsHKDPairs`

## 1.4.2.0

* Fix `mint` field Plutus translation bug.

## 1.4.1.0

* Add implementation for `spendableInputsTxBodyL`

## 1.4.0.0

* Added a protocol version constraint to:
  * `STS (BabbageUTXOS era)` instance
  * `STS (BabbageUTXOW era)` instance
  * `utxosTransition`
  * `babbageUtxowTransition`

## 1.3.0.0

* Introduction of `TxCert` and `EraTxCert`
* Add `EraTxCert` and `ShelleyEraTxCert` instances to `BabbageEra`
* Add `EraPlutusContext 'PlutusV1` instance to `BabbageEra`
* Add `EraPlutusContext 'PlutusV2` instance to `BabbageEra`
* Fix an issue where `TotalDeposits` didn't appear on Babbage era

## 1.2.1.0

* Fix `PParams BabbageEra` serialization. [#3440](https://github.com/input-output-hk/cardano-ledger/pull/3440)

## 1.2.0.0

* Replace `DPState c` with `CertState era`
* Add `TranslateEra` instances for:
  * `DState`
  * `PState`
  * `VState`
* Added support for Plutus V3 in the types and functions that use `Language`.
  (Note that the Alonzo and Babbage ledger era rules do not allow V3 scripts, however.).
  Addition of `babbageTxInfoV1` and `babbageTxInfoV2`

## 1.1.0.0

* Add `ToJSON` instance for `BabbageTxOut`.
* Add `ToJSON` instance for `BabbagePParams Identity` and `BabbagePParams StrictMaybe`
* Removed validation function `validateOutputTooBigUTxO`, in favor of the same function
  from `cardano-ledger-alonzo`.

###`testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
