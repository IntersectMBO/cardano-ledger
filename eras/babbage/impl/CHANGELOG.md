# Version history for `cardano-ledger-babbage`

## 1.2.0.0

* Replace `DPState c` with `CertState era`
* Add `TranslateEra` instances for:
  * `DState`
  * `PState`
  * `VState`
* Added support for Plutus V3 in the types and functions that use `Language`.
  (Note that the Alonzo and Babbage ledger era rules do not allow V3 scripts, however.).
  Addition of `babbageTxInfoV1` and `babbageTxInfoV2`
* Add `EraDCert` and `ShelleyEraDCert` instances to `BabbageEra`
* Add `EraPlutusContext 'PlutusV1` instance to `BabbageEra`
* Add `EraPlutusContext 'PlutusV2` instance to `BabbageEra`

## 1.1.0.0

* Add `ToJSON` instance for `BabbageTxOut`.
* Add `ToJSON` instance for `BabbagePParams Identity` and `BabbagePParams StrictMaybe`
* Removed validation function `validateOutputTooBigUTxO`, in favor of the same function
  from `cardano-ledger-alonzo`.

###`testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
