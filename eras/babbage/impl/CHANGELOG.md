# Version history for `cardano-ledger-babbage`

## 1.4.1.0

* Add implementation for `spendableInputsTxBodyL`

## 1.4.0.1

*

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
