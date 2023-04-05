# Version history for `cardano-ledger-babbage`

## 1.2.0.0

* Replace `DPState c` with `CertState era`
* Add `TranslateEra` instances for:
  * `DState`
  * `PState`
  * `VState`

## 1.1.0.0

* Add `ToJSON` instance for `BabbageTxOut`.
* Add `ToJSON` instance for `BabbagePParams Identity` and `BabbagePParams StrictMaybe`
* Removed validation function `validateOutputTooBigUTxO`, in favor of the same function
  from `cardano-ledger-alonzo`.

###`testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
