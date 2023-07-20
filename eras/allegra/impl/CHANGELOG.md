# Version history for `cardano-ledger-allegra`

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
  * `DState`
  * `PState`
  * `VState`

## 1.1.0.0

* Remove redundant pattern synonym `AllegraTxAuxData'`
* Hide internal `AllegraTxAuxDataRaw` constructor with `atadrMetadata` and `atadrTimelock`
  record fields.

### `testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
