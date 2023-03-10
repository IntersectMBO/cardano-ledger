# Version history for `cardano-ledger-allegra`

## 1.1.1.0

* Add `TranslateEra` instances for:
  * `DState`
  * `PState`
  * `VState`
* Add `ShelleyEraDCert` instance for `AllegraEra`

## 1.1.0.0

* Remove redundant pattern synonym `AllegraTxAuxData'`
* Hide internal `AllegraTxAuxDataRaw` constructor with `atadrMetadata` and `atadrTimelock`
  record fields.

### `testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
