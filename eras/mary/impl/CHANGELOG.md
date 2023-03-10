# Version history for `cardano-ledger-mary`

## 1.1.1.0

* Add `TranslateEra` instances for:
  * `DState`
  * `PState`
  * `VState`
* Add `EraDCert`, `ShelleyEraDCert` instances to `MaryEra`

## 1.1.0.0

* Addition of `ToJSON` instances for `AssetName`, `PolicyID`, `MultiAsset` and `MaryValue`.
* Add `ToJSONKey`/`FromJSONKey` instances for `PolicyID`

### `testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
