# Version history for `cardano-ledger-mary`

## 1.3.1.0

* Add implementation for `spendableInputsTxBodyL`

## 1.3.0.2

*

## 1.3.0.1

*

## 1.3.0.0

* Introduction of `TxCert` and `EraTxCert`
* Add `EraTxCert` and `ShelleyEraTxCert` instances to `MaryEra`

## 1.2.0.0

* Removed `genMintValues`

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
