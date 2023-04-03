# Version history for `cardano-ledger-alonzo`

## 1.1.0.0

* Add `ToJSON` instance for `AlonzoTxOut`, `AlonzoScript` and `Datum`
* Add `ToJSON` instance for `AlonzoPParams StrictMaybe`
* Stop exporting an internal function `decodeBinaryData`
* Remove redundant `Redeemers'` pattern synonym.
* Move `Cardano.Ledger.Alonzo.Tools` module into `cardano-ledger-api:Cardano.Ledger.Api.Scripts`
* Add helper lens `hashDataTxWitsL`
* Rename `smMap` to `cmValues`
* Remove redundant pattern synonym `AlonzoTxAuxData'{atadMetadata',atadTimelock',atadPlutus'}`


###`testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
