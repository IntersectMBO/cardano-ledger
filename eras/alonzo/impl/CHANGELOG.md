# Version history for `cardano-ledger-alonzo`

## 1.1.1.0

* Fix a bug of converting a mint field to the plutus context: [#3398](https://github.com/input-output-hk/cardano-ledger/pull/3398)

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

###`testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
