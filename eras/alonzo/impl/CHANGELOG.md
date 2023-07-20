# Version history for `cardano-ledger-alonzo`

## 1.3.2.0

* Add implementation for `spendableInputsTxBodyL`

## 1.3.1.2

*

## 1.3.1.1

*

## 1.3.1.0

* Added `Semigroup` and `Monoid` instances to `AlonzoScriptsNeeded`

## 1.3.0.0

* Introduction of `TxCert` and `EraTxCert`
* Add `EraTxCert` and `ShelleyEraTxCert` instances to `AlonzoEra`
* Add `PlutusTxCert`
* Add `unTxCertV1`, `unTxCertV2`, `unTxCertV3`
* Add `EraPlutusContext`
* Add `EraPlutusContext 'PlutusV1` instance to `AlonzoEra`
* Rename `transTxCert` to `transShelleyTxCert`
* Remove `witsVKeyNeeded`, in favor of the one from `cardano-ledger-shelley`
* Fix an issue where `TotalDeposits` didn't appear on Alonzo era

## 1.2.1.0

* Fix `PParams AlonzoEra` serialization. [#3440](https://github.com/input-output-hk/cardano-ledger/pull/3440)

## 1.2.0.0

* Replace `DPState c` with `CertState era`
* Add `TranslateEra` instances for:
  * `DState`
  * `PState`
  * `VState`
* Added support for Plutus V3 in the types and functions that use `Language`.
  (Note that the Alonzo ledger era rules do not allow V3 scripts, however.).
* Fix a bug of converting a mint field to the plutus context: [#3398](https://github.com/input-output-hk/cardano-ledger/pull/3398)
* Change parametrization of `ScriptPurpose`, `CollectError` and `TransactionScriptFailure`
  from `c`rypto to `era`

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

### `testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
