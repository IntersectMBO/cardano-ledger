# Version history for `cardano-ledger-mary`

## 1.10.0.0

* Add `burnedMultiAssets`
* Add `TxLevel` argument to `Tx` and `TxBody`
* Add `HasEraTxLevel` instances for `Tx` and `TxBody`
* Add `EraTxLevel` instance

## 1.9.0.0

* Hide `Cardano.Ledger.Mary.Translation` module
* Added `COMPLETE` pragma for `TxCert MaryEra`
* Added `COMPLETE` pragma for `NativeScript MaryEra`
* Move to `testlib` the `DecCBOR` instance for `TxBody MaryEra`
* Remove `MaryTxBody`
* Converted `MaryTxBodyRaw` into a type synonym
* Expose constructor `MkMaryTxBody`
* Add default implementation for `mintValueTxBodyF`

### `testlib`

* Added `EraSpecificSpec MaryEra` instance
* Added `Examples` module with: `ledgerExamples`, `exampleMultiAssetValue`
* Added `Arbitrary` instance for `TransitionConfig MaryEra`
* Added `Era` module with `MaryEraTest` class

## 1.8.0.0

* Add `DecCBOR` instance for `MaryTxBody`
* Converted `CertState` to a type family
* Add `MemPack` instance for `CompactValue` and `CompactForm MaryValue`
* Deprecate `Mary` type synonym
* Remove crypto parametrization from `MaryEra`

## 1.7.1.0

### `testlib`

* Add `mkTokenMintingTx`
* Switch to using `ImpSpec` package

## 1.7.0.1

*

## 1.7.0.0

* Change instance of `TranslationContext` to `NoGenesis`

## 1.6.1.0

* Add `AllegraEraScript` and `ShelleyEraScript` instances for `MaryEra`

## 1.6.0.0

* Change how we report tokens burned in `ValueNotConservedUTxO`: #4288
* Add `getProducedMaryValue`, `pruneZeroMultiAsset`, `filterMultiAsset` and `mapMaybeMultiAsset`
* Deprecate `prune`
* Rename `assetName` to `assetNameBytes`

### `testlib`

* Add `MaryEraImp`

## 1.5.1.0

* Add type `EraRuleEvent` instances for the event type of `TICK` and `LEDGER`
* Add instances for `InjectRuleFailure` and switch to using `injectFailure`
* Add implementation for `getMinFeeTxUtxo`

### `testlib`

* Add `RuleListEra` instance for Mary

## 1.5.0.0

* Change return type of `mintedTxBodyF` to `Set PolicyID`
* Stop exporting all of the internal `hkd*` functions and `PParamsHKD` from
  `Cardano.Ledger.Mary.Core`.
* Stop exporting `ValidityInterval` and `StrictMaybe` from `Cardano.Ledger.Mary.TxBody`
* Moved `ToExpr` instances out of the main library and into the testlib.
* Remove deprecated `translateValue` and `translateCompactValue`

## 1.4.0.0

* Switch `MaryValue` field for ADA from `Integer` to `Coin`
* Make sure that `getConsumedMaryValue` can also handle `DRep` deposits. This is safe for
  all pre-Conway eras and useful for Conway onwards eras.

### `testlib`

* Provide CDDL spec files with `readMaryCddlFileNames` and `readMaryCddlFiles` from
  `Test.Cardano.Ledger.Mary.Binary.Cddl`

## 1.3.5.0

* Add `ToExpr` instance for `CompactValue`
* Implement `getScriptsProvided`

### `testlib`

* Add `Test.Cardano.Ledger.Mary.ImpTest`
* Add `EraImpTest` instance for `MaryEra`

## 1.3.4.0

* Add `ToExpr` instance for:
  - `MaryTxBody`
  - `CompactForm (MaryValue)`
  - `CompactValue`
* Add `Generic` instance for `CompactValue`
* Add `EraTransition` instance.

## 1.3.3.0

* Add `EqRaw` instance for `MaryTxBody`

## 1.3.2.0

* Deprecate `translateValue` and `translateCompactValue`

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
  - `DState`
  - `PState`
  - `VState`
* Add `EraDCert`, `ShelleyEraDCert` instances to `MaryEra`

## 1.1.0.0

* Addition of `ToJSON` instances for `AssetName`, `PolicyID`, `MultiAsset` and `MaryValue`.
* Add `ToJSONKey`/`FromJSONKey` instances for `PolicyID`

### `testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
