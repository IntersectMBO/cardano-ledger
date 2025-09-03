# Version history for `cardano-ledger-babbage`

## 1.12.0.0

* Hide `Cardano.Ledger.Babbage.Translation` module
* Fixed `FromJSON` instance for `TransitionConfig BabbageEra`
* Added `COMPLETE` pragma for `TxCert BabbageEra`
* Added `COMPLETE` pragma for `NativeScript BabbageEra`
* Move to `testlib` the `DecCBOR` instance for `TxBody BabbageEra`
* Remove `BabbageNonDisjointRefInputs` for protocol versions >10
* Added `ppCoinsPerUTxOByte` to `PParams`
* Removed `babbagePParamsHKDPairs` and `babbageCommonPParamsHKDPairs` from `PParams`
* Remove `BabbageTxBody`
* Removed `era` parameter from `BabbageTxBodyRaw`
* Expose access to `BabbageTxBodyRaw`
* Expose constructor `MkBabbageTxBody`
* Deprecate `spendInputs'`, `collateralInputs'`, `referenceInputs'`, `outputs'`, `collateralReturn'`, `totalCollateral'`, `certs'`, `txfee'`, `withdrawals'`, `vldt'`, `update'`, `reqSignerHashes'`, `adHash'`, `mint'`, `scriptIntegrityHash'`, `spendInputs'`, `txnetworkid'`.

### `testlib`

* Added `EraSpecificSpec BabbageEra` instance
* Added `Examples` module with: `ledgerExamples`, `exampleBabbageNewEpochState`, `exampleCollateralOutput`
* Added `Twiddle` instances for Babbage core types
* Added `TxInfoSpec` (moved from `cardano-ledger-babbage-test`)
* Added `Arbitrary` instance for `TransitionConfig BabbageEra`
* Added `Era` module with `BabbageEraTest` class

## 1.11.0.0

* Add `DecCBOR` instance for `BabbageTxBody`
* Converted `CertState` to a type family
* Made the fields of predicate failures and environments lazy
* Add `MemPack` instance for `BabbageTxOut` and `PlutusScript BabbageEra`
* Deprecate `Babbage` type synonym
* Remove crypto parametrization from `BabbageEra`

### `testlib`

* Add `babbageFixupTx`

## 1.10.1.0

* Use `Mismatch` to clarify predicate failures. #4711

### `testlib`

* Switch to using `ImpSpec` package

## 1.10.0.0

* Add `ProtVer` argument to `TxInfo` functions:
  * `transTxRedeemers`
  * `toPlutusV2Args`

### `testlib`

* Move `TxInfo` golden tests over from the older `-test` package. #4599
  * Also move the `gen-golden` executable over.

## 1.9.0.0

* Change instance of `TranslationContext` to `NoGenesis`

## 1.8.2.0

### `testlib`

* Add `produceRefScript` and `produceRefScripts`

## 1.8.1.0

* Add `AllegraEraScript` and `ShelleyEraScript` instances for `BabbageEra`

## 1.8.0.0

* Make `utxoTransition` more general
* Change the return type of `collAdaBalance` to `DeltaCoin`
* Change the type of the provided collateral field in `IncorrectTotalCollateralField` to `DeltaCoin`
* Change the type of `validateCollateralEqBalance`

## 1.7.0.0

* Add type `EraRuleEvent` instances for the event type of:
  * `UTXOS`
  * `PPUP`
  * `LEDGER`
  * `TICK`
* Add instances for `InjectRuleFailure` and switch to using `injectFailure`
* Add `NFData` instance for `BabbageUtxoPredFailure`, `BabbageUtxowPredFailure`
* Add implementation for `getMinFeeTxUtxo`
* Add `getReferenceScriptsNonDistinct`
* Add the constructor `BabbageNonDisjointRefInputs` to `BabbageUtxoPredFailure`
  * Utxo rule raises that `PredicateFailure` in Conway and future Eras when they are not disjoint.
* Modify `PParams` JSON instances to match `cardano-api`

### `testlib`

* Add `RuleListEra` instance for Babbage

## 1.6.0.0

* Remove deprecated `getDatumBabbage`, `babbageTxScripts`, `refScripts`
* Add `ToJSON` for `BabbageContextError`
* `FromJSON` instance for `PParamsUPdate`: #3949
* Fixed JSON serialization of `PParams` to include the `protocolVersion`: #3953
* Stop exporting all of the internal `hkd*` functions and `PParamsHKD` from
  `Cardano.Ledger.Babbage.Core`.
* Stop exporting `AllegraEraTxBody`, `AlonzoEraTxBody`,`AlonzoTxBody`,
  `MaryEraTxBody`,`ShelleyEraTxBody`, `addrEitherBabbageTxOutL`,
  `valueEitherBabbageTxOutL`, `dataHashBabbageTxOutL`, `dataBabbageTxOutL`,
  `datumBabbageTxOutL`, `referenceScriptBabbageTxOutL`, `getDatumBabbageTxOut`, `Datum`
  `mkBabbageTxBody`, `inputsBabbageTxBodyL`, `outputsBabbageTxBodyL`, `feeBabbageTxBodyL`,
  `auxDataHashBabbageTxBodyL`, `mintedBabbageTxBodyF`, `mintValueBabbageTxBodyF`,
  `withdrawalsBabbbageTxBodyL`, `notSupportedInThisEraL`, `updateBabbageTxBodyL`,
  `certsBabbageTxBodyL`, `vldtBabbageTxBodyL`, `mintBabbageTxBodyL`,
  `collateralInputsBabbageTxBodyL`, `reqSignerHashesBabbageTxBodyL`,
  `scriptIntegrityHashBabbageTxBodyL`, `networkIdBabbageTxBodyL`,
  `sizedOutputsBabbageTxBodyL`, `referenceInputsBabbageTxBodyL`,
  `totalCollateralBabbageTxBodyL`, `collateralReturnBabbageTxBodyL`,
  `sizedCollateralReturnBabbageTxBodyL` from `Cardano.Ledger.Babbage.TxBody`
* Remove `txInfoOutV1`, `txInfoOutV2`, `txInfoInV1` and `txInfoInV2`.
* Add `transTxOutV1`, `transTxOutV2`, `transTxInInfoV1`, `transTxInInfoV2` and `transTxRedeemers`
* Remove `babbageScriptPrefixTag`
* Moved `ToExpr` instances out of the main library and into the testlib.
* Change the type of `ppEMaxL`

## 1.5.1.0

### `testlib`

* Provide CDDL spec files with `readBabbageCddlFileNames` and `readBabbageCddlFiles` from
  `Test.Cardano.Ledger.Babbage.Binary.Cddl`

## 1.5.0.0

* Add `Generic` instance for:
  * `BabbageUtxoPredFailure`
  * `BabbageUtxowPredFailure`
* Add `ToExpr` instance for:
  * `BabbageUtxoPredFailure`
  * `BabbageUtxowPredFailure`
  * `BabbageTxOut`
* Switch to using `AlonzoEraUTxO` in rules
* Deprecate `getDatumBabbage` in favor of `getBabbageSpendingDatum`
* Add `getBabbageSupplementalDataHashes`
* Remove unused `isTwoPhaseScriptAddress`
* Deprecate `babbageTxScripts` and `refScripts`

### `testlib`

* Add `Test.Cardano.Ledger.Babbage.ImpTest`
* Add `EraImpTest` instance for `BabbageEra`

## 1.4.5.0

* Add `ToExpr` instance for:
  * `BabbageTxBody`
  * `BabbageTxOut`
* Add `Generic` instance for `BabbageTxBody`
* Add `BabbageTxUpgradeError` and `BabbageTxBodyUpgradeError`
* Add `EraTransition` instance.

## 1.4.4.0

* Add `EqRaw` instance for `BabbageTxBody`

## 1.4.3.0

* Deprecate `translateTxOut`
* Added `babbagePParamsHKDPairs`

## 1.4.2.0

* Fix `mint` field Plutus translation bug.

## 1.4.1.0

* Add implementation for `spendableInputsTxBodyL`

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

* Fix `PParams BabbageEra` serialization. [#3440](https://github.com/intersectmbo/cardano-ledger/pull/3440)

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

### `testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
