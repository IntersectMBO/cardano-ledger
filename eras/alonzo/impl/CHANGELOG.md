# Version history for `cardano-ledger-alonzo`

## 1.7.0.1

*

## 1.7.0.0

* Add type `EraRuleEvent` instances for `PPUP`, `LEDGER` and `TICK` events
* Add `Eq` and `NFData` instances for `AlonzoUtxoEvent`, `AlonzoUtxowEvent` and `AlonzoUtxosEvent`
* Add instances for `InjectRuleFailure` and switch to using `injectFailure`
* Add `allegraToAlonzoUtxoPredFailure`
* Add `NFData` instances for:
  * `CollectError`
  * `AlonzoUtxoPredFailure`
  * `FailureDescription`
  * `TagMismatchDescription`
  * `AlonzoUtxosPredFailure`
  * `AlonzoUtxowPredFailure`
* Add `Semigroup` and `Monoid` instances for `Redeemers`
* Add `alonzoScriptPrefixTag`
* Add implementation for `getMinFeeTxUtxo`
* Deprecated `indexRedeemers` and `redeemerPointer`.
* Add `lookupRedeemer`
* Rename `AsIndex` to `AsIx` and `upgradePlutusPurposeAsIndex` to `upgradePlutusPurposeAsIx`
* Add `AsIxItem` as well as `toAsItem`, `toAsIx`.
* Switch `PlutusPurpose` to `AsIxItem` in `AlonzoScriptsNeeded`
* Add `hoistPlutusPurpose`, `toSpendingPurpose`, `toMintingPurpose`,
  `toCertifyingPurpose`, `toRewardingPurpose`, `fromSpendingPurpose`,
  `fromMintingPurpose`, `fromCertifyingPurpose`, `fromRewardingPurpose` to `AlonzoEraScript`
* Add `SpendingPurpose`, `MintingPurpose`, `CertifyingPurpose`, `RewardingPurpose` pattern synonyms.
* Add `getSpendingScriptsNeeded`, `getRewardingScriptsNeeded`, `getMintingScriptsNeeded`
* Add `zipAsIxItem`
* Modify `PParams` JSON instances to match `cardano-api`

### `testlib`

* Add `ToExpr` instances for `AlonzoUtxoEvent`, `AlonzoUtxowEvent` and `AlonzoUtxosEvent`
* Add `RuleListEra` instance for Alonzo
* Add:
  * `impLookupPlutusScriptMaybe`
  * `fixupOutputDatums`
* Remove root coin argument from `initAlonzoImpNES`
* Rename `fixupPlutusScripts` to `fixupRedeemers`
* Add:
  * `fixupPPHash`
  * `fixupPlutusScripts`
  * `addCollateralInput`
  * `impGetPlutusContexts`
  * `fixupDatums`
* Add `Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec`
* Add `alonzoFixupTx`

## 1.6.0.0

* Deprecated `getAlonzoSpendingTxIn` in favor of `plutusPurposeSpendingTxIn`
* Add `PlutusPurpose`, `plutusPurposeSpendingTxIn` and `upgradePlutusPurposeAsIndex` to
  `AlonzoEraScript`.
* Add `AsIndex`, `AsItem` and `AlonzoPlutusPurpose`
* Remove `RdmrPtr` and `Tag` in favor of `PlutusPurpose AsIndex`
* Remove `ScriptPurpose` in favor of `PlutusPurpose AsItem`
* Add `AlonzoTxAuxData'`, `atadMetadata'`, `atadTimelock'`, `atadPlutus'`
* Add `alonzoRedeemerPointer`, `alonzoRedeemerPointerInverse`
* Remove `rdptr` and `rdptrInv` in favor of `redeemerPointer` and `redeemerPointerInverse`
  respectively
* Rename `indexedRdmrs` to `indexRedeemers` and change the type of one of its arguments
* Deprecate `requiredSignersAreWitnessed`
* Add `ToJSON` for `AlonzoContextError` and `CollectError`
* Stop exporting all of the internal `hkd*` functions and `PParamsHKD` from
  `Cardano.Ledger.Alonzo.Core`.
* Export `AlonzoEraTx` from `Cardano.Ledger.Alonzo.Core`
* Add `transTxBodyId`, `transTxBodyCerts`, `transTxBodyWithdrawals`, `transTxBodyReqSignerHashes`,
  `transTxWitsDatums`
* Remove deprecated `VersionedTxInfo`,  `getDatum`, `getAllowedSupplimentalDataHashes`, `txscripts`
* Remove `ExtendedUTxO` in favor of `PlutusEraTxInfo` and `txInfo` in favor of `toPlutusTxInfo`
* Rename `alonzoTransTxCert` to `transTxCert`
* Remove `alonzoTxInfo` and `languages` as unnecessary.
* Remove `transTxOutAddr` in favor of `transAddr`
* Remove `txInfoIn'`, `txInfoIn` as unnecessary
* Rename `txInfoOut` to `transTxOut` for consistency
* Replace `valContext` with `toPlutusContext`,
* Stop exporting from `Cardano.Ledger.Alonzo.Plutus.TxInfo`: `getWitVKeyHash`,
  `transKeyHash`, `transKeyHash`, `transSafeHash`, `transStakeReference`,
  `slotToPOSIXTime`, `transCred`, `transProtocolVersion`, `validScript`, `transDataHash'`,
  `transDataHash`, `transExUnits`, `ScriptFailure`, `ScriptResult`, `scriptPass`,
  `scriptFail`, `PlutusDebugLang`, `PlutusDebug`, `PlutusData`, `PlutusError`,
  `PlutusDebugInfo`, `EraPlutusContext`, `PlutusWithContext`, `PlutusTxCert`,
  `unTxCertV1`, `unTxCertV2`, `unTxCertV3`, `debugPlutus`, `runPlutusScript`,
  `runPlutusScriptWithLogs`, `deserialiseAndEvaluateScript` and
  `explainPlutusEvaluationError`.
* Add new module `Cardano.Ledger.Alonzo.Plutus.Context` that contains new type classes for
  plutus context translation: `EraPlutusTxInfo` and `EraPlutusContext` as well as the type
  families for plutus specific types: `PlutusTxInfo`, `PlutusTxCert`,
  `PlutusScriptPurpose`, `PlutusScriptContext`
* Remove deprecated module `Cardano.Ledger.Alonzo.TxInfo`
* Add `lookupPlutusScript` and `transLookupTxOut`
* Add `transValidityInterval`
* Remove `transVITime` in favor of `transValidityInterval`.
* Remove deprecated `scriptsNeeded`, `scriptsNeededFromBody`, `evalScripts`,
  `evalScriptsWithLogs`, `collectTwoPhaseScriptInputs` `getDatumAlonzo`, `getSpendingTxIn`.
* Remove `language` in favor of two separate functions `plutusScriptLanguage` and `toPlutusScript`
* Remove  `knownToNotBe1Phase` in favor of `lookupPlutusScript`.
* Rename `Cardano.Ledger.Alonzo.PlutusScriptApi` to `Cardano.Ledger.Alonzo.Plutus.Evaluate`
* Turn `TranslationError` sum type into a `ContextError` data family
* Add `AlonzoEraScript` type class with associated data family `PlutusScript` and functions:
  `eraMaxLanguage`, `toPlutusScript`, `fromplutusScript`, `mkPlutusScript` and `withPlutusScript`
* Add `withPlutusScriptLanguage`, `plutusScriptLanguage`, `decodePlutusScript`,
  `plutusScriptBinary`, `mkBinaryPlutusScript`, `isValidPlutusScript` and
  `toPlutusSLanguage`
* Remove `Cardano.Ledger.Alonzo.TxInfo` module
* Remove `ExtendedUTxO`
* Instead of accepting `UTxO` the validation function `ppViewHashesMatch` now accepts
  `ScriptsProvided` and remove the no longer needed `languages` function.
* Delete `utxoPredFailMaToAlonzo`, `utxoPredFailShelleyToAlonzo`
* Moved `ToExpr` instances out of the main library and into the testlib.
* Changed the type of the ConwayPParams field appEMax
* Add `TxUTxODiff (UTxO era) (UTxO era)` inhabitant to the `AlonzoUtxosEvent era` data type.

### `testlib`

* Added `ShelleyEraImp` constraint to `emptyAlonzoImpNES`
* Rename `emptyAlonzoImpNES` to `initAlonzoImpNES`
* Remove `Test.Cardano.Ledger.Alonzo.CostModel` in favor of `Test.Cardano.Ledger.Plutus`
* Move `costModelParamsCount` to `Cardano.Ledger.Plutus.CostModels`
* Add `mkPlutusScript'`
* Add `alwaysSucceedsLang` and `alwaysFailsLang`
* Change `alwaysSucceeds` and `alwaysFails` to accept the language version at the type level.
* Export `genNativeScript` and `genPlutusScript` from Arbitrary

## 1.5.1.0

* Move `Cardano.Ledger.Alonzo.Scripts.Data` to `Cardano.Ledger.Plutus.Data`
* Deprecated `Cardano.Ledger.Alonzo.Language`
* Move `Cardano.Ledger.Alonzo.TxInfo` to `Cardano.Ledger.Alonzo.Plutus.TxInfo` with
  deprecation of the former.
* Add `evalScriptsWithLogs`, `evalPlutusScriptsWithLogs` and `runPlutusScriptWithLogs`: #3843

### `testlib`

* Provide CDDL spec files with `readAlonzoCddlFileNames` and `readAlonzoCddlFiles` from
  `Test.Cardano.Ledger.Alonzo.Binary.Cddl`
* Expose `genValidAndUnknownCostModels` in `Arbitrary`

## 1.5.0.0

* Add `Test.Cardano.Ledger.Alonzo.CostModel`
* Add `updateCostModels`
* Add `AlonzoEraUTxO` and use in the rules.
* Deprecate `getDatum` in favor of `getSpendingDatum`
* Deprecate `getDatumAlonzo` in favor of `getAlonzoSpendingDatum`
* Deprecate `getSpendingTxIn` in favor of `getAlonzoSpendingTxIn`
* Deprecate `getAllowedSupplimentalDataHashes` in favor of `getSupplementalDataHashes`
* Rename `NonOutputSupplimentaryDatums` to `NotAllowedSupplementalDatums`
* Remove unused `isTwoPhaseScriptAddress`
* Change arguments to `hasExactSetOfRedeemers`

### `testlib`

* Add `emptyAlonzoImpNES`
* Add `Test.Cardano.Ledger.Alonzo.ImpTest`
* Add `EraImpTest` instance for `AlonzoEra`
* Add `ToExpr` instances for:
  * `CollectError`
  * `AlonzoUtxoPredFailure`
  * `FailureDescription`
  * `TagMismatchDescription`
  * `AlonzoUtxosPredFailure`
  * `AlonzoUtxowPredFailure`
  * `BinaryData`
  * `Datum`
  * `IsValid`
  * `ScriptPurpose`
  * `TxOutSource`
  * `TranslationError`
  * `Addr28Extra`
  * `DataHash32`
  * `RdmrPtr`
* Expose `genValidCostModel` in `Arbitrary`

## 1.4.2.0

* Add `ToExpr` instance for:
  * `PlutusData`
  * `Data`
  * `BinaryData`
  * `Datum`
  * `AlonzoTx`
  * `AlonzoTxBody`
  * `AlonzoTxOut`
  * `AlozoTxWits`
  * `IsValid`
  * `Addr28Extra`
  * `DataHash32`
  * `RdmrPtr`
  * `Redeemers`
* Add `Generic` instance for :
  * `AlonzoTxBody`
  * `Redeemers`
  * `TxDats`
* Add `upgradeData`, `upgradeRedeemers` and `upgradeTxDats`
* Add `TxUpgradeError` type to `EraTx`
* Add `AlonzoTxBodyUpgradeError`, `AlonzoTxUpgradeError`
* Add `toAlonzoTransitionConfigPairs` and `EraTransition` instance.
* Rename `alonzoGenesisAesonPairs` -> `toAlonzoGenesisPairs` for consistency.

### `testlib`

* Add `Test.Cardano.Ledger.Alonzo.Binary.RoundTrip` module with:
  * `roundTripAlonzoCommonSpec`
  * `roundTripAlonzoEraTypesSpec`

## 1.4.1.0

* Made `isPlutusScript` more general.
* Add `alonzoEqTxRaw` and `alonzoEqTxWitsRaw`
* Add `EqRaw` instance for `AlonzoScript`, `AlonzoTxWits`, `AlonzoTxAuxData`,
  `AlonzoTxBody` and `AlonzoTx`

## 1.4.0.0

* Add `translateAlonzoTxAuxData`, `translateAlonzoScript` and `translateDatum`
* Deprecated `translateTxOut` in favor of `upgradeTxOut`
* Deprecated `transStakeCred` in favor of `transCred`
* Rename `transShelleyTxCert` to `alonzoTransTxCert`
* Change type of `Plutus` to use `BinaryPlutus` instead of `ShortByteString`
* Deprecate `runPLCScript` in favor of `runPlutusScript`
* Addition of `PlutusWithContext`
* Deprecation of `collectTwoPhaseScriptInputs` in favor of `collectPlutusScriptsWithContext`
* Deprecation of `evalScripts` in favor of `evalPlutusScripts`
* Deprecation of `runPLCScript` in favor of `runPlutusScript`
* Deprecation of `explainPlutusFailure` in favor of `explainPlutusEvaluationError`
* Move `BinaryPlutus` into `cardano-ledger-core` package in `Cardano.Ledger.Language` module
* Change `PlutusScript` constructor of the `AlonzoScript` type. It now accepts the new
  `Plutus` type instead of a `Language` and `ShortByteString`
* Rename `pdSBS` field to `pdPlutusScript` in the `PlutusDebugLang` data type

## 1.3.3.0

* Export `transMintValue`

## 1.3.2.0

* Add implementation for `spendableInputsTxBodyL`

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

* Fix `PParams AlonzoEra` serialization. [#3440](https://github.com/intersectmbo/cardano-ledger/pull/3440)

## 1.2.0.0

* Replace `DPState c` with `CertState era`
* Add `TranslateEra` instances for:
  * `DState`
  * `PState`
  * `VState`
* Added support for Plutus V3 in the types and functions that use `Language`.
  (Note that the Alonzo ledger era rules do not allow V3 scripts, however.).
* Fix a bug of converting a mint field to the plutus context: [#3398](https://github.com/intersectmbo/cardano-ledger/pull/3398)
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
