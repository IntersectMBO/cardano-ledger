# Version history for `cardano-ledger-core`

## 1.11.0.1

*

## 1.11.0.0

* Add `EraRuleEvent`, `InjectRuleEvent`
* Add `NFData` instance for `Obligations`, `PlutusWithContext`, `PlutusDatums`
* Add `Eq`, `Show`, `Generic` instances for `PlutusDatums`
* Add `CommitteeAuthorization` and use it to represent hot key credential in `CommitteeState`
* Change `applySTSValidateSuchThat` and `applySTSNonStatic`
  to use `NonEmpty (PredicateFailure _)` instead of `[PredicateFailure _]`
* Added `boom` placeholder
* Add `EraRuleFailure` type family and `InjectRuleFailure` type class
* Add `VoidFailure`
* Fix `ToJSON`/`FromJSON` instances of `Prices`
* Update `FromJSON` instance of `BoundedRatio`
* Fix `ToJSON` instance of `BoundedRatio` to avoid precision loss
* Add `pwcScriptHash` field to `PlutusWithContext`
* Parameterize `PlutusWithContext c` on crypto.
* Extract `hashScript` outside of `EraScript` type class.
* Add `plutusLanguageTag` and `hashPlutusScript`
* Add `setMinFeeTxUtxo`
* Add `getMinFeeTxUtxo` to `EraUTxO`
* Change signature by adding `refScriptsSize` parameter for:
  * `getMinFeeTx` in `EraTx`
  * `setMinFeeTx`
  * `estimateMinFeeTx`
* Add `originalBytesSize` and default implementation to `SafeToHash` typeclass
* Rename `RewardAccount` fields `getRwdNetwork` and `getRwdCred` to `raNetwork` and `raCredential` respectively
* Deprecate `ppRewardAcnt` in favor of `ppRewardAccount`
* Deprecate `fromCborRewardAcnt` in favor of `fromCborRewardAccount`
* Deprecate `decodeRewardAcnt` in favor of `decodeRewardAccount`
* Deprecate `putRewardAcnt` in favor of `putRewardAccount`
* Deprecate `deserialiseRewardAcnt` in favor of `deserialiseRewardAccount`
* Deprecate `serialiseRewardAcnt` in favor of `serialiseRewardAccount`
* Deprecate `RewardAcnt` in favor of `RewardAccount`
* Modify `Prices` and `Nonce` JSON instances to match `cardano-api`

### `testlib`

* Add `shouldContainExpr`
* Add `EraRuleProof`, `UnliftRules`, `roundTripAllPredicateFailures`
* Add `Test.Cardano.Ledger.Plutus.ScriptTestContext`
* Add `genByronVKeyAddr`, `genByronAddrFromVKey`
* Add `Uniform` instances for `ByronKeyPair` and `KeyPair`
* Add `mkKeyPairWithSeed` and `mkBootKeyPairWithSeed`
* Remove `epsilonMaybeEq` from `Utils`
* Add `PlutusArgs`, `ScriptTestContext`
* Add `txInAt`
* Add `mkScriptAddr`
* Deprecate `deserialiseRewardAcntOld` in favor of `deserialiseRewardAccountOld`
* Deprecate `mkVKeyRwdAcnt` in favor of `mkVKeyRewardAccount`
* Add `Stateful` random interface: `HasStatefulGen`, `HasGenEnv`, `HasSubState`,
  `StatefulGen`, `StateGen`, `StateGenM`, `uniformM`, `uniformRM`, `uniformListM`,
  `uniformListRM`, `uniformByteStringM`, `uniformShortByteStringM`
* Re-export the whole of `quickcheck-classes` from `Imp.Common`
* Re-export `replicateM_` and `replicateM` from `Imp.Common`

## 1.10.0.0

* Add `umElemsL`
* Add `txIdTxBody` and `txIdTx`. Deprecated `txid`.
* Add `getGenesisKeyHashCountTxBody` to `EraTxBody`
* Add `calcMinFeeTx` and `calcMinFeeTxNativeScriptWits`, `estimateMinFeeTx`,
  `addDummyWitsTx` and move `setMinFeeTx` to `Cardano.Ledger.Tools`
* Add `kindObject`
* Add `ToJSON` instance for `TxOutSource` and `PoolCert`
* Expose `txInToText`
* Add `getWitsVKeyNeeded` to `EraUTxO`
* Changed the type of the lenses `ppMaxBBSizeL`, `ppMaxTxSizeL`, `ppMaxBHSizeL`,
  `ppuMaxBBSizeL`, `ppuMaxTxSizeL` and `ppuMaxBHSizeL`
* Add `txInsFilter`
* Fix `ToJSON`/`FromJSON` for `CostModels`. Make sure that `CostModels` can roundtrip
  through JSON. Also report `CostModels` failures in JSON.
* Add `costModelParamsCount`
* Move Plutus related `Arbitrary` instances: `ExUnits`, `CostModels`,
  `FlexibleCostModels`, `CostModel`, `Prices`, `Language`
* Add `Cardano.Ledger.Plutus` that re-exports all plutus related functionality
* Add `Semigroup` and `Monoid` instances for `CostModels`
* Add `mkCostModels` and expose `flattenCostModels`
* Stop exporting `CostModels` constructor in order to improve safety.
* Fix `Eq`/`Ord` and add `EncCBOR`/`DecCBOR` instances for `CostModelError`
* Re-export `Cardano.Ledger.Keys.Bootstrap` and `Cardano.Ledger.Keys.WitsVKey` from
  `Cardano.Ledger.Keys`
* Add `unData`, `getCostModelEvaluationContext`
* Changes to `Cardano.Ledger.Plutus.Evaluate`:
  * Make `PlutusWithContext` era agnostic, but Language aware. `pwcScript` can be either
    in decoded or binary format. `pwcProtocolVersion` was added too.
  * `debugPlutus`, `runPlutusScript`, `runPlutusScriptWithLogs` and
      `explainPlutusEvaluationError` no longer accept `ProtVer` as argument, since major
      protocol version has been added to `PlutusWithContext`
  * Change constructor of `ScriptFailure` from `PlutusSF` to `ScriptFailure` and add
    record names: `scriptFailureMessage` and `scriptFailurePlutus`
  * Remove `PlutusDebugLang`, `PlutusDebug`, `PlutusData` and `PlutusError`
  * Stop re-exporting   removed:   `EraPlutusContext`,  `PlutusTxCert`,   `unTxCertV1`,
    `unTxCertV2` and `unTxCertV3`
  * Add `evaluatePlutusWithContext`
  * Remove `deserialiseAndEvaluateScript` in favor of `evaluatePlutusWithContext`
* Changes to `Cardano.Ledger.Plutus.Language`:
  * Rename `BinaryPlutus` to `PlutusBinary` for consistency with names of other Plutus types.
  * Add `Plutus` with helpers: `decodeWithPlutus`, `isValidPlutus`
  * Add `PlutusRunnable` with helpers: `plutusFromRunnable`
  * Add `asSLanguage`
  * Add `plutusSLanguage` and `plutusLanguage`
  * Deprecated `fromSLanguage` in favor of more general `plutusLanguage`
  * Rename `IsLanguage` class to `PlutusLanguage` and add these functions to the class:
    `decodePlutusRunnable`, `evaluatePlutusRunnable` and `evaluatePlutusRunnableBudget`
* Changes to `Cardano.Ledger.Plutus.TxInfo`:
  * `NFData` instance for `TxOutSource`
  * Remove: `transDataHash'`, `transHash` (use `hashToBytes` instead), `transTxOutAddr`,
    `txInfoIn'`, `getWitVKeyHash`, `VersionedTxInfo`, `EraPlutusContext`, `PlutusTxCert`,
    `unTxCertV1`, `unTxCertV2`, `unTxCertV3` and `txInfoId` (use `transTxId` instead)
  * Add `transCoinToValue`, `transTxId`
* Add `fromNativeScript`
* Remove unused `mapMaybeValidation` and `runTestMaybe`
* Remove `InjectMaybe` type class in favor of more general `Inject`
* Move `decodeAddrShort` and `decodeAddrShortEither` to
  `cardano-ledger-api:Cardano.Ledger.Api.Tx.Address`
* Remove deprecated `Cardano.Ledger.CompactAddress` and `Cardano.Ledger.UMapCompact`
* Add the newtype `EpochInterval`
* Change the type of `ppEMaxL` and `ppuEMaxL`
* Moved `ToExpr` instances out of the main library and into the testlib.

### `testlib`

* Add `integralToByteStringN`
* Add `ByronKeyPair`
* Add `Arbitrary` instances for Byron types: `Address`, `Attributes` and `AddrAttributes`
* Add `Test.Cardano.Ledger.Core.JSON` with `roundTripJsonSpec`, `roundTripJsonEraSpec` and
  `roundTripJsonProperty`
* Add `zeroTestingCostModel` and `zeroTestingCostModels`
* Add `mkCostModelConst`
* Add `diffExprCompact`
* Add `expectLeftDeep_`, `expectRightDeep_`
* Two new modules `Test.Cardano.Ledger.Plutus` and `Test.Cardano.Ledger.Plutus.Examples`

## 1.9.0.0

* Add `certsTotalDepositsTxBody` and `certsTotalRefundsTxBody`
* Add `getTotalDepositsTxBody` and `getTotalRefundsTxBody` to `EraTxBody`. Corresponding
  functions have been removed from `ShelleyEraTxBody` in favor of the two above
* Add `getTotalDepositsTxCerts` and `getTotalRefundsTxCerts` to `EraTxCert`
* Deprecated `Cardano.Ledger.Ap`, since we no longer use this module
* Moved `Cardano.Ledger.Language` to `Cardano.Ledger.Plutus.Language` with deprecation.
* Move `ExUnits` and `Prices` from `Cardano.Ledger.Alonzo.Scripts` to new
  `Cardano.Ledger.Plutus.ExUnits` module.
* Move `CostModels` from `Cardano.Ledger.Alonzo.Scripts` to new
  `Cardano.Ledger.Plutus.CostModels` module.
* Move `Cardano.Ledger.Alonzo.Scritps.Data` to `Cardano.Ledger.Plutus.Data`
* Move some parts of `Cardano.Ledger.Alonzo.TxInfo` into
  `Cardano.Ledger.Plutus.TxInfo` and `Cardano.Ledger.Plutus.Evaluate`
* Expose `decodeValidAndUnknownCostModels` in `CostModels`

### `testlib`

* Addition of `Test.Cardano.Ledger.Imp.Common`
* Re-export `ToExpr`, `showExpr`, `diffExpr`, `assertBool` and `assertFailure`
* Add `shouldBeExpr`, `shouldBeRight`, `shouldBeRightExpr`, `shouldBeLeft` and
  `shouldBeLeftExpr`
* Add `expectRight`, `expectRightDeep`, `expectRightExpr`, `expectRightDeepExpr`,
  `expectLeft`, `expectLeftExpr`, `expectLeftDeep` and `expectLeftDeepExpr`

## 1.8.0.0

* Add `ToJSON`/`FromJSON` and `ToJSONKey`/`FromJSONKey` instances for `DRep`
* Add `parseCredential`
* Add `NoUpdate`, `HKDNoUpdate`
* Add `toNoUpdate` and `fromNoUpdate` methods to `HKDFunctor`
* Add `Updatable` instance for `NoUpdate`
* Change functions to methods of `EraPParams`:
  * `ppProtocolVersionL`
  * `ppuProtocolVersionL`
* Add `Generic` instance for `AuxiliaryDataHash`
* Add `ToExpr` instances for:
  * `CompactAddr`
  * `AuxiliaryDataHash`
  * `CompactDeltaCoin`
  * `VKey`
* Add `setMinFeeTx`
* Add `ScriptsProvided`
* Require new `EraUTxO` class method `getScriptsProvided`
* `EraTx` replaced `EraTxBody` superclass for `EraUTxO`

### `testlib`

* Add `testGlobals` and `mkDummySafeHash`
* Add `Test.Cardano.Ledger.Core.Rational`

## 1.7.0.0

* Require `ToExpr` for `EraTx` class
* Add `upgradeTx` function to `EraTx` class
* Require `ToExpr` for `EraTxBody` class
* Add `upgradeTxBody` function to `EraTxBody` class
* Require `ToExpr` for `EraTxWits` class
* Add `upgradeTxWits` function to `EraTxWits` class
* Add `ToExpr` instance to:
  * `CompactAddr`
  * `Withdrawals`
  * `AuxiliaryDataHash`
  * `VKey`
  * `ChainCode`
  * `BootstrapWitness`
  * `WitVKey`
* Add `Generic` instance to `AuxiliaryDataHash`
* Add `vsNumDormantEpochs` to `VState` to track the number of contiguous epochs in which there were no governance proposals to vote on. #3729
* Add `fromEraShareCBOR`
* Remove redundant `DecCBOR` constraint in `eraDecoder`
* Add `FromJSON` instance to `Anchor`
* Change `ToJSON/FromJSON` implementation of `Credential` to use `keyHash` vs `key hash`
  and `scriptHash` vs `script hash` JSON keys.
* Change `ToJSONKey/FromJSONKey` implementation of `Credential` to use `keyHash-` vs `keyhash-`
  and `scriptHash-` vs `scripthash-` prefixes.

### `testlib`

* Added `BinaryUpgradeOpts`
* Add `Arbitrary` instance for `DRepDistr`
* Move `Arbitrary` instance for `SnapShot` and `SnapShots` from `cardano-ledger-shelley:testlib`
* Add `Test.Cardano.Ledger.Core.Binary.RoundTrip` with:
  * `roundTripEraSpec`
  * `roundTripAnnEraSpec`
  * `roundTripEraTypeSpec`
  * `roundTripAnnEraTypeSpec`
  * `roundTripShareEraSpec`
  * `roundTripShareEraTypeSpec`
  * `roundTripEraExpectation`
  * `roundTripEraTypeExpectation`
  * `roundTripAnnEraExpectation`
  * `roundTripAnnEraTypeExpectation`
  * `roundTripShareEraExpectation`
  * `roundTripShareEraTypeExpectation`
  * `roundTripCoreEraTypesSpec`

## 1.6.0.0

* Add `lookupRegStakeTxCert` and `lookupUnRegStakeTxCert` to `EraTxCert` typeclass #3700
* Change `ToJSONKey`/`FromJSONKey` implementation of `Credential` to flat text
* Add one more parameter to `getConsumedValue` to lookup DRep deposits #3688
  * `Credential 'DRepRole (EraCrypto era) -> Maybe Coin`
* Add `Ap`, `hoistAp`, `runAp`, `runAp_`
* Add `eqBootstrapWitnessRaw` and `eqWitVKeyRaw`
* Add `eqRawType`
* Add `EqRaw` type class with `eqRaw`.
* Add `EqRaw` instance for `WitVKey` and `BootstrapWitness`
* Require `EqRaw` instance for `Script`, `TxWits`, `TxAuxData`, `TxBody` and `Tx`
* Add `ToExpr` instance for `PoolCert`
* Require `ToExpr` instance for `Script`, `TxAuxData` and `TxCert`
* Require an extra argument for `decodePositiveCoin` in order to improve error reporting #3694

## 1.5.0.0

* Add `drepDeposit` to the `DRepState` #3628
* Introduce `CommitteeState` and `csCommitteeCredsL`.
* Change then name and the type of `vsCommitteeHotKeys` to `vsCommitteeState` and
  `vsCommitteeHotKeysL` to `vsCommitteeStateL`
* Add `drepExpiryL` and `drepAnchorL`
* Add `eraName`
* Add `addrPtrNormalize`
* Add `upgradeTxOut` function to `EraTxOut`
* Add `upgradeScript` function to `EraScript`
* Add `upgradeTxAuxData` function to `EraTxAuxData`
* Add `upgradeTxCert` function and `TxCertUpgradeError` family to `EraTxCert`
* Add `Default` instance for `Anchor`
* Add `Anchor` and `AnchorDataHash`
* Add `DRepState`
* Change `vsDReps` to a map
* Rename key roles #3588
  * `Voting` to `DRepRole`
  * `CommitteeHotKey` to `HotCommitteeRole`
  * `CommitteeColdKey` to `ColdCommitteeRole`
* Change `VState` to allow committee cold keys to be script-hashes #3581
  * `vsCommitteeHotKeys :: Map (Credential 'CommitteeColdKey eracrypto) (Maybe (Credential 'CommitteeHotKey eracrypto))`
* Adopt `Default` instances #3556
  * Moved instances for `SafeHash`, `RewardAcnt` and `Credential` from Shelley.RewardProvenance
* Change `VState` to allow committee hot keys to be script-hashes #3552
  * `vsCommitteeHotKeys :: Map (KeyHash 'CommitteeColdKey eracrypto) (Maybe (Credential 'CommitteeHotKey eracrypto))`
* Added `withSLanguage`
* Move `BinaryPlutus` from `cardano-ledger-alonzo` and changed its `Show` instance to
  display as base64 encoding.
* Added `Plutus`
* Changed `Phase2Script` constructor of the `PhaseScript` type. It now accepts new
  `Plutus` type instead of `Language` and `ShortByteString`
* Remove default implementation for `spendableInputsTxBodyL`
* Add lenses:
  * `dsUnifiedL`
  * `dsGenDelegsL`
  * `dsIRewardsL`
  * `dsFutureGenDelegsL`
  * `certDStateL`
  * `certPStateL`
  * `certVStateL`
* Add `getProducedValue` to `EraUTxO`

## 1.4.1.0

* Add `spendableInputsTxBodyL`

## 1.4.0.0

* Added `DRep`, `DRepCredential`
* Changed type `Credential 'Voting c` -> `DRep c` in:
  * `UMElem`
  * `umElemAsTuple`
  * `umElemDRep`
  * `UMap`
  * `DRepUView`
  * `dRepUView`
  * `dRepMap`
  * `unify`

## 1.3.1.0

* Addition of `getPoolCertTxCert`

## 1.3.0.0

* Add delegated representatives to the `UMap` and make its interface more coherent #3426
  * Additions
    * Add `Credential 'Voting (EraCrypto era)` as the fourth element to the `UMap` n-tuple.
    * `umElemPtrs :: UMElem c -> Maybe (Set Ptr)`
    * `umElemDRep :: UMElem c -> Maybe (Credential 'Voting (EraCrypto era))`
    * `UView (DRepUView)` constructor and `dRepUView`
    * `invPtrMap :: UMap c -> Map (Credential 'Staking c) (Set Ptr)`
    * `dRepMap :: UMap c -> Map (Credential 'Staking c) (Credential 'Voting c)`
    * synonym `unionL = (∪)`
    * synonym `unionR = (⨃)`
    * synonym `domDelete = (⋪)`
    * synonym `rngDelete = (⋫)`
  * Renames
    * `Trip` to `UMElem`, pattern `Triple` to `UMElem`
    * `viewTrip` to `umElemAsTuple`
    * `tripRewardActiveDelegation` to `umElemRDActive`
    * `tripReward` to `umElemRDPair`
    * `tripDelegation` to `umElemSPool`
    * `View (RewardDeposits, Delegations, Ptrs)` to `UView (RewDepUView, SPoolUView, PtrUView)`
      * `rdPairs` to `rewDepUView`
      * `delegations` to `sPoolUView`
      * `ptrs` to `ptrUView`
      * `unView` to `unUView`
      * `viewtoVMap` to `unUnifyToVMap`
      * `rewView` to `rewardMap`
      * `compactRewView` to `compactRewardMap`
      * `depositView` to `depositMap`
      * `rdPairView` to `rdPairMap`
      * `delView` to `sPoolMap`
      * `ptrView` to `ptrMap`
      * `domRestrictedView` to `domRestrictedMap`
    * `zero` to `nullUMElem`
    * `zeroMaybe` to `nullUMElemMaybe`
    * `sumRewardsView` to `sumRewardsUView`
    * `sumDepositView` to `sumDepositUView`
  * Reimplementations
    * `unionRewAgg` NOTE: It does not require `assert (Map.valid result) result` any more
      and has been tested for equivalence with the older version with
      `--qc-max-success=10000 --qc-max-size=1000`. The test is added to `UMapSpec`.
* Add `certsTxBodyL` to `EraTxBody`
* Introduce `TxCert` type family and `EraTxCert` type class.
* Deprecate `Delegation`
* Add `toKeyHashWitness`
* Addition of `getVKeyWitnessTxCert` and `getScriptWitnessTxCert` to `EraTxCert` type class
* Add new key roles: `CommitteeColdKey` and `CommitteeHotKey`
* Remove `ConstitutionalDelegCert`. Instead it now lives in `cardano-ledger-shelley` as
  `GenesisDelegCert`
* Add `StakeCredentials` and two helper functions: `toStakeCredentials` and
  `domRestrictedStakeCredentials`

### `testlib`

* Add `genValidUMapWithCreds` and `uniformSubset`

## 1.2.0.0

* Deprecate `Cardano.Ledger.UMapCompact` in favor of `Cardano.Ledger.UMap` and add tests for it. #3371
  * Add `Cardano.Ledger.UMap.rdPairView` to view the reward-deposits pair from the `UMap`.
* Replace `DPState c` with `CertState era`
* Add `VState`
* Add `certVState`
* Parametrize `DState` and `PState` by era
* Rename `Cardano.Ledger.DPState` module to `Cardano.Ledger.CertState`
* Rename:
  * `lsDPState` -> `lsCertState`
  * `dpsPState` -> `certPState`
  * `dpsDState` -> `certDState`
  * `obligationDPState` -> `obligationCertState`
* Add support for `PlutusV3`

## 1.1.0.0

* Add `ToJSON (PParamsHKD f era)` superclass constraints for `EraPParams`.
* Add `ToJSON (TxOut era)` superclass constraints for `EraTxOut`.
* Add superclass constraints for `Val`:
  * `NoThunks`, `EncCBOR`, `DecCBOR`, `ToJSON`, `NFData`, `Show`
  * `EncCBOR (CompactForm t)`, `DecCBOR (CompactForm t)`
* Add `ToJSONKey`/`FromJSONKey` instances for `ScriptHash`
* Add `ToJSON`/`FromJSON` instances for `CompactForm Coin`, `SafeHash` and `TxId`
* Add `ToJSON` instances for:
  * `Ptr`, `CertIx`, `TxIx`
  * `Trip` and `UMap`
  * `DeltaCoin` and `CompactForm DeltaCoin`
  * `InstantaneousRewards`, `FutureGenDeleg`, `PState`, `DState` and `DPState`.
  * `UTxO` and `TxIn`
  * `Stake`, `SnapShot`, `SnapShots`, `PoolDistr` and `IndividualPoolStake`
  * `Reward` and `RewardType`
  * `AuxiliaryDataHash`
  * `Credential`
* Make `getConsumedValue` accept a deposit lookup function instead of a `DPState`
* Add `lookupDepositDState` and `lookupRewardDState`. Former can be used with
  `getConsumedValue` to regain previous behavior.
* Add `hashScriptTxWitsL`
* Remove custom `Fail` type, in favor of
  [`FailT`](https://hackage.haskell.org/package/FailT) package
* Move `bBodySize` into `Cardano.Ledger.Core`
* Rename `TxId` field from `_unTxId` to `unTxId`

### `testlib`

* Remove `runFailError` in favor of `errorFail` from `FailT` package

## 1.0.0.0

* First properly versioned release.
