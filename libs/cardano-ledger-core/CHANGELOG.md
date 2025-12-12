# Version history for `cardano-ledger-core`

## 1.19.0.0

* Add mocked-up `PerasKey` type
* Add mocked-up `validatePerasCert` validation function
* Changed name and type to `CompactForm Coin`:
  - `hkdMinFeeAL` -> `hkdMinFeeACompactL`
  - `hkdMinFeeBL` -> `hkdMinFeeBCompactL`
  - `hkdKeyDepositL` -> `hkdKeyDepositCompactL`
  - `hkdMinUTxOValueL` -> `hkdMinUTxOValueCompactL`
  - `hkdMinPoolCostL` -> `hkdMinPoolCostCompactL`
* Added:
  - `ppMinFeeACompactL`,
  - `ppMinFeeBCompactL`,
  - `ppKeyDepositCompactL`,
  - `ppMinUTxOValueCompactL`,
  - `ppMinPoolCostCompactL`,
  - `ppuMinFeeACompactL`,
  - `ppuMinFeeBCompactL`,
  - `ppuKeyDepositCompactL`,
  - `ppuMinUTxOValueCompactL`,
  - `ppuMinPoolCostCompactL`,
* Change the return type of `withdrawalsThatDoNotDrainAccounts` to `Maybe (Withdrawals, Map RewardAccount (Mismatch RelEQ Coin))`
* Deprecate `StakeCredential` and `PaymentCredential` type synonyms
* Add `positiveUnitIntervalRelaxToUnitInterval`, `positiveUnitIntervalRelaxToPositiveInterval` and  `positiveIntervalRelaxToNonNegativeInterval`
* Changed the type of the following functions by adding `Network` argument:
  - `stakePoolStateToStakePoolParams`
  - `snapShotFromInstantStake`
* Changed type of `spsRewardAccount` in `StakePoolState` to `Credential Staking`
* Changed type of `parseCostModels` by adding `[Language]` argument
* Add `HasOKey` instance for `TxId (TxBody l era)`
* Add `cddl` sub-library.
* Limit `DecCBORGroup` decoding of `ProtVer` fields to `Word32` starting from protocol version `12`
* Change `Relation` type to only be visible at the type level
* Change `KeyRole` type to only be visible at the type level
* Rename `Genesis` constructor of `KeyRole` to `GenesisRole`
* Change `KeyRoleVRF` type to only be visible at the type level
* Add `lookupAccountStateIntern` to `State.Account` module
* Add `HasOKey` instance for `TxId (Tx l era)`
* Remove `Generic` instance from `BoundedRatio` type
* Remove deprecated function `addrPtrNormalize`
* Remove deprecated functions `mkTxIx`, `mkCertIx`, `hashAnchorData`
* Remove deprecated functions `bheader`, `bbody`
* Remove deprecated methods `fromTxSeq`, `toTxSeq`, `hashTxSeq` from `EraBlockBody` typeclass
* Remove deprecated function `normalizePtr`
* Remove deprecated functions `hashSignature`, `hashVerKeyVRF`
* Remove deprecated types `Hash`, `SignedDSIGN`, `SignKeyDSIGN`, `KESignable`, `SignedKES`, `SignKeyKES`, `VerKeyKES`, `VRFSignable`, `CertifiedVRF`, `SignKeyVRF`, `VerKeyVRF`
* Remove deprecated function `wvSig`
* Remove deprecated functions `shorten`, `showMemo`, `printMemo`
* Remove deprecated function `costModelParamsCount`
* Remove deprecated type `AccountState` and its accessors `asTreasury` and `asReserves`
* Remove deprecated functions `balance`, `coinBalance`
* Remove deprecated module `Cardano.Ledger.AuxiliaryData`
* Remove deprecated modules `Cardano.Ledger.Crypto` `Cardano.Ledger.Crypto.Internal`
  - Their content has moved to `Cardano.Protocol.Crypto` in the `cardano-protocol-tpraos` package
* Remove deprecated module `Cardano.Ledger.EpochBoundary`
* Remove deprecated module `Cardano.Ledger.PoolDistr`
* Remove deprecated module `Cardano.Ledger.PoolParams`
* Remove deprecated module `Cardano.Ledger.SafeHash`
* Remove deprecated module `Cardano.Ledger.UTxO`
* Add `unDelegReDelegStakePool` to `CertState` module
* Add `iRReservesL`, `iRTreasuryL`, `iRDeltaReservesL`, `iRDeltaTreasuryL`
* Add `spsDelegators` field to `StakePool`
* Add `spsDelegatorsL`
* Change parameter type of `removeStakePoolDelegations` from `Set (KeyHash 'StakePool)` to `Set (Credential 'Staking)`
* Add `fromStrictMaybeL`, `toStrictMaybeL`
* Add `memoRawTypeL`
* Remove `getterMemoRawType`
* Add `EncCBOR` instance for `MemoBytes`
* Add `TxLevel`, `EraTxLevel`, `HasEraTxLevel`
* Rename `PoolParams` to `StakePoolParams`
  - Replace the prefix for all the fields of this type from `pp*` -> `spp*`
  - Rename the lenses for the fields from `ppCostL`, `ppMetadataL` and `ppVrfL` to `sppCostL`, `sppMetadataL` and `sppVrfL`
  - Rename `stakePoolStateToPoolParams` to `stakePoolStateToStakePoolParams`
* Remove the `UMap` module and the `umap` benchmarks cabal target.
* Export `dRepToText`
* Deprecated `bheader` and `bbody`
* Add field accessors to `Block`: `blockHeader` and `blockBody`.
* Expose `dRepToText`
* Modify `withdrawalsThatDoNotDrainAccounts` to return `Maybe (Withdrawals, Withdrawals)` where the `fst` are either missing accounts or in the wrong network and `snd` are incomplete withdrawals.
* Add `FromJSON` instance for `PParamUpdate`

### `cddl`

* Add `HuddleRule1` typeclass.
* Export `Era` to reuse via the import chain of modules across eras.
* Add `HuddleRule`, `HuddleGroup` and `HuddleGRule` type class for era-polymorphic CDDL generation.
* Add `HuddleSpec` for all common CDDL types.

### `testlib`

* Remove `huddle-cddl` and the `CDDL` modules.
* Add `forEachEraVersion`
* Add `Test.Cardano.Ledger.Core.Binary.Golden`
* Add `kes_period` and `sequence_number` CDDL definitions.
* Add CDDL definitions for:
  - Credentials: `credential`, `stake_credential`
  - Pool primitives: `port`, `ipv4`, `ipv6`, `dns_name64`, `url64`, `single_host_addr`
* Add `mkPoolRules` function to generate protocol-version-specific pool-related definitions
* Remove CDDL definitions for Plutus V1 types: `big_int`, `big_uint`, `big_nint`, `bounded_bytes` (moved to Alonzo)
* Remove CDDL definitions for int64 types: `min_int64`, `max_int64`, `negative_int64`, `positive_int64`, `nonzero_int64`, `int64` (moved to Allegra)
* Add CDDL definitions for `script_hash`, `mkScriptPubkey`, `mkScriptAll`, `mkScriptAny`, `mkScriptNOfK`, `mkScriptInvalidBefore`, `mkScriptInvalidHereafter`
* Add CDDL definition for `transaction_index`, `metadatum_label`, `metadatum`, `metadata` and `auxiliary_data_hash`
* Remove deprecated generators `genAddrBadPtr`, `genCompactAddrBadPtr`, `genBadPtr`
* Remove deprecated functions `mkCred`, `mkScriptAddr`
* Add CDDL definition for `nonce`, `epoch`, `epoch_interval`, `slot` and `block_number`
* Remove `Test.Cardano.Ledger.Plutus.ExUnits`
* Remove the `accountsToUMap` member function from the `EraTest` class.
  - Also remove the related `accountsFromUMap` function.
* Remove the following from `Core.Arbitrary`:
  - `genValidUMap`
  - `genValidUMapNonEmpty`
  - `genValidUMapWithCreds`
  - `genValidTuples`
  - `genValidTuplesNonEmpty`
  - `genRightPreferenceUMap`
  - `genInsertDeleteRoundtripRDPair`
  - `genInsertDeleteRoundtripPtr`
  - `genInsertDeleteRoundtripSPool`
  - `genInsertDeleteRoundtripDRep`

### `tasty-compat`

* Library added as an interim compatibility layer for migrating from `tasty` to `hspec`

## 1.18.0.0

* Changed the type of `AtMostEra` and `AtLeastEra` to accept a type level string instead of an actual era type.
* Add `EraName` type family to the `Era` type class and use it for default implementation of
  `eraName` type class function.
* Changed `sizeTxF` and `sizeTxForFeeCalculation` to use `Word32`
* Move pool deposits from `PState` into `StakePoolState`. #5234
  - Add `spsDeposit` field to `StakePoolState`
  - Remove `psDeposits` field from `PState` data constructor
  - Update `mkStakePoolState` to take deposit parameter as first argument
  - Remove `psDepositsL` as no longer necessary.
  - Remove `payPoolDeposit` and `refundPoolDeposit` functions as they are no longer necessary
  - Update `EncCBOR`/`DecCBOR` instances for `PState` to handle new structure
  - Add lenses for `StakePoolState` fields
* Add `psVRFKeyHashes` to `PState`
* Add `psVRFKeyHashesL`
* Deprecate `costModelParamsCount` in favor of `costModelInitParamCount`
* Add `costModelInitParamNames`, `costModelInitParamCount`, `parseCostModelAsArray` and `parseCostModelAsMap`
* Export `credToDRep` and `dRepToCred`
* Add `ppVrfL`, `ppCostL`, `ppMetadataL`, `spsVrfL` to `StakePool` module
* Deprecate `PoolParams` in favor of `StakePoolState`. #5196
  - Move the `PoolParams` module to `Cardano.Ledger.State.StakePool` and export from there.
  - Add the `StakePoolState` data type to the new module.
  - Reexport `PoolParams` from its original module and deprecate it.
  - Replace `PoolParams` with `StatekPoolState` in `PState` and rename the fields and lenses.
    + `psStakePoolParams(L)` to `psStakePools(L)`, and
    + `psFutureStakePoolParams(L)` to `psFutureStakePools(L)`.
* Add `mkGenesisWith`
* Add `NFData` instance for `NoGenesis`
* Require `Eq`, `Show`, `Typeable`, `ToCBOR`, `FromCBOR`, `ToJSON`, `FromJSON`, `NFData` for every `Genesis` type.
* Add `HashHeader` (moved from `cardano-protocol-tpraos`)
* Add `drepDepositCompactL`
* Change the type of `drepDeposit` to `CompactForm Coin`
* Remove `AccountState` type synonym for `ChainAccountState`
* Remove `rewards`, `delegations`, `ptrsMap` and `dsUnifiedL`
* Replace `dsUnified` with `dsAccounts` in `DState`
* Add `Default` instance to `BlocksMade` and `PoolDistr`
* Add `Random`, `Uniform` and `UniformRange` instances for `TxIx`, `CertIx` and `SlotNo32`
* Add `Uniform` instances for `Ptr`
* Add `CanGetAccounts`, `CanSetAccounts`, `EraAccounts`, `lookupAccountState`, `updateLookupAccountState`, `isAccountRegistered`, `adjustAccountState`, `lookupStakePoolDelegation`, `sumBalancesAccounts`, `sumDepositsAccounts`, `addToBalanceAccounts`, `withdrawalsThatDoNotDrainAccounts`, `drainAccounts`, `removeStakePoolDelegations`
* Add `mkInlineDatum`, `mkHashedDatum`
* Rename `EraSegWits` to `EraBlockBody`. #5156
  - Rename `TxSeq` to `BlockBody`
  - Add `mkBasicBlockBody`
  - Deprecate `fromTxSeq` and `toTxSeq` in favour of the `txSeqBlockBodyL` lens
  - Deprecate `hashTxSeq` in favour of `hashBlockBody`
* Replaced `hkdPoolDepositL` method with `hkdPoolDepositCompactL`
* Add `ppPoolDepositCompactL` and `ppuPoolDepositCompactL`
* Add `standardHashSize` and `standardAddrHashSize`
* Add `zeroCostModels` method to `EraTest`
* Added support for `PlutusV4`
* Add a timeout argument to `plutusDebug`
* Add `pdoExUnitsEnforced` to `PlutusDebugOverrides` and add `defaultPlutusDebugOverrides`
* Add `NFData` instance for `PlutusDebugInfo`
* Add `DebugTimedOut` constructor to `PlutusDebugInfo`
* Add `debugPlutusUnbounded`
* Added `binaryUpgradeTx`, `binaryUpgradeTxBody`, `binaryUpgradeTxWits`, `binaryUpgradeTxAuxData`
* Remove `upgradeTx` and `TxUpgradeError` from `EraTx`
* Remove `upgradeTxBody` and `TxBodyUpgradeError` from `EraTxBody`
* Remove `upgradeTxAuxData` from `EraTxAuxData`
* Move `upgradeTxWits` from `EraTxWits`
* Moved `wireSizeTxF` out of `EraTx`
* Move to `DecCBOR` instance for `Block` to `testlib`
* Removed constraints from `EraPParams` for both `PParamsHKD Identity era` and `PParamsHKD StrictMaybe era`: `EncCBOR`, `DecCBOR`, `ToCBOR`, `FromCBOR`, `ToJSON`, `FromJSON`
* Added `eraPParams` to `EraPParams`
* Added `jsonPairsPParams` and `jsonPairsPParamsUpdate` to `EraPParams` along with their implementation
* Replaced positional `PParam` constructor with a record-style constructor and added constraints on t: `DecCBOR`, `EncCBOR`, `FromJSON`, `ToJSON`
* Added `PParamUpdate`
* Added error-throwing `ToPlutusData` instance for `Nonce`
* Rename `ppLens` and `ppuLens` to `ppLensHKD` and `ppuLensHKD`, respectively
* Add `sumUTxO` and `sumCoinUTxO`
* Deprecate `balance` and `coinBalance` in favor of `sumUTxO` and `sumCoinUTxO`
* Remove `delegators` field from JSON serialiser for `DRepState` for correct round-tripping. #5004
* Change `TxBody` to an associated `data` family
* Remove `HeapWords` instances for: #5001
  - `Coin`
  - `DeltaCoin`
  - `CompactFormCoin`
  - `CompactFormDeltaCoin`
  - `SafeHash`
  - `StrictMaybe DataHash`
  - `TxId`
  - `TxIn`
* Add `addCompactCoin` to `Cardano.Ledger.Coin` and deprecate `Cardano.Ledger.UMap.addCompact`
  in its favor
* Move `sumCompactCoin` to `Cardano.Ledger.Coin`
* Add `eraDecoderWithBytes`
* Expose `MkData` constructor.
* Remove `Text` parameter from `translateEraThroughCBOR`
* Add `keyHashWitnessesTxWits`
* Remove export of `mkMemoBytes` from `Cardano.Ledger.MemoBytes` as unsafe
* Add `CanGetChainAccountState` and `CanSetChainAccountState`
* Add `treasuryL` and `reservesL`
* Add `casTreasuryL` and `casReservesL`
* Rename `AccountState` to `ChainAccountState`. Rename accessor fields `asTreasury` and `asReserves` to `casTreasury` and `casReserves` respectively.
* Added `sizeTxForFeeCalculation` to `EraTx` with a default implementation
* Added `consumed` to `EraUTxO`
* Removed `upgradeCertState`
* Removed `VState` (moved to `cardano-ledger-conway`) and related functions
* Removed from `EraCertState` type family:
  - `mkCertState`
  - `certVStateL`
* Moved `CertState` to `State` module
* Rename `wvkSig` to `wvkSignature`
* Remove `eqBootstrapWitnessRaw` and `BootstrapWitnessRaw`
* Rename `bwSig` to `bwSignature` for `BootstrapWitness`
* Remove `witVKeyBytes` and `eqWitVKeyRaw`
* Remove `EqRaw` instance for `WitVKey`
* Replace `Block'` constructor with `Block`
* Remove patterns: `Block`, `UnserialisedBlock` and `UnsafeUnserialisedBlock`
* Add `EncCBORGroup (TxSeq era)` and `EncCBOR h` constraints to `EncCBOR` and `ToCBOR` instances for `Block`

### `testlib`

* Add `Arbitrary` instances for `PlutusLedgerApi.V1.Ex{Budget,CPU,Memory}`
* Add `Test.Cardano.Ledger.Plutus.ExUnits`
* Added `genericShrinkMemo`
* Add `maxWord32` and `posWord32` rules to CDDL
* Add `registerTestAccount` and `accountsFromUMap`
* Add `mkTestAccountState`, `accountsFromAccountsMap` and `accountsToUMap` to `EraTest`
* Add `expectLeftDeepExpr_` and `expectRightDeepExpr_`
* Add `expectJust`, `expectJustDeep`, `expectJustDeep_`, `expectNothing`
* Change argument of `txInAt` from `Integral` to `Int`
* Add `roundTripJsonShelleyEraSpec`
* Add superclass constraint to `EraTest` for `Eq`, `Show`, `Typeable`, `ToJSON`, `FromJSON` and `Arbitrary` classes for `TranslationContext` type
* Added `goldenJsonPParamsUpdateSpec`
* Added `Era` module with `EraTest` class

## 1.17.0.0

* Add `goldenJsonPParamsSpec`
* Add `BoootstrapWitnessRaw` type
* Add `EraStake`, `CanGetInstantStake`,  `CanSetInstantStake` , `snapShotFromInstantStake`, `resolveActiveInstantStakeCredentials`
* Add boolean argument to `fromCborRigorousBothAddr` for lenient `Ptr` decoding
* Add `ToCBOR` and `FromCBOR` instances for:
  - `BoundedRatio`
  - `PositiveUnitInterval`
  - `ActiveSlotCoeff`
  - `Network`
  - `NoGenesis`
* Move `EraGov` to `Cardano.Ledger.State` from `cardano-ledger-shelley`
* Add DecCBOR instances for:
  - `PlutusData`
  - `Data`
  - `BootstrapWitness`
  - `WitVKey`
  - `Block`
* Converted `CertState` to a type family
* Remove `applySTSValidateSuchThat` and `applySTSNonStatic` as redundant.
* Move `AccountState` to `Cardano.Ledger.State` from `cardano-ledger-shelley`
* Move `Cardano.Ledger.PoolDistr` module contents into `Cardano.Ledger.State` and deprecated the former
* Move `Cardano.Ledger.SnapShots` module contents into `Cardano.Ledger.State` and deprecated the former
* Move `Cardano.Ledger.UTxO` module contents into `Cardano.Ledger.State` and deprecated the former
* Add `CanGetUTxO` and `CanSetUTxO` type classes
* Add `CanGetUTxO` and `CanSetUTxO` instances for `UTxO`
* Add `DecShareCBOR` instances for `DRep` and `DRepState`
* Added `ToPlutusData` instance for `NonZero`
* `maxpool'` now expects `nOpt` to be a `NonZero Word16`
* Add `HasZero` instance for `Coin` together with lifted conversion functions:
  - `toCompactCoinNonZero`
  - `fromCompactCoinNonZero`
  - `unCoinNonZero`
  - `toCoinNonZero`
  - `compactCoinNonZero`
* Add `Cardano.Ledger.BaseTypes.NonZero`
* Remove `era` type parameter from `MemoBytes` type
* Remove `Era era` constraint from:
  - `Memo` pattern
  - `decodeMemoBytes`
  - `DecCBOR (Annotator (MemoBytes t))` instance
  - `DecCBOR (MemoBytes t)` instance
  - `memoBytes`
  - `mkMemoized`
  - `lensMemoRawType`
  - `Data` pattern
  - `dataToBinaryData`
* Introduce `mkMemoizedEra` and `memoBytesEra`
* Add `Version` parameter to:
  - `memoBytes`
  - `mkMemoized`
  - `lensMemoRawType`
* Remove `era` type parameter from `Mem` type
* Reduce the kind of `MemoHashIndex` type family parameter to a concrete type
* Reduce the kind of `RawType` type to a concrete type
* Add `byteCountMemoBytes`, `packMemoBytesM` and `unpackMemoBytesM` to `MemoBytes` module
* Require `MemPack` instance for `TxOut` and `CompactForm (Value era)` for `EraTxOut` type class.
* Add `decodeMemoBytes`
* Add `MemPack` instance for `CompactAddr`, `TxIx`, `TxId`, `TxIn`, `CompactForm Coin`,
  `KeyHash`, `ScriptHash`, `Credential`, `SafeHash`, `Plutus`, `PlutusBinary`, `BinaryData` and `Datum`
* Add `DecShareCBOR` instance for `TxIn`
* Add `fromCborRigorousBothAddr`
* Add `SlotNo32` and use it in `Ptr` definition
* Add `mkPtrNormalized`
* Deprecate `normalizePtr` and `addrPtrNormalize`
* Switch `TxIx` and `CertIx` to use `Word16` instead of `Word64`
* Deprecate `mkTxIx` and `mkCertIx`
* Extract `indexProxy` from `HashAnnotated` type class and deprecate it.
* Extract `hashTxAuxData` from `EraTxAuxData` into a standalone function.
* Add `TxAuxDataHash` and deprecate `AuxiliaryDayaHash` in its favor.
* Deprecate `Cardano.Ledger.Crypto` module in favor of `cardano-protocol-tpraos:Cardano.Protocol.Crypto`
* Deprecate `KESignable`, `SignedKES`, `SignKeyKES`, `VerKeyKES`, `VRFSignable`,
  `CertifiedVRF`, `SignKeyVRF` and `VerKeyVRF` type synonyms.
* Deprecate `Hash`, `SignedDSIGN` and `SignKeyDSIGN` type synonyms.
* Deprecate `hashSignature` in favor of new `hashTxBodySignature`
* Move into `Cardano.Ledger.Hashes`:
  - `HASH` and `ADDRHASH`
  - `KeyHash` and `HashKey`
  - `SafeHash`, `SafeToHash`, `HashAnnotated`, `castSafeHash` and `extractHash`.
  - `KeyRoleVRF`, `VRFVerKeyHash`, `toVRFVerKeyHash`, `fromVRFVerKeyHash`
  - `GenDelegPair` and `GenDelegs`
* Re-export `KeyRole` from `Cardano.Ledger.Hashes`.
* Re-export some of the new additions to `Cardano.Ledger.Hashes` from `Cardano.Ledger.Core`.
* Remove `GenesisCredential` as unused.
* Remove `HASH`, `ADDRHASH` and `DSIGN` type families out of `Crypto` type class and turn
  them into type synonyms for the exact algorithms previously being used in
  `StandardCrypto`
* Remove crypto parametrization from types:
  - `Addr`, `BootstrapAddress`, `RewardAccount`, `CompactAddr` and `Withdrawals`
  - `AuxiliaryDataHash`
  - `BlocksMade`, `Anchor`
  - `FutureGenDeleg`, `InstantaneousRewards`, `CommitteeAuthorization`
  - `ByronEra` and `VoidEra`
  - `PoolCert`
  - `Credential`, `StakeReference` and `GenesisCredential`
  - `DRep` and `DRepState`
  - `Stake`, `SnapShot` and `SnapShots`
  - `DataHash`, `ScriptHash`
  - `BootstrapWitness`
  - `VKey`, `KeyHash`, `GenDelegPair`, `GenDelegs`, `VRFVerKeyHash`
  - `PlutusWithConext`, `ScriptFailure`, `ScriptResult`, `PlutusDebugInfo`
  - `TxOutSource`
  - `IndividualPoolStake`, `PoolDistr`
  - `PoolParams`
  - `Reward`
  - `SafeHash` and `SafeToHash`
  - `TxId`, `TxIn`
  - `UMElem`, `RewardDelegation`, `StakeCredentials`, `UView` and `UMap`
* Remove `HashWithCrypto` as no longer needed.
* Deprecate `hashAnchorData`
* Change superclass of `Crypto` from `KESAlgorithm` to `UnsoundPureKESAlgorithm`
* Add `PlutusDebugOverrides` argument to `debugPlutus`
* Add `PlutusDebugOverrides` data type
* Add `Read` instance for `Language`

### `testlib`

* Add `Arbitrary` instance for `PV1.Data`
* Add `Arbitrary` instances for `Data`, `BinaryData` and `Datum`
* Add `decoderEquivalenceEraSpec`
* Converted `CertState` to a type family
* Re-export `KeyPair`, `mkAddr` and `mkCredential` from `Test.Cardano.Ledger.Imp.Common`
* Add `MakeStakeReference` and `MakeCredential`
* Deprecate `mkCred` in favor of `mkCredential` from `MakeCredential`
* Change `mkAddr` to accept two polymorphic arguments instead of a tuple of keypairs
* Deprecate `mkScriptAddr`
* Add `runGen`
* Added `Arbitrary` and `ToExpr` instances for `NonZero`
* Deprecate `genBadPtr`, `genAddrBadPtr` and `genCompactAddrBadPtr`
* Remove crypto parametrization from types: `KeyPair` and `KeyPairs`

## 1.16.0.0

* Add `toVRFVerKeyHash` and `fromVRFVerKeyHash`
* Change lens type of `hkdNOptL`, `ppNOptL`, and `ppuNOptL` to `Word16`
* Add `epochFromSlot`
* Remove usage of a `Reader` monad in `epochInfoEpoch`, `epochInfoFirst` and `epochInfoSize`.
* Add `eraProtVersions`
* Remove deprecated `_unTxId` and `adaOnly`
* Remove deprecated module `Cardano.Ledger.Serialization`
* Remove deprecated `fromSLanguage`
* Remove deprecated `decodeValidAndUnknownCostModels` and `decodeCostModelFailHard`
* Remove deprecated module `Cardano.Ledger.Language`
* Remove deprecated `KeyPair`
* Remove `Cardano.Ledger.Era` that had all of its contents deprecated.
* Remove `getAllTxInputs` as unused.
* Remove deprecated `Delegation` and `poolCWitness`
* Remove deprecated `txid`
* Remove deprecated `ValidateAuxiliaryData`, `AuxiliaryData`, `hashAuxiliaryData` and `validateAuxiliaryData`
* Remove deprecated `Cardano.Ledger.Ap` module and all if its contents
* Remove deprecated `ppRewardAcnt`
* Remove deprecated `mkRwdAcnt`, `deserialiseAddr`, `serialiseRewardAcnt`,
  `deserialiseRewardAcnt`, `RewardAcnt`, `getRwdNetwork`, `getRwdCred`, `putRewardAcnt`,
  `decodeRewardAcnt`, `fromCborRewardAcnt`
* Remove deprecated `HashAlgorithm`
* Remove `Safe` and `hashSafeList`
* Remove requirement for `FromCBOR` instance for `TxOut` in `EraTxOut`
* Add `decodeMemoized`
* Add `DecCBOR` instance for `MemoBytes`
* Add `VRFVerKeyHash` and `KeyRoleVRF`.
* Switch `genDelegVrfHash`, `individualPoolStakeVrf` and `ppVrf` to using `VRFVerKeyHash`.
* Add `{Enc|Dec}CBORGroup` instances for `Mismatch`. #4666
  - Add `(un)swapMismatch` to swap `Mismatch` values to preserve serialisation when necessary.
* Add `drepDelegsL`

### `testlib`

* Add `decodeHexPlutus` to `Plutus` module
* Add `Guardrail` module
* Switch to using `ImpSpec` package
* Remove `HasSubState`, `subStateM`, `setSubStateM`, `StateGen` and `StateGenM` as no longer useful.
* Re-export `withImpInit` and `modifyImpInit`
* Remove deprecated `mkVKeyRwdAcnt`
* Remove deprecated `deserialiseRewardAcntOld`
* Generalize the return type of `assertColorFailure` to `MonadIO`
* Moved `Test.Cardano.Ledger.Core.Tools` into the test suite.

## 1.15.0.0

* Add `Mismatch` type to clarify predicate-failures reporting supplied and expected values. #4649
* Added `drepDelegs` to `DRepState`
* Add `member'` function to `UMap` module. #4639
* Add `credKeyHash` to `Credential`
* Remove `maxMajorPV` from `Globals`
* Add `deleteStakingCredential` and `extractStakingCredential` to `UMap` module.

### `testlib`

* Add `uniformSubMap` and `uniformSubMapElems`
* Rename `uniformSubset` to `uniformSubSet`
* Add `tracedDiscard`
* Add re-exported functions that were added in `cardano-ledger-binary-1.4.0.0`
  - `ansiExpr`
  - `ansiExprString`
  - `diffExprString`
  - `diffExprCompactString`
* Existing re-exported functions `diffExpr` and `diffExprCompact` have new return types
  per `cardano-ledger-binary-1.4.0.0`
* Add a function `expectRawEqual`
* Add `assertColorFailure`
* Add `Test.Cardano.Ledger.Core.Binary.CDDL` containing core CDDL definitions
  shared across eras.

## 1.14.0.0

* Add `EncCBOR` instance for `PoolCert`
* Add `mkTermToEvaluate` to `PlutusLanguage` class.
* Add a field to `DebugFailure`
* Convert `debugPlutus` to an `IO` action
* Add `plutus-debug` executable
* Change default implementation of `translateEra`
* Add `EraGenesis` and `Genesis` type family. New `NoGenesis` type to be used for eras that do not have a genesis file
* Move `ensureMinCoinTxOut` from `cardano-ledger-api` to `Cardano.Ledger.Tools`
* Change default implementation of `translateEra`
* Add `EraGenesis` and `Genesis` type family. New `NoGenesis` type to be used for eras that do not have a genesis file
* Move `ensureMinCoinTxOut` from `cardano-ledger-api` to `Cardano.Ledger.Tools`
* Add `wireSizeTxF` to `EraTx` class

### `testlib`

* Add `Show` instances for `ScriptTestContext` and `PlutusArgs`
* Add `setSubStateM`
* Rename `subState` to `subStateM`

## 1.13.2.0

* Add `setMinCoinTxOut` and `setMinCoinTxOutWith`
* Add `authorizedHotCommitteeCredentials`

## 1.13.1.0

* Improve error reporting for `CostModel` json parsing

## 1.13.0.0

* Add `subStateL` to `HasSubState`
* Create a catch all `Inject a a` instance for the same type
* Remove unnecessary instances: `Inject a ()` and `Inject Void a`
* Add `integralToBounded` to `BaseTypes`
* Add `PlutusScriptContext` type family moved from Alonzo
* Add `LegacyPlutusArgs` data
* Add `PlutusArgs` data family to `PlutusLanguage` and instances
* Change signature of `evaluatePlutusRunnable` and `evaluatePlutusRunnableBudget` to replace data list with `PlutusArgs`

### `testlib`

* Rename test scripts:
  - `alwaysSucceeds2` -> `alwaysSucceedsNoDatum`
  - `alwaysSucceeds3` -> `alwaysSucceedsWithDatum`
  - `alwaysFails2` -> `alwaysFailsNoDatum`
  - `alwaysFails3` -> `alwaysFailsWithDatum`
  - `guessTheNumber3` -> `redeemerSameAsDatum`
  - `evendata3` -> `evenDatum`
  - `evenRedeemer2` -> `evenRedeemerNoDatum`
  - `evenRedeemer3` -> `evenRedeemerWithDatum`
* Remove test scripts: `odddata`, `oddRedeemer`, `oddRedeemer2`, `sumsTo10`, `guessTheNumber2`, `redeemerIs102`

## 1.12.0.0

* Change `computeDRepPulser` to also process proposal-deposits for SPOs. #4324
  - Add `sumAllStakeCompact`.
  - Add `UMap.umElemDelegations` to extract SPO, DRep and rewards for a given stake credential.
  - Add `pdTotalActiveStake` field to `PoolDistr`, to hold the total active stake delegated to pools, including proposal deposits.
  - Add `individualTotalPoolStake` to `IndividualPoolStake` to hold the delegated stake as an absolute number, including proposal deposits.
  - Add lenses:
    + `individualTotalPoolStakeL`
    + `poolDistrTotalL`
* Add lenses to `RewardAccount`. #4309
  - `rewardAccountCredentialL`
  - `rewardAccountNetworkL`
* Add `umElemDRepDelegatedReward` to `UMap`. #4273
* Add `fromDeltaCoin`
* Add trivial `Inject` instances for `()` and `Void`
* Add `byteStringToNum`
* Add functions `rdRewardCoin`, `rdDepositCoin` in UMap.hs
* Add function `mkCoinTxOut` in Core.hs
* Add typeclass `HKDApplicative` and make instances for the following: #4252
  - `HKD Identity`
  - `HKD Maybe`
  - `HKD StrictMaybe`
* Move `Metadatum` from `cardano-ledger-shelley` into a new module `Cardano.Ledger.Metadata`
* Add `mkBasicTxAuxData` and `metadataTxAuxDataL` to `EraTxAuxData` type class.
* Add `Random`, `Uniform` and `UniformRange` instances for `Language`
* Add `decodeCostModelsLenient` and `decodeCostModelsFailing`
* Make decoder for `CostModels` very lenient starting with protocol version `9`.
* Deprecate `decodeValidAndUnknownCostModels`
* Disable deserialization of `CostModels` for `PlutusV3` and newer whenever current
  protocol version is not set to at least version `9`
* Remove `CostModelError` and `costModelsErrors`
* Stop re-exporting `CostModelApplyError`
* Change `CostModel` parameter value type from `Integer` to `Int64`. Affects type
  signatures of all functions that work on raw cost model parameter values.
* `mkCostModelsLenient` was changed to work in `MonadFail`, since failures during
  construction is now again possible.
* Deprecate `decodeCostModelFailHard` in favor of new `decodeCostModel`.

### `testlib`

* Export `subState`
* Remove `FlexibleCostModels` and make `Arbitrary` instance for `CostModels` more flexible.

## 1.11.0.0

* Add `shouldSatisfyExpr`
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
  - `getMinFeeTx` in `EraTx`
  - `setMinFeeTx`
  - `estimateMinFeeTx`
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
  - Make `PlutusWithContext` era agnostic, but Language aware. `pwcScript` can be either
    in decoded or binary format. `pwcProtocolVersion` was added too.
  - `debugPlutus`, `runPlutusScript`, `runPlutusScriptWithLogs` and
    `explainPlutusEvaluationError` no longer accept `ProtVer` as argument, since major
    protocol version has been added to `PlutusWithContext`
  - Change constructor of `ScriptFailure` from `PlutusSF` to `ScriptFailure` and add
    record names: `scriptFailureMessage` and `scriptFailurePlutus`
  - Remove `PlutusDebugLang`, `PlutusDebug`, `PlutusData` and `PlutusError`
  - Stop re-exporting   removed:   `EraPlutusContext`,  `PlutusTxCert`,   `unTxCertV1`,
    `unTxCertV2` and `unTxCertV3`
  - Add `evaluatePlutusWithContext`
  - Remove `deserialiseAndEvaluateScript` in favor of `evaluatePlutusWithContext`
* Changes to `Cardano.Ledger.Plutus.Language`:
  - Rename `BinaryPlutus` to `PlutusBinary` for consistency with names of other Plutus types.
  - Add `Plutus` with helpers: `decodeWithPlutus`, `isValidPlutus`
  - Add `PlutusRunnable` with helpers: `plutusFromRunnable`
  - Add `asSLanguage`
  - Add `plutusSLanguage` and `plutusLanguage`
  - Deprecated `fromSLanguage` in favor of more general `plutusLanguage`
  - Rename `IsLanguage` class to `PlutusLanguage` and add these functions to the class:
    `decodePlutusRunnable`, `evaluatePlutusRunnable` and `evaluatePlutusRunnableBudget`
* Changes to `Cardano.Ledger.Plutus.TxInfo`:
  - `NFData` instance for `TxOutSource`
  - Remove: `transDataHash'`, `transHash` (use `hashToBytes` instead), `transTxOutAddr`,
    `txInfoIn'`, `getWitVKeyHash`, `VersionedTxInfo`, `EraPlutusContext`, `PlutusTxCert`,
    `unTxCertV1`, `unTxCertV2`, `unTxCertV3` and `txInfoId` (use `transTxId` instead)
  - Add `transCoinToValue`, `transTxId`
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
  - `ppProtocolVersionL`
  - `ppuProtocolVersionL`
* Add `Generic` instance for `AuxiliaryDataHash`
* Add `ToExpr` instances for:
  - `CompactAddr`
  - `AuxiliaryDataHash`
  - `CompactDeltaCoin`
  - `VKey`
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
  - `CompactAddr`
  - `Withdrawals`
  - `AuxiliaryDataHash`
  - `VKey`
  - `ChainCode`
  - `BootstrapWitness`
  - `WitVKey`
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
  - `roundTripEraSpec`
  - `roundTripAnnEraSpec`
  - `roundTripEraTypeSpec`
  - `roundTripAnnEraTypeSpec`
  - `roundTripShareEraSpec`
  - `roundTripShareEraTypeSpec`
  - `roundTripEraExpectation`
  - `roundTripEraTypeExpectation`
  - `roundTripAnnEraExpectation`
  - `roundTripAnnEraTypeExpectation`
  - `roundTripShareEraExpectation`
  - `roundTripShareEraTypeExpectation`
  - `roundTripCoreEraTypesSpec`

## 1.6.0.0

* Add `lookupRegStakeTxCert` and `lookupUnRegStakeTxCert` to `EraTxCert` typeclass #3700
* Change `ToJSONKey`/`FromJSONKey` implementation of `Credential` to flat text
* Add one more parameter to `getConsumedValue` to lookup DRep deposits #3688
  - `Credential 'DRepRole (EraCrypto era) -> Maybe Coin`
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
  - `Voting` to `DRepRole`
  - `CommitteeHotKey` to `HotCommitteeRole`
  - `CommitteeColdKey` to `ColdCommitteeRole`
* Change `VState` to allow committee cold keys to be script-hashes #3581
  - `vsCommitteeHotKeys :: Map (Credential 'CommitteeColdKey eracrypto) (Maybe (Credential 'CommitteeHotKey eracrypto))`
* Adopt `Default` instances #3556
  - Moved instances for `SafeHash`, `RewardAcnt` and `Credential` from Shelley.RewardProvenance
* Change `VState` to allow committee hot keys to be script-hashes #3552
  - `vsCommitteeHotKeys :: Map (KeyHash 'CommitteeColdKey eracrypto) (Maybe (Credential 'CommitteeHotKey eracrypto))`
* Added `withSLanguage`
* Move `BinaryPlutus` from `cardano-ledger-alonzo` and changed its `Show` instance to
  display as base64 encoding.
* Added `Plutus`
* Changed `Phase2Script` constructor of the `PhaseScript` type. It now accepts new
  `Plutus` type instead of `Language` and `ShortByteString`
* Remove default implementation for `spendableInputsTxBodyL`
* Add lenses:
  - `dsUnifiedL`
  - `dsGenDelegsL`
  - `dsIRewardsL`
  - `dsFutureGenDelegsL`
  - `certDStateL`
  - `certPStateL`
  - `certVStateL`
* Add `getProducedValue` to `EraUTxO`

## 1.4.1.0

* Add `spendableInputsTxBodyL`

## 1.4.0.0

* Added `DRep`, `DRepCredential`
* Changed type `Credential 'Voting c` -> `DRep c` in:
  - `UMElem`
  - `umElemAsTuple`
  - `umElemDRep`
  - `UMap`
  - `DRepUView`
  - `dRepUView`
  - `dRepMap`
  - `unify`

## 1.3.1.0

* Addition of `getPoolCertTxCert`

## 1.3.0.0

* Add delegated representatives to the `UMap` and make its interface more coherent #3426
  - Additions
    + Add `Credential 'Voting (EraCrypto era)` as the fourth element to the `UMap` n-tuple.
    + `umElemPtrs :: UMElem c -> Maybe (Set Ptr)`
    + `umElemDRep :: UMElem c -> Maybe (Credential 'Voting (EraCrypto era))`
    + `UView (DRepUView)` constructor and `dRepUView`
    + `invPtrMap :: UMap c -> Map (Credential 'Staking c) (Set Ptr)`
    + `dRepMap :: UMap c -> Map (Credential 'Staking c) (Credential 'Voting c)`
    + synonym `unionL = (∪)`
    + synonym `unionR = (⨃)`
    + synonym `domDelete = (⋪)`
    + synonym `rngDelete = (⋫)`
  - Renames
    + `Trip` to `UMElem`, pattern `Triple` to `UMElem`
    + `viewTrip` to `umElemAsTuple`
    + `tripRewardActiveDelegation` to `umElemRDActive`
    + `tripReward` to `umElemRDPair`
    + `tripDelegation` to `umElemSPool`
    + `View (RewardDeposits, Delegations, Ptrs)` to `UView (RewDepUView, SPoolUView, PtrUView)`
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
    + `zero` to `nullUMElem`
    + `zeroMaybe` to `nullUMElemMaybe`
    + `sumRewardsView` to `sumRewardsUView`
    + `sumDepositView` to `sumDepositUView`
  - Reimplementations
    + `unionRewAgg` NOTE: It does not require `assert (Map.valid result) result` any more
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
  - Add `Cardano.Ledger.UMap.rdPairView` to view the reward-deposits pair from the `UMap`.
* Replace `DPState c` with `CertState era`
* Add `VState`
* Add `certVState`
* Parametrize `DState` and `PState` by era
* Rename `Cardano.Ledger.DPState` module to `Cardano.Ledger.CertState`
* Rename:
  - `lsDPState` -> `lsCertState`
  - `dpsPState` -> `certPState`
  - `dpsDState` -> `certDState`
  - `obligationDPState` -> `obligationCertState`
* Add support for `PlutusV3`

## 1.1.0.0

* Add `ToJSON (PParamsHKD f era)` superclass constraints for `EraPParams`.
* Add `ToJSON (TxOut era)` superclass constraints for `EraTxOut`.
* Add superclass constraints for `Val`:
  - `NoThunks`, `EncCBOR`, `DecCBOR`, `ToJSON`, `NFData`, `Show`
  - `EncCBOR (CompactForm t)`, `DecCBOR (CompactForm t)`
* Add `ToJSONKey`/`FromJSONKey` instances for `ScriptHash`
* Add `ToJSON`/`FromJSON` instances for `CompactForm Coin`, `SafeHash` and `TxId`
* Add `ToJSON` instances for:
  - `Ptr`, `CertIx`, `TxIx`
  - `Trip` and `UMap`
  - `DeltaCoin` and `CompactForm DeltaCoin`
  - `InstantaneousRewards`, `FutureGenDeleg`, `PState`, `DState` and `DPState`.
  - `UTxO` and `TxIn`
  - `Stake`, `SnapShot`, `SnapShots`, `PoolDistr` and `IndividualPoolStake`
  - `Reward` and `RewardType`
  - `AuxiliaryDataHash`
  - `Credential`
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
