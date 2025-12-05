# Version history for `cardano-ledger-shelley`

## 1.18.0.0

* Renamed:
  - `sppMinFeeA` -> `sppMinFeeFactor`
  - `ppMinFeeA` -> `ppMinFeeFactor`
  - `sppMinFeeB` -> `sppMinFeeConstant`
  - `ppMinFeeB` -> `ppMinFeeConstant`
* Changed type of `sppMinFeeA` to `CoinPerByte`:
* Change sets containing errors into `NonEmptySet` for `ShelleyUtxoPredFailure`, `ShelleyUtxowPredFailure`
* Change all lists into `NonEmpty` for `ShelleyUtxoPredFailure`, `ShelleyUtxowPredFailure`
* Changed the type of the following fields to `CompactForm Coin` in `ShelleyPParams`:
  - `sppMinFeeB`
  - `sppKeyDeposit`
  - `sppMinUTxOValue`
  - `sppMinPoolCost`
* Re-export `ShelleyBlockBody` from `Cardano.Ledger.Shelley.Core`
* Add `cddl` sub-library, and `generate-cddl` executable.
* Change the field type of `ShelleyIncompleteWithdrawals` to `Map RewardAccount (Mismatch RelEQ Coin)`
* Replace `StakePoolState` values in `psFutureStakePoolParams` with `StakePoolParams`
* Remove `psFutureStakePoolsL`
* Add `psFutureStakePoolParamsL`
* Remove deprecated function `getPoolParameters`
* Remove deprecated function `toShelleyGenesisPairs`
* Remove deprecated type `RewardAccounts`
* Remove deprecated lenses `esAccountStateL`, `asTreasuryL`, `asReservesL`, `utxosUtxoL`, `epochStateTreasuryL`
* Remove deprecated type `PPUpdateEnv`
* Remove deprecated function `toShelleyTransitionConfigPairs`
* Remove deprecated function `hashShelleyTxAuxData`
* Remove deprecated function `addrWits'`
* Remove deprecated type `Shelley`
* Remove deprecated module `Cardano.Ledger.Shelley.BlockChain`
* Change state of `ShelleyDELEG` rule from `DState` to `CertState`
* Add `EraCertState` constraint to `STS` instance for `ShelleyDELEG`
* Add `TxLevel` argument to `Tx` and `TxBody`
* Add `HasEraTxLevel` instances for `Tx` and `TxBody`
* Add `EraTxLevel` instance
* Add `EncCBOR` and `DecCBOR` instances to `ShelleyBbodyPredFailure`
* Rename `poolParamsP` field to `stakePoolParamsP` in `RewardProvenancePool`
* Move withdrawals-draining from `DELEGS` to `LEDGER`
  - Remove `WithdrawalsNotInRewardsDELEGS`
  - Add to `ShelleyLedgerPredFailure`
    + `ShelleyWithdrawalsMissingAccounts`
    + `ShelleyIncompleteWithdrawals`
  - Add `testIncompleteAndMissingWithdrawals`
* Added `Generic` instance to `ShelleyTxOut`
* Add `AtMostEra "Conway" era` constraint to `ShelleyEraTxCert`, effectively disabling it for Dijkstra onwards

### `cddl`

* Add `HuddleRule1` instances for sets.
* Move `cddl-files` to `cddl/data`.
* Export `scriptAllGroup` and `scriptAnyGroup`.
* Add `HuddleSpec` module with `Huddle{Rule|Group}` instances for all types.
* Add and export smart constructors for transaction components, certificates, pool infrastructure, and block structures.
* Add `generate-cddl` executable target to test the generation of `.cddl` files against the existing `huddle-cddl` executable.

### `testlib`

* Renamed:
  - `maxMinFeeA` -> `maxMinFeeFactor`
  - `maxMinFeeB` -> `maxMinFeeConstant`
* Add:
  - `submitBlock_`
  - `submitBlock`
  - `submitFailingBlock`
  - `submitFailingBlockM`
  - `withTxsInBlock_`
  - `withTxsInBlock`
  - `withTxsInFailingBlock`
  - `withTxsInFailingBlockM`
  - `tryTxsInBlock`
* Remove `tryRunImpBBODY`
* Add `Eq` instances for:
  - `AlonzoBbodyEvent`
  - `ShelleyBbodyEvent`
  - `ShelleyLedgersEvent`
* Add `NFData` and `ToExpr` constraints and instances for:
  - `AlonzoBlockBody`
  - `AlonzoBbodyPredFailure`
  - `ConwayBbodyPredFailure`
  - `ShelleyBlockBody`
  - `ShelleyBbodyPredFailure`
  - `BHeaderView`
  - `Block`
* Add a `Generic` instance for `BHeaderView`
* Add `impEventsFrom`, `impRecordSubmittedTxs`
* Change type of `ImpTestState.impEvents` field from `[]` to `Seq`
* Remove `huddle-cddl` and the `CDDL` modules.
* Add `ToCBOR (StashedAVVMAddresses era)` superclass to `ShelleyEraTest`
* Add `duplicateDelegCertsTxBody`
* Renamed `impLastTick` to `impCurSlotNo` and `impLastTickG` to `impCurSlotNoG`
* Add CDDL certificate definitions: `account_registration_cert`, `account_unregistration_cert`, `delegation_to_stake_pool_cert`
* Add CDDL pool certificate definitions via `mkPoolRules`: `pool_registration_cert`, `pool_retirement_cert`
* Add CDDL legacy certificate definitions: `genesis_delegation_cert`, `genesis_hash`, `genesis_delegate_hash`, `move_instantaneous_rewards_cert`, `move_instantaneous_reward`, `delta_coin`
* Remove old CDDL certificate definitions: `stake_registration`, `stake_deregistration`, `stake_delegation`, `pool_registration`, `pool_retirement`, `genesis_key_delegation`
* Remove CDDL pool primitives (moved to core): `credential`, `stake_credential`, `pool_params`, `port`, `ipv4`, `ipv6`, `dns_name`, `single_host_addr`, `single_host_name`, `multi_host_name`, `relay`, `pool_metadata`, `url`
* Rename `multisig_script` -> `shelley_native_script` in CDDL
* Move `script_hash` to core CDDL
* Move `transaction_metadatum_label`, `transaction_metadatum`, `transaction_index` and `metadata_hash` to core CDDL
* Remove deprecated module `Test.Cardano.Ledger.Shelley.Generator.Constants`
* Remove deprecated module `Test.Cardano.Ledger.Shelley.Generator.Delegation`
* Remove deprecated module `Test.Cardano.Ledger.Shelley.Generator.Trace.DCert`
* Remove deprecated functions `getReward`, `lookupReward`, `getRewardAccountAmount`
* Correct the CDDL definition of the `sequence_number` to be sized
* Export polymorphic CDDL definition for `protocol_version`
* Move CDDL definition for `nonce`, `epoch`, `slot` and `block_number` to core
* Rename `examplePoolParams` to `exampleStakePoolParams`
* Removed `shelleyEraSpecificSpec`
* Added `shelleyGenUnRegTxCert`
* Added `genUnRegTxCert` to `ShelleyEraImp`
* Added `shelleyGenRegTxCert`
* Added `genRegTxCert` to `ShelleyEraImp`
* Added `delegStakeTxCert` to `ShelleyEraImp`
* Added `expectStakeCredRegistered`, `expectStakeCredNotRegistered`, `expectDelegatedToPool`, `expectNotDelegatedToPool` to `ImpTest`

## 1.17.0.0

* Changed `MaxTxSizeUTxO` and `sizeShelleyTxF` to use `Word32`
* Remove:
  - `ShelleyEpochPredFailure`
  - `ShelleyMirPredFailure`
  - `ShelleyNewEpochPredFailure`
  - `ShelleyPoolreapPredFailure`
  - `ShelleyRupdPredFailure`
  - `ShelleySnapPredFailure`
  - `ShelleyTickPredFailure`
  - `ShelleyTickfPredFailure`
  - `ShelleyUpecPredFailure`
  - `TickTransitionError`
  - `UpecPredFailure`
* Remove `withCborRoundTripFailures`
* Refactor pool deposits to use `StakePoolState`. #5234
  - Update `Pool` rule to store deposits in individual `StakePoolState` records
  - Add and export `prUTxOStateL`, `prChainAccountStateL`, and `prCertStateL` lenses for `ShelleyPoolreapState`
  - Update genesis stake pool registration to use `mempty` deposits for initial pools per specification
  - Remove `epochStatePoolParamsL` lens.
  - Remove `psDepositsL` as no longer necessary.
* Add `hardforkConwayDisallowDuplicatedVRFKeys`
* Add `VRFKeyHashAlreadyRegistered` to `ShelleyPoolPredFailure` type
* Add `NFData` for `NominalDiffTimeMicro`, `ShelleyGenesisStaking` and `ShelleyGenesis`
* Deprecate `PoolParams` in favor of `StakePoolState`. #5196
  - Deprecate the API `getPoolParameters` in favor of `getStakePools`.
  - Deprecate the lens `epochStatePoolParamsL` in favor of `epochStateStakePoolsL`.
  - Rename
    + `psStakePoolParamsL` to `psStakePoolsL`
    + `psFutureStakePoolParamsL` to `psFutureStakePoolsL`
* Add `mkBasicBlockBodyShelley` and `txSeqBlockBodyShelleyL` to use in Allegra and Mary.
* Add `Default` instance for `NewEpochState`
* Remove `epochStateUMapL`, `unifiedL`, `rewards`, `delegations`, `ptrsMap` and `dsUnifiedL`
* Add `ShelleyEraAccounts` with `mkShelleyAccountState`, `accountsPtrsMapG`, `ptrAccountStateG`
* Add `ShelleyAccounts`, `ShelleyAccountState`, `shelleyAddAccountState`,  `registerShelleyAccount` and ` unregisterShelleyAccount`
* Rename `registerInitialFundsThenStaking` to `shelleyRegisterInitialFundsThenStaking`
* Deprecate `Cardano.Ledger.Shelley.BlockChain` in favor of `Cardano.Ledger.Shelley.BlockBody`. #5156
  - Rename `ShelleyTxSeq` to `ShelleyBlockBody`
* Rename `shelleyEqTxRaw` to `shelleyTxEqRaw`
* Add `Generic` instances for `ShelleyBbodyEvent` and `ShelleyLedgersEvent`
* Move some hard-fork triggers and export them from `Cardano.Ledger.Shelley` module:
  - `aggregateRewards` to `hardforkAllegraAggregateRewards`.
  - `allowMIRTransfer` to `hardforkAlonzoAllowMIRTransfer`.
  - `validatePoolRewardAccountNetID` to `hardforkAlonzoValidatePoolRewardAccountNetID`.
  - `forgoRewardPrefilter` to `hardforkBabbageForgoRewardPrefilter`.
  - Delete the `Shelley.HardForks` module.
* Add `disallowUnelectedCommitteeFromVoting` to `Shelley.HardForks`. #5091
  - This tests if the protocol version is greater than 10.
* Deprecated `toShelleyGenesisPairs`
* Add `ToJSON` and `FromJSON` instances for `FromByronTranslationContext`
* Deprecated `toShelleyGenesisPairs` and `toShelleyTransitionConfigPairs`
* Removed `toShelleyGenesisPairs`
* Remove `ShelleyTxRaw`, `MkShelleyTx`, `segWitTx`, `unsafeConstructTxWithBytes`
* Added `Generic` instances for:
  - `ShelleyBbodyState`
  - `ShelleyScriptsNeeded`
* Remove Bool argument from `auxDataSeqDecoder`
* Move to `testlib` `DecCBOR` instances for: `ShelleyTxSeq`, `ShelleyTxRaw`, `ShelleyTx`, `TxBody ShelleyEra`, `ShelleyTxAuxData`, `ShelleyTxWitsRaw`, `ShelleyTxWits`, `MultiSigRaw`, `MultiSig`
* Remove `segWitTx`
* Add:
  - `unlessMajorVersion`
  - `whenMajorVersion`
  - `whenMajorVersionAtLeast`
  - `whenMajorVersionAtMost`
  - `cantFollow`
  - `majorFollow`
  - `minorFollow`
* Remove `CorruptRewardUpdate` predicate failure and replace that check with an assertion. #5007
* Added to `PParams`: `shelleyPParams`, `ppA0`,`ppD`,`ppEMax`,`ppExtraEntropy`,`ppMaxBBSize`,`ppKeyDeposit`,`ppMinFeeA`,`ppMinFeeB`,`ppMinPoolCost` `ppMaxBHSize`,`ppMaxTxSize`,`ppNOpt`,`ppProtocolVersion`,`ppPoolDeposit`,`ppRho`,`ppTau`
* Removed from `PParams`: `shelleyCommonPParamsHKDPairs`,`shelleyCommonPParamsHKDPairsV6`,`shelleyCommonPParamsHKDPairsV8`
* Replace export from `Cardano.Ledger.Shelley.UTxO` of deprecated `balance` and `coinBalance` with `sumUTxO` and `sumCoinUTxO` respectively
* Remove `ShelleyTxBody`
* Removed `era` parameter from `ShelleyTxBodyRaw`
* Remove `HeapWords` instances for `ShelleyTxOut`: #5001
* Deprecate `witsFromTxWitnesses`
* Expose access to `ShelleyTxRaw`, `ShelleyTxAuxDataRaw`, `ShelleyTxBodyRaw`, `ShelleyTxWitsRaw`, `MkMultiSig`
* Expose constructors `MkShelleyTx`, `MkShelleyTxAuxData`, `MkShelleyTxBody`, `MkShelleyTxWits`, `MultiSigRaw`
* Remove re-export of `WitVKey` from `Cardano.Ledger.Shelley.TxWits`
* Deprecate `esAccountStateL`, `epochStateTreasuryL`, `asTreasuryL` and `asReservesL`
* Rename `esAccountState` to `esChainAccountState`
* Replaced `prDState` and `prPState` with `prCertState` in `ShelleyPoolreapState`
* Removed `ShelleyPoolreapEnv` (became obsolete)
* Removed `shelleyCertVState` from `ShelleyCertState`
* Moved `CertState` to `State` module:
  - `epochStateRegDRepL` moved to Conway
  - `vsDRepsL` moved to Conway
  - `vsCommitteeStateL` moved to Conway

### `testlib`

* Remove `shelleyAccountsToUMap` corresponding to the removal of `UMap` from core.
* Added `impSatisfySignature` and `impSatisfyMNativeScripts`
* Added `EraSpecificSpec ShelleyEra` instance
* Added `EraSpecificSpec` class
* Removed `ShelleyEraTxCert` from `ShelleyEraImp`, so added `ShelleyEraTxCert` constraint to:
  - `registerStakeCredential`
  - `delegateStake`
  - `registerRewardAccount`
  - `registerPool`
* Added `withEachEraVersion`
* Added `Examples` module with: `LedgerExamples`, `ledgerExamples`, `mkLedgerExamples`, `exampleCerts`,`exampleWithdrawals`, `exampleAuxDataMap`, `exampleNonMyopicRewards`, `exampleCoin`, `examplePayKey`, `exampleStakeKey`, `exampleNewEpochState`, `examplePoolDistr`, `examplePoolParams`, `exampleTxIns`, `exampleProposedPPUpdates`, `exampleByronAddress`, `testShelleyGenesis`, `keyToCredential`, `mkDSIGNKeyPair`, `mkKeyHash`, `mkScriptHash`, `mkWitnessesPreAlonzo`, `seedFromByte`, `seedFromWords`
* Add `nativeAlwaysFails`, `nativeAlwaysSucceeds`
* Deprecated `getRewardAccountAmount`, `lookupReward` and `getReward`
* Add `getBalance`, `lookupBalance`, `getAccountBalance`, `lookupAccountBalance`
* Add `mkShelleyTestAccountState`, `shelleyAccountsFromAccountsMap`, `shelleyAccountsToUMap`
* Add `ToExpr` instances for `ShelleyBbodyEvent` and `ShelleyLedgersEvent`
* Move `EncCBOR` instance for `RawSeed` from `cardano-ledger-shelley-test`
* Added `Arbitrary` instance for `TransitionConfig ShelleyEra`
* Rename `poolParams` to `freshPoolParams`
* Added `ToExpr` instances for:
  - `ShelleyScriptsNeeded`
  - `ShelleyBbodyState`
  - `ShelleyTickPredFailure`
  - `ShelleyNewEpochPredFailure`
  - `ShelleyEpochPredFailure`
  - `ShelleyUpecPredFailure`
  - `ShelleyPoolPredFailure`
  - `ShelleySnapPredFailure`
  - `ShelleyMirPredFailure`
  - `ShelleyRupdPredFailure`
* Added `Era` module with `ShelleyEraTest` class

## 1.16.0.0

* Add `ShelleyInstantStake`, `shelleyInstantStakeCredentialsL`, `addShelleyInstantStake`, `deleteShelleyInstantStake`, `resolveShelleyInstantStake`
* Remove  `IncrementalStake` type, `incrementalStakeDistr`, `updateStakeDistribution`, `utxosStakeDistrL`
* Replace `IncrementalStake` with `InstantStake` in `UTxOState`
* Add `ToCBOR` and `FromCBOR` instance for `FromByronTranslationContext`
* Add `auxDataSeqDecoder`
* Remove `constructMetadata`
* Remove `getProposedPPUpdates` as no longer relevant
* Remove `proposalsL` and `futureProposalsL` as unused
* Remove redundant supercalss constraints for `ApplyBlock`
* Add `applyBlockEither`, `applyBlockEitherNoEvents`, `applyBlockNoValidaton`, `applyTickNoEvents`.
* Add `applyBlock` and `applyTick` to `ApplyBlock` type class.
* Remove `applyBlockOpts` (in favor of `applyBlockEither`), `reapplyBlock` (in favor of
  `applyBlockNoValidation`)  and `applyTickOpts` (in favor `applyTick`).
* Disable validation level for `applyTick`
* Add `DecCBOR` instances for:
  - `ShelleyTxWits`
  - `ShelleyTxAuxData`
  - `ShelleyTxBody`
  - `ShelleyTx`
  - `ShelleyTxSeq`
* Add `segWitTx`
* Rename `segwitTx` to `segWitAnnTx`
* Converted `CertState` to a type family
* Restrict the monad of `applyTx` and `reapllyTx` to `Either` from abstract `MonadError`
* Remove `applyTxOpts` in favor of new `applyTxValidation` function in `ApplyTx`
* Move `reapplyTx` outside of the `ApplyTx` type class.
* Add helper `ruleApplyTxValidation` that is used to implement `applyTxValidation`
* Remove the no longer necessary `ledgerMempool` field
* Fix `NFData` instance for `LedgerEnv`
* Move `AccountState` to `Cardano.Ledger.State`
* Deprecated `RewardAccounts`
* Deprecated `utxosUtxoL`
* Added `CanGetUTxO` and `CanSetUTxO` instances for `EpochState`, `UTxOState`, `NewEpochState`, `LedgerState`
* Made the fields of predicate failures and environments lazy
* Changed the type of `sgSecurityParam` to `NonZero Word64`
* Following functions now expect a `NonZero Word64` security parameter:
  - `startStep`
  - `createRUpd`
  - `desirability`
  - `getTopRankedPools`
  - `getTopRankedPoolsVMap`
* Remove `Era era` constraint from `sizeShelleyTxF` and `wireSizeShelleyTxF`
* Add `MemPack` instance `ShelleyTxOut`
* Deprecate `hashShelleyTxAuxData`
* Stop re-exporting `ScriptHash` from `Cardano.Ledger.Shelley.Scripts`. Import it instead from `Cardano.Ledger.Hashes`.
* Deprecate `Shelley` type synonym
* Deprecate `PPUpdateEnv`
* Remove crypto parametrization from:
  - `ShelleyEra`
  - `ShelleyGenesisStaking`
  - `ShelleyGenesis`
  - `IncrementalStake`
  - `NonMyopic`
  - `RewardProvenancePool`, `RewardProvenance`
  - `RewardAns`, `RewardUpdate`, `RewardSnapShot`, `FreeVars`, `RewardPulser`, `PulsingRewUpdate`
  - `PulsingRewUpdate`, `PoolRewardInfo`
  - `FromByronTranslationContext`
  - `GenesisDelegCert`, `MIRTarget`, `MIRCert`, `ShelleyDelegCert`

### `testlib`

* Renamed:
  - `lookupKeyPair` -> `getKeyPair`
  - `lookupByronKeyPair` -> `getByronKeyPair`
  - `tryLookupReward` -> `lookupReward`
  - `lookupReward` -> `getReward`
  - `lookupImpRootTxOut` -> `getImpRootTxOut`
  - `impGetNativeScript` -> `impLookupNativeScript`
  - `impLookupUTxO` -> `impGetUTxO`
* Converted `CertState` to a type family
* Add `disableImpInitExpectLedgerRuleConformance`. #4821

## 1.15.0.0

* Change param of `PoolRank.desirability` to `Word16`
* Change type of `nOpt` in `RewardParams` to `Word16`
* Add lenses for `LedgerEnv`. #4748
  - `ledgerSlotNoL`
  - `ledgerEpochNoL`
  - `ledgerIxL`
  - `ledgerPpL`
  - `ledgerAccountL`
  - `ledgerMempoolL`
* Change `PoolEnv` to take `EpochNo` instead of `SlotNo`
* Add `EpochNo` to `DelplEnv`
* Add `Maybe EpochNo` to `LedgerEnv`
* Add `EpochNo` to `LedgersEnv`
* Added `Generic`, `Eq`, `Show`, `NFData`, `EncCBOR` instances for `ShelleyLedgersEnv`
* Remove deprecated `witsVKeyNeededGov`, `witsVKeyNeededNoGov`, `shelleyWitsVKeyNeeded` and `propWits`
* Remove deprecated `PPUPPredFailure`, `delPlAcnt`, `prAcnt`, `votedValue`
* Remove impossible predicate failure `StakeKeyInRewardsDELEG`
* Change parameter type  of `StakeKeyNonZeroAccountBalanceDELEG` from  `Maybe Coin` to `Coin`
* Remove deprecated `hashMultiSigScript` and `txwitsScript`
* Remove deprecated `keyBy`, `requiresVKeyWitness` and `Metadata`
* Remove deprecated `updatePParams`, `txup`, `scriptCred`, `scriptStakeCred`, `scriptsNeeded`
* Remove deprecated `proposals`, `futureProposals`, `sgovPp` and `sgovPrevPp`
* Remove deprecated module `Cardano.Ledger.Shelley.Metadata`
* Remove deprecated module `Cardano.Ledger.Shelley.PoolParams`
* Remove deprecated module `Cardano.Ledger.Shelley.EpochBoundary`
* Remove deprecated module `Cardano.Ledger.Shelley.Delegation.Certificates`
* Remove deprecated module `Cardano.Ledger.Shelley.Address.Bootstrap`
* Remove deprecated `applyTxs` and `applyTxsTransition`
* Remove `Cardano.Ledger.Shelley.API.Genesis` module with deprecated type class
  `CanStartFromGenesis`. Also removes helper binding `initialStateFromGenesis`.
* Remove deprecated `ShelleyEraCrypto`
* Remove deprecated `evaluateMinLovelaceOutput`, `addShelleyKeyWitnesses`, `evaluateTransactionBalance`
* Remove deprecated `KeyPairs` and stop exporting `KeyPair`
* Remove `mkDelegation` and deprecated `Delegate`, `RegKey`, `DeRegKey`, `delegCWitness`, `isRegKey` and `isDeRegKey`
* Remove export of deprecated `RewardAcnt` and `poolCWitness`
* Change type of VRF key hash in `GenesisDelegCert`, `GenesisDelegTxCert` and `DuplicateGenesisVRFDELEG` to `VRFVerKeyHash`
* Added `EncCBOR` instance for `LedgerEnv`
* Use `Mismatch` to clarify *some more* predicate failures. #4711
  - `Shelley/InsufficientForInstantaneousRewardsDELEG`
  - `Shelley/MIRCertificateTooLateinEpochDELEG`
  - `Shelley/InsufficientForTransferDELEG`
  - `Shelley/ExpiredUTxO`
  - `Shelley/ValueNotConservedUTxO`

### `testlib`

* Changed the return type of `iteExpectLedgerRuleConformance`
* Add `runSTS`
* Add `iteExpectLedgerRuleConformance` to `ImpTestEnv` for additionally checking conformance with ImpTests. #4748
  - Add lens `iteExpectLedgerRuleConformanceL`.
  - Add `modifyImpInitExpectLedgerRuleConformance`.
* Added `tryLookupReward`
* Switch to using `ImpSpec` package
* Remove: `runImpTestM`, `runImpTestM_`, `evalImpTestM`, `execImpTestM`, `runImpTestGenM`, `runImpTestGenM_`, `evalImpTestGenM`, `execImpTestGenM`, `withImpState` and `withImpStateModified`.
* Add `LedgerSpec`, `modifyImpInitProtVer`.
* Re-export `ImpM` and `ImpInit`
* Remove `iteState` and `iteQuickCheckSize` from `ImpTestEnv`
* Added `ToExpr` instance for `ShelleyLedgersEnv`
* Changed type signature of `freshKeyHashVRF` to return `VRFVerKeyHash` instead of just a `Hash`
* Added `expectUTxOContent`
* Added `disableTreasuryExpansion`
* Added a `MonadFail` constraint to two methods of `ShelleyEraImp`:
  - `initGenesis`
  - `initNewEpochState`
* Added a `MonadFail` constraint to:
  - `defaultInitNewEpochState`
  - `defaultInitImpTestState`
* Added `logText`
* Added `ToExpr` instance for `LedgerEnv`
* Added `tryRunImpRuleNoAssertions` to `ImpTest`

## 1.14.1.0

* Revert changes to serialization of `ShelleyPoolPredFailure`

## 1.14.0.0

* Remove `ShelleyNewppPredFailure`. #4649
* Change predicate-failures and their serialization to use the `Mismatch` type to report supplied and expected values. #4649
  - `MaxTxSizeUTxO`
  - `FeeTooSmallUTxO`
  - `WrongBlockBodySizeBBODY`
  - `InvalidBodyHashBBODY`
  - `ConflictingMetadataHash`
  - `StakePoolRetirementWrongEpochPOOL`
  - `StakePoolCostTooLowPOOL`
  - `WrongNetworkPOOL`
  - `NonGenesisUpdatePPUP`
* Deprecated `applyTxs` and `applyTxsTransition` in `Mempool`
* Replaced `applyTx` in `ApplyTx` class with `applyTxOpts`
* Added and exposed `applyTx` in `Mempool`
* Added `mempool` to `LedgerEnv`
* Added `registerStakeCredential` and `delegateStake` to `ImpTest`
* Remove protocol version argument from `mkShelleyGlobals` (`maxMajorPV` was removed from `Globals`)
* Added `EncCBOR` instances for:
  - `UtxoEnv`
  - `CertEnv`
* Expose `ptrMapL`

### `testlib`

* Exposed `registerPoolWithRewardAccount`
* Added `poolParams` to `ImpTest`
* Changed signature of `registerPool`
* Remove `minMajorPV` and `maxMajorPV` from `Constants`
* Add `logDoc` that takes a `Doc AnsiStyle` instead of a `String`
* Rename `logEntry` to `logString`
* Added CDDL definitions in Test.Cardano.Ledger.Shelley.CDDL

## 1.13.0.0

* Add `translateToShelleyLedgerStateFromUtxo`
* Added `wireSizeShelleyTxF`

### `testlib`

* Add `submitFailingTxM`
* Add `fixupTxOuts`
* Add `initGenesis` and `initNewEpochState` to `ShelleyEraImp`. Change type signature and
  semantics of `initImpTestState`
* Add `defaultInitNewEpochState` and `defaultInitImpTestState`
* Add `impEraStartEpochNo`
* Add `fixupAuxDataHash`
* Add `impSetSeed`

## 1.12.2.0

### `testlib`

* Add `ToExpr` instance for `RewardUpdate`
* Add `advanceToPointOfNoReturn`

## 1.12.1.0

### `testlib`

* Add `impLogToExpr`
* Add `freshKeyAddr_`

## 1.12.0.0

* Add `NFData` instance for `PoolEnv`.
* Add `Generic` and `NFData` instances for `DelegEnv`
* Introduce `ShelleyEraScript` class
* Add `ShelleyEraScript` for `ShelleyEra`
* Replace patterns within `MultiSig` with `ShelleyEraScript`-constrained ones:
  - `RequireSignature`
  - `RequireAllOf`
  - `RequireAnyOf`
  - `RequireMOf`
* Change signatures of `evalMultiSig` and `validateMultiSig`:
  - replace `Era` constraint with `ShelleyEraScript`
  - replace `MultiSig` with `NativeScript`
* Add `Inject` instances for:
  - `UTxOState`
  - `UtxoEnv`

### `testlib`

* Change return type of `trySubmitTx` to include the fixedup transaction
* Add `ToExpr` for `PoolEnv`.
* Add `withCborRoundTripFailures`
* Change signatures of `Arbitrary` instances for `MultiSig`:
  - replace `Era` constraint with `ShelleyEraScript`
  - add `NativeScript era ~ MultiSig era` constraint

## 1.11.0.0

* Introduce `futurePParamsGovStateL`, `futurePParamsShelleyGovStateL` and `sgsFuturePParams`
* Deprecate `votedValue` in favor of `votedFuturePParams`.

### `testlib`

* Add `Testable` instance for `ImpTestM`
* Export `impNESL` instead of of `impNESG` from `ImpTest`
* Replace `initImpNES` with `initImpTestState` and change its return type to MonadState
* Add functions to Shelley `ImpTest`:
  - `withFixup`
  - `withCustomFixup`
  - `withPreFixup`
  - `withPostFixup`
* Add `ToExpr` and `NFData` instances for `UtxoEnv`
* Stop fixing up multi assets in the transaction.
* Change how quickcheck generator is initialized in `runImpTestM` and others derived from it.

## 1.10.0.0

* Remove the `PParams` param from `validateMissingScripts`
* Remove the `missingScriptsSymmetricDifference` function
* Add `NFData` instance for `AdaPots`, `ShelleyDelegEvent`
* Add `Generic`, `Eq` and `NFData` instances for:
  - `ShelleyDelegsEvent`
  - `ShelleyDelplEvent`
  - `ShelleyEpochEvent`
  - `ShelleyLedgerEvent`
  - `ShelleyMirEvent`
  - `ShelleyNewEpochEvent`
  - `PoolEvent`
  - `PoolReap`
  - `PpupEvent`
  - `RupdEvent`
  - `SnapEvent`
  - `ShelleyTickEvent`
  - `UtxoEvent`
  - `ShelleyUtxowEvent`
* Rename `NewEpoch` constructor of `ShelleyDelegEvent` to `DelegNewEpoch`
* Rename `ShelleyGovState` fields:
  - `proposals` to `sgsCurProposals`
  - `futureProposals` to `sgsFutureProposals`
  - `sgovPp` to `sgsCurPParams`
  - `sgovPrevPp` to `sgsPrevPParams`
* Change `UPEC` environment to `LedgerState`
* Rename `currentPp` to `usCurPParams` and `ppupState` to `usGovState`
* Change `ApplyTxError`, `TickTransitionError` and `BlockTransitionError`
  to use `NonEmpty (PredicateFailure _)` instead of `[PredicateFailure _]`
* Removed `prettyWitnessSetParts`
* Re-implemented `ShelleyTxWits` with `MemoBytes`
* Re-implemented `ShelleyTxAuxData` with `MemoBytes`
* Remove `getNextEpochCommitteeMembers` from `EraGov`
* Deprecated `PPUPPredFailure`
* Add instances for `InjectRuleFailure` and switch to using `injectFailure`
* Remove `poolCertTransition`
* Remove `getDRepDistr`, `getConstitution` and `getCommitteeMembers` from `EraGov` #4033
  - Move `Constitution` to `Conway.Governance.Procedures`
* Deprecated `keyBy`
* Add `getShelleyMinFeeTxUtxo`
* Add implementation for `getMinFeeTxUtxo`
* Remove deprecated functions from `Wallet`: `evaluateMinFee` and `evaluateTransactionFee`
* Replace `depositsAdaPot` in `AdaPots` with `obligationsPot`
* Add `sumAdaPots`
* Deprecate `delPlAcnt` in favor of `delPlAccount`
* Rename `RewardAccount` fields `getRwdNetwork` and `getRwdCred` to `raNetwork` and `raCredential` respectively
* Deprecate `prAcnt` in favor of `prAccountState`
* Deprecate `RewardAcnt` in favor of `RewardAccount`
* Remove `registerInitialFunds` and `registerInitialStaking`
* Add `registerInitialFundsThenStaking`
* Modify `PParams` JSON instances to match `cardano-api`
* Add wrapper to `PParams` in `ShelleyGenesis` to preserve the legacy behaviour of JSON instances

### `testlib`

* Add `ToExpr` instances for:
  - `ShelleyDelegsEvent`
  - `ShelleyDelplEvent`
  - `ShelleyEpochEvent`
  - `ShelleyLedgerEvent`
  - `ShelleyMirEvent`
  - `ShelleyNewEpochEvent`
  - `PoolEvent`
  - `PoolReap`
  - `PpupEvent`
  - `RupdEvent`
  - `SnapEvent`
  - `ShelleyTickEvent`
  - `UtxoEvent`
  - `ShelleyUtxowEvent`
* Add `SomeSTSEvent`
* Replaced `small-steps-test` dependency with `small-steps:testlib`
* Change `submitFailingTx`, `tryRunImpRule` and `trySubmitTx`
  to use `NonEmpty (PredicateFailure _)` instead of `[PredicateFailure _]`
* Added `ToExpr` instance for `ShelleyTxWitsRaw`
* Add `RuleListEra` instance for Shelley
* Add argument to `impSatisfyNativeScript` that accounts for already satisifed witnesses
* Remove `impFreshIdxL`
* Remove root coin argument from `initImpNES`, `initShelleyImpNES`
* Adjust `sendCointTo` and `sendValueTo` to return `TxIn`
* Added `ToExpr` instance for `ShelleyTxAuxDataRaw`
* Add:
  - `PlutusArgs`
  - `ScriptTestContext`
  - `shelleyFixupTx`
  - `impGetScriptTestContext`
  - `impGetScriptContextMaybe`
  - `updateAddrTxWits`
  - `addNativeScriptTxWits`
  - `addRootTxIn`
  - `fixupFees`
  - `logFeeMismatch`
  - `impScriptsL`
  - `impNativeScriptsG`
* Add:
  - `expectRegisteredRewardAddress`
  - `expectNotRegisteredRewardAddress`
  - `expectTreasury`
* Add `ToExpr` instances for `AdaPots` and `Obligations`

## 1.9.0.0

* Add `getShelleyGenesisKeyHashCountTxBody`
* Change `PPUP` signal to `StrictMaybe` from `Maybe`
* Deprecate `txup` function.
* Prevent instances for `SahelleyEraTxBody` starting with protocol version 9.
* Add `ToJSON` instance for `ShelleyTxCert`, `ShelleyDelegCert`, `MIRCert`, `MIRTarget`,
  `MIRPot` and `GenesisDelegCert`
* Deprecate `evaluateTransactionFee` in favor of `Cardano.Ledger.Api.Tx.estimateMinFeeTx`
* Change the type signature and the logic in `validateNeededWitnesses`
* Add `getShelleyWitsVKeyNeeded` and `getShelleyWitsVKeyNeededNoGov`
* Deprecate `witsVKeyNeededNoGov` and `shelleyWitsVKeyNeeded`
* Get rid of `ueGenDelegs` and `ueGenDelegsL` as unnecessary.
* Change the signature of `protectMainnet` to make it work with any
  `TransitionConfig`
* Add `protectMainnetLens`
* Add `tcNetworkIDG`
* Add `sgInitialFundsL` and `sgStakingL`
* Stop exporting all of the internal `hkd*` functions and `PParamsHKD` from
  `Cardano.Ledger.Shelley.Core`.
* Deprecated unused `hashMultiSigScript`, `txwitsScript`
* Delete deprecated modules:
  - `Cardano.Ledger.Shelley.Orphans`
  - `Cardano.Ledger.Shelley.Delegation.PoolParams`
  - `Cardano.Ledger.Shelley.LedgerState.RefundsAndDeposits`
* Remove deprecated functions: `updateTxBodyG`, `totalCertsDeposits`,
  `totalCertsDepositsCertState`, `totalTxRefundsShelley`, `keyCertsRefunds`,
  `keyCertsRefundsCertState`, `totalTxDeposits`, `totalTxDepositsShelley` `minfee`,
  `extractKeyHashWitnessSet`, `witKeyHash`, `wvkBytes`, `validateNativeMultiSigScript`,
  `evalNativeMultiSigScript`, `diffWitHashes`, `nullWitHashes`, `unWitHashes` and
  `emptyDState`
* Remove deprecated type: `WitHashes`
* Stop exporting `ShelleyTxBody`, `ShelleyTxOut`, `TxIn`, `TxId` from
  `Cardano.Ledger.Shelley.Tx`
* Stop exporting `GenesisDelegCert`, `MIRCert`, `MIRPot`, `MIRTarget`, `ShelleyDelegCert`,
  `PoolCert`, `PoolMetadata`, `PoolParams`, `Ptr`, `StakePoolRelay`, `TxOut`,
  `ShelleyTxOut`, `Url`, `SizeOfPoolRelays`, `SizeOfPoolOwners`, `Delegation`,
  `addrEitherShelleyTxOutL`, `valueEitherShelleyTxOutL`, `nativeMultiSigTag` and
  `Cardano.Ledger.Keys.WitVKey` module from `Cardano.Ledger.Shelley.TxBody`
* Export `RegTxCert`, `UnRegTxCert`, `DelegStakeTxCert` and `GenesisDelegTxCert` from
  `Cardano.Ledger.Shelley.Core`.
* Change the type of `ppEMaxL`
* Change the type of `pvCanFollow` to make sure new protocol version is not optional
* Add `hasLegalProtVerUpdate`

### `testlib`

* Add `freshKeyHashVRF`
* Add `registerPool`
* Add `sendValueTo` and `sendCoinTo`
* Add `dispenseIndex`
* Add `fixupTx`, `impAddNativeScript` and `logToExpr`
* Remove `mkTxWits`
* Add `impNESG`, `impLastTickG`, `impKeyPairsG` and `impScriptsG`. Stop exporting
  `impNESL` and `impLastTickL`
* Add `impScripts` and `impScriptsG`
* Add `impSatisfyNativeScript` and `impShelleySatisfyNativeScript`
* Rename `emptyImpNES` to `initImpNES`
* Rename `emptyShelleyImpNES` to `initShelleyImpNES`
* Add `lookupImpRootTxOut` and remove `impRootTxCoin`
* Add `submitTx_`, `submitTxAnn` and `submitTxAnn_`.
* Change type signature for `submitTx` and `trySubmitTx`.
* Add `tryRunImpRule`
* Extract `impWitsVKeyNeeded` from the `ShelleyEraImp` type class and make it into an
  `ImpTestM` action
* Add `fixupFees` `impLastTickL` `withNoFixup` `ImpTestEnv` `iteDoFixupL`
* Add `runImpTestM`, `runImpTestM_`, `evalImpTestM` and `execImpTestM`
* Add instance `Example (a -> ImpTestM era ())`, which allows use of `Arbitrary`
* Add `getNextEpochCommitteeMembers` to `EraGov` typeclass, with a default empty implementation
* Add `TxUTxODiff (UTxO era) (UTxO era)` inhabitant to the `UtxoEvent era` data type.

## 1.8.0.0

* Add `shelleyTotalDepositsTxCerts` and `shelleyTotalRefundsTxCerts`
* Remove dead functions: `depositPoolChange` and `reapRewards`
* Move `getTotalDepositsTxBody` and `getTotalRefundsTxBody` into
  `cardano-ledger-core:Cardano.Ledger.Core.EraTxBody` from `ShelleyEraTxBody`
* Change type signature and behavior of `updateUTxOState`
* Deprecate `totalCertsDeposits`, `totalCertsDepositsCertState`, `totalTxDepositsShelley`,
  `keyCertsRefundsCertState`, `keyCertsRefunds` and `totalTxRefundsShelley`

### `testlib`

* Provide CDDL spec files with `readBabbageCddlFileNames` and `readBabbageCddlFiles` from
  `Test.Cardano.Ledger.Babbage.Binary.Cddl`
* Remove `impExpectFailure` in favor of `expectLeft`
* Remove `impExpectSuccess` in favor of `expectRightDeep`
* Remove `itM` in favor of `it`
* Remove `impIOMsg` in favor of new `impAnn`
* Remove `impIO` and `predicateFailureShouldBe` as no longer needed
* Add `withImpState`
* Expose type `ImpTestState`
* Add `Example` instance for `ImpTestM`

## 1.7.0.0

* Change the return type of `getCommitteeMembers`
* Remove `validateDelegationRegistered` in favor of a new and more specific validating
  function `validateStakePoolDelegateeRegistered`.
* Add `ToExpr` instances for:
  - `ShelleyDelegPredFailure`
  - `ShelleyDelegsPredFailure`
  - `ShelleyDelplPredFailure`
  - `ShelleyEpochPredFailure`
  - `ShelleyLedgerPredFailure`
  - `ShelleyPpupPredFailure`
  - `ShelleyUtxoPredFailure`
  - `VotingPeriod`
* Add `NFData` instances for:
  - `ShelleyMirPredFailure`
  - `ShelleyNewEpochPredFailure`
  - `ShelleyNewppPredFailure`
  - `ShelleyPoolreapPredFailure`
  - `ShelleySnapPredFailure`
  - `ShelleyUpecPredFailure`
* Add `epochStateGovStateL`
* Add `shelleyCommonPParamsHKDPairsV8`
* Add `ToExpr` instances for:
  - `ShelleyPoolPredFailure`
  - `ShelleyUtxowPredFailure`
* Add `NFData` instance for:
  - `ShelleyRupdPredFailure`
  - `ShelleyTickPredFailure`
* Rename `validateFailedScripts` to `validateFailedNativeScripts` and change its
  arguments.
* Change type of `validateMissingScripts`

### `testlib`

* Add `Test.Cardano.Ledger.Shelley.ImpTest`
* Add `Test.Cardano.Ledger.Shelley.ImpTestSpec`
* Add `EraImpTest` instance for `ShelleyEra`

## 1.6.1.0

* Add `ToExpr` instance for:
  - `Update`
  - `ShelleyTx`
  - `ShelleyTxBody`
  - `ShelleyTxWits`
* Add `Generic` instance for `ShelleyTx`
* Add `Memoized` instance for `ShelleyTx`
* Introduce `Cardano.Ledger.Shelley.Transition` module with `EraTransition` interface.
* Deprecated `CanStartFromGenesis` interface in favor of `EraTransition`
* Add `Semigroup` and `Monoid` instances for `ShelleyGenesisStaking`
* Add `FromJSON` instance for `Constitution`

### `testlib`

* Add `Arbitrary` instance for `Constitution`
* `Test.Cardano.Ledger.Shelley.Binary.RoundTrip` module with:
  - `roundTripShelleyCommonSpec`
  - `roundTripStateEraTypesSpec`

## 1.6.1.0

* Add lens `epochStateTreasuryL` #3748

## 1.6.0.0

* Deprecate `isRegKey` and `isDeRegKey` in favor of `isRegStakeTxCert` and `isUnRegStakeTxCert` #3700
* Add lenses for `UTxOEnv` #3688
* Add `getTotalTxDepositsBody` to `ShelleyEraTxBody`
* Add `obligationGovState` to `EraGov`
* Add `ToExpr` instance to `Constitution`
* Replace `obligationCertState` with `totalObligation`
* Deprecated `totalTxDeposits`
* Add `potEqualsObligation`
* Add `shelleyLedgerAssertions`
* Add `totalTxDepositsShelley`
* Add `eqMultiSigRaw`, `shelleyEqTxRaw` and `shelleyEqTxWitsRaw`
* Add `EqRaw` instance for `MultiSig`, `ShelleyTxWits`, `ShelleyTxAuxData`, `TxBody` and `Tx`
* Add `ToExpr` instance for `GenesisDelegCert`, `MIRPot`, `MirTarget`, `MIRCert`,
  `ShelleyTxCert`, `ShelleyDelegCert`, `MultiSig` and `MultiSigRaw`

## 1.5.1.0

* Add new lens `epochStateIncrStakeDistrL`, that points to the `credMap` field of
  `IncrementalStake` from `EpochState`

## 1.5.0.0

* Introduce `ShelleyPoolreapEnv` and replace `Environment` for `ShelleyPOOLREAP`
* Remove redundant reimplementation of lens function `%~`: `updateWithLens`.
* Change `getConstituitionHash` to `getConstitution`
* Replace `constitutionHash` with `constitutionAnchor`
* Add `upgradeShelleyTxCert`
* Rename `*governance*` to `*gov*` #3607
  - `EraGovernance` to `EraGov`
  - `GovernanceState` to `GovState`
  - `witsVKeyNeededNoGovernance` to `witsVKeyNeededNoGov`
  - `witsVKeyNeededGovernance` to `witsVKeyNeededGov`
  - `utxosGovernance` to `utxosGovState`
* Filter out zero valued `TxOut`'s on Byron/Shelley boundary.
* Rename `getProducedValue` to `shelleyProducedValue`
* Change the constraints on `produced` and `evaluateTransactionBalance`
* Add `lsCertStateL`
* Make new `Constitution` datatype #3556
  - Adopt some Default instances for example SafeHash
* Add new methods to `EraGovernance`:
  - `curPParamsGovStateL`
  - `prevPParamsGovStateL`
* Rename `ShelleyPPUPState` to `ShelleyGovState`
* Add new fields to `ShelleyGovState`:
  - `sgovPp`
  - `sgovPrevPp`
* Add lenses:
  - `proposalsL`
  - `futureProposalsL`
  - `esAccountStateL`
  - `esSnapshotsL`
  - `esNonMyopicL`
* Remove `esPrevPp` and `esPp` from `EpochState`
* Rename `esPrevPpL` to `prevPParamsEpochStateL`
* Rename `esPpL` to `curPParamsEpochStateL`
* Swap the order of `esSnapshots` and `esLState` in `EpochState`
* Add lenses:
  - `lsCertStateL`
  - `utxosStakeDistrL`
  - `utxosDonationL`
  - `utxosUtxoL`
  - `utxosDepositedL`
  - `esAccountStateL`
  - `asTreasuryL`
  - `asReservesL`

## 1.4.2.0

* Add implementation for `spendableInputsTxBodyL`
* Fix an issue with where `witsVKeyNeededNoGovernance` and `witsVKeyNeeded` required
  witnesses for `allInputsTxL`, which affected reference inputs in Babbage.

## 1.4.1.0

* Add `getConstitutionHash` to `EraGovernance` #3506
  - Also add `nesEpochStateL` to `LedgerState.Types`

## 1.4.0.0

* Deprecated `updateTxBodyG`
* Changed the argument types of `witsVKeyNeeded`
* Changed the signature of `validateNeededWitnesses`
* Added `witsVKeyNeededGovernance` and `witsVKeyNeededNoGovernance`
* Added protocol version bound to
  - `STS (ShelleyUTXOW era)` instance
  - `transitionRulesUTXOW`
  - `witsVKeyNeeded`
* Prevent using `getMirTxCert` from being used in eras after Babbage. This also affects
  all functions that use it
* Prevent using `mkGenesisDelegTxCert`, `getGenesisDelegTxCert` and `GenesisDelegTxCert`
  from being used in eras after Babbage. This also affects all functions that use those
  functions and patern synonym
* Remove `WrongCertificateTypePOOL` as an impossible case.

## 1.3.0.0

* Deprecated `poolSpec` function
* Deprecated `Cardano.Ledger.Shelley.Delegation.Certificates` and
  `Cardano.Ledger.Shelley.Delegation.PoolParams` modules
* Added `Cardano.Ledger.Shelley.TxCert` module
* Make `DCert` parameterized on `era` instead of `c`rypto and rename it as `ShelleyTxCert`:
  - `DCertDelegCert` -> `ShelleyTxCertDeleg`
  - `DCertPool` -> `ShelleyTxCertPool`
  - `DCertGenesis` -> `ShelleyTxCertGenesis`
  - `DCertMir` -> `ShelleyTxCertMir`
* Introduce `TxCert` type family with pattern synonyms and rename the actual `DCert` type into
  `ShelleyTxCert`
* Introduce `ShelleyEraTxCert` type class.
* Add `EraTxCert` and `ShelleyEraTxCert` instances to `ShelleyEra`
* Remove `certsTxBodyL` and `certsTxBodyG` from `ShelleyEraTxBody`. Former migrated to `EraTxBody`.
* Add helper functions `shelleyTxCertDelegDecoder`, `commonTxCertDecoder`, `encodeShelleyDelegCert`,
  `encodePoolCert` and `encodeConstitutionalCert`
* Deprecate:
  - `RegKey` in favor of `ShelleyRegCert`
  - `DeRegKey` in favor of `ShelleyUnRegCert`
  - `Delegate` in favor of `ShelleyDelegCert`
* Addition of `getVKeyWitnessShelleyTxCert` and `getScriptWitnessShelleyTxCert`
* Deprecate:
  - `extractKeyHashWitnessSet` in favor of `credKeyHashWitness`
  - `scriptCred` in favor of `credScriptHash`
  - `scriptStakeCred` in favor of `getScriptWitnessTxCert`
  - `requiresVKeyWitness` in favor of `getVKeyWitnessTxCert`
  - `delegCWitness` - no longer used.
  - `propWits` - will become an internal function in the future version
* `validateNeededWitnesses` no longer accepts `witsVKeyNeeded` as an argument.
* Move `ConstitutionalDelegCert` from `cardano-ledger-core` as `GenesisDelegCert`.
* Fixed `NoThunks (ShelleyGenesis c)` instance, as it was incorrectly disallowing thunks in its `sgInitialFunds` and `sgStaking` fields

## 1.2.0.0

* Replace `DPState c` with `CertState era`
* Parametrize `DState` and `PState` by era
* Rename `obligationDPState` to `obligationCertState`
* Rename `keyCertsRefundsDPState` to `keyCertsRefundsCertState`
* Rename `totalCertsDepositsDPState` to `totalCertsDepositsCertState`
* Added new functions to `DELEGS` rule
  - `drainWithdrawals`
  - `validateZeroRewards`
  - `validateDelegationRegistered`

## 1.1.1.0

* Disable `TICKF` rule optimization: [#3375](https://github.com/intersectmbo/cardano-ledger/pull/3375)

## 1.1.0.0

* Added a default implementation for `emptyGovernanceState`
* Added lenses:
  - `esAccountStateL`
  - `esSnapshotsL`
  - `esLStateL`
  - `esPrevPpL`
  - `esPpL`
  - `esNonMyopicL`
  - `lsUTxOState`
  - `lsDPState`
  - `utxosUtxo`
  - `utxosDeposited`
  - `utxosFees`
  - `utxosGovernance`
  - `utxosStakeDistr`
* Added `ToJSON` instance for `ShelleyTxOut`
* Added `ToJSON` instance for `AlonzoPParams StrictMaybe`
* Added `ToJSON (GovernanceState era)` superclass constraint for `EraGovernance`
* Added `ToJSON` instance for:
  - `ShelleyTxOut`
  - `AlonzoPParams StrictMaybe`
  - `ProposedPPUpdates` and `ShelleyPPUPState`
  - `AccountState`, `EpochState`, `UTxOState`, `IncrementalStake` and `LedgerState`
  - `Likelihood` and `NonMyopic`
  - `RewardUpdate` and `PulsingRewUpdate`
* Added of `ToJSON`/`FromJSON` instances for `LogWeight`
* Change `totalCertsDeposits` to accept a function that checks for registered pools,
  rather than the `DPState`. Use `totalCertsDepositsDPState` for the previous behavior
* Added `getProducedValue` and `totalCertsDepositsDPState`.
* Deprecate `evaluateTransactionBalance`
* Change types in `StakePoolRetirementWrongEpochPOOL` from `Word64` to `EpochNo`

### `testlib`

* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
