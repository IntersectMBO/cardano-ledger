# Version history for `cardano-ledger-api`

## 1.13.0.0

* Add `TxLevel` argument to `Tx` and `TxBody`
* Add `EraTxLevel` instance
* Add `AnyEraScript`
* Add `AnyEraSpendingPurpose`, `AnyEraMintingPurpose`, `AnyEraCertifyingPurpose`, `AnyEraRewardingPurpose`, `AnyEraVotingPurpose`, `AnyEraProposingPurpose`, `AnyEraGuardingPurpose` patterns
* Re-export `DijkstraEraScript`, `toGuardingPurpose` and `GuardingPurpose` pattern
* Add `producedTxOuts` and `mkCollateralTxIn`
* Add `AnyEraTx`
* Add `AnyEraTxBody`
* Add `ttlToValidityInterval`
* Re-export `DijkstraEraTxBody`, `guardsTxBodyL`
* Add `AnyEraTxCert`
* Add `AnyEraTxWits`
* Add `AnyEraTxOut` and `AnyEraTxAuxData`

### `testlib`

* Remove the `State.Query` module and `getFilteredDelegationsAndRewardAccounts` function.

## 1.12.1.0

* Fix a bug in `queryPoolState`, where current Pool parameters where returned instead of the future ones.

## 1.12.0.0

* Add `upgradeNativeScript` method to `EraApi`
* Replace `timelockScriptsTxAuxDataL` with `nativeScriptsTxAuxDataL`
* Changed the type of `translateAlonzoTxAuxData` to only work with consecutive eras
* Add `EraHasName` type class and add `EraName` type family to the `Era` type class.
* Add `queryPoolState` and bring back `queryPoolParameters` state query.
* Add `queryDRepDelegations` state query
* Remove `filterStakePoolDelegsAndRewards` as unnecessary. Use `queryStakePoolDelegsAndRewards` instead
* Expose `binaryUpgradeTx`, `binaryUpgradeTxBody`, `binaryUpgradeTxWits`, `binaryUpgradeTxAuxData`, `upgradeTx`, `upgradeTxBody`, `upgradeTxWits`, `upgradeTxAuxData`
* Add `EraApi` class
* Export `VotingProcedure`, `ProposalProcedure`, `currentTreasuryValueTxBodyL` and
  `treasuryDonationTxBodyL` from `Cardano.Ledger.Api.Tx.Body`
* Export `DijkstraEra` from `Cardano.Ledger.Api.Era`
* Set `LatestKnownEra` to `DijkstraEra`
* Stop exporting deprecated `Shelley`, `Mary`, `Allegra`, `Alonzo`, `Babbage` and `Conway` type synonyms
* Moved `Annotator` instances to `testlib` for all respective packages
* Rename `queryAccountState` to `queryChainAccountState`

### `testlib`

* Add Consensus example modules (moved from test-packages): `Shelley`, `Allegra`, `Mary`, `Alonzo`, `Babbage`, `Conway`, `Dijkstra`
* Re-export all Ledger `Arbitrary` instances from `Test.Cardano.Ledger.Api.Arbitrary`

## 1.11.0.0

* Remove `witVKeyBytes`
* Converted `CertState` to a type family
* Expose new `TxAuxDataHash` and deprecate old `AuxiliaryDataHash`
* Stop re-exporting `Crypto` and `StandardCrypto`, since they have been moved to `cardano-protocol-tpraos`
* Removal of crypto parametrization:
  - `LatestKnownEra`
  - `HotCredAuthStatus` and `CommitteeMemberState`
  - `DecAddr`
  - Many other re-exported types. See individual packages for changelog
* Add `unRedeemersL`, `unTxDatsL`

## 1.10.0.0

* Added `queryRegisteredDRepStakeDistr` state query
* Remove deprecated `RewardAcnt`, `mkRwdAcnt`, `serialiseRewardAcnt` and `deserialiseRewardAcnt`.
* Added `queryStakePoolDefaultVote` state query and its return type `DefaultVote`

## 1.9.4.0

* Add new module `Test.Cardano.Ledger.Api.DebugTools` with `readCBOR`, `writeCBOR`, `readEraCBOR` and `writeEraCBOR`

## 1.9.3.0

* Add `queryRatifyState` state query
* Add `queryProposals` state query
* Export `EraGenesis` and `NoGenesis`

## 1.9.2.1

*

## 1.9.2.0

* Add `querySPOStakeDistr` query.
* Fix `queryDRepState` to report correct DRep expiries even in the absence of credentials to filter.

## 1.9.1.0

* Add `redeemerPointer` and `redeemerPointerInverse`.
* Export `mkBasicTxAuxData` and `metadataTxAuxDataL`.
* Export `AllegraEraTxAuxData` and `timelockScriptsTxAuxDataL`
* Export `AlonzoEraTxAuxData` and `plutusScriptsTxAuxDataL`

## 1.9.0.0

* Add optional `Anchor` to `MemberResigned` in `CommitteeMembersState`
* Change return type of `queryCommitteeMembersState` to `CommitteeMembersState`
* Change type of `csThreshold` in `CommitteeMembersState` to `Maybe UnitInterval`
* Remove `getNextEpochCommitteeMembers` from `EraGov`
* Remove `getDRepDistr`, `getConstitution` and `getCommitteeMembers` from `EraGov` #4033
* Add `refScriptSize` parameter to `getMinFeeTx`, `setMinFeeTx` and `estimateMinFeeTx`
* Add `hoistGovRelation`, `withGovActionParent` and `GovRelation`
* Remove `PrevGovActionIds`
* Rename `cgProposalsL` to `cgsProposalsL`
* Remove `cgEnactStateL`
* Rename `RewardAccount` fields `getRwdNetwork` and `getRwdCred` to `raNetwork` and `raCredential` respectively
* Deprecate `deserialiseRewardAcnt` in favor of `deserialiseRewardAccount`
* Deprecate `serialiseRewardAcnt` in favor of `serialiseRewardAccount`
* Deprecate `RewardAcnt` in favor of `RewardAccount`
* Remove `registerInitialFunds`, `registerInitialStaking`, `registerInitialDReps`, and
  `registerDelegs` in favor of `injectIntoTestState`.
* Change the semantics of `createInitialState` to not register any initial funds or delegates.

## 1.8.0.0

* Add `txIdTxBody` and `txIdTx`.
* Add `estimateMinFeeTx` and `calcMinFeeTx`
* Add `queryAccountState`
* Add `estimateMinFeeTx`
* Add `registerInitialDReps`, `registerDelegs`
* Stop exporting `CostModels` internal representation
* Change return type of `mintedTxBodyF` to `Set PolicyID`
* Remove old and deprecated functions `evaluateTransactionExecutionUnits`,
  `evaluateTransactionExecutionUnitsWithLogs` and `updateTxBodyG`
* Remove no longer necessary `ValidationFailed`
* Adjust `TransactionScriptFailure`
* Add `DecAddr` and `decodeAddrLenient` with `decodeAddrLenientEither`
* Change the type of `ppEMaxL`, `ppuEMaxL`, `ppGovActionLifetimeL`, `ppDRepActivityL`,
  `ppCommitteeMaxTermLengthL`, `ppuGovActionLifetimeL`, `ppuDRepActivityL` and
  `ppuCommitteeMaxTermLengthL`
* Move all `ToExpr` instances into `testlib`s.
* Introduce `ToBeExpired` data constructor to `NextEpochChange`
* Introduce `TermAdjusted` data constructor to `NextEpochChange`

## 1.7.0.1

*

## 1.7.0.0

* Deprecate `GetCommitteeState` query
* Add `GetCommitteeMembersState` query
* Add `invalidBeforeL` and `invalidHereAfterL`
* Export `isPlutusScript`
* Add `NativeScript`, `getNativeScript` and `validateNativeScript`
* Removed `phaseScript`
* Remove `GovSnapshots`, `cgGovSnapshotsL`, `cgRatifyStateL`, `cgGovSnapshotsL` and
  `cgRatifyStateL`
* Add `cgProposalsL`

## 1.6.0.0

* Add `LatestKnownEra`
* Add `Cardano.Ledger.Api.Transition` module
* Add the dormant-epochs counter to `DRep` expiry in `queryDRepState` #3729
  - If it is not zero.
* Rename:
  - `GovActionsState` to `GovSnapshots`
  - `cgGovActionsStateL` to `cgGovSnapshotsL`
* Add `lookupRegStakeTxCert` and `lookupUnRegStakeTxCert`
* Add `isRegStakeTxCert` and `isUnRegStakeTxCert`

## 1.5.0.0

* Add one more parameter to `evalBalanceTxBody` #3688
  - `Credential 'DRepRole (EraCrypto era) -> Maybe Coin`
* Add `cgEnactStateL`

## 1.4.0.0

* Add `queryGovState`, `queryDRepState`, `queryDRepStakeDistr` and `queryCommitteeState`
* Add `queryConstitution`.
* Replace `constitutionHash` with `constitutionAnchor`
* Add `eraName`
* Add `upgradeTxAucData` function to `EraTxAuxData`
* Add `upgradeTxOut` function to `EraTxOut`
* Add `upgradeScript` function to `EraScript`
* Add `upgradeTxAuxData` function to `EraTxAuxData`
* Add `upgradeTxCert` function and `TxCertUpgradeError` family to `EraTxCert`
* Export:
  - Procedures: `VotingProcedure`, `VotingProcedures` and `ProposalProcedure`
  - Constitution: `Constitution`, `constitutionHashL` and `constitutionScriptL`
  - GovActions: `GovActionPurpose`, `PrevGovActionId`, `govActionIdToText`,
    `PrevGovActionId` and `GovActionPurpose`
* Add optional `PrevGovActionId` to `ParameterChange`, `HardForkInitiation`,
  `NoConfidence`, `NewCommittee` and `NewConstitution` governance actions.
* Rename `*governance*` to `*gov*` #3607
* Update `queryConstitutionHash` return type to `SafeHash (EraCrypto era) (Constitution era)` #3556
* Rename `cgTallyL` to `cgGovL`
* Rename `ConwayTallyState` to `ConwayGovState`
* Change constraints on `evalBalanceTxBody`

## 1.3.1.0

* Add `spendableInputsTxBodyL`

## 1.3.0.0

* Add `queryConstitutionHash` to `Cardano.Ledger.Api.State.Query` #3506
* Addition of `Cardano.Ledger.Api.Tx.Cert`
* Removal of `VoterRole` in favor of `Voter`
* Removal of `cgVoterRolesL` and `cgVoterRoles` as no longer needed.

## 1.2.1.0

* Deprecated `updateTxBodyG`

## 1.2.0.0

* Add support for Plutus V3
* Start on `Cardano.Ledger.Api.State.Query` interface:
  - Add `filterStakePoolDelegsAndRewards` and `queryStakePoolDelegsAndRewards`

### `testlib`

* Started `testlib` with first module `Test.Cardano.Ledger.Api.State.Query` that exposes
  `getFilteredDelegationsAndRewardAccounts`

## 1.1.0.0

* Add `Cardano.Ledger.Api.Governance` that exposes all of the governance related types.
* Addition of `Cardano.Ledger.Api.PParams`
* Addition of `Cardano.Ledger.Api.Tx.Address`
* Addition of `Cardano.Ledger.Api.Tx.In`
* Hide `ShelleyTxBody`, `AlonzoTxBody`, `BabbageTxBody` from `Cardano.Ledger.Api.Tx.Body`
* Export `Withdrawals` from `Cardano.Ledger.Api.Tx.Body`
* Export `ShelleyTxAuxData`, `AllegraTxAuxData` and `AlonzoTxAuxData` from
  `Cardano.Ledger.Api.Tx.AuxData`
* Export `LangDepView` and `getLanguageView` from `Cardano.Ledger.Api.PParams`
* Hide the internal compact versions from `Cardano.Ledger.Api.Tx.Out` module:
  `compactValueTxOutL`, `valueEitherTxOutL`, `compactAddrTxOutL` and `addrEitherTxOutL`
* Export from `Cardano.Ledger.Api.Tx.Out`: `coinTxOutL`, `bootAddrTxOutF`,
  `ValidityInterval` and `DataHash`.
* Export from `Cardano.Ledger.Api.Scripts.Data`: `BinaryData`, `DataHash`, `Datum`,
  `binaryDataToData`, `dataToBinaryData`, `datumDataHash`, `getPlutusData`,
  `hashBinaryData` and `makeBinaryData`.
* Export `Tx` and `IsValid` from `Cardano.Ledger.Api.Tx`
* Export from `Cardano.Ledger.Api.Tx.AuxData`: `mkAlonzoTxAuxData` and `getAlonzoTxAuxDataScripts`
* Export from `Cardano.Ledger.Api.Tx.Wits`: `WitVKey`, `BootstrapWitness`,
  `AlonzoEraTxWits`, `TxDats`, `unTxDats`, `Redeemers`, `unRedeemers`, `RdmrPtr` and
  `Tag`.
* Export from `Cardano.Ledger.Api.Scripts`: `CostModels` and `ValidityInterval`.
* Export from `Cardano.Ledger.Api.Era`:
  - `Era`
  - `ByronEra`
  - Protocol version related functionality: `eraProtVerHigh`, `eraProtVerLow`,
    `AtLeastEra`, `AtMostEra`, `ExactEra`, `ProtVerAtLeast`, `ProtVerAtMost`,
    `ProtVerInBounds`, `atLeastEra` and `atMostEra`
* Move `Cardano.Ledger.Alonzo.Tools` module from `cardano-ledegr-alonzo` into
  `Cardano.Ledger.Api.Scripts`
* Addition of `evalBalanceTxBody`
* Addition of `evalTxExUnits` and `evalTxExUnitsWithLogs`. Deprecated
  `evaluateTransactionExecutionUnits` and `evaluateTransactionExecutionUnitsWithLogs` if
  favor of the newly added ones.
* Export `hashDataTxWitsL` and `hashScriptTxWitsL`

## 1.0.0.0

* Initial release
