# Version history for `cardano-ledger-conway`

## 1.21.0.0

* Update `resolveConwayInstantStake` to return `ActiveStake`
* Remove `asBoundedIntegralHKD`
* Change lens type of `hkdCommitteeMinSizeL` and `ppCommitteeMinSize` from `Natural` to `Word16`
* Add `HeaderProtVerTooHigh` predicate failure.
* Change `STS` instance of `ConwayUTOXS`: use `PParams` as `Environment`
* Remove `TotalDeposits` and `TxUTxODiff` data constructors from `ConwayUtxosEvent`
* Add `Generic` instance for `ApplyTxError`
* Change `ScriptsNotPaidUTxO` to use `NonEmptyMap TxIn (TxOut era)` instead of `UTxO era`
* Add `conwayLedgerTransitionTRC`
* Deprecate
  - `constitutionScriptL` in favor of new `constitutionGuardrailsScriptHashL`
  - `InvalidPolicyHash` in favor of new `InvalidGuardrailsScriptHash`
  - `checkPolicy` in favor of new `checkGuardrailsScriptHash`
* Rename `gePPolicy` to `geGuardrailsScriptHash`
* Add `ConwayApplyTxError` constructor for `ApplyTxError era`
* Renamed:
  - `cppMinFeeA` -> `cppTxFeePerByte`
  - `cppMinFeeB` -> `cppTxFeeFixed`
* Changed type of `cppMinFeeA` to `CoinPerByte`
* Change sets containing errors into `NonEmptySet` for `ConwayGovPredFailure`, `ConwayUtxoPredFailure`, `ConwayUtxowPredFailure`
* Change all maps into `NonEmptyMap` for `ConwayGovPredFailure` and `ConwayLedgerPredFailure`
* Change all lists into `NonEmpty` for `ConwayUtxoPredFailure`, `ConwayUtxosPredFailure`, `ConwayUtxowPredFailure`
* Add `cddl` sub-library, and `generate-cddl` executable.
* Re-export `UtxoEnv` from `Cardano.Ledger.Conway.Rules.Utxo`
* Changed the type of the following fields to `CompactForm Coin` in `ConwayPParams`:
  - `cppMinFeeB`
  - `cppKeyDeposit`
  - `cppMinPoolCost`
  - `cppGovActionDeposit`
* Added:
  - `ppGovActionDepositCompactL`
  - `ppuGovActionDepositCompactL`
* Changed name and type to `CompactForm Coin`:
  - `hkdGovActionDepositL` -> `hkdGovActionDepositCompactL`
* Generalise and expose some rule transition functions: `conwayBbodyTransition`, `conwayGovTransition`, `conwayGovCertTransition`, `conwayLedgerTransition`
* Change the field type of `ConwayIncompleteWithdrawals` to `Map RewardAccount (Mismatch RelEQ Coin)`
* Make `ConwayAccountState` a pattern synonym
* Remove deprecated type `Conway`
* Remove deprecated function `toConwayGenesisPairs`
* Remove deprecated function `toUpgradeConwayPParamsUpdatePairs`
* Remove deprecated function `toConwayTransitionConfigPairs`
* Remove deprecated function `getVoteDelegatee`
* Add `unDelegReDelegDRep` to `VState` module
* Expose `conwayRegisterInitialAccounts`
* Add `TxLevel` argument to `Tx` and `TxBody`
* Add `HasEraTxLevel` instances for `Tx` and `TxBody`
* Add `EraTxLevel` instance
* Add `shelleyToConwayLedgerPredFailure`.
* Move withdrawal-validation and DRep expiry updates from `CERTS` to `LEDGER` starting protocol version 11.
  - Add `ConwayWithdrawalsMissingAccounts` and `ConwayIncompleteWithdrawals` to `ConwayLedgerPredFailure`.
  - Add `hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule` to `Conway.Era`.
  - Add `updateDormantDRepExpiries`  and `updateVotingDRepExpiries`

### `cddl`

* Renamed `policy_hash` to `guardrails_script_hash` in governance actions to avoid confusion with multi-asset policy IDs
* Add `HuddleRule1` instances for sets.
* Move `cddl-files` to `cddl/data`.
* Export for `dnsNameRule`, `urlRule`, `voteRule` among others for reuse.
* Add full `HuddleSpec`.

### `testlib`

* Rename `mkProposalWithRewardAccount` to `mkProposalWithAccountAddress`
* Remove `huddle-cddl` and the `CDDL` modules.
* Add `witsEmptyFieldWithTag`
* Add `conwayDecodeDuplicateDelegCertFails`
* Add CDDL definitions:
  - Credentials: `drep_credential`, `committee_cold_credential`, `committee_hot_credential`
  - Governance primitives: `drep`, `anchor`
  - New pool primitives with 128-byte limits: `dns_name128`, `url128`
  - New pool certificate definitions via `mkPoolRules`: `pool_registration_cert`, `pool_retirement_cert`
  - Certificates:
    + `account_registration_deposit_cert`, `account_unregistration_deposit_cert`
    + `delegation_to_drep_cert`, `delegation_to_stake_pool_and_drep_cert`
    + `account_registration_delegation_to_stake_pool_cert`, `account_registration_delegation_to_drep_cert`, `account_registration_delegation_to_stake_pool_and_drep_cert`
    + `committee_authorization_cert`, `committee_resignation_cert`
    + `drep_registration_cert`, `drep_unregistration_cert`, `drep_update_cert`
* Remove old CDDL certificate definitions: `reg_cert`, `unreg_cert`, `vote_deleg_cert`, `stake_vote_deleg_cert`, `stake_reg_deleg_cert`, `vote_reg_deleg_cert`, `stake_vote_reg_deleg_cert`, `auth_committee_hot_cert`, `resign_committee_cold_cert`, `reg_drep_cert`, `unreg_drep_cert`, `update_drep_cert`
* Remove CDDL pool-related definitions: `pool_params`, `dns_name`, `single_host_name`, `multi_host_name`, `relay`, `url`, `pool_metadata` (now generated via `mkPoolRules`)
* Rename `shelley_auxiliary_data`, `shelley_ma_auxiliary_data`, `alonzo_auxiliary_data` to more suitable `metadata`, `auxiliary_data_array`, `auxiliary_data_map`
* Remove redefinition of `metadatum_label`, `metadata` from CDDL
* Remove CDDL `protocol_version` redefinition
* Remove `epoch_no`, `epoch_interval`, `slot_no` and `block_no`. Reuse definitions from core
* Removed `regDelegToDRep`
* Removed `registerRewardAccountWithDeposit`
* Removed `registerPoolWithDeposit`
* Removed `registerStakeCredentialWithDeposit`
* Remove `conwayAccountsToUMap` corresponding to the removal of `UMap` from core.

## 1.20.0.0

* Decoupled `ConwayEraTxCert` from `ShelleyEraTxCert`, so added `ShelleyEraTxCert` constraint to:
  - `DecCBOR ConwayTxCert`
  - `transTxCert`
  - `transTxCertV1V2`
* Added `conwayGovCertVKeyWitness`
* Added `conwayTxCertDelegDecoder`
* Changed `MaxTxSizeUTxO` to use `Word32`
* Rename `transScriptPurpose` to `transPlutusPurposeV3`
* Make `transValidityInterval` implicit to eras instead of protocol versions.
  - Implement `transValidityInterval` for Conway.
* Add `NFData` for `ConwayGenesis`
* Deprecate `PoolParams` in favor of `StakePoolState`. #5196
  - Update `DRepPulser` and `RatifyEnv` to use `StakePoolState` instead of `PoolParams`.
* Better predicate failures for incorrect deposits and refunds.
  - Add `hardforkConwayDELEGIncorrectDepositsAndRefunds` for protocol 11 onwards, to `Conway.Era`.
  - Add `DepositIncorrectDELEG` and `RefundIncorrectDELEG` to `ConwayDelegPredFailure`.
* Add `ScriptIntegrityHashMismatch`
* Change the type of `cppDRepDeposit` to `CompactForm Coin`
* Add `ppDRepDepositCompactL` and `ppuDRepDepositCompactL`
* Replace `hkdDRepDepositL` with `hkdDRepDepositCompactL`
* Delete `Tx` newtype wrapper
* Hide `Cardano.Ledger.Conway.Translation` module and remove its re-exports: `addrPtrNormalize` and `translateDatum`
* Removed:
  - `maxRefScriptSizePerTx`
  - `maxRefScriptSizePerBlock`
  - `refScriptCostMultiplier`
  - `refScriptCostStride`
* Added:
  - `ppMaxRefScriptSizePerTxG`
  - `ppMaxRefScriptSizePerBlockG`
  - `ppRefScriptCostMultiplierG`
  - `ppRefScriptCostStrideG`
* Add `mkDelegatee` and `getDRepDelegatee`
* Depercated `getVoteDelegatee` in favor of `getDRepDelegatee`
* Add `conwayRegisterInitialFundsThenStaking`
* Add `ConwayEraAccounts`, `ConwayAccountState`, `ConwayAccounts`, `lookupDRepDelegation`,
  `registerConwayAccount`, `unregisterConwayAccount` and `accountStateDelegatee`
* Change type in `WithdrawalsNotInRewardsCERTS` from `Map RewardAccount Coin` to `Withdrawals`
* Add `AlonzoEraTx` constraint to `STS` instance for `ConwayBBODY`
* Add `totalRefScriptSizeInBlock`
* Move some hard-fork triggers and export them from `Cardano.Ledger.Conway` module.
  - `bootstrapPhase` to `hardforkConwayBootstrapPhase`.
  - `disallowUnelectedCommitteeFromVoting` to `hardforkConwayDisallowUnelectedCommitteeFromVoting`.
* Add `UnelectedCommitteeVoters` to `ConwayGovPredFailure` #5091
* Change the type of `authorizedELectedCommitteeCredentials` to
  `StrictMaybe (Committee era) -> CommitteeState era -> Set.Set (Credential 'HotCommitteeRole)` #5091
* Deprecated `toConwayTransitionConfigPairs`
* Fixed `FromJSON` instance for `TransitionConfig ConwayEra`
* Added `COMPLETE` pragma for `NativeScript ConwayEra`
* Add default implementation for `tcConwayGenesisL`
* Remove `tcDelegsL` and `tcInitialDRepsL`
* Export `registerDRepsThenDelegs`
* Deprecated `toUpgradeConwayPParamsUpdatePairs` and `toConwayGenesisPairs`
* Add:
  - `alonzoToConwayUtxosPredFailure`
  - `alonzoToConwayUtxosEvent`
  - `getConwayScriptsNeeded`
  - `getConwayMinFeeTxUtxo`
  - `getConwayMinFeeTx`
  - `conwayRedeemerPointer`
  - `conwayRedeemerPointerInverse`
  - `transMintValue`
  - `transTxBodyId`
  - `transVotingProcedures`
  - `transProposal`
  - `transTxCertV1V2`
  - `transPlutusPurposeV1V2`
  - `guardConwayFeaturesForPlutusV1V2`
  - `transTxInInfoV3`
* Remove era parametrization from `GovPurposeId`, `GovRelation`
* Move to `testlib` the `DecCBOR` instance for `TxBody ConwayEra`
* Add `ReferenceInputsNotDisjointFromInputs`
* Remove `ConwayNewEpochPredFailure` and replace it with `Void`. #5007
* Added to `PParams`: `ppCommitteeMaxTermLength`,`ppCommitteeMinSize`,`ppDRepActivity`,`ppDRepDeposit`,`ppDRepVotingThresholds`,`ppGovActionDeposit`,`ppGovActionLifetime`,`ppGovProtocolVersion`,`ppMinFeeRefScriptCostPerByte`,`ppPoolVotingThresholds`
* Moved `ConwayEraPlutusTxInfo` class from `Context` module to `TxInfo`
* Removed `Cardano.Ledger.Conway.Plutus.Context` module
* Moved orphan `ToPlutusData` instance for `PParamsUpdate` from `TxInfo` to `PParams`
* Bump `ProtVerHigh ConwayEra` to `11`
* Remove `ConwayTxBody`
* Removed `era` parameter from `ConwayTxBodyRaw`
* Add `MkConwayTxBody` and all members of `ConwayTxBodyRaw`:
  (`ConwayTxBodyRaw`, `ctbrAuxDataHash`, `ctbrCerts`, `ctbrCollateralInputs`,
  `ctbrCollateralReturn`, `ctbrCurrentTreasuryValue`, `ctbrFee`, `ctbrMint`, `ctbrNetworkId`,
  `ctbrOutputs`, `ctbrProposalProcedures`, `ctbrReferenceInputs`, `ctbrReqSignerHashes`,
  `ctbrScriptIntegrityHash`, `ctbrSpendInputs`, `ctbrTotalCollateral`, `ctbrTreasuryDonation`,
  `ctbrVldt`, `ctbrVotingProcedures`, `ctbrWithdrawals`)
* Expose access to `ConwayTxBodyRaw`
* Expose constructor `MkConwayTxBody`
* Added `VState` (moved from `cardano-ledger-core`) and related functions
* Added `ConwayEraCertState` class
* Added `ConwayCertState` and related functions
* Moved `CertState` to `State` module
* Move `ToPutusData` instances for `CoinPerByte`, `DRepVotingThresholds` and `PoolVotingThresholds` with their respective types

### `testlib`

* Added `EraSpecificSpec ConwayEra` instance
* Added `registerRewardAccountWithDeposit`
* Added `regDelegToDRep`
* Generalised the following helpers and thus changed their constraints to `ConwayEraImp`:
  - `setupPoolWithStake`
  - `setupPoolWithoutStake`
  - `trySubmitGovAction`
  - `trySubmitGovActions`
  - `mkProposal`
  - `submitGovAction`
  - `submitGovAction_`
  - `submitGovActions`
  - `submitTreasuryWithdrawals`
  - `submitFailingGovAction`
* Decoupled `ConwayEraTxCert` from `ShelleyEraTxCert`, so added `ShelleyEraTxCert` constraint to:
  - `genUnRegTxCert`
  - `genRegTxCert`
* Added `registerPoolWithDeposit`
* Added `registerStakeCredentialWithDeposit`
* Added `Examples` module with: `ledgerExamples`, `exampleConwayCerts`
* Fix CDDL for `MultiAsset` in `TxOut` as well as the `Tx` mint field.
* Add `mkConwayTestAccountState` and `conwayAccountsToUMap`
* Rename `electCommittee` to `submitCommitteeElection` #5091
* Fixed `Arbitrary` instance for `ConwayGenesis`
* Added `Arbitrary` instance for `TransitionConfig ConwayEra`
* Added `ToExpr` instances for:
  - `ConwayPlutusPurpose AsIxItem`
  - `DRepPulser`
  - `ConwayBbodyPredFailure`
* Added `Era` module with `ConwayEraTest` class

## 1.19.0.0

* Add `ConwayInstantStake`, `conwayInstantStakeCredentialsL`, `addConwayInstantStake`, `deleteConwayInstantStake`, `resolveConwayInstantStake`
* Replace `IncrementalStake` with `InstantStake` in `DrepPuser`, `RatifyEnv`
* Replace stake-map with `InstantStake` in `computeDRepDistr`
* Add `ToCBOR` and `FromCBOR` instance for `ConwayGenesis`
* Switch `ctbrCerts` to use `TxCert` instead of `ConwayTxCert`
* Add `DecCBOR` instance for `ConwayTxBody`
* Converted `CertState` to a type family
* Remove `ConwayMempoolPredFailure` and `ConwayMempoolEvent`
* Switch to `MEMPOOL` rule to be the entry point for `ApplyTx` instead of `LEDGER` and invert their
  invocation.
* Added `ToCBOR` and `FromCBOR` instances for `DefaultVote`.
* Made the fields of predicate failures and environments lazy
* Add `MemPack` instance for `PlutusScript ConwayEra`
* Deprecate `Conway` type synonym
* Remove crypto parametrization from `ConwayEra`

### `testlib`

* Converted `CertState` to a type family
* Add `sendCoinTo_` and `sendValueTo_`
* Add `genRegTxCert` and `genUnRegTxCert`. #4830
* Add `Arbitrary` instance for `ConwayBbodyPredFailure` and `ConwayMempoolPredFailure`

## 1.18.0.0

* Remove `SlotNo` from `CertEnv` and `CertsEnv`
* Remove deprecated `translateTxOut` and `conwayWitsVKeyNeeded`
* Add `DefaultVote` and `defaultStakePoolVote`
* Add new event `GovRemovedVotes` for invalidated votes.

### `testlib`

* Remove `mintingTokenTx` (which is replaced by `mkTokenMintingTx` in Mary)
* Add `minFeeUpdateGovAction`
* Add `mkTreasuryWithdrawalsGovAction` and `mkParameterChangeGovAction`
* Switch to using `ImpSpec` package
* Remove `withImpStateWithProtVer`
* Added `delegateSPORewardAddressToDRep_`
* Add `mkUpdateCommitteeProposal`
* Add `SubmitFailureExpectation`, `FailBoth`, `submitBootstrapAwareFailingVote`, `submitBootstrapAwareFailingProposal`, `submitBootstrapAwareFailingProposal_`
* Add `mkConstitutionProposal`
* Remove `submitConstitutionGovAction` and change signature of `submitConstitution`
* Add `mkProposal` and `mkProposalWithRewardAccount`
* Add `whenBootstrap`

## 1.17.2.0

* Change the state used for `ConwayWdrlNotDelegatedToDRep` predicate failure checking

## 1.17.1.0

* Add `processDelegation`

## 1.17.0.0

* Added `reDelegatees` and `rePoolParams` to `RatifyEnv` for updated SPO vote calculation #4645
* Added `dpPoolParams` to `DRepPulser` to track the parameters of each stake pool
* Add `HardForkEvent` constructor to `ConwayEpochEvent`
* Add `HardFork` module, `ConwayHARDFORK` and `ConwayHardForkEvent`
* Add predicate failures to guard against invalid reward accounts (return addresses) in proposals and treasury withdrawals. #4639
  - `ProposalReturnAddressDoesNotExist`, and
  - `TreasuryWithdrawalReturnAddressDoesNotExist`.
* Add `refScriptCostStride` and `refScriptCostMultiplier`
* Added protocol version argument to `ppuWellFormed`
* Add `ConwayMempoolEvent` type
* Add `MempoolEvent` to `ConwayLedgerEvent`
* Add `Mempool` module, `ConwayMEMPOOL` and `ConwayMempoolPredFailure`
* Add `ConwayMempoolFailure` to `ConwayLedgerPredFailure`
* Add `ZeroTreasuryWithdrawals` to `ConwayGovPredFailure`
* Add `ProtVer` argument to `TxInfo` functions:
  - `transTxCert`
  - `transScriptPurpose`
  - `transPlutusPurposeV1V2`
  - `toPlutusV3Args`
* Changed `ConwayWdrlNotDelegatedToDRep` to wrap `NonEmpty KeyHash`
* Removed `DRepAlreadyRegisteredForStakeKeyDELEG`
* Add `showGovActionType`, `acceptedByEveryone`
* Added `unRatifySignal`
* Added lenses:
  - `ratifySignalL`
  - `reStakeDistrL`
  - `reStakePoolDistrL`
  - `reDRepDistrL`
  - `reDRepStateL`
  - `reCurrentEpochL`
  - `reCommitteeStateL`
* Add a new field to `GovInfoEvent` and change "unclaimed" field from `Set` to a `Map`.
* Changed return type of `proposalsShowDebug`
* Added `gen-golden` executable needed for golden tests: #4629
* Change `State` for `CERT` and `GOVCERT` to `CertState`
* Add `DelegateeDRepNotRegisteredDELEG` predicate failure
* Rename `DelegateeNotRegisteredDELEG` to `DelegateeStakePoolNotRegisteredDELEG`

### `testlib`

* Added `expectMembers`
* Removed `redelegateDRep` from `ImpTest`
* Changed signature of `delegateToDRep` to take a `Credential` parameter
* Move `TxInfo` golden tests over from the older `-test` package. #4599
  - Also move the `gen-golden` executable over.
* Added Test.Cardano.Ledger.Conway.CDDL with CDDL definitions in Conway.
* Change `ImpException` to contain `Doc`
* Add `impAnnDoc`
* Add `ifBootstrap`

## 1.16.1.0

* Replace GOVCERT `updateDRepExpiry` with `computeDRepExpiry`
* Added `Eq`, `Show`, `NFData` and `Generic` instances for `CertsEnv`
* Add `delegateToDRep` and `redelegateDRep`

### testlib

* Added `ToExpr` instance for `CertsEnv`
* Added `submitUpdateCommittee`, `expectCommitteeMemberPresence`,
  `expectCommitteeMemberAbsence` and `donateToTreasury`

## 1.16.0.0

* Add `maxRefScriptSizePerBlock` and `maxRefScriptSizePerTx` to `Cardano.Ledger.Conway.Rules`
* Add `ConwayBBODY` and `ConwayBbodyPredFailure` type for BBody rule
* Added `ConwayCommitteeIsUnknown` predicate failure to  `ConwayGovCertPredFailure`
* Added `ceCurrentCommittee` and `ceCommitteeProposals` to `CertEnv`
* Added `certsCurrentCommittee` and `certsCommitteeProposals` to `CertsEnv`
* Added `cgceCurrentCommittee` and `cgceCommitteeProposals` to `ConwayGovCertEnv`
* Added `proposalsWithPurpose`, `isGovActionWithPurpose` and `ToGovActionPurpose`
* Added `ConwayTxRefScriptsSizeTooBig` predicate failure to `ConwayLedgerPredFailure`
* Replaced `geCommitteeState` with `geCertState` in `GovEnv`
* Added `VotersDoNotExist` predicate failure to `ConwayGovPredFailure`
* Export `ConwayEraTxCert`, `RegDepositTxCert`, `UnRegDepositTxCert`, `DelegTxCert`,
  `RegDepositDelegTxCert`, `AuthCommitteeHotKeyTxCert`, `ResignCommitteeColdTxCert`,
  `RegDRepTxCert`, `UnRegDRepTxCert` and `UpdateDRepTxCert` from
  `Cardano.Ledger.Conway.Core`
* Remove `GovProcedures` in favor of newly added type `GovSignal`

### `testlib`

* Added `ToExpr` instance for `ConwayNewEpochPredFailure`
* Change the return type of `resignCommitteeColdKey`
* Add an argument to `registerCommitteeHotKeys`

## 1.15.1.0

* Add `tierRefScriptFee` and `txNonDistinctRefScriptsSize`
* Make reference scripts fee grow exponentially with size

### `testlib`

* Add `registerCommitteeHotKeys`

## 1.15.0.0

* Add `psPoolDistrL` lens.
* Add `psPoolDistr` field to `PulsingSnapshot` for tracking the pool stake distribution.
* Fix DRep expiry updates based on the number of dormant epochs.
  - Remove `DRepPulser.dormantEpoch`.
  - Add `CERTS.updateDormantDRepExpiry`.
  - Fix `updateNumDormantEpochs` to take current proposals, rather than ones from the pulser.
  - In `GOVCERT`, make `ConwayUpdateDRep` calculate `currentEpochNo + drepActivity - numDormantEpochs`.
    + Add `GOVCERT.updateDRepExpiry` to calculate this way.
  - Add `CertState.vsActualDRepExpiry` to get the actual expiry of a DRep considering `numDormantEpochs`.
* Add `NFData` instance for `ConwayDelegEnv`
* Add `NFData` instances for:
  - `RatifySignal`
  - `EnactSignal`
* Add instances for `ConwayGovCertEnv`. #4348
  - `NFData`
  - `ToExpr`
* Add `NFData` instance for `RatifySignal`
* Add `AllegraEraScript` and `ShelleyEraScript` instances for `ConwayEra`

### `testlib`

* Add `submitYesVoteCCs_` utility to Conway ImpTest
* Add tests for DRep expiry, considering dormant epochs.
  - Add `unregisterDRep`.
  - Add `updateDRep`.
  - Add `expectDRepResigned`
  - Add `isDRepExpired`.
  - Remove `expectExtaDRepExpiry` in favour of `expectDRepExpiry`.
  - Add `expectActualDRepExpiry` that considers `numDormantEpochs`.
  - Add `passNEpochsChecking`.
* Add `ToExpr` instance for `RatifySignal`
* Add `ToExpr` instances for:
  - `RatifySignal`
  - `EnactSignal`

## 1.14.0.0

* Added `ConwayDelegEnv`
* Changed the `Environment` of `ConwayDELEG` rule to `ConwayDelegEnv`
* Moved `DelegateeNotRegisteredDELEG` to `ConwayDelegPredFailure`
* Remove `gePrevGovActionIds` from `GovEnv`
* Include proposal deposits in the DRep active voting stake. #4309
  - Add `proposalsDeposits` to `Governance.Proposals`.
    + This extracts a `Map (Credential 'Staking (EraCrypto era)) (CompactForm Coin)` from reward-account-staking-credential to deposit amounts
  - Add `dpProposalDeposits` field to `DRepPulser`.
  - Change `computeDRepDistr` to also take as argument the `dpProposalDeposits`.
* Add lenses:
  - `dvtHardForkInitiationL`
  - `dvtMotionNoConfidenceL`
  - `dvtTreasuryWithdrawalL`
* Add`DisallowedProposalDuringBootstrap` and `DisallowedVotesDuringBootstrap` to `ConwayGovPredFailure`
* Make `DRepDistr` calculation include rewards when no UTxO stake is delegated. #4273
  - Rename `computeDrepPulser` to `computeDRepPulser`.
* Implement `NoThunks` instance for:
  - `ConwayUtxoPredFailure`
  - `ConwayUtxowPredFailure`
* Add `ConwayUtxowPredFailure` era rule failure:
  - Implement its `InjectRuleFailure` instances for:
    + `BBODY`
    + `LEDGER`
    + `LEDGERS`
    + `UTXOW`
  - Implement instances:
    + `Generic`
    + `Show`
    + `Eq`
    + `EncCBOR`
    + `DecCBOR`
    + `NFData`
  - Add mappings:
    + `babbageToConwayUtxowPredFailure`
    + `alonzoToConwayUtxowPredFailure`
    + `shelleyToConwayUtxowPredFailure`
  - Update `Embed (ConwayUTXO era) (ConwayUTXOW era)` instance
* Add `ConwayUtxoPredFailure` era rule failure:
  - Implement its `InjectRuleFailure` instances for:
    + `BBODY`
    + `LEDGER`
    + `LEDGERS`
    + `UTXO`
    + `UTXOW`
  - Implement instances:
    + `Generic`
    + `Show`
    + `Eq`
    + `EncCBOR`
    + `DecCBOR`
    + `NFData`
  - Add mappings:
    + `babbageToConwayUtxoPredFailure`
    + `alonzoToConwayUtxoPredFailure`
  - Update `allegraToConwayUtxoPredFailure` mapping
* Add `ConwayUTXO` era rule:
  - Implement instances:
    + `STS`
    + `Embed (ConwayUTXOS era) (ConwayUTXO era)`
    + `Embed (ConwayUTXO era) (ConwayUTXOW era)`
* Add `ucppPlutusV3CostModel` to `UpgradeConwayPParams`. #4252
  - Remove the `Default` instance for `ConwayGenesis`.
* Add `foldrVotingProcedures`.

### `testlib`

* Add `ToExpr` instances for `CertEnv` and `ConwayDelegEnv`
* Add `withPostBootstrap` to Conway ImpTest
* Add `withImpStateWithProtVer` to Conway ImpTest
* Add the following utilities. #4273
  - to `Conway.ImpTest`
    + `setupDRepWithoutStake`
    + `setupPoolWithoutStake`
    + `submitAndExpireProposalToMakeReward`
  - to `Shelley.ImpTest`
    + `getRewardAccountFor`
    + `registerAndRetirePoolToMakeReward`
* Add `getConstitution` to Conway ImpTest
* Change return type of `setupSingleDRep` to Credential instead of KeyHash
* Add `registerInitialCommittee` and `getCommitteeMembers` to Conway ImpTest
* Implement `ConwayUtxowPredFailure` instances:
  - `Arbitrary`
  - `ToExpr`
* Implement `ConwayUtxoPredFailure` instances:
  - `Arbitrary`
  - `ToExpr`
* Updated `exampleConwayGenesis` to `conway-genesis.json`. #4252

## 1.13.1.0

* Fix typo in `ToJSON` instance of `ConwayGovState`

### `testlib`

* Add `ToExpr` instance for `GovProcedures`

## 1.13.0.0

* Add `geCommitteeState`
* Remove `ConwayDelegEvent`, `ConwayGovCertEvent`
* Add `GovInfoEvent`
* Add `ConwayUtxosEvent`
* Add `Generic`, `Eq` and `NFData` instances for `ConwayEpochEvent`
* Add `Eq` and `NFData` instances for:
  - `ConwayGovEvent`
  - `ConwayCertEvent`
  - `ConwayCertsEvent`
  - `ConwayLedgerEvent`
  - `ConwayNewEpochEvent`
* Add type `EraRuleEvent` instances for the event type of:
  - `UPEC`
  - `NEWPP`
  - `PPUP`
  - `MIR`
  - `DELEGS`
  - `TICK`
  - `ENACT`
  - `LEDGER`
  - `UTXOS`
* Add `ConwayDRepIncorrectRefund`
* Stop exporting `utxosGovStateL` from `Cardano.Ledger.Conway.Governance`
* Remove deprecated `curPParamsConwayGovStateL` and `prevPParamsConwayGovStateL`
* Add `EraRuleFailure "POOL"` type instance for `ConwayEra`
* Add `ConwayUtxosPredFailure`
* Support for intra-era hard fork with `ProtVerHigh` set to `10`
* Guard Conway-specific features in transactions that use Plutus v1 or v2. #4112
  - Add `PlutusContextError` variants:
    + `CurrentTreasuryValueFieldNotSupported`
    + `VotingProceduresFieldNotSupported`
    + `ProposalProceduresFieldNotSupported`
    + `TreasuryDonationFieldNotSupported`
  - Allow `RegDepositTxCert` and `UnRegDepositTxCert` to pass by ignoring the deposit or refund values, respectively.
* Switch `EPOCH` rule environment back to `()`. Start using the latest stake pool
  distribution: #4115
* Add:
  - `transTxInInfoV1`
  - `transTxOutV1`
* Add instances for `InjectRuleFailure` and switch to using `injectFailure`
* Remove `ConwayPOOL` rule, in favor of `ShelleyPOOL`
* Add `NFData` instance for `BabbageUtxoPredFailure`
* Rename `MinFeeRefScriptCoinsPerByte` to `MinFeeRefScriptCostPerByte` and change its type from `CoinsPerByte` to `NonNegativeInterval` #4055
* Rename `committeeQuorum` to `committeeThreshold` #4053
* Changed `GovActionState` to have 1 field (`gasProposalProcedure`) rather than 3 (`gasDeposit`, `gasAction`, `gasReturnAddr`)
  - the old field names (`gasDeposit`, `gasAction`, `gasReturnAddr`) become functions, and the lenses
  - (`gasDepositL`, `gasActionL`, `gasReturnAddrL`) have the same type, but behave differently.
  - Added the lenses: `pProcDepositL`, `pProcGovActionL`, `pProcReturnAddrL`,  `pProcAnchorL`, `gasProposalProcedureL`.
* Add `getDRepDistr`, `getConstitution` and `getCommitteeMembers` from `ConwayEraGov` #4033
  - Move `Constitution` to `Conway.Governance.Procedures`
* Add implementation for `getMinFeeTxUtxo`
* Add `cppMinFeeRefScriptCoinsPerByte` to `ConwayPParams` and `ppMinFeeRefScriptCoinsPerByteL`
* Add `ucppMinFeeRefScriptCoinsPerByte` to `UpgradeConwayPParams` and `ppuMinFeeRefScriptCoinsPerByteL`
* Fix `ConwayTxBody` pattern synonym, by changing its certificates arguments to `OSet`
  from a `StrictSeq`.
* Add `VotingPurpose` and `ProposingPurpose` pattern synonyms
* Add `ConwayEraScript` with `toVotingPurpose`, `toProposingPurpose`, `fromVotingPurpose`,
  `fromProposingPurpose`.
* Add upgrade failure: `CTBUEContainsDuplicateCerts`
* Rename `proposalsRemoveDescendentIds` to `proposalsRemoveWithDescendants` (fixed spelling too)
* Rename:
  - `pfPParamUpdateL` to `grPParamUpdateL`
  - `pfHardForkL` to `grHardForkL`
  - `pfCommitteeL` to `grCommitteeL`
  - `pfConstitutionL` to `grConstitutionL`
* Rename:
  - `cgProposalsL` to `cgsProposalsL`
  - `cgEnactStateL` to `cgsEnactStateL`
  - `cgDRepPulsingStateL` to `cgsDRepPulsingStateL`
* Add:
  - `cgsPrevPParamsL`
  - `cgsCommitteeL`
  - `cgsConstitutionL`
  - `govStatePrevGovActionIds`
  - `mkEnactState`
* Deprecated `curPParamsConwayGovStateL` and `curPParamsConwayGovStateL`
* Rename `PForest` to `GovRelation`
* Add `hoistGovRelation` and `withGovActionParent`
* Add `TreeMaybe`, `toGovRelationTree` and `toGovRelationTreeEither`
* Remove `proposalsAreConsistent`
* Remove `registerDelegs` and `registerInitialDReps`
* Modify `PParams` JSON instances to match `cardano-api`

### `testlib`

* Add `ToExpr` instances for:
  - `ConwayNewEpochEvent`
  - `ConwayEpochEvent`
  - `ConwayLedgerEvent`
  - `ConwayCertsEvent`
  - `ConwayCertEvent`
  - `ConwayGovEvent`
* Change the types of some functions in `Test.Cardano.Ledger.Conway.ImpTest`
  to use `NonEmpty (PredicateFailure _)` instead of `[PredicateFailure _]`
  - `submitFailingVote`
  - `trySubmitVote`
  - `trySubmitProposal`
  - `trySubmitProposals`
  - `submitFailingProposal`
  - `trySubmitGovAction`
  - `trySubmitGovActions`
* Add `Test.Cardano.Ledger.Conway.Imp.GovCertSpec`
* Add `RuleListEra` instance for Conway
* Rename `canGovActionBeDRepAccepted` to `isDRepAccepted` and refactor #4097
  - Add `isSPOAccepted`
  - Change `setupSingleDRep` to return relevant keyhashes
  - Change `setupPoolWithStake` to return relevant keyhashes
  - Add `getLastEnactedCommittee`
  - Add `getRatifyEnvAndState`
* Add `Test.Cardano.Ledger.Conway.Imp.UtxosSpec`
* Add `getGovPolicy`
* Add `submitGovActions` and `trySubmitGovActions`
* Add `submitProposals` and `trySubmitProposal`

## 1.12.0.0

* Changed the types in `GovernanceActionsDoNotExist`, `DisallowedVoters`
  and `VotingOnExpiredGovAction` to `NonEmpty`
* Add `cgDelegsL`
* Add `FromJSON`, `EncCBOR` and `DecCBOR` instances for `Delegatee`
* Add `pvtPPSecurityGroup`
* Add lenses:
  - `pvtCommitteeNormalL`
  - `pvtCommitteeNoConfidenceL`
  - `pvtPPSecurityGroupL`
  - `dvtCommitteeNoConfidenceL`
* Add `PPGroups` and `StakePoolGroup`
* Add `ToStakePoolGroup` typeclass
* Add `DRepGroup` and `ToDRepGroup` typeclass
* Modify `THKD` replacing `PPGroup` with `PPGroups`
* Add `ConwayPlutusPurpose`
* Add `unGovActionIx`
* Add `foldlVotingProcedures`
* Add a policy field to `ParameterChange` and `TreasuryWithdrawals` constructors
  of `GovAction`
* Add `InvalidPolicyHash` to `ConwayGovPredFailure`
* Add `ToJSON` instance for `ConwayContextError`, `ConwayTxCert`, `ConwayDelegCert`,
  `Delegatee` and `ConwayGovCert`
* Add `forceDRepPulsingState`
* Add `registerInitialDReps` and `registerDelegs`
* Add `cgDelegs`, `cgInitialDReps` to `ConwayGenesis`
* Changed the type of lenses ppCommitteeMaxTermLengthL, ppuCommitteeMaxTermLengthL
* Change 'getScriptWitnessConwayTxCert' so that DRepRegistration certificate requires a witness
* Implement `ToJSON` and `FromJSON` instances for `PoolVotingThresholds` and
  `DRepVotingThreshold`, instead of deriving that doesn't handle field names
  correctly.
* Hide `Cardano.Ledger.Conway.TxOut` module
* Export `ConwayEraPParams` and `ConwayEraTxBody` from `Cardano.Ledger.Conway.Core`
* Stop exporting `BabbagePParams` from `Cardano.Ledger.Conway.PParams`
* Add `transTxBodyWithdrawals`, `transTxCert`, `transDRepCred`, `transColdCommitteeCred`,
  `transHotCommitteeCred`, `transDelegatee`, `transDRep`, `transScriptPurpose`
* Remove `conwayTxInfo` and `babbageScriptPrefixTag`
* Remove deprcated `translateScript`
* Add `getVoteDelegatee`
* Track and prune unreachable proposals #3855 #3919 #3978 #3981
  - Consolidate the entire proposals-tree under the `Proposals` module and expose all its operations in a convenient manner
  - Move `PrevGovActionIds` from `Governance` to `Governance.Proposals`
  - Add `rsEnacted` field to `RatifyState` to track enacted proposals separately from removed ones and rename `rsRemoved` to `rsExpired` in order to better represent its role
  - Add `ProposalsSerializable` as an accompanying type used to correctly serialize `Proposals` in a space-efficient way
  - Add the following operations to `Governance.Proposals`
    + `mkProposals` as the only way to reconstruct the `Proposals` tree from, for instance, a deserialized one
    + `proposalsAddAction` as the only way to add new proposals to the system
    + `proposalsApplyEnactment` as the only way to replay from `ENACT` operations upon `Proposals` in the ledger state, outside of the pulser.
    + Rename `PrevGovActionId purpose (EraCrypto era)` to `GovPurposeId purpose era`
    + Add the following accessors and lenses, among others:
      * `PForest`
      * `PRoot`
      * `PEdges`
      * `PHierarchy`
      * `pRootsL`
      * `prRootL`
      * `prChildrenL`
      * `pnChildrenL`
      * `pHierarchyL`
      * `pHierarchyNodesL`
      * `pfPParamUpdateL`
      * `pfHardForkL`
      * `pfCommitteeL`
      * `pfConstitutionL`
  - Add the pruning functionality and the deposit refunds in the `EPOCH` rule
  - In the `Gov` rule
    + Modify the rule transition implementation to accept new proposals into the `Proposals` forests based on proposal purpose
  - In the `Ratify` rule
    + Account for the tracking of enacted and expired proposals
* Moved `ToExpr` instances out of the main library and into the testlib.
* Changed the type of ConwayPParams fields  cppEMax,  cppGovActionLifetime, cppDRepActivity
* Changed types of lenses: `ppGovActionLifetimeL`, `ppDRepActivityL`, `ppCommitteeMaxTermLengthL` and `ppuGovActionLifetimeL`, `ppuDRepActivityL`, `ppuCommitteeMaxTermLengthL`
* Implement `getNextEpochCommitteeMembers` in Conway `EraGov`
* Change argument of `validCommitteeTerm` function from `StrictMaybe Committee` to `GovAction`

### `testlib`

* Add the previous governance action ID to the outputs of `electBasicCommittee`
* Add `setupPoolWithStake`
* Add:
  - `registerPool`
  - `sendCoinTo` and `sendValueTo`
* Add `submitProposal_`
* Add `submitTreasuryWithdrawals`
* Track and prune unreachable proposals #3855 #3919 #3978 #3981
  - Add invariant-respecting `Arbitrary` generators for `Proposals`
  - Add property tests for all `Proposals` operations
  - Add procedural unit tests for all `Proposals` operations
* Remove `Test.Cardano.Ledger.Conway.PParamsSpec` and replace the unit test it contained
  with a new property test in `Test.Cardano.Ledger.Alonzo.Binary.CostModelsSpec`

## 1.11.0.0

* Switch `ppCommitteeMaxTermLength` to `EpochNo`, rather than `Natural`
* Add `conwayTotalDepositsTxBody` and `conwayProposalsDeposits`
* Add `conwayDRepDepositsTxCerts`, `conwayDRepRefundsTxCerts`,
  `conwayTotalDepositsTxCerts` and `conwayTotalRefundsTxCerts`
* Rename data-type `ProposalsSnapshot` to `Proposals`. #3859
  - Rename module `Governance.Snapshots` to `Governance.Proposals`.
  - Rename all the functions related to the data-type.
* Switch to using `OMap` for `ProposalsSnapshot` #3791
* Add `VotingOnExpiredGovAction` predicate failure in `GOV` #3825
* Rename `modifiedGroups` -> `modifiedPPGroups` and move into `ConwayEraPParams`
* Expose `pparamsUpdateThreshold`
* Fix [#3835](https://github.com/intersectmbo/cardano-ledger/issues/3835)
* Rename `PParamGroup` to `PPGroup` and `GovernanceGroup` to `GovGroup`
* Introduce `THKD` and use it for `ConwayPParams`
* Add `data ConwayGovEvent era` with constructor `GovNewProposals !(TxId (EraCrypto era)) !(ProposalsSnapshot era)`. #3856
* Add `EpochBoundaryRatifyState (RatifyState era)` inhabitant to the `ConwayEpochEvent era` data type.

### `testlib`

* Provide CDDL spec files with `readConwayCddlFileNames` and `readConwayCddlFiles` from
  `Test.Cardano.Ledger.Conway.Binary.Cddl`

## 1.10.0.0

* Add `ToJSON` instance for `ProposalProcedure`
* Fix `NewEpochState` translation: #3801
* Change order of arguments for `committeeAccepted` adn `spoAccepted` for consistency #3801
* Add `spoAcceptedRatio` #3801
* Export `snapshotGovActionStates` #3801
* Change type for `snapshotRemoveIds` to also return the removed actions. #3801
* Add `reDRepDistrL` #3759
* Remove `GovSnapshots` #3759
* Move `DRepPulser` from `cardano-ledger-core`. #3759
* Add `DRepPulsingState` #3759: `pulseDRepPulsingState`, `completeDRepPulsingState`,
  `extractDRepPulsingState`, `finishDRepPulser`, `computeDrepDistr`, `getRatifyState`,
  `getPulsingStateDRepDistr`, `dormantEpoch`, `setFreshDRepPulsingState`,
  `setCompleteDRepPulsingState`
* Add `PulsingSnapshot` and `psProposalsL`, `psDRepDistrL`, `psDRepStateL` #3759
* Add `RunConwayRatify` class #3759
* Enforce no duplicates from `certsTxBodyL` and `proposalProceduresTxBodyL` #3779
* Remove `WrongCertificateTypeDELEG` predicate failure.
* Add `getDelegateeTxCert` and `getStakePoolDelegatee`
* Add `enactStateGovStateL` to `ConwayEraGov`
* Add `psDRepDistrG`.
* Rename `ensPParams` to `ensCurPParams`.
* Add `ToJSON` instance for `RatifyState`
* Change `ToJSON` instance for `ConwayGovState`:
  - Add `"nextRatifyState"` field
  - Rename `"ratify"` to `"enactState"`
  - Rename `"gov"` to `"proposals"`
* Fix `ToJSON` instance for `EnactState`:
  - Current PParams were wrongfully used for `"prevPParams"`.
  - Remove `"treasury"` and `"withdrawals"` as those are temporary bindings needed only
    for `ENACT` rule
* Add an anchor argument to `ResignCommitteeColdTxCert`
* Prevent invalid previous gov-action ids in proposals #3768
  - Also, add lenses
    + `govProceduresProposalsL`
    + `pProcGovActionL`
    + `gasActionL`
* Add `ToExpr` instance for:
  - `Voter`
  - `ConwayCertPredFailure`
  - `ConwayCertsPredFailure`
  - `ConwayDelegPredFailure`
  - `ConwayGovPredFailure`
  - `ConwayGovCertPredFailure`
  - `ConwayLedgerPredFailure`
  - `ConwayTxBody`
* Add `Generic` and `NFData` instance for:
  - `ConwayNewEpochPredFailure`
* Add `totalObligation`
* Add `utxosDepositedL`
* Add `conwayWitsVKeyNeeded`
* Add  `ConwayEraPParams era` constraint to `isCommitteeVotingAllowed` and `votingCommitteeThreshold`
* Switch to using `AlonzoEraUTxO` in rules
* Change `cppProtocolVersion` to a `HKDNoUpdate` field

### `testlib`

* Addition of `ImpTest` interface

## 1.9.0.0

* Add `ConwayEraPParams era` constraint to `isCommitteeVotingAllowed` and `votingCommitteeThreshold`
* Add `ToExpr` instance for:
  - `Voter`
  - `VotingProcedures`
  - `VotingProcedure`
  - `ProposalProcedure`
  - `ConwayTxBody`
* Add `ConwayTxBodyUpgradeError`, `ConwayTxCertUpgradeError`
* Add to `Ratify`:
  - `committeeAccepted`
  - `committeeAcceptedRatio`
* Add `reCommitteeState` to `RatifyEnv`
* Add PredicateFailure for current treasury value mismatch in tx body in LEDGER #3749
* Change `To/FromJSON` format for `ConwayGenesis`
* Add `EraTransition` instance and `toConwayTransitionConfigPairs`.
* Expose `toConwayGenesisPairs` and `toUpgradeConwayPParamsUpdatePairs`
* Rename `ConwayPParams` to be consistent with the Agda specification. #3739
  - `govActionExpiration` to `govActionLifetime`
  - `committeeTermLimit` to `committeeMaxTermLength`
  - `minCommitteeSize` to `committeeMinSize`
* Prevent `DRep` expiry when there are no active governance proposals to vote on (in
  ConwayCERTS). #3729
  - Add `updateNumDormantEpochs` function in `ConwayEPOCH` to update the dormant-epochs counter
  - Refactor access to `ConwayGovState` by making its lens part of `ConwayEraGov`.
  - Export `gasExpiresAfterL` for use in tests
* Add `ExpirationEpochTooSmall` data constructor to `ConwayGovPredFailure`
* Add `ConflictingCommitteeUpdate` data constructor to `ConwayGovPredFailure`
* Rename `NewCommitte` to `UpdateCommittee`
* Remove `NewCommitteeSizeTooSmall` data constructor from `ConwayGovPredFailure`
* Fix invalid order in `fromGovActionStateSeq`, thus also `DecCBOR` for `ProposalsSnapshot`
* Remove `DecCBOR`/`EncCBOR` and `FromCBOR`/`ToCBOR` for `RatifyState`, since that state
  is ephemeral and is never serialized.
* Add `PredicateFailure` for `Voter` - `GovAction` mismatches, with `checkVotesAreValid`. #3718
  - Add `DisallowedVoters (Map (GovActionId (EraCrypto era)) (Voter (EraCrypto era)))`
    inhabitant to the `ConwayGovPredFailure` data type.
  - Fix naming for `toPrevGovActionIdsParis` to `toPrevGovActionIdsPairs`
* Rename:
  - `thresholdSPO` -> `votingStakePoolThreshold`
  - `thresholdDRep` -> `votingDRepThreshold`
  - `thresholdCC` -> `votingCommitteeThreshold`
* Add:
  - `isStakePoolVotingAllowed`
  - `isDRepVotingAllowed`
  - `isCommitteeVotingAllowed`
* Fix `ConwayTxBodyRaw` decoder to disallow empty `Field`s #3712
  - `certsTxBodyL`
  - `withdrawalsTxBodyL`
  - `mintTxBodyL`
  - `collateralInputsTxBodyL`
  - `reqSignerHashesTxBodyL`
  - `referenceInputsTxBodyL`
  - `votingProceduresTxBodyL`
  - `proposalProceduresTxBodyL`
* Add `reorderActions`, `actionPriority`
* Remove `ensProtVer` field from `EnactState`: #3705
* Move `ConwayEraTxBody` to `Cardano.Ledger.Conway.TxBody`
* Move `ConwayEraPParams` to `Cardano.Ledger.Conway.PParams`
* Rename:
  - `GovActionsState` to `GovSnapshots`
  - `cgGovActionsStateL` to `cgGovSnapshotsL`
  - `curGovActionsStateL` to `curGovSnapshotsL`
  - `prevGovActionsStateL` to `prevGovSnapshotsL`
* Add:
  - `ProposalsSnapshot`
  - `snapshotIds`
  - `snapshotAddVote`
  - `snapshotInsertGovAction`
  - `snapshotActions`
  - `snapshotRemoveIds`
  - `fromGovActionStateSeq`
* Add lenses:
  - `gasCommitteeVotesL`
  - `gasDRepVotesL`
  - `gasStakePoolVotesL`
* Add `FromJSON` instance for `Committee`
* Add `constitution` and `committee` fields to `ConwayGenesis`

### testlib

* Add `Test.Cardano.Ledger.Conway.ImpTest`
* Rename `genNewCommittee` to `genUpdateCommitteee`
* Add `genNewCommittee`
* Add `genNoConfidence`
* Add `genTreasuryWithdrawals`
* Add `genHardForkInitiation`
* Add `genParameterChange`
* Add `genNewConstitution`
* Add `genGovActionStateFromAction`
* Add `govActionGenerators`

## 1.8.1.0

* Apply enacted `TreasuryWithdrawals` in `ConwayEPOCH` #3748
  - Add lenses `ensWithdrawalsL` and `ensTreasuryL`

## 1.8.0.0

* Add all Conway `TxCert` to consumed/produced calculations in the `UTXO` rule. #3700
* Change `ToJSONKey` implementation of `Voter` to flat text
* Add DRep refund calculation #3688
  - Add `conwayConsumedValue` as `getConsumedValue` for Conway
* Change `PredicateFailure (ConwayENACT era)` to `Void`
* Remove `EnactPredFailure`
* Change `PredicateFailure (ConwayEPOCH era)` to `Void`
* Remove `ConwayEpochPredFailure`
* Remove `EpochFailure` and `RatifyFailure` from `ConwayNewEpochPredFailure`
* Change `PredicateFailure (ConwayRATIFY era)` to `Void`
* Add:
  - `rsDelayed`
  - `PParamGroup`
  - `ParamGrouper`
  - `pGroup`
  - `pUngrouped`
  - `modifiedGroups`
  - `dvtPPNetworkGroupL`
  - `dvtPPGovGroupL`
  - `dvtPPTechnicalGroupL`
  - `dvtPPEconomicGroupL`
  - `threshold`
  - `ensCommitteeL`
* Add `pparamsGroups` to `ConwayEraPParams`
* Add `PrevGovActionIds`
* Change `EnactState` to add `ensPrevGovActionIds`
* Add  `ensPrevGovActionIdsL`, `ensPrevPParamUpdateL`, `ensPrevHardForkL` `ensPrevCommitteeL`, `ensPrevConstitutionL`
* Add `EnactSignal` and the signal of `Enact` to it
* Remove `rsFuture` from `RatifyState`
* Add to `GovActionsState`:
  - `curGovActionsState`
  - `prevGovActionsState`
  - `prevDRepState`
  - `prevCommitteeState`
* Add `ToExpr` instances to:
  - `PoolVotingThresholds`
  - `DRepVotingThresholds`
  - `GovActionState`
  - `GovActionsState`
  - `EnactState`
  - `RatifyState`
  - `ConwayGovState`
  - `GovActionIx`
  - `GovActionId`
  - `Vote`
  - `Committee`
  - `PrevGovActionId`
  - `GovAction`
  - `ConwayPParams` with `Identity` and `StrictMaybe`
* Add lenses:
  - `cgEnactStateL`
  - `curGovActionsStateL`
  - `prevGovActionsStateL`
  - `prevDRepStateL`
  - `prevCommitteeStateL`
* Replace `cgRatifyState` with `cgEnactState`
* Deprecate `cgRatifyStateL`
* Add `ProposalDepositIncorrect` predicate failure, that is produced when `ProposalProcedure` deposit does not match `"govActionDeposit"` from `PParams` #3669
* Add "minCommitteeSize" `PParam` validation for `NewCommittee` `GovAction` #3668
  - Add `committeeMembersL` and `committeeQuorumL` lenses for `Committee`
  - Add `NewCommitteeSizeTooSmall` `PredicateFailure` in `GOV`
* Add `EqRaw` instance for `ConwayTxBody`
* Add `ToExpr` instance for `Delegatee`, `ConwayDelegCert`, `ConwayGovCert` and
  `ConwayTxCert`
* Implement expiry for governance proposals #3664
  - Update `ppGovActionExpiration` to be an `EpochNo`
  - Add `gasExpiresAfter :: !EpochNo` to `GovActionState`
  - Add `gePParams` to `GovEnv`
  - Rename `teTxId` to `geTxId` and `teEpoch` to `geEpoch`
* Add `reDRepState` to `RatifyEnv`
* Add field `gasId` to `GovActionState`
* Add `insertGovActionsState`
* Change type of `rsRemoved` in `RatifyState` to use  `GovActionState` instead of a tuple
* Change `RatifySignal` to use `GovActionsState` instead of a tuple

## 1.7.1.0

* Fix DRep distribution computation.

## 1.7.0.0

* Add `Network` validation for `ProposalProcedure` and `TreasuryWithdrawals` in GOV #3659
* Make `DELEG`, `POOL` and `GOVCERT` conform to spec-v0.8 #3628
  - Add `CertEnv` and `CertsEnv` to pass `EpochNo` down from `LEDGER` to sub-rules
  - Add `drepDeposit` to `DRepState` to track deposits paid by `DRep`s
  - Update `DRep` expiry in `LEDGER` for all `DRep`s who are voting in current `Tx`
* Add `ConwayGovCertEnv`
* Change the environment of `GOVCERT` to `ConwayGovCertEnv`
* Add `ConwayEraGov` with `constitutionGovStateL`
* Add `PrevGovActionId` and `GovActionPurpose`
* Add optional `PrevGovActionId` to `ParameterChange`, `HardForkInitiation`,
  `NoConfidence`, `NewCommittee` and `NewConstitution` governance actions.
* Rename `*governance*` to `*gov*` #3607
  - `GovernanceAction` to `GovAction`
  - `GovernanceActionId` to `GovActionId`
  - `GovernanceActionIx` to `GovActionIx`
  - `GovernanceActionState` to `GovActionState`
  - `ConwayGovState` to `GovActionsState`
  - `ConwayGovernance` to `ConwayGovState`
* Add `MalformedProposal` to `ConwayGovPredFailure`
* Add `ppuWellFormed` to `ConwayEraPParams`
* Filter out zero valued `TxOut`'s on Byron/Shelley boundary instead of on Babbage/Conway.
* Deprecate `translateTxOut` in favor of `upgradeTxOut`
* Deprecate `translateScript` in favor of `upgradeScript`
* Switch GovernanceActionIx to `Word32` fro `Word64` and remove `Num` and `Enum`
  instances, which are dangerous due to potential overflows.
* Add `currentTreasuryValue :: !(StrictMaybe Coin)` as a new field to Conway TxBody #3586
* Add an optional Anchor to the Conway DRep registration certificate #3576
* Change `ConwayCommitteeCert` to allow:
  - committee cold keys to be script-hashes #3581
  - committee hot keys to be script-hashes #3552
* Change EnactState.ensConstitution #3556
  - from `SafeHash (EraCrypto era) ByteString`
  - to `Constitution era`
  - Use this datatype for GovernanceAction.NewConstitution
* Add `ConwayPParams` #3498
  - Add `UpgradeConwayPParams`
  - Add `ConwayEraPParams`
  - Add `PoolVotingThresholds`
  - Add `DRepVotingThresholds`
* Rename:
  - `cgTally` -> `cgGovActionsState`
  - `cgTallyL` -> `cgGovActionsStateL`
  - `VDelFailure` -> `GovCertFailure`
  - `VDelEvent` -> `GovCertEvent`
  - `certVState` -> `certGState`
  - `ConwayVDelPredFailure` -> `ConwayGovCertPredFailure`
  - `ConwayTallyPredFailure` -> `ConwayGovPredFailure`
  - `TallyEnv` -> `GovEnv`
  - `ConwayTallyState` -> `ConwayGovState`
  - `TALLY` -> `GOV`
  - `VDEL` -> `GOVCERT`
* Make `Anchor` required in `ProposalProcedure`.
* Add `ConwayUTXO`
* Add `indexedGovProps`
* Add `rsRemoved` to `RatifyState`
* Add `conwayProducedValue`
* Changed the superclasses of `STS (ConwayUTXOS era)`
* Add `VotingProcedures` type
* Remove `vProcGovActionId` and `vProcVoter` from `VotingProcedure`
* Change the type of `votingProceduresL` to return `VotingProcedures`, which is a nested Map
  instead of a sequence, as before.
* Change `GovernanceActionDoesNotExist` to `GovernanceActionsDoNotExist`, which now
  reports all actions as a set, rather than one action per each individual failure.
* Type of `gpVotingProcedures` in `GovernanceProcedures` was aslo changed to `GovernanceProcedures`
* Rename:
  - `ConwayCommitteeCert` -> `ConwayGovCert`
  - `ConwayTxCertCommittee` -> `ConwayTxCertGov`
* Remove `DelegStakeTxCert` from the `COMPLETE` pragma for `TxCert`
* Add `Committee` and adjust `NewCommittee` governance action
* Add `treasuryDonationTxBodyL` to `ConwayEraTxBody`
* Add `ConwayUpdateDRep` constructor to `ConwayGovCert` type and corresponding pattern `UnRegDRepTxCert`
* Update `ProposalProcedure` return address to be a `RewardAcnt`
* Add `ensPrevPParams` field to `EnactState`
* Add lenses:
  - `ensPrevPParamsL`
  - `ensCurPParamsL`

## 1.6.3.0

* Implement stake distribution handling in the `TICKF` rule.

## 1.6.2.0

* Add implementation for `spendableInputsTxBodyL`

## 1.6.1.0

* Removal of TxOuts with zero `Coin` from UTxO on translation

## 1.6.0.0

* Removal of `GovernanceProcedure` in favor of `GovernanceProcedures`

## 1.5.0.0

* Add `ensConstitutionL` and `rsEnactStateL` to `Governance` #3506
  - Override `getConsitutionHash` for Conway to return just the hash of the constitution
* Added `ConwayWdrlNotDelegatedToDRep` to `ConwayLedgerPredFailure`
* Changed the type of voting delegatee from `Credential` to `DRep`
* Removal of `VoterRole` in favor of `Voter`
* Removal of `vProcRole` and `vProcRoleKeyHash` in favor of `vProcVoter` in `VotingProcedure`
* Removal of `cgVoterRolesL` and `cgVoterRoles` for `ConwayGovernance` as no longer needed.
* Removal of `gasVotes` in favor of `gasCommitteeVotes`, `gasDRepVotes` and
  `gasStakePoolVotes` in `GovernanceActionState`
* Removal of `reRoles` from `RatifyEnv` as no longer needed
* Addition of `reStakePoolDistr` to `RatifyEnv`
* Remove `VoterDoesNotHaveRole` as no longer needed from `ConwayTallyPredFailure`
* Added `ConwayEpochPredFailure`
* Added instance for `Embed (ConwayRATIFY era) (ConwayEPOCH era)`
* Removed instance for `Embed (ConwayRATIFY era) (ConwayNEWEPOCH era)`
* Changed superclasses of `STS (ConwayEPOCH era)` and `STS (ConwayNEWEPOCH era)`

## 1.4.0.0

* Added `ConwayUTXOW` rule

### `testlib`

* Add `Arbitrary` instances for `ConwayCertPredFailure`, `ConwayVDelPredFailure`, and `ConwayDelegPredFailure`

## 1.3.0.0

* Add `VDEL` rules to Conway #3467
* Add `EncCBOR`/`DecCBOR` for `ConwayCertPredFailure`
* Add `EncCBOR`/`DecCBOR` for `ConwayVDelPredFailure`
* Add `POOL` rules to Conway #3464
  - Make `ShelleyPOOL` rules reusable in Conway
* Add `CERT` and `DELEG` rules to Conway #3412
  - Add `domDeleteAll` to `UMap`.
* Introduction of `TxCert` and `EraTxCert`
* Add `ConwayEraTxCert`
* Add `EraTxCert`, `ShelleyEraTxCert` and `ConwayEraTxCert` instances for `ConwayEra`
* Add `EraPlutusContext 'PlutusV1` instance to `ConwayEra`
* Add `EraPlutusContext 'PlutusV2` instance to `ConwayEra`
* Add `EraPlutusContext 'PlutusV3` instance to `ConwayEra`
* Added `toShelleyDelegCert` and `fromShelleyDelegCert`
* Changed `ConwayDelegCert` structure #3408
* Addition of `getScriptWitnessConwayTxCert` and `getVKeyWitnessConwayTxCert`
* Add `ConwayCommitteeCert`

## 1.2.0.0

* Added `ConwayDelegCert` and `Delegatee` #3372
* Removed `toShelleyDCert` and `fromShelleyDCertMaybe` #3372
* Replace `DPState c` with `CertState era`
* Add `TranslateEra` instances for:
  - `DState`
  - `PState`
  - `VState`
* Add `ConwayDelegsPredFailure`
* Renamed `DELPL` to `CERT`
* Added `ConwayDELEGS` rule
* Added `ConwayCERT` rule
* Added `ConwayDelegsPredFailure` rule
* Added `ConwayDelegsEvent` rule
* Change the Conway txInfo to allow Plutus V3
  NOTE - unlike V1 and V2, the ledger will no longer place the "zero ada" value
  in the script context for the transaction minting field.
* Added instances for ConwayDelegsPredFailure:
  `NoThunks`, `EncCBOR`, `DecCBOR`, and `Arbitrary`
* Added `GovernanceActionMetadata`
* Added `RatifyEnv` and `RatifySignal`

## 1.1.0.0

* Added `RATIFY` rule
* Added `ConwayGovernance`
* Added lenses:
  - `cgTallyL`
  - `cgRatifyL`
  - `cgVoterRolesL`
* Removed `GovernanceActionInfo`
* Replaced `ctbrVotes` and `ctbrGovActions` with `ctbrGovProcedure`
* Renamed `ENACTMENT` to `ENACT`
* Add `ToJSON` instance for: #3323
  - `ConwayGovernance`
  - `ConwayTallyState`
  - `GovernanceAction`
  - `GovernanceActionState`
  - `GovernanceActionIx`
  - `GovernanceActionId`
* Add `ToJSONKey` instance for `GovernanceActionId` #3323
* Fix `EncCBOR`/`DecCBOR` and `ToCBOR`/`FromCBOR` for `ConwayTallyState` #3323
* Add `Anchor` and `AnchorDataHash` types. #3323
* Rename `transDCert` to `toShelleyDCert`
* Add `fromShelleyDCertMaybe`
* Renamed `Vote` type to `VotingProcedure`
* Add `ProposalProcedure`
* Use `VotingProcedure` and `ProposalProcedure` in `GovernanceProcedure`
* Rename `VoteDecision` to `Vote`. Rename `No`/`Yes` -> `VoteNo`/`VoteYes`.
* Export `govActionIdToText`
* Export constructors for `ConwayTallyPredFailure`
* Add `ensTreasury` and `ensWithdrawals` to `EnactState` #3339
* Add `EnactPredFailure` as the failure for `ENACT` and `RATIFY` #3339
* Add `RatifyFailure` to `ConwayNewEpochPredFailure` #3339
* Add `EncCBOR`/`DecCBOR` and `ToCBOR`/`FromCBOR` for `ConwayTallyPredFailure`
* Add `ToCBOR`/`FromCBOR` for `ConwayGovernance`
* Remove `cgAlonzoGenesis` from `ConwayGenesis`.
* Set `ConwayGenesis` as `TranslationContext`

### `testlib`

* Fix `Arbitrary` for `ConwayTallyState`. #3323
* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285
* Add `Arbitrary` instances for:
  - `ConwayTallyPredFailure`
  - `EnactState`
  - `RatifyState`
  - `ConwayGovernance`
* Fix `Arbitrary` for `ConwayTxBody`.

## 1.0.0.0

* First properly versioned release.
