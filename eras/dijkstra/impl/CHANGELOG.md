# Revision history for `cardano-ledger-dijkstra`

## 0.4.0.0

* Add `AlonzoEraTransition` instance for `DijkstraEra`
* Add `era` parameter to `PoolCert`s
* Enforce `accountBalanceIntervals` in `ENTITIES` and `SUBENTITIES`, and `startingAccountBalanceIntervals` in `ENTITIES`:
  - Add `validateAccountBalanceIntervals` and `validateStartingAccountBalanceIntervals`
  - Add `WrongNetworkInAccountBalanceIntervals`, `MissingAccountsInAccountBalanceIntervals`, and `BalancesOutsideAccountBalanceIntervals` to `EntitiesPredFailure`
  - Add `WrongNetworkInStartingAccountBalanceIntervals`, `MissingAccountsInStartingAccountBalanceIntervals`, and `BalancesOutsideStartingAccountBalanceIntervals` to `EntitiesPredFailure`
  - Add `SubWrongNetworkInAccountBalanceIntervals`, `SubMissingAccountsInAccountBalanceIntervals`, and `SubBalancesOutsideAccountBalanceIntervals` to `SubEntitiesPredFailure`
* Key `AccountBalanceIntervals` by `AccountAddress` (reward account) instead of `AccountId` (bare credential), consistent with `withdrawals`/`direct_deposits`:
  - Change `AccountBalanceIntervals` to `Map AccountAddress (AccountBalanceInterval era)`
* Restructure `EntitiesEnv`:
  - Remove `eePlutusLegacyMode` and `eeCertsEnv` fields
  - Add `eeCurrentEpoch`, `eePParams`, `eeCurrentCommittee`, `eeCommitteeProposals`, `eeOriginalAccounts`
* Change the `STS` `Signal` of `ENTITIES` to `StAnnTx TopTx era`
* Add `localProducedValue` helper in `UTxO` module
* Add `ValueNotConservedInLegacy` constructor to `DijkstraUtxoPredFailure`
* Change the block body serialization: each transaction in a block now carries its `is_valid` flag as the trailing element and the `invalid_transactions` field was removed from the block body:
  - Add `toCBORForBlockInclusion` and `decodeDijkstraTopTxInBlock`
  - Change `decodeDijkstraTopTx` to only decode the mempool format, by removing its `Bool` parameter
  - Remove `alignedValidFlags`
* Add a Dijkstra `EPOCH` rule
  - Take the stake snapshot at the end of the transition after stake pool and governance-action refunds and treasury withdrawals are applied, so that stake pool voting stake is consistent with DRep voting stake (#5014)
* Add `localProducedValue` helper in `UTxO` module
* Add `ValueNotConservedInLegacyInLegacyMode` constructor to `DijkstraUtxoPredFailure`
* Rename:
  - `DijkstraUtxoEnv` -> `UtxoEnv` and add `uePState` field
  - `dueSlot` -> `ueSlot`
  - `duePParams` -> `uePParams`
  - `dueCertState` -> `ueOriginalCertState`
  - `dueOriginalUtxo` -> `ueOriginalUtxo`
* Add `ScriptHashNotFoundForPurpose` constructor to `DijkstraContextError`
* Change `PointerPresentInOutput` constructor of `DijkstraContextError` to contain a `NonEmptySet TxOutSource` instead of `NonEmpty (TxOut era)`
* Add `udppPlutusV4CostModel` field to `UpgradeDijkstraPParams`
* Add `HKDSemialign` constraint to `upgradeDijkstraPParams`
* Add `transRedeemerPointerV4`
* Add `EncCBOR`, `ToCBOR` for `Block`
* Add `DecCBOR` instances for `Annotator Block`
* Remove `EncCBORGroup` instance for `DijkstraBlockBody`
* Add the `maxPledgeLeverage` protocol parameter (serializes as `nonnegative_interval / nil`):
  - Add `dppMaxPledgeLeverage` field to `DijkstraPParams`
  - Add `udppMaxPledgeLeverage` field to `UpgradeDijkstraPParams`
  - Add `hkdMaxPledgeLeverageL` to `DijkstraEraPParams`
  - Add `ppMaxPledgeLeverageL` and `ppuMaxPledgeLeverageL`
* Add `validateMissingAccountsInDirectDeposits`
* Remove `SubExceededBalancesInWithdrawals` constructor from `SubEntitiesPredFailure`
* Add `SubMissingOriginalAccountsInWithdrawals` constructor to `SubEntitiesPredFailure`
* Add `SubEntitiesEnv` and use it as `Environment` in the `SUBENTITIES` `STS` instance
* Change the `STS` `Signal` of `SUBENTITIES` to `Tx SubTx era`
* Add `sleOriginalAccounts` to `SubLedgerEnv`
* Memoize `getScriptsHashesNeeded` for subtransactions:
  - Add `dsastScriptsHashesNeeded` field to `DijkstraStAnnTx SubTx`, holding `Set ScriptHash`
  - Add `scriptsHashesNeededStAnnTx` method to `DijkstraEraUTxO`
* Add `minPoolMargin` protocol parameter (CIP-0023):
  - Add `dppMinPoolMargin` to `DijkstraPParams`
  - Add `udppMinPoolMargin` to `UpgradeDijkstraPParams`
  - Add `hkdMinPoolMarginL` to `DijkstraEraPParams`
  - Add `ppMinPoolMarginL` and `ppuMinPoolMarginL`
  - Override `ppMinPoolMarginG` in `EraPParams` instance
* Add the nine Leios protocol parameters of CIP-164:
  - Add `dppLeiosAnnouncementPeriodLength`, `dppLeiosVotePeriodLength`,
    `dppLeiosDiffusionPeriodLength`, `dppLeiosCommitteeSize`, `dppLeiosQuorumStakeThreshold`,
    `dppMaxEndorserBlockReferencesSize`, `dppMaxEndorserBlockTxsSize`,
    `dppMaxEndorserBlockExUnits`, `dppMaxRefScriptSizePerEndorserBlock` to `DijkstraPParams`
  - Add corresponding fields to `UpgradeDijkstraPParams`
  - Add corresponding HKD lenses, pp/ppu lenses to `DijkstraEraPParams`

### `cddl`

* Key `account_balance_intervals` and `starting_account_balance_intervals` by `reward_account` instead of `credential`
* Replace the `transaction` and `transaction_mempool` rules with `block_transaction` and `mempool_transaction`
* Remove the `invalid_transactions` rule and drop the field from `block_body`
* Add `HuddleRule "vrf_cert"` instance
* Add Leios protocol parameter entries (tags 40-48) in `protocol_param_update`
* Add `max_pledge_leverage` rule and its entry in `protocol_param_update`

### `testlib`

* Add `switchTxToLegacyMode` helper
* Add `balanceSubTransactions`
* Expose `fixupSubTransactions`
* Add `Inject (DijkstraContextError era) (ContextError era)` as a superclass of `DijkstraEraTest`
* Add `DecCBOR` instance for `Block`
* Add `DijkstraEraPParams` as a superclass of `DijkstraEraTest`
* Add `Test.Cardano.Ledger.Dijkstra.Imp.PoolSpec`

## 0.3.0.0

* Add `Ord` instances for `DijkstraBbodyPredFailure`, `DijkstraGovCertPredFailure`,
  `DijkstraGovPredFailure`, `DijkstraLedgerPredFailure`, `DijkstraMempoolPredFailure`,
  `DijkstraSubCertPredFailure`, `DijkstraSubCertsPredFailure`, `DijkstraSubDelegPredFailure`,
  `DijkstraSubGovCertPredFailure`, `DijkstraSubGovPredFailure`, `DijkstraSubLedgerPredFailure`,
  `DijkstraSubLedgersPredFailure`, `DijkstraSubPoolPredFailure`, `DijkstraSubUtxoPredFailure`,
  `DijkstraSubUtxowPredFailure`, `DijkstraUtxoPredFailure`, `DijkstraUtxowPredFailure`
* Add `Ord` instances for `DijkstraContextError`, `DijkstraNativeScriptRaw`, `DijkstraNativeScript`,
  `PerasCert`
* Rename constructors of `SubEntitiesPredFailure`:
  - `SubWithdrawalsMissingAccounts` -> `SubMissingAccountsInWithdrawals`
  - `SubWithdrawalAmountsExceedAccountBalances` -> `SubExceededBalancesInWithdrawals`
  - `SubDirectDepositsToMissingAccounts` -> `SubMissingAccountsInDirectDeposits`
* Add `SubWrongNetworkInWithdrawals` and `SubWrongNetworkInDirectDeposits` constructors to `SubEntitiesPredFailure`
* Rename constructors of `EntitiesPredFailure`:
  - `WithdrawalsMissingAccounts` -> `MissingAccountsInWithdrawals`
  - `WithdrawalAmountsExceedAccountBalances` -> `ExceededBalancesInWithdrawals`
  - `DirectDepositsToMissingAccounts` -> `MissingAccountsInDirectDeposits`
* Add `WrongNetworkInWithdrawals` and `WrongNetworkInDirectDeposits` constructors to `EntitiesPredFailure`
* Add `validateWrongNetworkInDirectDeposit`
* Remove `WrongNetworkWithdrawal` and `WrongNetworkInDirectDeposit `constructors from `DijkstraUtxoPredFailure`
* Remove `SubWrongNetworkWithdrawal` and `SubWrongNetworkInDirectDeposit` constructors from `DijkstraSubUtxoPredFailure`
* Remove `sleTxIx` from `SubLedgerEnv`
* Rename the phase-2 validity field on `DijkstraTx`: `dtIsValid` -> `dtIsPhase2Valid`
* Add `dsattPlutusRunnableCache` field to `DijkstraStAnnTx TopTx`, holding `Map ScriptHash (SupportedPlutusRunnable era)`
* Restrict `dijkstraCertsTotalDepositsTxBody` to `TxBody TopTx era` type
* Add `encodeLeiosCert`, `decodeLeiosCert`
* Add `startingAccountBalanceIntervals` to the top-level transaction body:
  - Add `startingAccountBalanceIntervalsTxBodyL` to the `DijkstraEraTxBody` typeclass
  - Add `dtbStartingAccountBalanceIntervals` to `TxBody`
  - Add `dtbrStartingAccountBalanceIntervals` to `DijkstraTxBodyRaw`
  - Add `startingAccountBalanceIntervalsDijkstraTxBodyRawL`
* Add `dsastPlutusRunnableCache` field to `DijkstraStAnnTx SubTx` and `dsattPlutusRunnableCache` field to `DijkstraStAnnTx TopTx`, holding `Map ScriptHash (SupportedPlutusRunnable era)`
* Change `EraRule "LEDGERS" DijkstraEra` from `Shelley.LEDGERS` to `Babbage.LEDGERS`
* Add `TranslateEra` instance for `SnapShots`
* Add `dijkstraConsumed` and `validateValueNotConservedUTxO`
* Add `AccountBalanceExact` constructor to `AccountBalanceInterval`
* Make `requiredTopLevelGuards` available on the top-level transaction body:
  - Change the type of `requiredTopLevelGuardsL` to `Lens' (TxBody l era) (Map (Credential Guard) (StrictMaybe (Data era)))`
  - Change the type of `requiredTopLevelGuardsDijkstraTxBodyRawL` to `Lens' (DijkstraTxBodyRaw l era) (Map (Credential Guard) (StrictMaybe (Data era)))`
  - Add `dtbRequiredTopLevelGuards` to `TxBody`
  - Add `dtbrRequiredTopLevelGuards` to `DijkstraTxBodyRaw`
* Add `MalformedGuardDatums` constructor to `DijkstraUtxowPredFailure`
* Add `validateGuardDatums`
* Add `RequiredTopLevelGuardsNotSupported` constructor to `DijkstraContextError`
* Add `SubEntitiesEvent` to `DijkstraSubLedgerEvent`
* Remove `SubCertsEvent` from `DijkstraSubLedgerEvent`
* Add `SubEntitiesFailure` to `DijkstraSubLedgerPredFailure`
* Remove constructors from `DijkstraSubLedgerPredFailure`:
  - `SubCertsFailure`
  - `SubWdrlNotDelegatedToDRep`
* Add `plutusLegacyModeStAnnTxG` method to `DijkstraEraUTxO`
* Remove constructors from `DijkstraLedgerPredicateFailure`:
  - `DijkstraCertsFailure`
  - `DijkstraWdrlNotDelegatedToDRep`
  - `DijkstraWithdrawalsMissingAccounts`
  - `DijkstraIncompleteWithdrawals`
* Remove `CertsEvent` constructor from `DijkstraLedgerEvent`
* Add:
  - `SUBENTITIES` and `STS` instance for it
  - `SubEntitiesPredFailure`
  - `SubEntitiesEvent`
* Add:
  - `ENTITIES` and `STS` instance for it
  - `EntitiesPredFailure`
  - `EntitiesEvent`
  - `EntitiesEnv`
* Rename `DijkstraRewarding` to `DijkstraWithdrawing` and deprecate the old name
* Rename rule types and deprecate the old names:
  - `DijkstraBBODY` -> `BBODY`
  - `DijkstraCERT` -> `CERT`
  - `DijkstraGOV` -> `GOV`
  - `DijkstraGOVCERT` -> `GOVCERT`
  - `DijkstraLEDGER` -> `LEDGER`
  - `DijkstraMEMPOOL` -> `MEMPOOL`
  - `DijkstraSUBCERT` -> `SUBCERT`
  - `DijkstraSUBCERTS` -> `SUBCERTS`
  - `DijkstraSUBDELEG` -> `SUBDELEG`
  - `DijkstraSUBGOV` -> `SUBGOV`
  - `DijkstraSUBGOVCERT` -> `SUBGOVCERT`
  - `DijkstraSUBLEDGER` -> `SUBLEDGER`
  - `DijkstraSUBLEDGERS` -> `SUBLEDGERS`
  - `DijkstraSUBPOOL` -> `SUBPOOL`
  - `DijkstraSUBUTXO` -> `SUBUTXO`
  - `DijkstraSUBUTXOW` -> `SUBUTXOW`
  - `DijkstraUTXO` -> `UTXO`
  - `DijkstraUTXOW` -> `UTXOW`
* Add `DijkstraEraUTxO` type class with `subTransactionsStAnnTx` method
* Add `TranslateEra` instance for `DijkstraEra VState`
* Fix `TranslateEra` instance for `DijkstraEra CertState`
* Add `GuardScriptHashesNotSupported` constructor to `DijkstraContextError`
* Add `decodeDijkstraTopTx`
* Change `Signal` to `StAnnTx TopTx era` for: `DijkstraLEDGER`, `DijkstraMEMPOOL`, `DijkstraUTXOW`, `DijkstraUTXO`
* Change `Signal` to `StAnnTx SubTx era` for: `DijkstraSUBLEDGER`, `DijkstraSUBUTXOW`, `DijkstraSUBUTXO`
* Change `DijkstraSUBLEDGERS` `Signal` to `[StAnnTx SubTx era]`
* Add `WithdrawalsExceedAccountBalance` to `DijkstraLedgerPredFailure`
* Removed `DijkstraSpendingOutputFromSameTx` from `DijkstraLedgerPredFailure`
* Added `batchNonDistinctRefScriptsSize`
* Add `guardDijkstraFeaturesForPlutusV1toV3`
* Add `DirectDepositsNotSupported` and `AccountBalanceIntervalsNotSupported` constructors to `DijkstraContextError`
* Add `SubTxContextError`
* Add `DijkstraStAnnTx`
* Add `scriptsProvidedDijkstraStAnnTx`
* Add `mkDijkstraStAnnTopTx`
* Remove `ToCBOR` and `FromCBOR` instances for `DijkstraGovPredFailure`
* Add `getDijkstraScriptsProvided`
* Add `MissingRequiredGuards` constructor to `DijkstraUtxowPredFailure`
* Add `DijkstraUtxoEnv` and use it as `Environemnt` in `STS` instance of `UTXOW`
* Refactor `DijkstraBlockBody` to use `MemoBytes` for memoized serialization
* Add `blockBodySize` implementation for `DijkstraEra`
* Add `DijkstraBlockBodyRaw`, `MkDijkstraBlockBody`
* Add `ApplyTick` instance for `DijkstraEra`
* Add `WrongNetworkInDirectDeposit` constructor to `DijkstraUtxoPredFailure`
* Add `SubWrongNetworkInDirectDeposit` constructor to `DijkstraSubUtxoPredFailure`
* Add `validateWrongNetworkInDirectDeposit`
* Add `checkPointerPresentInOutput`
* Add `UnsupportedScriptInSubTx` and `transFailUnsupportedScriptInSubTx`
* Remove `transPlutusPurposeV3` and `transPlutusPurposeV1V2`.
* `DijkstraTxInfoResult` changed its content type to `PlutusTxInfoResult`
* Add `EraForecast` instance for `DijkstraEra`.
* Deprecate `BHeaderView` in favour of `DijkstraEraBlockHeader` typeclass.
  - Add `PerasCert`, `PerasKey` and `validatePerasCert` to `Dijkstra.BlockBody` (moved from core).
  - Add `DijkstraEraBlockHeader` typeclass and the `DijkstraBbodySignal` GADT.
  - Remove `PrevEpochNonceNotPresent` from `DijkstraBbodyPredFailure`.
* Add `SubLedgerEnv` and `SubUtxoEnv`
* Remove `OutputTooSmallUTxO` constructor from `DijkstraUtxoPredFailure`
* Remove `SubOutputTooSmallUTxO` constructor from `DijkstraSubUtxoPredFailure`
* Remove `NoThunks` instances for all predicate failure types:
  - `DijkstraBbodyPredFailure`
  - `DijkstraGovPredFailure`
  - `DijkstraGovCertPredFailure`
  - `DijkstraLedgerPredFailure`
  - `DijkstraSubCertPredFailure`
  - `DijkstraSubCertsPredFailure`
  - `DijkstraSubDelegPredFailure`
  - `DijkstraSubGovPredFailure`
  - `DijkstraSubGovCertPredFailure`
  - `DijkstraSubLedgerPredFailure`
  - `DijkstraSubLedgersPredFailure`
  - `DijkstraSubPoolPredFailure`
  - `DijkstraSubUtxoPredFailure`
  - `DijkstraSubUtxowPredFailure`
  - `DijkstraUtxoPredFailure`
  - `DijkstraUtxowPredFailure`
* Remove `NoThunks` instance for `DijkstraContextError`
* Make `DijkstraContextError` constructors lazy
* Add `ToJSON` and `FromJSON` instances for
  - `DijkstraNativeScript era`
  - `AccountBalanceInterval era`
* Add `FromJSON` instance for
  - `DijkstraScript era`
  - `DijkstraDelegCert`
  - `DijkstraTxCert era`
* Export `dijkstraBasedEraNativeScriptToJSON` and `dijkstraBasedEraNativeScriptJSONParser` from `Cardano.Ledger.Dijkstra.Scripts`
* Remove DRep requirement for reward withdrawals
  - Remove `WdrlNotDelegatedToDRep` constructor from `EntitiesPredFailure`
  - Remove `SubWdrlNotDelegatedToDRep` constructor from `SubEntitiesPredFailure`

### `cddl`

* Add `eb_announcement` rule and extend `header_body` with `block_body_contains_leios_cert` and `eb_announcement` for the Leios block header
* Remove re-exported `genByteString`, `distinct`, `genHash28`, `majorProtocolVersionRule`, `ipRule` and `ipValidator`
* Remove `dijkstraProtocolVersionRule`
* Add `transaction_mempool` rule
* Add `peras_certificate`, `block_body`
* Extend `constr` CDDL rule to include tags 1280–1400 for Plutus `Data` constructor indexes
* Add optional `bls_key` field to `pool_params`
* Add `bls_key` rule with `bls_pubkey` (96 bytes) and `bls_possession_proof` (48 bytes)

### `testlib`

* Add `exampleBlsKey` to `Test.Cardano.Ledger.Dijkstra.Examples`
* Add `genSmallDijkstraBlockBody`
* Add to `Test.Cardano.Ledger.Dijkstra.Examples`:
  - `exampleDijkstraOnwardsEraPParams`
  - `exampleDijkstraOnwardsEraPParamsUpdate`
* Move `exampleDijkstraGenesis` from `Test.Cardano.Ledger.Dijkstra.ImpTest` to `Test.Cardano.Ledger.Dijkstra.Examples` (still re-exported from `ImpTest`).
* Make `Test.Cardano.Ledger.Dijkstra.Imp.spec` accept `Proxy era`
* Add `ToExpr` instance for `DijkstraBlockBody`
* Add `DecCBOR` instance for `DijkstraBlockBodyRaw`
* Add `genNonEmptyAccountBalanceIntervals`
* In `Test.Cardano.Ledger.Dijkstra.Examples`:
  - Remove `mkDijkstraBasedExampleTx`, `mkDijkstraBasedExampleTxBody`
  - Add `exampleDijkstraBasedTopTx`, `exampleDijkstraBasedSubTx`, `exampleDijkstraTx`, `exampleDijkstraGenesis`
* Move `exampleDijkstraGenesis` from `Test.Cardano.Ledger.Dijkstra.ImpTest` to `Test.Cardano.Ledger.Dijkstra.Examples`

## 0.2.0.0

* Expose `conwayToDijkstraUtxowPredFailure`
* Add `accountBalanceIntervalsTxBodyL` lens to `DijkstraEraTxBody` typeclass.
  - Add the corresponding field to both `TopTx` and `SubTx` levels of `TxBody`.
  - Add `AccountBalanceInterval` and `AccountBalanceIntervals` data types.
* Add `Generic` instance for `ApplyTxError`
* Change `ScriptsNotPaidUTxO` to use `NonEmptyMap TxIn (TxOut era)` instead of `UTxO era`
* Add `dijkstraToConwayDelegCert`
* Add:
  - `DijkstraLedgerEvent`
  - `DijkstraSubLedgersEvent`
  - `DijkstraSubLedgerEvent`
  - `DijkstraSubCertsEvent`
  - `DijkstraSubCertEvent`
  - `DijkstraSubPoolEvent`
  - `DijkstraSubGovEvent`
  - `DijkstraSubUtxowEvent`
  - `DijkstraSubUtxoEvent`
* Add `DijkstraLedgerEvent`
* Add `DirectDeposits` to transaction bodies at both (top and sub) levels.
  - Add `directDepositsTxBodyL` lens to the `DijkstraEraTxBody` typeclass.
* Add `DijkstraSpendingOutputFromSameTx` to `DijkstraLedgerPredFailure`, to report when a sub-tx-id is being spent within the same transaction.
* Add:
  - `DijkstraSUBCERT`
  - `DijkstraSUBCERTS`
  - `DijkstraSUBDELEG`
  - `DijkstraSUBGOV`
  - `DijkstraSUBGOVCERT`
  - `DijkstraSUBLEDGER`
  - `DijkstraSUBLEDGERS`
  - `DijkstraSUBPOOL`
  - `DijkstraSUBUTXOW`
  - `DijkstraSUBUTXO`
    and `STS` instances for them
* Add:
  - `DijkstraSubCertPredFailure`
  - `DijkstraSubCertsPredFailure`
  - `DijkstraSubDelegPredFailure`
  - `DijkstraSubGovPredFailure`
  - `DijkstraSubGovCertPredFailure`
  - `DijkstraSubLedgerPredFailure`
  - `DijkstraSubLedgersPredFailure`
  - `DijkstraSubPoolPredFailure`
  - `DijkstraSubUtxoPredFailure`
  - `DijkstraSubUtxowPredFailure`
* Add `DijkstraSubLedgersFailure` to `DijkstraLedgerPredFailure`
* Add `SubCertsEnv`
* Deprecate `InvalidPolicyHash` in favor of new `InvalidGuardrailsScriptHash`
* Move the `DijkstraMempoolFailure` constructor from `DijkstraLedgerPredFailure` to `DijkstraMempoolPredFailure`
* Add the `DijkstraMempoolPredFailure` predicate failure for the MEMPOOL rule
* Add `DijkstraApplyTxError` constructor for `ApplyTxError era`
* Renamed:
  - `dppMinFeeA` -> `dppTxFeePerByte`
  - `dppMinFeeB` -> `dppTxFeeFixed`
* Changed type of `dppMinFeeA` to `CoinPerByte`
* Change sets containing errors into `NonEmptySet` for `DijkstraGovPredFailure`, `DijkstraUtxoPredFailure`, `DijkstraUtxowPredFailure`
* Change all maps into `NonEmptyMap` for `DijkstraGovPredFailure` and `DijkstraLedgerPredFailure`
* Change Dijkstra BBODY rule to validate Peras certificates when present
* Add new block body predicate falures for Dijkstra:
  - `PrevEpochNonceNotPresent` for missing optional nonce needed for validation
  - `PerasCertValidationFailed` for certification validation failures
* Change all lists into `NonEmpty` for `DijkstraUtxoPredFailure`, `DijkstraUtxowPredFailure`
* Add `cddl` sub-library, and `generate-cddl` executable.
* Add `bhviewPrevEpochNonce` to `BHeaderView`
* Change `makeHeaderView` to expect an additional `Maybe Nonce`
* Add `dijkstraBbodyTransition` to the BBODY rule
* Add `DijkstraBlockBody` type and pattern
* Add `mkBasicBlockBodyDijkstra`
* Add `DijkstraEraBlockBody` class and instance for `DijkstraEraBlockBody`
* Add `EraBlockBody` instance for `DijkstraEra`
* Re-export `DijkstraBlockBody` from `Cardano.Ledger.Dijkstra.Core`
* Add `DijkstraUtxoPredFailure`
* Add `DijkstraUTXO`
* Changed the type of the following fields to `CompactForm Coin` in `DijkstraPParams`:
  - `dppMinFeeB`
  - `dppKeyDeposit`
  - `dppMinPoolCost`
  - `dppGovActionDeposit`
* Change some rule transitions to use Dijkstra's own rules instead of reusing Conway's:
  - `DijkstraBBODY`
  - `DijkstraGOV`
  - `DijkstraGOVCERT`
  - `DijkstraLEDGER`
  - `DijkstraMEMPOOL`
  - `DijkstraUTXO`
  - `DijkstraUTXOW`
* Change some rule predicate failures to use Dijkstra-era versions:
  - `DijkstraBbodyPredFailure` for the BBODY rule
  - `DijkstraGovPredFailure` for the GOV rule
  - `DijkstraGovCertPredFailure` for the GOVCERT rule
  - `DijkstraLedgerPredFailure` for the LEDGER rule
  - `DijkstraUtxoPredFailure` for the UTXO rule
  - `DijkstraUtxowPredFailure` for the UTXOW rule
* Add `requiredTopLevelGuardsDijkstraTxBodyRawL`
* Add `dstbRequiredTopLevelGuards` to `TxBody`
* Add `dstbrRequiredTopLevelGuards` to `DijkstraSubTxBodyRaw`
* Add `requiredTopLevelGuardsL` to `DijkstraEraTxBody` class
* Add `DijkstraContextError`
* Add `dtbSubTransactions` to `TxBody`
* Add `subTransactionsTxBodyL` method to `DijkstraEraTxBody` class
* Add `DijkstraTx` type with `DijkstraTx` and `DijkstraSubTx` constructors
* Add `DijkstraSubTxBody` constructor to `DijkstraTxBodyRaw`
* Add `TxLevel` argument to `Tx` and `TxBody`
* Add `HasEraTxLevel` instances for `Tx` and `TxBody`
* Add `EraTxLevel` instance
* Add `DijkstraNativeScript` and `DijkstraNativeScriptRaw` along with type instances
* Change `NativeScript` type family to `DijkstraNativeScript`
* Add `evalDijkstraNativeScript` to `Scripts` module
* Add `upgradeTimelock` to `Scripts` module
* Add `validateDijkstraNativeScript` to `Tx` module
* Add `RequireGuard` pattern to `Scripts` module
* Add `ConwayEraScript` constraint to `DijkstraEraScript`

### `cddl`

* Add `account_balance_intervals` to `transaction_body` and `sub_transaction_body`.
  - Add `accountBalanceInterval` and `accountBalanceIntervals` rules.
* Remove `account_registration_cert` and `account_unregistration_cert`.
* Add `directDepositsRule` to the transaction body.
* Constrain `protocol_version` minor field to `uint .size 4`.
* Renamed `policy_hash` to `guardrails_script_hash` in governance actions to avoid confusion with multi-asset policy IDs
* Move `cddl-files` to `cddl/data`.
* Add full `HuddleSpec`.

### `testlib`

* Add `Test.Cardano.Ledger.Dijkstra.Imp.LedgerSpec`
* Add `Test.Cardano.Ledger.Dijkstra.Imp.UtxoSpec`
* Remove `huddle-cddl` and the `CDDL` modules.
* Re-export `Test.Cardano.Ledger.Conway.Binary.Golden`
* Remove CDDL `certificate` redefinition to reuse from conway.
* Add CDDL exports for `plutus_v4_script`, `dijkstra_native_script`, `script_require_guard`
* Remove CDDL `protocol_version` redefinition
* Add `impDijkstraSatisfyNativeScript`
* Add `DijkstraEraTxBody` and `DijkstraEraScript` constraints to `DijkstraEraTest`

## 0.1.0.0

* First version. Released on an unsuspecting world.
