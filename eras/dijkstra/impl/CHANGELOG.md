# Revision history for cardano-ledger-dijkstra

## 0.2.0.0

* Deprecate `InvalidPolicyHash` in favor of new `InvalidGuardrailsScriptHash`
* Move the `DijkstraMempoolFailure` constructor from `DijkstraLedgerPredFailure` to `DijkstraMempoolPredFailure`
* Add the `DijkstraMempoolPredFailure` predicate failure for the MEMPOOL rule
* Add `DijkstraApplyTxError` constructor for `ApplyTxError era`
* Renamed:
  - `dppMinFeeA` -> `dppTxFeePerByte`
  - `dppMinFeeB` -> `dppTxFeeFixed`
* Changed type of `dppMinFeeA` to `CoinPerByte`
* Change sets containing errors into `NonEmptySet` for `DijkstraGovPredFailure`, `DijkstraUtxoPredFailure`, `DijkstraUtxowPredFailure`
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
* Add `Test.Cardano.Ledger.Dijkstra.Imp.UtxoSpec`
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

* Renamed `policy_hash` to `guardrails_script_hash` in governance actions to avoid confusion with multi-asset policy IDs
* Move `cddl-files` to `cddl/data`.
* Add full `HuddleSpec`.

### `testlib`

* Remove `huddle-cddl` and the `CDDL` modules.
* Re-export `Test.Cardano.Ledger.Conway.Binary.Golden`
* Remove CDDL `certificate` redefinition to reuse from conway.
* Add CDDL exports for `plutus_v4_script`, `dijkstra_native_script`, `script_require_guard`
* Remove CDDL `protocol_version` redefinition
* Add `impDijkstraSatisfyNativeScript`
* Add `DijkstraEraTxBody` and `DijkstraEraScript` constraints to `DijkstraEraTest`

## 0.1.0.0

* First version. Released on an unsuspecting world.
