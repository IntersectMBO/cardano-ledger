# Revision history for cardano-ledger-dijkstra

## 0.2.0.0

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

### `testlib`

* Remove CDDL `certificate` redefinition to reuse from conway.
* Add CDDL exports for `plutus_v4_script`, `dijkstra_native_script`, `script_require_guard`
* Remove CDDL `protocol_version` redefinition
* Add `impDijkstraSatisfyNativeScript`
* Add `DijkstraEraTxBody` and `DijkstraEraScript` constraints to `DijkstraEraTest`

## 0.1.0.0

* First version. Released on an unsuspecting world.
