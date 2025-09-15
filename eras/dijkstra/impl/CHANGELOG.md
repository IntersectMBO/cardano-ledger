# Revision history for cardano-ledger-dijkstra

## 0.2.0.0

* Add `DijkstraNativeScript` and `DijkstraNativeScriptRaw` along with type instances
* Change `NativeScript` type family to `DijkstraNativeScript`
* Add `evalDijkstraNativeScript` to `Scripts` module
* Add `upgradeTimelock` to `Scripts` module
* Add `validateDijkstraNativeScript` to `Tx` module
* Add `RequireGuard` pattern to `Scripts` module
* Add `ConwayEraScript` constraint to `DijkstraEraScript`

### `testlib`

* Add `impDijkstraSatisfyNativeScript`
* Add `DijkstraEraTxBody` and `DijkstraEraScript` constraints to `DijkstraEraTest`

## 0.1.0.0

* First version. Released on an unsuspecting world.
