# Version history for `cardano-protocol`

## 0.3.0.0

* Change the Leios `HeaderBody` in `Cardano.Protocol.Leios.BlockHeader` to a memoized type:
  - Add `HeaderBodyRaw`, `HeaderBodyConstr` and `mkHeaderBody`
  - Change `HeaderBody` from a record type to a newtype over `MemoBytes (HeaderBodyRaw crypto)` with a read-only `HeaderBody` pattern synonym
  - Add `DecCBOR` instance for `Annotator (HeaderBody crypto)`
  - Change `SignableRepresentation` for `HeaderBody` to use the original bytes instead of re-serializing
  - Change `Eq` for `HeaderBody` to also compare the original bytes
* Remove the non-annotator `DecCBOR` instances for the Leios `HeaderBody` and `HeaderRaw`

### `testlib`

* Add `genHeaderBody` to `Test.Cardano.Protocol.Leios.BlockHeader.Arbitrary`
* Add non-annotator `DecCBOR` instances for the Leios `HeaderBody` and `HeaderRaw`

## 0.2.0.0

* Remove the `Header` pattern synonym from `Cardano.Protocol.Leios.BlockHeader`
* Add `mkHeader`, `headerBody` and `headerSig`, `HeaderRaw` to `Cardano.Protocol.Leios.BlockHeader`
* Rename `hbEbAnnouncement` to `hbEbReferencesAnnouncement` and change its type to `EbReferencesAnnouncement`
* Remove `EbAnnouncement` (moved to `cardano-ledger-core`)
* Change the `hbProtVer` field of the Leios `HeaderBody` to `hbVersionInfo :: BlockHeaderVersionInfo`
* Widen `cardano-crypto-class` upper bound to `<2.7`
* Export `HeaderConstr` from `Cardano.Protocol.Praos.BlockHeader` and `Cardano.Protocol.Leios.BlockHeader`

### `testlib`

* Add `genHeader` to `Test.Cardano.Protocol.Leios.BlockHeader.Arbitrary`
* Add `testlib` with `Test.Cardano.Protocol.TPraos.BlockHeader.Arbitrary`, `Test.Cardano.Protocol.Praos.BlockHeader.Arbitrary` and `Test.Cardano.Protocol.Leios.BlockHeader.Arbitrary`, providing `Arbitrary` instances for `OCert`, `KESPeriod`, `PrevHash`, `InputVRF`, the TPraos `BHeader`/`BHBody`/`Block`, and the Praos and Leios `Header`/`HeaderBody`/`Block`, and non-annotator `DecCBOR` instances for the TPraos `BHeader` and the Praos and Leios `Header`

## 0.1.0.0

* Add `Cardano.Protocol.Leios.BlockHeader`
* Initial release. Provides:
  - `Cardano.Protocol.Crypto`
  - `Cardano.Protocol.TPraos.OCert`
  - `Cardano.Protocol.TPraos.BlockHeader`
  - `Cardano.Protocol.Praos.VRF`
  - `Cardano.Protocol.Praos.BlockHeader`
