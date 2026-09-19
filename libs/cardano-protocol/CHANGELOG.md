# Version history for `cardano-protocol`

## 0.1.1.0

* Widen `cardano-crypto-class` upper bound to `<2.7`
* Export `HeaderConstr` from `Cardano.Protocol.Praos.BlockHeader` and `Cardano.Protocol.Leios.BlockHeader`

### `testlib`

* Add `testlib` with `Test.Cardano.Protocol.TPraos.BlockHeader.Arbitrary`, `Test.Cardano.Protocol.Praos.BlockHeader.Arbitrary` and `Test.Cardano.Protocol.Leios.BlockHeader.Arbitrary`, providing `Arbitrary` instances for `OCert`, `KESPeriod`, `PrevHash`, `InputVRF`, the TPraos `BHeader`/`BHBody`/`Block`, the Praos and Leios `Header`/`HeaderBody`/`Block` and `EbAnnouncement`, and non-annotator `DecCBOR` instances for the TPraos `BHeader` and the Praos and Leios `Header`

## 0.1.0.0

* Add `Cardano.Protocol.Leios.BlockHeader`
* Initial release. Provides:
  - `Cardano.Protocol.Crypto`
  - `Cardano.Protocol.TPraos.OCert`
  - `Cardano.Protocol.TPraos.BlockHeader`
  - `Cardano.Protocol.Praos.VRF`
  - `Cardano.Protocol.Praos.BlockHeader`
