# Version history for `cardano-protocol`

## 0.1.0.1

*

## 0.1.0.0

* Add `Cardano.Protocol.Leios.BlockHeader`
* Initial release. Provides:
  - `Cardano.Protocol.Crypto`
  - `Cardano.Protocol.TPraos.OCert`
  - `Cardano.Protocol.TPraos.BlockHeader`
  - `Cardano.Protocol.Praos.VRF`
  - `Cardano.Protocol.Praos.BlockHeader`

### `testlib`

* Add `testlib` with `Test.Cardano.Protocol.Praos.Arbitrary` and `Test.Cardano.Protocol.Leios.Arbitrary`, providing `Arbitrary` instances for the Praos and Leios `Header`/`HeaderBody`/`Block`, `EbAnnouncement`, `InputVRF`, `OCert`, `KESPeriod` and `PrevHash`, and non-annotator `DecCBOR` instances for the Praos and Leios `Header`
