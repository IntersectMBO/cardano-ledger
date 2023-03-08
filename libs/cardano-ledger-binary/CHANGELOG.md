# Version history for `cardano-ledger-binary`

## 1.0.1.0

* Add `ToJSON`/`FromJSON` instances for `Version`
* Add `decodeFullFromHexText` and `serializeAsHexText`
* Add `Arbitrary` instance for `Term` to `testlib`.
* Re-export crypto related encoding and decoding functions for `VRF`, `KES` and `DSIGN`
  from `Cardano.Ledger.Binary.Plain`

## 1.0.0.0

* First version. Released on an unsuspecting world.
