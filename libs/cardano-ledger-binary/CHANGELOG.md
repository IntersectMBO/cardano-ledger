# Version history for `cardano-ledger-binary`

## 1.0.1.0

* Add `ToJSON`/`FromJSON` instances for `Version`
* Add `decodeFullFromHexText` and `serializeAsHexText`
* Add `Arbitrary` instance for `Term` to `testlib`.
* Add `encodeStrictMaybe`/`decodeStrictMaybe` and `encodeNullStrictMaybe`/`decodeNullStrictMaybe`
* Fix CBOR instance for `StrictMaybe`. It was never used in Byron, so special
  serialization is not needed for pre-protocol version `2`.

### `testlib`

* Add `Arbitrary` instance for `Term`

## 1.0.0.0

* First version. Released on an unsuspecting world.
