# Version history for `cardano-ledger-binary`

## 1.1.2.0

* Re-export `ToExpr` from `Test.Cardano.Ledger.Binary.TreeDiff`

## 1.1.1.2

*

## 1.1.1.1

* Changed bounds on plutus-ledger-api
* GHC-9.6 compatibility

## 1.1.1.0

* Add `unlessDecoderVersionAtLeast` and `guardUntilAtLeast`
* Set bound on cborg >=0.2.9

## 1.1.0.0

* Add `ToJSON`/`FromJSON` instances for `Version`
* Add `decodeFullFromHexText` and `serializeAsHexText`
* Add `Arbitrary` instance for `Term` to `testlib`.
* Add `encodeStrictMaybe`/`decodeStrictMaybe` and `encodeNullStrictMaybe`/`decodeNullStrictMaybe`
* Fix CBOR instance for `StrictMaybe`. It was never used in Byron, so special
  serialization is not needed for pre-protocol version `2`.
* Re-export crypto related encoding and decoding functions for `VRF`, `KES` and `DSIGN`
  from `Cardano.Ledger.Binary.Plain`
* Fix deserializer for `Rational` and allow optionally tag `30` starting with protocol version `2`
* Fix serializer for `Ratio` and encode tag `30` starting with protocol version `2`
* Add new encoder `encodeRatioNoTag` for `Ratio`
* Changed: Starting in version 9, duplicate keys in CBOR sets are not longer allowed.
  Additionally, the CBOR set tag 258 is permitted but not enforced.

### `testlib`

* Add `Arbitrary` instance for `Term`
* Renamed:
  * `roundTripAnnFailureRangeExpectation` -> `roundTripAnnRangeFailureExpectation`
  * `roundTripFailureCborRangeExpectation` -> `roundTripCborRangeFailureExpectation`
  * `roundTripAnnFailureRangeExpectation` -> `roundTripAnnRangeFailureExpectation`
* Added:
  * `embedTripFailureExpectation`
  * `embedTripRangeFailureExpectation`
  * `roundTripRangeFailureExpectation`

## 1.0.0.0

* First version. Released on an unsuspecting world.
