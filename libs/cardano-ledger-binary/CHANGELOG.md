# Version history for `cardano-ledger-binary`

## 1.3.1.1

*

## 1.3.1.0

* Increase `MaxVersion` to `11`
* Add `EncCBOR` and `DecCBOR` instances for `EpochInterval`

## 1.3.0.0

* Add `decodeNonEmptyList`
* Add instance `EncCBOR`/`DecCBOR` instance for `SignedDSIGN` and add `encodedSignedDSIGNSizeExpr`
* Remove unused and badly named `decodeMapNoDuplicates`
* Fix `decodeVMap` to no longer allow duplicates
* Add `decodeMapLikeEnforceNoDuplicates` and `decodeListLikeWithCount`
* Change the semantics of one argument for `decodeListLikeEnforceNoDuplicates` and
  `decodeSetLikeEnforceNoDuplicates` functions from checking membership to geting the size
* Change `decodeListLikeEnforceNoDuplicates` to also accept length decoding function
* Add `decodeListLikeT` and `decodeListLike`
* Moved `ToExpr` instances out of the main library and into the testlib.
* Add `encodeEnum` and `decodeEnumBounded`
* Change first argument of `decodeRecordSum` and `Summands` from `String` to `Text`

### `testlib`

* Add `diffExprCompact`

## 1.2.1.0

* Export `decodeListLikeEnforceNoDuplicates` #3791
* Add `Show` and `Eq` for `CBORGroup`

### `testlib`

* Addition of `Test.Cardano.Ledger.Binary.Cddl` spec testing.
* Export `toExpr` from `Test.Cardano.Ledger.Binary.TreeDiff`

## 1.2.0.0

* Export `decodeSetTag`, `allowTag`, `variableListLenEncoding`, and
  `decodeSetLikeEnforceNoDuplicates` #3779
* Moved `diffExpr`, `diffExprNoColor`, `ediffEq` into the `testlib`

## 1.1.3.0

* Add `ToExpr` instance for:
  * `Sized`
  * `SignedDSIGN`
* Add `Generic` instance for `CompactValue`
* Add `fieldGuarded` to be able to conditionally construct a `Field` #3712
* Expose `showDecoderError` from `Cardano.Ledger.Binary.Plain`

### `testlib`

* Add `roundTripCborSpec` and `roundTripAnnCborSpec`
* Adjust output of `showHexBytesGrouped`
* Add helper functions: `showMaybeDecoderError` and `showFailedTermsWithReSerialization`
* Improve failure reporting when re-serialization does not match for RoundTrip tests.

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
