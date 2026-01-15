# Version history for `cardano-ledger-binary`

## 1.8.0.0

* Change `Version` from `Word64` to `Word32`
  - Add `mkVersion32` and `getVersion32`
* Remove `listLenBound` from `EncCBORGroup`
* Change type of `listLen` in `EncCBORGroup` to accept a `Proxy` instead of a concrete value
* Add `decodeNonEmptySetLikeEnforceNoDuplicatesAnn` to `Annotated` module
* Add `ofieldA` and `fieldAGuarded` to `Coders` module
* Change `Density` type to only be available at the type level
* Change `Wrapped` type to only be available at the type level
* Make `decodeAnnSet` fail when there are duplicates, starting with protocol version `12`.
* Provide ability for `Annotator` to fail, by changing its type signature to return `Either` and adding `MonadFail` instance.
* Remove `encodedSizeExpr` and `encodedListSizeExpr` from `EncCBOR`
* Remove `Typeable` superconstraint from `EncCBOR`
* Remove `Range`, `szEval`, `Size`, `Case`, `caseValue`, `LengthOf`, `SizeOverride`, `isTodo`, `szCases`, `szLazy`, `szGreedy`, `szForce`, `szWithCtx`, `szSimplify`, `apMono`, `szBounds`, `encodedVerKeyDSIGNSizeExpr`, `encodedSignKeyDSIGNSizeExpr`, `encodedSigDSIGNSizeExpr`, `encodedSignedDSIGNSizeExpr`, `encodedVerKeyKESSizeExpr`, `encodedSignKeyKESSizeExpr`, `encodedSigKESSizeExpr`, `encodedVerKeyVRFSizeExpr`, `encodedSignKeyVRFSizeExpr` and `encodedCertVRFSizeExpr`

### `testlib`

* Remove `Arbitrary` instances for `CertifiedVRF`, `SigDSIGN`, `SignKeyDSIGN`, `SignedDSIGN`, `VerKeyDSIGN`.
  These have been moved to `cardano-crypto-class:testlib`.
* Remove `Arbitrary` instances for `StrictSeq`, `StrictMaybe`.
  These have been moved to `cardano-strict-containers:testlib`.
* Add `huddleRoundTripGenValidate`
* Remove `Test.Cardano.Ledger.Binary.Cddl`
* Add `ToExpr` instances to `DeserialiseFailure` and `DecoderError`
* Remove `assertExprEqualWithMessage`

## 1.7.0.0

* Add `Random` instance for `Version`.
* Add `liftST`
* Bump `MaxVersion` to `12`
* Add `decodeFullFromHexText`
* Moved `Annotator` orphan instance for plutus `Data` into `testlib`

## 1.6.0.0

* Add `Typeable` constraint to `invalidKey`
* Add `Typeable` constraint to various Coders functions and types: `field`, `fieldGuarded`,
  `ofield`, `fieldA`, `fieldAA`, `ApplyD`, `Map`, `Ann`, `ApplyAnn`, `ApplyErr`, `<!`, `<*!`, `<?`,
  `decode`, `decodE`, `decodeCount`, `decodeClosed`, `listDecodeA`, `setDecodeA`, `mapDecodeA`
* Add `mapCoder` as a replacement for `fmap`
* Remove `Functor` and `Applicative` instance for `Decode`
* Add to `Plain`:
  - `assertTag`
  - `decodeTagMaybe`
  - `encodeRatioWithTag`,
* Add `DecCBOR` instance for `Data.IntMap`
* Add `decodeIntMap`
* Add `ToCBOR` instance for `PV1.Data`
* Add `DecCBOR` instance for `Annotated a ByteString`
* Add `originalBytesExpectedFailureMessage` needed for testing
* Add `decodeListLikeWithCountT`
* Add `internMap`, `internSet`, ` internsFromSet`
* Add `DecShareCBOR` for  `Set`
* Add `Semigroup` instance for  `Interns`
* Add `encodeMemPack` and `decodeMemPack` helper functions.
* Remove `encodeSignKeyKES` and `decodeSignKeyKES`
* Remove `EncCBOR` and `DecCBOR` instances for `SignKeyKES`

## 1.5.0.0

* Remove deprecated `decodeAnnotator`, `decCBORMaybe`, `encCBORMaybe`, `sizedDecoder`, `encodePair`
* Add `decodeAnnotated`
* Add `getOriginalBytes`
* `toPlainDecoder` now optionally expects one extra argument for the original `ByteString`
* Extend `Coders` to accommodate `{Enc|Dec}CBORGroup`. #4666
  - Add `ToGroup` to `Encode`
  - Add `FromGroup` to `Decode`
* Add `{Enc|Dec}CBORGroup` instance for `(a, a)`. #4666

### `testlib`

* Remove `Arbitrary` instances for `Data.Vector.Primitive` that are now in `quickcheck-instances`

## 1.4.0.0

### `testlib`

* Add:
  - `decoderEquivalenceSpec`
  - `decoderEquivalenceExpectation`
  - `decoderEquivalenceProp`
  - `cddlDecoderEquivalenceSpec`
  - `huddleDecoderEquivalenceSpec`
* Re-export types `Doc` and `AnsiStyle` in `Test.Cardano.Ledger.Binary.TreeDiff`
* `diffExpr` and `diffExprCompact` changed type signature
* Add `diffExprString` and `diffExprCompactString`, which replace the old implementations
  of `diffExprNoColor` and `diffExprCompactNoColor` and embed ANSI color sequences into the strings
* Add functions `ansiExpr` and `ansiExprString` that produce colored output via `toExpr`
* Add a function `tableDoc` that formats a table using `Prettyprinter`
* Add a function `ansiDocToString` that converts a prettyprinted document to a string
  with embedded ANSI color sequences
* Add `assertColorFailure`, `callStackToLocation` and `srcLocToLocation`

## 1.3.4.0

* Add `writeSpec` funtion to Test.Cardano.Ledger.Binary.Cuddle

### `testlib`

* Add the module Test.Cardano.Ledger.Binary.Cuddle with the
  functions `huddleRoundTripCborSpec` and `huddleRoundTripAnnCborSpec`.

## 1.3.3.0

* Add `EncCBOR` and `DecCBOR` instances for `ScriptContext` for PlutusV1/V2/V3

### `testlib`

* Re-export:
  - `Pretty`
  - `ansiWlPretty`
  - `ppEditExpr`
  - `ediff`

## 1.3.2.0

* Add `decodeFullAnnotatorFromHexText` and `withHexText`

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
  - `Sized`
  - `SignedDSIGN`
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
  - `roundTripAnnFailureRangeExpectation` -> `roundTripAnnRangeFailureExpectation`
  - `roundTripFailureCborRangeExpectation` -> `roundTripCborRangeFailureExpectation`
  - `roundTripAnnFailureRangeExpectation` -> `roundTripAnnRangeFailureExpectation`
* Added:
  - `embedTripFailureExpectation`
  - `embedTripRangeFailureExpectation`
  - `roundTripRangeFailureExpectation`

## 1.0.0.0

* First version. Released on an unsuspecting world.
