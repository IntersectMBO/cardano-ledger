# Version history for `cardano-protocol-tpraos`

## 1.4.0.0

* Converted `CertState` to a type family
* Made the fields of predicate failures and environments lazy
* Move `Crypto` and `StandardCrypto` definitions from `cardano-ledger-core` into new
  `Cardano.Protocol.Crypto` module.
* Remove crypto parametrization from types:
  * `LedgerView`, `ChainDepState`
  * `HashHeader` `PrevHash`, `LastAppliedBlock`
  * `OCertEnv`, `OcertPredicateFailure`
  * `OverlayEnv`, `OBftSlot`
  * `PrtclState`, `PrtclEnv`, `PrtlSeqFailure`

### `testlib`

* Add `genAllIssuerKeys`
* Add `Arbitrary` instances for `KESKeyPair` and `VRFKeyPair`
* Move `VRFNatVal` from `cardano-ledger-shelley-test` in here.
* Change type of `kesSignKey` in `KESKeyPair` and result of `evolveKESUntil` from `SignKeyKES` to `UnsoundPureSignKeyKES`

## 1.3.0.0

* Change the type of hashes that are contained in `WrongGenesisVRFKeyOVERLAY` and `VRFKeyWrongVRFKey` predicate failures

## 1.2.0.1

*

## 1.2.0.0

* Change `FutureLedgerViewError` and `ChainTransitionError`
  to use `NonEmpty (PredicateFailure _)` instead of `[PredicateFailure _]`

## 1.1.0.0
* Change the type of `bsize` and `hBbsize` to `Word32`

## 1.0.3.7

* Add a test suite

## 1.0.3.6

*

## 1.0.3.5

*

## 1.0.3.4

* Compatibility patch for `cardano-ledger-shelley-1.5.0.0`

## 1.0.3.3

*

## 1.0.3.2

* Changed upper bound on cardano-ledger-shelley  and base

## 1.0.3.1

* upper bounds on cardano-ledger-core and cardano-ledger-shelley

## 1.0.3.0

* lower bound on cardano-crypto-class

## 1.0.2.0

* Add a `testlib` and move `Arbitrary` instances from
  `Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators` over.

## 1.0.1.0

* Add `ToCBOR`/`FromCBOR` instaces for `OCert` and `KESPeriod`
* Make fields for `OCertEnv` strict.

### `testlib`

* Add a `testlib` and move `Arbitrary` instances from
  `Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators` over.
* Create `VRFKeyPair` and `KESKeyPair`. Dtart using them everywhere instead of tuples.
* Add `mkBHeader`
* Move from `cardano-ledger-shelley-test`: `mkOCert`, `mkBHBody`, `mkBlock`
* Move `AllIssuerKeys` from `cardano-ledegr-shelley-test`. Rename its fields:
  *  `cold` - > `aikCold`
  *  `hot` - > `aikHot`
  *  `vrf` - > `aikVrf`
  *  `hk` - > `aikColdKeyHash`
* Bring back `genBlock`
* Move `genCoherentBlock` from `cardano-ledegr-shelley-test` and change it to accept
  `AllIssuerKeys` as an argument.

## 1.0.0.0

* First properly versioned release.
