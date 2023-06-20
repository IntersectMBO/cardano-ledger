# Version history for `cardano-protocol-tpraos`

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

