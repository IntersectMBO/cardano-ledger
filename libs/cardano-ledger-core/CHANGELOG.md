# Version history for `cardano-ledger-core`

## 1.3.0.0

* Add `certsTxBodyL` to `EraTxBody`
* Introduce `TxCert` type family and `EraTxCert` type class.
* Deprecate `Delegation`
* Add `toKeyHashWitness`
* Addition of `getVKeyWitnessTxCert` and `getScriptWitnessTxCert` to `EraTxCert` type class

## 1.2.0.0

* Deprecate `Cardano.Ledger.UMapCompact` in favor of `Cardano.Ledger.UMap` and add tests for it. #3371
  * Add `Cardano.Ledger.UMap.rdPairView` to view the reward-deposits pair from the `UMap`.
* Replace `DPState c` with `CertState era`
* Add `VState`
* Add `certVState`
* Parametrize `DState` and `PState` by era
* Rename `Cardano.Ledger.DPState` module to `Cardano.Ledger.CertState`
* Rename:
  * `lsDPState` -> `lsCertState`
  * `dpsPState` -> `certPState`
  * `dpsDState` -> `certDState`
  * `obligationDPState` -> `obligationCertState`
* Add support for `PlutusV3`

## 1.1.0.0

* Add `ToJSON (PParamsHKD f era)` superclass constraints for `EraPParams`.
* Add `ToJSON (TxOut era)` superclass constraints for `EraTxOut`.
* Add superclass constraints for `Val`:
  * `NoThunks`, `EncCBOR`, `DecCBOR`, `ToJSON`, `NFData`, `Show`
  * `EncCBOR (CompactForm t)`, `DecCBOR (CompactForm t)`
* Add `ToJSONKey`/`FromJSONKey` instances for `ScriptHash`
* Add `ToJSON`/`FromJSON` instances for `CompactForm Coin`, `SafeHash` and `TxId`
* Add `ToJSON` instances for:
  * `Ptr`, `CertIx`, `TxIx`
  * `Trip` and `UMap`
  * `DeltaCoin` and `CompactForm DeltaCoin`
  * `InstantaneousRewards`, `FutureGenDeleg`, `PState`, `DState` and `DPState`.
  * `UTxO` and `TxIn`
  * `Stake`, `SnapShot`, `SnapShots`, `PoolDistr` and `IndividualPoolStake`
  * `Reward` and `RewardType`
  * `AuxiliaryDataHash`
  * `Credential`
* Make `getConsumedValue` accept a deposit lookup function instead of a `DPState`
* Add `lookupDepositDState` and `lookupRewardDState`. Former can be used with
  `getConsumedValue` to regain previous behavior.
* Add `hashScriptTxWitsL`
* Remove custom `Fail` type, in favor of
  [`FailT`](https://hackage.haskell.org/package/FailT) package
* Move `bBodySize` into `Cardano.Ledger.Core`
* Rename `TxId` field from `_unTxId` to `unTxId`

### `testlib`

* Remove `runFailError` in favor of `errorFail` from `FailT` package

## 1.0.0.0

* First properly versioned release.
