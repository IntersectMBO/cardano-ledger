# Version history for `cardano-ledger-core`

## 1.4.1.0

* Add `spendableInputsTxBodyL`

## 1.4.0.0

* Added `DRep`, `DRepCredential`
* Changed type `Credential 'Voting c` -> `DRep c` in:
  * `UMElem`
  * `umElemAsTuple`
  * `umElemDRep`
  * `UMap`
  * `DRepUView`
  * `dRepUView`
  * `dRepMap`
  * `unify`

## 1.3.1.0

* Addition of `getPoolCertTxCert`

## 1.3.0.0

* Add delegated representatives to the `UMap` and make its interface more coherent #3426
  * Additions
    * Add `Credential 'Voting (EraCrypto era)` as the fourth element to the `UMap` n-tuple.
    * `umElemPtrs :: UMElem c -> Maybe (Set Ptr)`
    * `umElemDRep :: UMElem c -> Maybe (Credential 'Voting (EraCrypto era))`
    * `UView (DRepUView)` constructor and `dRepUView`
    * `invPtrMap :: UMap c -> Map (Credential 'Staking c) (Set Ptr)`
    * `dRepMap :: UMap c -> Map (Credential 'Staking c) (Credential 'Voting c)`
    * synonym `unionL = (∪)`
    * synonym `unionR = (⨃)`
    * synonym `domDelete = (⋪)`
    * synonym `rngDelete = (⋫)`
  * Renames
    * `Trip` to `UMElem`, pattern `Triple` to `UMElem`
    * `viewTrip` to `umElemAsTuple`
    * `tripRewardActiveDelegation` to `umElemRDActive`
    * `tripReward` to `umElemRDPair`
    * `tripDelegation` to `umElemSPool`
    * `View (RewardDeposits, Delegations, Ptrs)` to `UView (RewDepUView, SPoolUView, PtrUView)`
      * `rdPairs` to `rewDepUView`
      * `delegations` to `sPoolUView`
      * `ptrs` to `ptrUView`
      * `unView` to `unUView`
      * `viewtoVMap` to `unUnifyToVMap`
      * `rewView` to `rewardMap`
      * `compactRewView` to `compactRewardMap`
      * `depositView` to `depositMap`
      * `rdPairView` to `rdPairMap`
      * `delView` to `sPoolMap`
      * `ptrView` to `ptrMap`
      * `domRestrictedView` to `domRestrictedMap`
    * `zero` to `nullUMElem`
    * `zeroMaybe` to `nullUMElemMaybe`
    * `sumRewardsView` to `sumRewardsUView`
    * `sumDepositView` to `sumDepositUView`
  * Reimplementations
    * `unionRewAgg` NOTE: It does not require `assert (Map.valid result) result` any more
      and has been tested for equivalence with the older version with
      `--qc-max-success=10000 --qc-max-size=1000`. The test is added to `UMapSpec`.
* Add `certsTxBodyL` to `EraTxBody`
* Introduce `TxCert` type family and `EraTxCert` type class.
* Deprecate `Delegation`
* Add `toKeyHashWitness`
* Addition of `getVKeyWitnessTxCert` and `getScriptWitnessTxCert` to `EraTxCert` type class
* Add new key roles: `CommitteeColdKey` and `CommitteeHotKey`
* Remove `ConstitutionalDelegCert`. Instead it now lives in `cardano-ledger-shelley` as
  `GenesisDelegCert`
* Add `StakeCredentials` and two helper functions: `toStakeCredentials` and
  `domRestrictedStakeCredentials`

### `testlib`

* Add `genValidUMapWithCreds` and `uniformSubset`

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
