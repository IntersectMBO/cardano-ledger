# Version history for `cardano-ledger-conway`

## 1.1.0.0

* Added `RATIFY` rule
* Added `ConwayGovernance`
* Added `RatifyEnv` and `RatifySignal`
* Added `EnactState` and `RatifyState`
* Added lenses:
  * `cgTallyL`
  * `cgRatifyL`
  * `cgVoterRolesL`
* Removed `GovernanceActionInfo`
* Replaced `ctbrVotes` and `ctbrGovActions` with `ctbrGovProcedure`
* Renamed `ENACTMENT` to `ENACT`
* Add `ToJSON` instance for: #3323
  * `ConwayGovernance`
  * `ConwayTallyState`
  * `GovernanceAction`
  * `GovernanceActionState`
  * `GovernanceActionIx`
  * `GovernanceActionId`
* Add `ToJSONKey` instance for `GovernanceActionId` #3323
* Fix `EncCBOR`/`DecCBOR` and `ToCBOR`/`FromCBOR` for `ConwayTallyState` #3323
* Add `Anchor` and `AnchorDataHash` types. #3323
* Rename `transDCert` to `toShelleyDCert`
* Add `fromShelleyDCertMaybe`

### `testlib`

* Fix `Arbitrary` for `ConwayTallyState`. #3323
* Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285

## 1.0.0.0

* First properly versioned release.
