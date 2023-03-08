# Version history for `cardano-ledger-conway`

## 1.1.0.0

* Add `ToJSON` instance for:
  * `ConwayTallyState`
  * `GovernanceAction`
  * `GovernanceActionState`
  * `GovernanceActionIx`
  * `GovernanceActionId`
* Add `ToJSONKey` instance for `GovernanceActionId`
* Rename `transDCert` to `toShelleyDCert`
* Add `fromShelleyDCertMaybe`

## 1.0.0.0

* First properly versioned release.
