# Version history for `cardano-ledger-conway`

## 1.1.0.0

Added:
 * Added `RATIFY` rule
 * Added `GovernanceActionMetadata`
 * Added `RatifyEnv` and `RatifySignal`
 * Added lenses:
   * `cgTallyL`
   * `cgRatifyL`
   * `cgVoterRolesL`

Removed:
 * Removed `GovernanceActionInfo`
 * Removed `Vote`

Changed:
 * Replaced `ctbrVotes` and `ctbrGovActions` with `ctbrGovProcedure`
 * Renamed `ENACTMENT` to `ENACT`

## 1.0.0.0

* First properly versioned release.
