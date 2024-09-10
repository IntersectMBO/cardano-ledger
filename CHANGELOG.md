# Changelog

This changelog contains notable changes that made into each [`cardano-node` release](https://github.com/input-output-hk/cardano-node/releases),
starting with version `8.0`. We also provide per-Haskell-package changelogs. Please see
the `CHANGELOG.md` for each individual package for any changes relevant for developers.

If you are looking for the Ledger Releasing and Versioning Process then you can find it in
[RELEASING.md](https://github.com/intersectmbo/cardano-ledger/blob/master/RELEASING.md#changelogmd).

## 9.3

- Disallow withdrawals to non-delegated keyhashes post-bootstrap [#4555](https://github.com/IntersectMBO/cardano-ledger/pull/4555)
- Remove `maxMajorPV` from `Globals`, because it was unused: [#4218](https://github.com/IntersectMBO/cardano-ledger/pull/4218)

## 9.2

- DRep registration expiration fix: [#4547](https://github.com/IntersectMBO/cardano-ledger/pull/4547)
- Use correct DRep threshold to ratify `NoConfidence`: [4516](https://github.com/IntersectMBO/cardano-ledger/pull/4516)
- Added method to compute over-the-wire CBOR encoded transaction size needed for network layer: [4521](https://github.com/IntersectMBO/cardano-ledger/pull/4521)
- Add governance related ledger state queries: [4514](https://github.com/IntersectMBO/cardano-ledger/pull/4514)
- Improve Plutus debug functionality: [4503](https://github.com/IntersectMBO/cardano-ledger/pull/4503)

## 9.1.1

- Hotfix patch for inability to deserialize Conway version of the Ledger State due to
  retained bogus address pointers:
  [#4591](https://github.com/IntersectMBO/cardano-ledger/pull/4591)

## 9.1

- There were no ledger changes. The only difference with `cardano-node-9.0` is that `9.1`
  provided genesis file for `Conway` era.

## 9.0

- Authorization of hot credentials for constitutional committee members is now only
  possible for cold credentials that are present in the ledger state, either in the current
  committee or in one of the proposals.
- Voting is restricted to entities that are present in the ledger state
- DRep votes are removed whenever DRep unregisters

## 8.12.2

- Pricing model for the size of reference scripts was changed from linear to
  exponential. Moreover, extra limits on the total size of reference scripts being used
  have been put in place.

## 8.12

- Implemented [CIP-0069](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0069)
  that improves `PlutusV3` functionality by making spending datums optional and enforcing
  all scripts to have exactly one argument.
- Ensure that `CommitteeMinSize` protocol parameter is ignored during bootstrap phase.
- Restrict types for some protocol parameters and fixup CDDL specification to reflect
  actual deserializers.
- Create pool stake distribution query for voting.
- Included rewards accounts in stake pool distribution calculation for voting.
- Bug fixes:
  - Fixes to `DRep` expiry logic and the choice of correct threshold for the committee
    voting
  - Script execution for certificates with the same plutus script did not execute correctly.
  - Prevent delegation to a non-existent pool.

## 8.11

- Prepared changes for the bootstrap period by restricting government actions and by ignoring `DRep` thresholds.
- Added the ability to do a `HardForkInitiation` into a new era.
- Added `PlutusV3` cost model to `Conway` genesis.
- Stopped counting expired committee members in active committee size calculation.
- Included rewards accounts in `DRep` distribution calculation.
- Added `AccountState` ledger state query.

- Bug fixes:
  - Stopped embedding `UTXO` and `UTXOS` predicate failures from previous eras.
  - Fixed predicate failure deserialisation bug when re-submitting a `Conway` transaction with an invalid plutus script.

## 8.10

- Retention of Anchor for proposal procedures in the ledger state in order for users to be able to query the node for those anchors
- The minimum fee calculation has been updated to take into consideration a new protocol parameter controlling the contribution of reference scripts sizes
- Redeemers serialization improvement: encode redeemers as a Map, instead of a list for Conway
- Support for intra-era hardfork initiation and necessary updates to the SPO stake distribution calculation.
- Fail translation when Conway features are present in transactions that use Plutus v1/v2
- New Conway predicate failure: `ConwayUtxosPredFailure` (to decouple Conway from `PPUP` dependency)
- Remove `small-steps-test` package and convert it into a sublibrary of `small-steps`
- Support for injecting transition configuration data in `NewEpochState`, to decouple this concern from Consensus
- Query-related changes:
  - Era-specific queries: Conway functionality supported only starting with Conway era
  - Change committee query to return non-optional `CommitteeMembersState`
  - Add optional `Anchor` to resigned status in `CommitteeMembersState`

- Important bug fixes:
  - Use the correct stake pool distribution for voting (instead of one that is an epoch too old)
  - Fix DRep stake distribution to correctly add rewards to deleged stake
  - JSON serialization of rational numbers in protocol parameters and governance procedures are encoded without loss of precision
    (`BoundedRatio` and `Prices`)
  - JSON serialization fix for `ConwayGovState`
  - Fixed the intermittent failure of Github page containing haddock documentation

## 8.8

- Full PlutusV3 support
- Support for governance policy script
- Remove the library `cardano-ledger-pretty`
- Ensure all sets are prefixed with tag 258 in CBOR serialization
- Added DRep injections into the ledger state, thus allowing more complex testing and
  benchmarking of Conway features
- Addition of new ledger events
- Increasing the limit for Url in Anchors to 128 bytes
- Provide accurate transaction fee estimation functionality for Haskell tools
- Important bug fixes:

  - missing `"protocolVersion"` field in JSON instance for Babbage and Conway protocol parameters
  - requiring witnesses for DRep registration
  - new committee governance action ratification was implemented incorrectly
  - guard against invalid protocol versions in hard fork initiation proposals
  - failures during updates of Plutus CostModels were not retained
  - fixed the types for some protocol parameters in order to prevent values that are too large
  - serialization and CDDL fixes for transaction witnesses in the Conway Era

## 8.7

- Fix `PParamsUpdate` governance action ratification. Votes of DReps are now accounted for.
- Move CDDL specification files from test packages into libraries that actually implement each era.
- Add ability to retain Plutus logs for debugging when running scripts
- Adition of `ConwayGovEvent`
- Enforce no duplicates for submitted ProposalProcedures
- Fix deserialization of `CostModels` in the `PParamsUpdate`. Invalid `CostModels` are no longer allowed, only `CostModels` for unrecognized Plutus versions are allowed starting with Conway
- Improve deposit and refund calculation logic
- Fix deserialization of `ValueNotConservedUTxO` predicate failure that could not
  previously report zero ADA.

## 8.6

- Prevent updating protocol version with PParamUpdate
- Slight performance improvements to native script handling:
  - Remove redundant script hash verification.
  - Short circuit multisig and timelock verification for `RequireMOf` as soon as necessary number of scripts have been validated.
- Check that the previous governance action of a specific type is either the very last action that has been enacted or is already in the current proposals set.
- Replace `queryCommitteeState` with more powerful `queryCommitteeMembersState`, which returns more information about committee members and supports filters based on credentials and statuses.
- Repurpose `DRepPulser` to encapsulate the Snapshots needed to run the `EPOCH` and `RATIFY` Rules. This has fixed some problem related to snapshots and behaviour at the epoch boundary.
- Add an optional anchor to committee member resignation certificate
- Prevent delegation to non-registered pools

## 8.5

- Prevent `DRep` expiry when there are no Governance Actions.
- Refreshing of expiry with votes and `UpdateTxCert`
- Disable ability to submit transactions with votes by Stake Pool Operators and
  Constitutional Committee members on Governance Actions that they should not
  be able to vote on.
- Add ability to supply initial Constitutional Committee and initial version of Constitution.
- Preserve order of submitted ProposalProcedures and account for their priority.
- Disallow empty fields in CBOR of Conway TxBody
- Add some sanity checks for `UpdateCommittee` proposals and change semantics of the
  proposal from overwrite to modify the Committee
- Implement Constitutional Committee expiration, validation and modification
- Respect Constitutional Committee members votes and thresholds during ratification
- Whenever Constitutional Committee size goes below `CommitteeMinSize` parameter the whole
  committee will vote `No` on all proposals.
- Restructure initial configuration in such a way that makes it possible for Conway era to
  start without going through all previous eras, which is needed for testing and
  benchmarking.
- Apply ADA treasury transfers after enactment of `TreasuryWithdrawals` proposals
- Enforce `currentTreasuryValue` field in the TxBody matches the actual Treasury amount,
  whenver the field is supplied

## 8.4

- `GovernanceProposals` functionality:
  - Implement expiry
  - Enforce the supplied deposit amount
  - Enforce previous GovActionId is correct
- Enforce deposits and ensure refunds for `DRep`s. This affects transaction building functionality.
- Enforce thresholds for votes from PParams for Stake Pool Operators and DReps
- Delaying of ratification after `NoConfidence`, `HardForkInitiation`, `NewCommittee`,
  `NewConstitution` actions have been enacted.

## 8.3

- `DRep` functionality progress:
  - Implement `DRep` voting and use actual `DRep` stake distribution for ratification
  - Implement `DRep` expiry for stale DRep's
  - Add `DRep` deposit tracking.
  - Add `Anchor`s for `DRep`s
- Fix `ProposalProcedure` deposit tracking.
- Add ability to upgrade core types from one era to the next.
- Add `Script` capability to Constitutional Committee Hot and Cold credentials.
- Add `NetworkId` validation for `ProposalProcedure` and `TreasuryWithdrawals`
- Add `currentTreasuryValue` as a new field to Conway `TxBody`
- Voting thresholds have been added to `PParams`

## 8.2

- Restructure certificate interface
- Conway related changes:
  - Introduction of a `DRep` with the defaults that vote No and Abstain.
  - Change `DCert` type to a `TxCert` type family and introduce new certificates
  - Implement voting on `NewConstitution` by StakePools as the most basic example of voting.
  - Implement query for getting `Constitution` hash from the ledger state.
  - Change structure of Governance Procedures in the TxBody
  - Fix `TICKF` rule, avoiding VRF verification and syncing issues
  - Clear out TxOuts with zero value from the UTxO upon translation into Conway

## 8.1

- Plutus V3 support, only available in the conway ledger era.
- `PState` is now parametric in era, not crypto.
- Adjust for new conway era certificates.
- Ledger `UMapCompact` is now `UMap`
- Ledger types with names involving `DState` are renamed to `CertState`.

## 8.0

- The provenance for the reward calculation has been removed. The type signature to the API function `getRewardProvenance` has not changed, it just returns an empty provenance value.
- We have created a Ledger API, with the aim of providing a user-friendly interface to the ledger libraries. See [here](https://github.com/intersectmbo/cardano-ledger/tree/b00e28698d9c7fbbeda1c9cfdd1238d3bc4569cf/libs/cardano-ledger-api).
- The initial funds and staking in the Shelley genesis type (used only for testing) now use `ListMap` instead of `Map`.
- There is a new `calculatePoolDistr'` function which is similar to `calculatePoolDistr` but has a new filter argument to only include the stake pool ids (stake pool key hashes) that are needed.
- The ledger events are not guaranteed to appear in any given order within a block. For this reason, motivated by the use case in db-sync, the `TotalDeposits` event now includes a transaction ID and emits the change in deposits instead of the value.
- We changed the way deposits are tracked. See [here](https://github.com/intersectmbo/cardano-ledger/blob/b00e28698d9c7fbbeda1c9cfdd1238d3bc4569cf/docs/adr/2022-12-05_003-track-individual-deposits.md) for the details.
- We changed the API function Cardano.Ledger.Shelley.API.Wallet(`evaluateTransactionBalance`) to take `DPState` as input. This can no longer be computed without the `DPState`
- Some noticeable changes have been made which will only become apparent starting at major protocol version 9:
  - There is a new ledger era, namely `conway`.
  - We now have the ability to more easily change serializations when the major protocol version changes. Though this change is not immediately visible to the node, it enables visible changes in the future. See [CIP-80](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0080) For the deprecation cycle.
  - Pointer addresses will no longer accrue rewards starting with version 9.
  - We fixed the incorrect conversion of the validity interval's upper bound in `transVITime`.
  - Starting in version 9, duplicate keys in CBOR maps and sets are no longer allowed.
  - Starting in version 9, `CostModel`s can now be deserialized from any map of Word8 values to lists of integers. Only valid cost models are actually converted to evaluation contexts that can be used. Errors and unrecognized language versions are stored in the `CostModel` type so that:
      - They can accept cost models that they do not yet understand.
      - Upon deserializing after a software update, new cost models are available from the prior serialization.

# Deprecated changlog

Below was the last `cardano-ledger` repository branch based release and we now have fully
switched to [CHaPs](https://github.com/input-output-hk/cardano-haskell-packages).

## Release tag `ledger/1.2.0`

Changes recorded below are for all of the package versions in the list:

  * `small-steps-1.0.0.0`
  * `small-steps-test-1.0.0.0`
  * `vector-map-1.0.0.0`
  * `cardano-data-1.0.0.0`
  * `set-algebra-1.0.0.0`
  * `cardano-ledger-binary-1.0.0.0`
  * `cardano-ledger-core-1.0.0.0`
  * `cardano-protocol-tpraos-1.0.0.0`
  * `cardano-ledger-api-1.0.0.0`
  * `cardano-ledger-pretty-1.0.0.0`
  * `cardano-ledger-shelley-1.0.0.0`
  * `cardano-ledger-allegra-1.0.0.0`
  * `cardano-ledger-mary-1.0.0.0`
  * `cardano-ledger-alonzo-1.0.0.0`
  * `cardano-ledger-babbage-1.0.0.0`
  * `cardano-ledger-conway-1.0.0.0`
  * `cardano-ledger-shelley-ma-1.0.0.0`
  * `cardano-ledger-alonzo-test-1.0.0.0`
  * `cardano-ledger-babbage-test-1.0.0.0`
  * `cardano-ledger-conway-test-1.0.0.0`
  * `cardano-ledger-shelley-ma-test-1.0.0.0`
  * `cardano-ledger-shelley-test-1.0.0.0`

### Added

- Addition of `encodeWithOrigin` and `decodeWithOrigin`. #3297
- Change `mkVersion` to accept any `Integral`. Add `getVersion` function. #3297
- Added type classes: `EraPParams`, `AlonzoEraPParams` and `BabbageEraPParams` with lenses
  that can access and manipulate both `PParams` and `ParamsUpdate`. #3242
- Added types `CoinPerWord` and `CoinPerByte` to prevent mixing up the two: #3242
- Add `fromShelleyPParams` to `CanStartFromGenesis`: #3224
- Add `setMinFeeTx` to  `Cardano.Ledger.Api.Tx`: #3201
- Add ToExpr (from tree-diff) instances for all types inside NewEpochState
  - Add module Cardano.Ledger.TreeDiff in cardano-binary. Includes all needed operations, classes, and orphan instances
- Add deposits (key deposits) to DState{dsDeposits} and (pool deposits) PState{psDeposits}
  - Added property tests that test the new invariants that must hold
    - utxosDeposits == sum (dsDeposited) + sum (psDeposited)
    - dom rewards == dom dsDeposited
- Add functions that computed deposits, obligation, and refunds that take DPState as input
- Add `PlutusDebug` to `TransactionScriptFailure.ValidationFailedV{1|2}`: #3135
  - Also add a harmless dummy `Show` instance for `PlutusDebug` with a constant `"PlutusDebug Omitted"`
- Create a `cardano-ledger-binary` package that does versioned serialization as a
  replacement for `cardano-binary` package: #3063, #3078
- Switch to `cardano-ledger-binary` package throughout the ledger codebase: #3138
- Created `translateEraThroughCBOR`: #3138
- Start on the `cardano-ledger-api` package and implement
  `setMinCoinTxOut`+`setMinCoinSizedTxOut`: #2995
- Added `getMinCoinTxOut`/`getMinCoinSizedTxOut` to `EraTxOut`: #3008
- Added `getMinFeeTx` to `EraTx`
- Added `datumTxOutF` to `AlonzoEraTxOut`
- Added `allInputsTxBodyF`
- Added `EraUTxO` class with `getConsumedValue`
- Added type synonyms for eras: `Shelley`, `Allegra`, `Mary`, `Alonzo`, `Babbage` and `Conway`.
- Added `Twiddle` class to test alternative serializations: #2994
- Added `getScriptsNeeded` and `getScriptsHashesNeeded` to `EraUTxO` class: #3019
- Added `evaluateTransactionExecutionUnitsWithLogs` to `Alonzo`: #3111
- Added `mkAlonzoTxAuxData` and `getAlonzoTxAuxDataScripts` that help to recover previous
  behavior of `AlonzoTxAuxData` #3165 and #3166
- Addition of `Memoized` type class and helper functions that utilize this new
  abstraction: `mkMemoized`, `getMemoSafeHash`, `getMemoRawType`, `zipMemoRawType`,
  `getMemoRawBytes` and `lensMemoRawType` #3165
- Added upper protocol version bounds to `updateTxBodyL` and `certsTxBodyL` and
  removed the corresponding fields from the Conway transaction body #3176
- Added `ConwayEraTxBody` class with new lenses #3176
- Added `ConwayDCert` data type which is similar to `DCert` but does not have
  MIR certificates #3176
- Added CDDL files for Conway era #3176
- Added `Vote`, `VoterRole`, `VoteDecision`, `GovernanceActionInfo` and
  `GovernanceAction` data types #3176
- Added `ctbrVotes` and `ctbrGovActions` fields to `ConwayTxBodyRaw` and
  corresponding fields to `ConwayTxBody` #3176
- Added `ToCBOR` and `FromCBOR` instances to `ConwayTxBody` and all its
  constituent data types #3176
- Added `ConwayUTXOS` rule #3176
- Added `PPUPPredFailurePV` type family and `PPUPPredFailure` type synonym #3216
- Added `EraGovernance` type class with `GovernanceState` type family and implemented it
  for each era #3279
- Added `updateTxBodyG` getter to EraTxBody #3216
- Added new rules to the Conway era: #3216
  - `TALLY` (new)
  - `ENACTMENT` (new)
  - `NEWEPOCH`
  - `EPOCH`
  - `LEDGER`
- Added `ConwayTallyState` #3216
- Added `ConwayEpochEvent` #3216
- Added `ConwayLedgerPredFailure` #3216
- Added `ConwayNewEpochPredFailure` #3216
- Added `ConwayNewEpochEvent` #3216
- Added new governance related data types to Conway: #3216
  - `GovernanceActionState`
  - `GovernanceProcedure`
  - `TallyEnv`

### Changed

- Renamed `GenesisDelegCert` to `ConstitutionalDelegCert`: #3176
- Fix `ToCBOR`/`FromCBOR` insatance for `PParams Babbage` and `PParams Alonzo`: #3297 and #3288
- Disallow decoding a 0-value `MultiAsset` in the de-serialization. #3241
- Move `Wdrl` to `Core`. Also rename it to `Withdrawals`, while switching `Test.Cardano.Ledger.Generic.Fields.Withdrawals` to `Withdrawals'` #3239
- Moved `PreviousEra` into `Era` type class. Added `Era` instance for a new data type
  `ByronEra`.
- Renamed record fields for all `PParams` types. Too many to list, but follows the same
  convention, eg. `AlonzoPParams._maxTxSize` -> `AlonzoPParams.appMaxTxSize`
- Renamed `AlonzoGenesis` fields #3242:
  - `coinsPerUTxOWord` -> `agCoinsPerUTxOWord`
  - `costmdls` -> `agCostModels`
  - `prices` -> `agPrices`
  - `maxTxExUnits` -> `agMaxTxExUnits`
  - `maxBlockExUnits` -> `agMaxBlockExUnits`
  - `maxValSize` -> `agMaxValSize`
  - `collateralPercentage` -> `agCollateralPercentage`
  - `maxCollateralInputs` -> `agMaxCollateralInputs`
- Split `Cardano.Ledger.Alonzo.Data` module into `Cardano.Ledger.Alonzo.Scripts.Data` and `Cardano.Ledeger.Alonzo.TxAuxData`: #3229
- Changed `ConwayGenesis`, by adding `AlonzoGenesis` to it: #3224
- Switch parameterization of `ShelleyGenesis` on crypto, i.e. `ShelleyGenesis era` -> `ShelleyGenesis c`: #3224
- Moved `KeyPair` type and related functions to a new `KeyPair` module in `cardano-ledger-core:testlib` #3210
- Changed the representaion of  key deposits. Deleted the field dsDeposits. Moved it to the UMap. Added some new functions to UMapCompact module to accomodate changes.
- Replace `NominalDiffTime` with a `newtype` wrapper `NominalDiffTimeMicro`. Remove use of `NominalDiffTime`, as we don't use its full precision. #3208
- Switched `PlutusDebug` to use a `GADT` with `singletons` for better type-safety. #3167
  - Made `Plutus` imports uniform.
- Removed the module `Test.Cardano.Ledger.Generic.Types`. Functionality moved to `Test.Cardano.Ledger.Generic.Functions`
- Renamed module `Cardano.Ledger.Shelley.Metadata` -> `Cardano.Ledger.Shelley.TxAuxData` #3205
- Updated `Conway` low protocol version to 9 and `Babbage` high protocol version to 8: #3174
- Fixed mismathed parenthesis in the `Show` instance for `Ptr`: #3184.
- Moved Cardano.Ledger.Shelley.LedgerState(DPState) to Cardano.Ledger(DPState) in Core
  - Since the old file was hidden, this will have no noticeable effects.
- Changed the API function Cardano.Ledger.Shelley.API.Wallet(evaluateTransactionBalance) to take DPState as input. This can no lnger be computed without the DPState
- Changed UtxoEnv by replacing pool info (Map (KeyHash 'StakePool c) (PoolParams c)) with DPState
    to show differences in two NewEpochState's.
- Split `cardano-ledger-shelley-ma` into `cardano-ledger-allegra` and `cardano-ledger-mary` #3175:
  - Moved `ShelleyMA.AuxiliaryData` -> `Allegra.TxAuxData`
  - Moved `ShelleyMA.Timelocks` -> `Allegra.Scripts`
  - `ShelleyMA.TxBody.MATxBody` was split into `Allegra.AllegraTxBody` and `Mary.MaryTxBody`. pattern record fields were renamed correspondingly
    - `inputs` to `atbInputs` and `mtbInputs`
    - `outputs` to `atbOutputs` and `mtbOutputs`
    - `certs` to `atbCerts` and `mtbCerts`
    - `wdrls` to `atbWdrls` and `mtbWdrls`
    - `txfee` to `atbTxFee` and `mtbTxFee`
    - `vldt` to `atbValidityInterval` and `mtbValidityInterval`
    - `update` to `atbUpdate` and `mtbUpdate`
    - `adHash` to `atbAuxDataHash` and `mtbAuxDataHash`
    - `mint` to `mtbMint`
- Changed structure and field names of `Cardano.Ledger.Alonzo.Data.AlonzoTxAuxData` #3165:
  - Renamed `AlonzoTxAuxData.txMD` to `AlonzoTxAuxData.atadMetadata`:
  - Removed `AlonzoTxAuxData.scripts` in favor of two new fields `atadTimelock` and
    `atadPlutus`. This was needed due to #3166
- Changed instance for `TranslationContext (ShelleyEra c)` to a data type that can be used to
  translate from Byron to Shelley: #3164
- Changed major version in `ProtVer` to use new type `Version` instead of `Natural`: #3138
- Renamed records fields in `Cardano.Ledger` to names without `_` (underscores) #3126
  - `Alonzo.TxBody.AlonzoTxBody` pattern synonym
    - `inputs` to `atbInputs`
    - `collateral` to `atbCollateral`
    - `outputs` to `atbOutputs`
    - `txcerts` to `atbCerts`
    - `txwdrls` to `atbWdrls`
    - `txfee` to `atbTxFee`
    - `txvldt` to `atbValidityInterval`
    - `txUpdates` to `atbUpdate`
    - `reqSignerHashes` to `atbReqSignerHashes`
    - `mint` to `atbMint`
    - `scriptIntegrityHash` to `atbScriptIntegrityHash`
    - `adHash` to `atbAuxDataHash`
    - `txnetworkid` to `atbTxNetworkId`
  - `Babbage.TxBody.BabbageTxBody` pattern synonym
    - `inputs` to `btbInputs`
    - `collateral` to `btbCollateral`
    - `referenceInputs` to `btbReferenceInputs`
    - `outputs` to `btbOutputs`
    - `collateralReturn` to `btbCollateralReturn`
    - `totalCollateral` to `btbTotalCollateral`
    - `txcerts` to `btbCerts`
    - `txwdrls` to `btbWdrls`
    - `txfee` to `btbTxFee`
    - `txvldt` to `btbValidityInterval`
    - `txUpdates` to `btbUpdate`
    - `reqSignerHashes` to `btbReqSignerHashes`
    - `mint` to `btbMint`
    - `scriptIntegrityHash` to `btbScriptIntegrityHash`
    - `adHash` to `btbAuxDataHash`
    - `txnetworkid` to `btbrTxNetworkId`
  - `Shelley.TxBody.TxBodyRaw` to `Shelley.TxBody.ShelleyTxBodyRaw`
    - `inputsX` to `stbrInputs`
    - `outputsX` to `stbrOutputs`
    - `certsX` to `stbrCerts`
    - `wdrlsX` to `stbrWdrls`
    - `txfeeX` to `stbrTxFee`
    - `ttlX` to `stbrTTL`
    - `txUpdateX` to `stbrUpdate`
    - `mdHashX` to `stbrMDHash`
  - `Shelley.TxBody.ShelleyTxBody` pattern synonym
    - `_inputs` to `stbInputs`
    - `_outputs` to `stbOutputs`
    - `_certs` to `stbCerts`
    - `_wdrls` to `stbWdrls`
    - `_txfee` to `stbTxFee`
    - `_ttl` to `stbTTL`
    - `_txUpdate` to `stbUpdate`
    - `_mdHash` to `stbMDHash`
 - Renamed records fields in `Cardano.Ledger` to names without `_` (underscores) #3118
  - `Shelley.LedgerState.Types.AccountState`
    - `_treasury -> asTreasury`
    - `_reserves -> asReserves`
  - `PoolParams.PoolParams`
    - `_poolId -> ppId`
    - `_poolVrf -> ppVrf`
    - `_poolPledge -> ppPledge`
    - `_poolCost -> ppCost`
    - `_poolMargin -> ppMargin`
    - `_poolRAcnt -> ppRewardAcnt`
    - `_poolOwners -> ppOwners`
    - `_poolRelays -> ppRelays`
    - `_poolMD -> ppMetadata`
  - `PoolParams.PoolMetadata`
    - `_poolMDUrl -> pmUrl`
    - `_poolMDHash -> pmHash`
- Renamed records fields in `Cardano.Ledger` to names without `_` (underscores) #3120
  - `Shelley.Delegation.Certificates`
    - `_delegator` to `dDelegator`
    - `_delegatee` to `dDelegatee`
  - `EpochBoundary.SnapShot`
    - `_stake` to `ssStake`
    - `_delegations` to `ssDelegations`
    - `_poolParams` to `ssPoolParams`
  - `EpochBoundary.SnapShots`
    - `_pstakeMark` to `ssStakeMark`
    - `_pstakeSet` to `ssStakeSet`
    - `_pstakeGo` to `ssStakeGo`
    - `_feeSS` to `ssFee`
- Renamed records fields in `Cardano.Ledger.Shelley.LedgerState.DPState` to names without `_` (underscores) #3116
  - `DState`
    - `_unified -> dsUnified`
    - `_fGenDelegs -> dsFutureGenDelegs`
    - `_genDelegs -> dsGenDelegs`
    - `_irwd -> dsIRewards`
  - `PState`
    - `_pParams -> psStakePoolParams`
    - `_fPParams -> psFutureStakePoolParams`
    - `_retiring -> psRetiring`
- Renamed `Cardano.Ledger.Shelley.LedgerState.Types.UTxOState` fields to names without `_` (underscores) #3108
  - `_utxo -> utxosUtxo`
  - `_deposited -> utxosDeposited`
  - `_fees -> utxosFees`
  - `_ppups -> utxosPpups`
  - `_stakeDistro -> utxosStakeDistr` (notice the lacking `o` at the end)
- Moved Cardano.Ledger.Shelley.Orphans to Cardano.Ledger.Orphans  in the core modules
- Moved Cardano.Ledger.Shelley.PoolParms to Cardano.Ledger.PoolParams  in the core modules
- Moved Cardano.Ledger.Shelley.EpochBoundary to Cardano.Ledger.EpochBoundary  in the core modules
- Added deprecated modules that import the moved ones and export their contents
- Moved `Data.AbstractSize` from `cardano-data` to `byron-spec-ledger` #3046
- Renamed in `Cardano.Ledger.Mary.Value`: #3047
  - `insert` to `insertMultiAsset`
  - `lookup` to `lookupMultiAsset`
- Changed `mint` field type to `MultiAsset (Crypto era)` in `MATxBody`, `AlonzoTxBody`, `BabbageTxBody`
  #2954
- All Shelley rules are now available through `Cadano.Ledger.Shelley.Rules` module: #2996
- Renamed the `Crypto` dependent type in the `Era` class to `EraCrypto` #3009
- Renamed in `Cardano.Ledger.Core` #2976:
  - `EraWitnesses` to `EraTxWits`
  - `Witnesses` to `EraTxWits`
  - `mkBasicWitnesses` to `mkBasicTxWits`
  - `addrWitsL` to `addrTxWitsL`
  - `bootAddrWitsL` to `bootAddrTxWitsL`
  - `scriptWitsL` to `scriptTxWitsL`

- Renamed in (new) module `Cardano.Ledger.Alonzo.TxWits` (renamed from `Cardano.Ledger.Alonzo.TxWitness`) #2976:
  - `AlonzoEraWitnesses` to `AlonzoEraTxWits`
  - `TxWitness` to `AlonzoTxWits`
  - `addrAlonzoWitsL` to `addrAlonzoTxWitsL`
  - `bootAddrAlonzoWitsL` to `bootAddrAlonzoTxWitsL`
  - `scriptAlonzoWitsL` to `scriptAlonzoTxWitsL`
  - `datsWitsL` to `datsTxWitsL`
  - `datsAlonzoWitsL` to `datsAlonzoTxWitsL`
  - `rdmrsWitsL` to `rdmrsTxWitsL`
  - `rdmrsAlonzoWitsL` to `rdmrsAlonzoTxWitsL`

- Renamed in (new) module `Cardano.Ledger.Shelley.TxWits` (extracted from `Cardano.Ledger.Shelley.Tx`) #2976:
  - `ShelleyWitnesses` to `ShelleyTxWits`
  - `addrShelleyWitsL` to `addrShelleyTxWitsL`
  - `bootAddrShelleyWitsL` to `bootAddrShelleyTxWitsL`
  - `scriptShelleyWitsL` to `scriptShelleyTxWitsL`
- Updated package metadata #3023
- Moved `TxOut` to a separate module in each era #3024
- Moved `mintedTxBodyF` into `ShelleyMAEraTxBody` class #3019
- Moved thet `RewardType` and `Reward` types from the `Cardano.Ledger.Shelley.Reward` module in the
  `cardano-ledger-shelley` package into a new module `Cardano.Ledger.Reward`
  inside the `cardano-ledger-core` package. #3059
- Added the tx hash to the `TotalDeposits` event and replaced the deposits by the deposits change #3212
- Renamed the old `PPUPState` datatype to `ShelleyPPUPState` #3216
- Removed some references to specific rules in constraints: #3216
  - Replaced `State (EraRule "PPUP" era)` with `PPUPState era`
  - Replaced `PredicateFailure (EraRule "PPUP" era)` with `PPUPPredFailure era`
  - Replaced `PredicateFailure (EraRule "UPEC" era)` with `UpecPredFailure era`
- Changed the type of the first field of `ShelleyBbodyState` to  #3216
  `State (EraRule "LEDGERS" era)`
- Starting in version 9, CostModels can now be deserialized from any map of Word8 values to lists of integers.
  Only valid cost models are actually converted to evaluation contexts that can be used.
  Errors and unrecognized language versions are stored in the CostModels type so that:
    - they can accept cost models that they do not yet understand
    - upon deserializing after a software update, new cost models are available from the prior serialization.
  #3283

### Removed

- Removed `extendPP` and `retractPP` in favor of `EraPParams` type class functions
  `upgradePParams`/`upgradePParamsUpdate` and `downgradePParams`/`downgradePParamsUpdate`
  which can be used to change `PParams` from and to a previous era: #3224
- Removed deprecated synonyms: `PParams`, `PParams'`, `PParamsUpdate`, `PParamsDelta`,
  `Value`, `TxBody`, `TxOut`
- Removed `Cardano.Ledger.Babbage.Genesis` module, since now it is completely empty: #3224
- `TranslateEra` instance for `ShelleyGenesis`: #3224
- Deprecated `Cardano.Ledger.CompactAddress` module in favor of `Cardano.Ledger.Address`: #3218
- Removed deprecated `Cardano.Ledger.Shelley.CompactAddr`: #3218
- Removed depracted type synonyms: `ValidatedTx`, `TxOut`, `TxBody`, `TxSeq`, `Script`,
  `Value`, `PParamDelta`. #3205
- Removed unused `deserialiseAddrStakeRef` function from `Address` module:  #3174
- `MAClass` is gone: #3175
- `ShelleyMAEra` type in favor of `AllegraEra` and `MaryEra`: #3175
- `MATxBody` type in favor of `AllegraTxBody` and `MaryTxBody`: #3175
- Deprecated `Cardano.Ledger.Serialization` in favor of `Cardano.Ledger.Binary` from
  `cardano-ledger-binary`: #3138
- Removed `Data.Coders` from `cardano-data` in favor of `Cardano.Ledger.Binary.Coders` from
  `cardano-ledger-binary`: #3138
- Removed `Data.Sharing` from `cardano-data` in favor of `Cardano.Ledger.Binary` from
  `cardano-ledger-binary`: #3138
- Removed `boundedRationalFromCBOR`/`boundedRationalToCBOR` as obsolete and invalid: #3138
- Removed pattern synonym `Cardano.Ledger.ShelleyMA.TxBody.MATxBody.TxBody'` with fields #3126
  - `adHash'`
  - `certs'`
  - `inputs'`
  - `mint'`
  - `outputs'`
  - `txfee'`
  - `update'`
  - `vldt'`
  - `wdrls'`
- Deprecated the `validPlutusdata` function: #3006
- Deprecated the misspelled `HasAlgorithm` type alias: #3007
- Deprecated `CLI.evaluateMinLovelaceOutput` in favor of newly added
  `EraTxOut.getMinCoinTxOut`/`EraTxOut.getMinCoinSizedTxOut` #3008
- Deprecated `minfee` and `CLI.evaluateMinfee` in favor of new `EraTx.getMinFeeTx`
- Deprecated `ExtendedUTxO.getTxOutDatum` in favor of new `AlonzoEraTxOut.datumTxOutF`
- Removed `ExtendedUTxO.allOuts` and `ExtendedUTxO.allSizedOuts` in favor of
  `BabbageEraTxBody.allInputsTxBodyF`
- Deprecated `consumed` and `evaluateConsumed` in favor of new `EraUTxO.getConsumedValue`
- Removed `CLI` class
- Deprecated `scriptsNeededFromBody` and `scriptsNeeded` in all eras in favor of new class
  function `EraUTxO.getScriptsNeeded` #3019
- Remove model test framework #3019
- The `Cardano.Ledger.Alonzo.Scripts` module no longer re-exports the
  `plutus-ledger-api`'s `assertWellFormedCostModelParams`. #3065
- Removed unused `Data.BiMap` module from `cardano-data` #3089
- Removed `getMultiSigBytes` as unused #3138
- Removed `hashCostModel` as unused and invalid #3138
- Removed `EncodeMint`/`DecodeMint` classes in favor of regular `ToCBOR`/`FromCBOR` #3172

### Fixed

- Fixed typo in makeHashWithExplicitProxys phantom type (indexl to index). #3072
- Fixed the incorrect conversion of the validity interval's upper bound in `transVITime` (fixes #3043). #3200
- Enforce that the CostModel deserializers expect a specific length prior to version 9.
- Starting in version 9, duplicate keys in CBOR maps are not longer allowed

## Release tag `ledger/1.1.0`

### Added
- New `calculatePoolDistr'` function which is similar to `calculatePoolDistr` but has a new
  filter argument to only include the stake pool ids (stake pool key hashes) that are needed.
  #2957
- New package `cardano-ledger-conway`, which defines new era: the `ConwayEra`
- Added `coinsPerUTxOByteToCoinsPerUTxOWord` helper function for Babbage
  #2896
- Core type classes: `EraSegWits`, `EraTx`, `EraTxBody`, `EraTxOut`, `EraPParams`,
  `EraAuxiliaryData`, `EraTxWits`, `EraScript`
- Era specific type classes: `ShelleyEraTxBody`, `ShelleyMAEraTxBody`,
  `AlonzoEraTxBody`, `AlonzoEraTxOut`, `AlonzoEraTx`, `BabbageEraTxBody`, `BabbageEraTxOut`
- Type class hierarchy:
```
EraSegWits --> EraTx --> EraTxBody --> EraTxOut --> Era
                     \             `--> EraPParams --> Era
                      `--> EraTxWits --> EraScript --> Era
                       `--> EraAuxiliaryData --> Era
```
- Shelley:
```
ShelleyEraTxBody --> EraTxBody --> EraTxOut --> Era
```
- ShelleyMA:
```
ShelleyMAEraTxBody --> ShelleyEraTxBody --> EraTxBody --> EraTxOut --> Era
```
- Alonzo:
```
AlonzoEraTx --> EraTx --> ...
           `--> AlonzoEraTxBody --> ShelleyMAEraTxBody --> ShelleyEraTxBody --> EraTxBody --> ...
                                `--> AlonzoEraTxOut -> ShelleyEraTxOut --> EraTxOut --> ...
```
- Babbage:
```
BabbageEraTxBody --> AlonzoEraTxBody --> ....
                `--> BabbageEraTxOut -> AlonzoEraTxOut -->
```
### Changed
- Introduced a new switch in `HardForks` that turns off pointer address resolution in `IncrementalStake` starting with version 9
- Some types have been moved:
  - The `WitVKey` type has been moved into its own module in core.
  - The `HKD` type has been moved to `cardano-ledger-core`.
  - The `PoolParams` type has been moved into its own module
  - The `DCert` type and related functionality from `TxBody` to `Cardano.Ledger.Shelley.Delegation.Certificates`.
  #2880
- The initial funds and staking in the Shelley genesis type (used only for testing) now use `ListMap` instead of `Map`.
  #2871, #2890, #2892, #2895
- Renamed `SupportsSegWit` to `EraSegWits`
- Split `ValidateScript` into `EraScript` and `EraTx.validateScript`
- Renamed `ValidateAuxiliaryData` to `EraAuxiliaryData` while removing usage of FunDeps.
- Renamed in `Cardano.Ledger.Shelley`:
  - `Tx` to `ShelleyTx` (kept type synonym with a deprecation message)
  - `TxOut` to `ShelleyTxOut` (kept type synonym with a deprecation message)
  - `TxBody` to `ShelleyTxBody` (kept type synonym with a deprecation message)
  - `PParams` to `ShelleyPParams` (kept type synonym with a deprecation message)
  - `PParamsUpdate` to `ShelleyPParamsUpdate` (kept type synonym with a deprecation message)
  - `AuxiliaryData` to `ShelleyAuxiliaryData` (kept type synonym with a deprecation message)
- Renamed in `Cardano.Ledger.Mary`:
  - Renamed `Value` to `MaryValue` (kept type synonym with a deprecation message)
- Renamed in `Cardano.Ledger.ShelleyMA`:
  - `TxBody` to `MATxBody` (kept type synonym with a deprecation message)
  - `AuxiliaryData` to `ShelleyAuxiliaryData` (kept type synonym with a deprecation message)
- Renamed in `Cardano.Ledger.Alonzo`:
  - `ValidatedTx` to `AlonzoTx` (kept type synonym with a deprecation message)
  - `TxOut` to `AlonzoTxOut` (kept type synonym with a deprecation message)
  - `TxBody` to `AlonzoTxBody` (kept type synonym with a deprecation message)
  - `Script` to `AlonzoScript` (kept type synonym with a deprecation message)
  - `PParams` to `AlonzoPParams` (kept type synonym with a deprecation message)
  - `PParamsUpdate` to `AlonzoPParamsUpdate` (kept type synonym with a deprecation message)
  - `AuxiliaryData` to `AlonzoAuxiliaryData` (kept type synonym with a deprecation message)
- Renamed in `Cardano.Ledger.Babbage`:
  - `TxOut` to `BabbageTxOut` (kept type synonym with a deprecation message)
  - `TxBody` to `BabbageTxBody` (kept type synonym with a deprecation message)
  - `PParams` to `BabbagePParams` (kept type synonym with a deprecation message)
  - `PParamsUpdate` to `BabbagePParamsUpdate` (kept type synonym with a deprecation message)
- Renamed Rules:
  - `BBODY` -> `ShelleyBBODY`
  - `DELEG` -> `ShelleyDELEG`
  - `DELEGS` -> `ShelleyDELEGS`
  - `DELPL` -> `ShelleyDELPL`
  - `EPOCH` -> `ShelleyEPOCH`
  - `LEDGER` -> `ShelleyLEDGER`
  - `LEDGERS` -> `ShelleyLEDGERS`
  - `MIR` -> `ShelleyMIR`
  - `NEWEPOCH` -> `ShelleyNEWEPOCH`
  - `NEWPP` -> `ShelleyNEWPP`
  - `POOL` -> `ShelleyPOOL`
  - `POOLREAP` -> `ShelleyPOOLREAP`
  - `PPUP` -> `ShelleyPPUP`
  - `RUPD` -> `ShelleyRUPD`
  - `SNAP` -> `ShelleySNAP`
  - `TICK` -> `ShelleyTICK, ShelleyTICKF`
  - `UPEC` -> `ShelleyUPEC`
  - `UTXO` -> `ShelleyUTXO`
  - `UTXOW` -> `ShelleyUTXOW`
- Renamed rules environments:
  - `PPUPEnv -> PpupEnv`
- Renamed rules events:
  - `BbodyEvent` -> `ShelleyBbodyEvent`
  - `DelegEvent` -> `ShelleyDelegEvent`
  - `DelegsEvent` -> `ShelleyDelegsEvent`
  - `DelplEvent` -> `ShelleyDelplEvent`
  - `EpochEvent` -> `ShelleyEpochEvent`
  - `LedgerEvent` -> `ShelleyLedgerEvent`
  - `LedgersEvent` -> `ShelleyLedgersEvent`
  - `MirEvent` -> `ShelleyMirEvent`
  - `NewEpochEvent` -> `ShelleyNewEpochEvent`
  - `PoolreapEvent` -> `ShelleyPoolreapEvent`
  - `TickEvent` -> `ShelleyTickEvent`
  - `TickfEvent` -> `ShelleyTickfEvent`
  - `UtxowEvent` -> `ShelleyUtxowEvent`
- Renamed predicate failure type names:
  - `Cardano.Ledger.Shelley.Rules`:
    - `BbodyPredicateFailure` -> `ShelleyBbodyPredFailure`
    - `DelegPredicateFailure` -> `ShelleyDelegPredFailure`
    - `DelegsPredicateFailure` -> `ShelleyDelegsPredFailure`
    - `DelplPredicateFailure` -> `ShelleyDelplPredFailure`
    - `EpochPredicateFailure` -> `ShelleyEpochPredFailure`
    - `LedgerPredicateFailure` -> `ShelleyLedgerPredFailure`
    - `LedgersPredicateFailure` -> `ShelleyLedgersPredFailure`
    - `MirPredicateFailure` -> `ShelleyMirPredFailure`
    - `NewEpochPredicateFailure` -> `ShelleyNewEpochPredFailure`
    - `NewppPredicateFailure` -> `ShelleyNewppPredFailure`
    - `LedgerPredicateFailure` -> `ShelleyLedgerPredFailure`
    - `PoolPredicateFailure` -> `ShelleyPoolPredFailure`
    - `PoolreapPredicateFailure` -> `ShelleyPoolreapPredFailure`
    - `PpupPredicateFailure` -> `ShelleyPpupPredFailure`
    - `RupdPredicateFailure` -> `ShelleyRupdPredFailure`
    - `SnapPredicateFailure` -> `ShelleySnapPredFailure`
    - `TickPredicateFailure` -> `ShelleyTickPredFailure`
    - `TickfPredicateFailure` -> `ShelleyTickfPredFailure`
    - `UpecPredicateFailure` -> `ShelleyUpecPredFailure`
    - `UtxoPredicateFailure` -> `ShelleyUtxoPredFailure`
    - `UtxowPredicateFailure` -> `ShelleyUtxowPredFailure`
  - `Cardano.Ledger.ShelleyMA.Rules`:
    - `UtxoPredicateFailure` -> `ShelleyMAUtxoPredFailure`
  - `Cardano.Ledger.Alonzo.Rules`:
    - `AlonzoBbodyPredFail` -> `AlonzoBbodyPredFailure` and constructor:
      - `ShelleyInAlonzoPredFail` -> `ShelleyInAlonzoBbodyPredFailure`
    - `UtxoPredicateFailure` -> `AlonzoUtxoPredFailure`
    - `UtxosPredicateFailure` -> `AlonzoUtxosPredFailure`
    - `UtxowPredicateFail` -> `AlonzoUtxowPredFailure` and constructor:
      - `WrappedShelleyEraFailure` -> `ShelleyInAlonzoUtxowPredFailure`
  - `Cardano.Ledger.Babbage.Rules`:
    - `BabbageUtxoPred` -> `BabbageUtxoPredFailure` and constructor:
      - `FromAlonzoUtxoFail` -> `AlonzoInBabbageUtxoPredFailure`
    - `BabbageUtxowPred` -> `BabbageUtxowPredFailure` and constructor:
      - `FromAlonzoUtxowFail` -> `AlonzoInBabbageUtxowPredFailure`
### Deprecated
- The provenance for the reward calculation has been removed.
  The type signature to the API function `getRewardProvenance` has not change,
  it just returns an empty provenance value.
  The type signature will be changed once the corresponding query has been
  deprecated in the ouroboros-network repository.
  #2879
- `getTxOutAddr txOut` in favor of `txOut ^. addrTxOutL`
- `getTxOutEitherAddr txOut` in favor of `txOut ^. addrEitherTxOutL`
- `getTxOutCompactAddr txOut` in favor of `txOut ^. compactAddrTxOutL`
- `getTxOutBootstrapAddress txOut` in favor of `txOut ^. bootAddrTxOutF`
- `getAllInputs txBody` in favor of ` txBody ^. allInputsTxBodyF`
- `getCoin txOut` in favor of `txOut ^. coinTxOutL`
### Removed
- The `StakeCreds` type was unused and is now removed.
  #2880
- The`Ord` instance for `MemoBytes` was removed.
  #2881
- `makeTxOut` in favor of `mkBasicTxOut`
- `HasField` instances for: `"inputs"`, `"outputs"`, `"txfee"`,
  `"auxiliaryData"`, `"minted"`, `"wdrls"`, `"ttl"`, `"update"`, `"certs"`,
  `"vldt"`, `"mint"`, `"collateral"`, `"reqSignerHashes"`,
  `"scriptIntegrityHash"`, `"txnetworkid"`, `"sizedOutputs"`,
  `"referenceInputs"`, `"totalCollateral"`, `"collateralReturn"`,
  `"sizedCollateralReturn"`, `"body"`, `"wits"`, `"auxData"`, `"size"`,
  `"isValid"`, `"addrWits"`, `"scriptWits"`, `"bootWits"`, `"txdatahash"`,
  `"addr"`, `"bootAddr"`, `"script"`, `"dats"`, `"rdmrs"`
- `ValidateScript` in favor of `EraScript` and `EraTx`
- Type class synonyms:
  - `Trans*`
  - `Uses*`: `UsesPParams`, `UsesScript`, `UsesTxBody`, `UsesTxOut`, `UsesAuxiliaryData`
  - `BlockAnn`
  - `ChainData`
  - `AnnotatedData`
  - `SerialisableData`
  - `WellFormed`
  - `ConcreteAlonzo`
  - `ConcreteBabbage`
  - ...

### Fixed

- The error message for failed Plutus V2 scripts was being obscured by a bug which has now been fixed.
  #2888

## Release tag `ledger/1.0.0`

The first release branch in the cardano-ledger repository,
namely `release/1.0.0`, branches from the
cardano-ledger commit used for the 1.35.0 release of cardano-node release.

### Fixed

- Ensure Babbage TxOut decoder can't fail due to malformed Ptr.
  This bug was manifesting in the cardano-node as:
  if a node is running in the Babbage era and shuts down, when it comes back up it has to re-sync from genesis.
  #2897
- The Alonzo UTxO rule was previously using the incorrect minfee function (from Shelley).
  It now uses the Alonze minfee function.
  #2936
