# Changelog
All notable changes to this repository will be documented in this file.
At at later date we may adopt per-Haskell-package changelogs.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
in the naming of release branches.


## [Unreleased]

### Added

- Start on the `cardano-ledger-binary` package as a replacement for `cardano-binary` package: #3036
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

### Changed

- Moved `Data.AbstractSize` from `cardano-data` to `byron-spec-ledger` #3046
- Renamed in `Cardano.Ledger.Mary.Value`: #3047
  - `insert` to `insertMultiAsset`
  - `lookup` to `lookupMultiAsset`
- Changed `mint` field type to `MultiAsset (Crypto era)` in `MATxBody`, `AlonzoTxBody`, `BabbageTxBody`
  #2954
- All Shelley rules are now available through `Cadano.Ledger.Shelley.Rules` module: #2996
- Renamed the `Crypto` dependent type in the `Era` class to `EraCrypto` #3009
- Renamed in `Cardano.Ledger.Core`:
  - `EraWitnesses` to `EraTxWits`
  - `Witnesses` to `EraTxWits`
  - `mkBasicWitnesses` to `mkBasicTxWits`
  - `addrWitsL` to `addrTxWitsL`
  - `bootAddrWitsL` to `bootAddrTxWitsL`
  - `scriptWitsL` to `scriptTxWitsL`

- Renamed in (new) module `Cardano.Ledger.Alonzo.TxWits` (renamed from `Cardano.Ledger.Alonzo.TxWitness`):
  - `AlonzoEraWitnesses` to `AlonzoEraTxWits`
  - `TxWitness` to `AlonzoTxWits`
  - `addrAlonzoWitsL` to `addrAlonzoTxWitsL`
  - `bootAddrAlonzoWitsL` to `bootAddrAlonzoTxWitsL`
  - `scriptAlonzoWitsL` to `scriptAlonzoTxWitsL`
  - `datsWitsL` to `datsTxWitsL`
  - `datsAlonzoWitsL` to `datsAlonzoTxWitsL`
  - `rdmrsWitsL` to `rdmrsTxWitsL`
  - `rdmrsAlonzoWitsL` to `rdmrsAlonzoTxWitsL`

- Renamed in (new) module `Cardano.Ledger.Shelley.TxWits` (extracted from `Cardano.Ledger.Shelley.Tx`):
  - `ShelleyWitnesses` to `ShelleyTxWits`
  - `addrShelleyWitsL` to `addrShelleyTxWitsL`
  - `bootAddrShelleyWitsL` to `bootAddrShelleyTxWitsL`
  - `scriptShelleyWitsL` to `scriptShelleyTxWitsL`
- Updated package metadata #3023
#2976
- Moved `TxOut` to a separate module in each era #3024
- Moved `mintedTxBodyF` into `ShelleyMAEraTxBody` class #3019
- Moved thet `RewardType` and `Reward` types from the `Cardano.Ledger.Shelley.Reward` module in the
  `cardano-ledger-shelley` package into a new module `Cardano.Ledger.Reward`
  inside the `cardano-ledger-core` package. #3059

### Removed

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

### Fixed

- Fixed typo in makeHashWithExplicitProxys phantom type (indexl to index). #XXXX

## Release branch 1.1.x

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

## Release branch 1.0.x

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
