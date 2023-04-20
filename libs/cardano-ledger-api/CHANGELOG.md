# Version history for `cardano-ledger-api`

## 1.2.0.0

* Add support for Plutus V3

## 1.1.0.0

* Add `Cardano.Ledger.Api.Governance` that exposes all of the governance related types.
* Addition of `Cardano.Ledger.Api.PParams`
* Addition of `Cardano.Ledger.Api.Tx.Address`
* Addition of `Cardano.Ledger.Api.Tx.In`
* Hide `ShelleyTxBody`, `AlonzoTxBody`, `BabbageTxBody` from `Cardano.Ledger.Api.Tx.Body`
* Export `Withdrawals` from `Cardano.Ledger.Api.Tx.Body`
* Export `ShelleyTxAuxData`, `AllegraTxAuxData` and `AlonzoTxAuxData` from
  `Cardano.Ledger.Api.Tx.AuxData`
* Export `LangDepView` and `getLanguageView` from `Cardano.Ledger.Api.PParams`
* Hide the internal compact versions from `Cardano.Ledger.Api.Tx.Out` module:
  `compactValueTxOutL`, `valueEitherTxOutL`, `compactAddrTxOutL` and `addrEitherTxOutL`
* Export from `Cardano.Ledger.Api.Tx.Out`: `coinTxOutL`, `bootAddrTxOutF`,
  `ValidityInterval` and `DataHash`.
* Export from `Cardano.Ledger.Api.Scripts.Data`: `BinaryData`, `DataHash`, `Datum`,
  `binaryDataToData`, `dataToBinaryData`, `datumDataHash`, `getPlutusData`,
  `hashBinaryData` and `makeBinaryData`.
* Export `Tx` and `IsValid` from `Cardano.Ledger.Api.Tx`
* Export from `Cardano.Ledger.Api.Tx.AuxData`: `mkAlonzoTxAuxData` and `getAlonzoTxAuxDataScripts`
* Export from `Cardano.Ledger.Api.Tx.Wits`: `WitVKey`, `BootstrapWitness`,
  `AlonzoEraTxWits`, `TxDats`, `unTxDats`, `Redeemers`, `unRedeemers`, `RdmrPtr` and
  `Tag`.
* Export from `Cardano.Ledger.Api.Scripts`: `CostModels` and `ValidityInterval`.
* Export from `Cardano.Ledger.Api.Era`:
  * `Era`
  * `ByronEra`
  * Protocol version related functionality: `eraProtVerHigh`, `eraProtVerLow`,
    `AtLeastEra`, `AtMostEra`, `ExactEra`, `ProtVerAtLeast`, `ProtVerAtMost`,
    `ProtVerInBounds`, `atLeastEra` and `atMostEra`
* Move `Cardano.Ledger.Alonzo.Tools` module from `cardano-ledegr-alonzo` into
  `Cardano.Ledger.Api.Scripts`
* Addition of `evalBalanceTxBody`
* Addition of `evalTxExUnits` and `evalTxExUnitsWithLogs`. Deprecated
  `evaluateTransactionExecutionUnits` and `evaluateTransactionExecutionUnitsWithLogs` if
  favor of the newly added ones.
* Export `hashDataTxWitsL` and `hashScriptTxWitsL`

## 1.0.0.0

* Initial release
