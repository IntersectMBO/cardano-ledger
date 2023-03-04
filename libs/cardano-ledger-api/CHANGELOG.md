# Version history for `cardano-ledger-api`

## 1.1.0.0

* Add `Cardano.Ledger.Api.Governance` that exposes all of the governance related types.
* Addition of `Cardano.Ledger.Api.PParams`
* Addition of `Cardano.Ledger.Api.Tx.Address`
* Hide `ShelleyTxBody`, `AlonzoTxBody`, `BabbageTxBody` from `Cardano.Ledger.Api.Tx.Body`
* Export `Withdrawals` from `Cardano.Ledger.Api.Tx.Body`
* Export `ShelleyTxAuxData`, `AllegraTxAuxData` and `AlonzoTxAuxData` from
  `Cardano.Ledger.Api.Tx.AuxData`
* Export `LangDepView` and `getLanguageView` from `Cardano.Ledger.Api.PParams`


## 1.0.0.0

* Initial release
