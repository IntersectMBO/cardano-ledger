module Cardano.Ledger.Allegra.TxBody (
  AllegraEraTxBody (..),
  AllegraTxBody (
    AllegraTxBody,
    atbAuxDataHash,
    atbCerts,
    atbInputs,
    atbOutputs,
    atbTxFee,
    atbUpdate,
    atbValidityInterval,
    atbWithdrawals
  ),
  emptyAllegraTxBodyRaw,
  AllegraTxBodyRaw (..),
  StrictMaybe (..),
  ValidityInterval (..),
)
where

import Cardano.Ledger.Allegra.TxBody.Internal
