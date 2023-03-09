{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Api.Tx.Body (
  -- | Building and inspecting transaction outputs
  module Cardano.Ledger.Api.Tx.Out,
  -- | Working with Timelock scripts and Plutus scripts
  module Cardano.Ledger.Api.Scripts,
  EraTxBody (TxBody),
  mkBasicTxBody,
  inputsTxBodyL,
  outputsTxBodyL,
  feeTxBodyL,
  withdrawalsTxBodyL,
  Withdrawals (..),
  auxDataHashTxBodyL,
  AuxiliaryDataHash,
  allInputsTxBodyF,

  -- * Shelley Era
  ShelleyEraTxBody,
  ttlTxBodyL,
  updateTxBodyL,
  updateTxBodyG,
  certsTxBodyL,
  certsTxBodyG,

  -- * Allegra Era
  AllegraEraTxBody,
  vldtTxBodyL,
  ValidityInterval (..),

  -- * Mary Era
  MaryEraTxBody,
  mintTxBodyL,
  mintValueTxBodyF,
  mintedTxBodyF,

  -- * Alonzo Era
  AlonzoEraTxBody,
  collateralInputsTxBodyL,
  reqSignerHashesTxBodyL,
  scriptIntegrityHashTxBodyL,
  networkIdTxBodyL,

  -- * Babbage Era
  BabbageEraTxBody,
  sizedOutputsTxBodyL,
  referenceInputsTxBodyL,
  totalCollateralTxBodyL,
  collateralReturnTxBodyL,
  sizedCollateralReturnTxBodyL,
  allSizedOutputsTxBodyF,

  -- * Conway Era
  ConwayEraTxBody,
  govActionsTxBodyL,
  votesTxBodyL,
  conwayCertsTxBodyL,
)
where

import Cardano.Ledger.Address (Withdrawals (..))
import Cardano.Ledger.Allegra.Core (AllegraEraTxBody (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (..))
import Cardano.Ledger.Api.Era ()
import Cardano.Ledger.Api.Scripts
import Cardano.Ledger.Api.Tx.Out
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.Babbage.TxBody (BabbageEraTxBody (..))
import Cardano.Ledger.Conway.Core (ConwayEraTxBody (..))
import Cardano.Ledger.Core (EraTxBody (..))
import Cardano.Ledger.Mary.Core (MaryEraTxBody (..))
import Cardano.Ledger.Shelley.Core (ShelleyEraTxBody (..))
