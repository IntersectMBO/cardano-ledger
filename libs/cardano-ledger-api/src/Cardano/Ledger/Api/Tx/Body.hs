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
  spendableInputsTxBodyF,
  allInputsTxBodyF,
  evalBalanceTxBody,

  -- * Shelley Era
  ShelleyEraTxBody,
  ttlTxBodyL,
  updateTxBodyL,
  updateTxBodyG, -- deprecated
  certsTxBodyL,

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
  votingProceduresTxBodyL,
  proposalProceduresTxBodyL,
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
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core (ConwayEraTxBody (..))
import Cardano.Ledger.Core (Era (EraCrypto), EraTxBody (..), PParams, Value)
import Cardano.Ledger.Credential (StakeCredential)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Core (MaryEraTxBody (..))
import Cardano.Ledger.Shelley.Core (ShelleyEraTxBody (..))
import Cardano.Ledger.Shelley.UTxO (getProducedValue)
import Cardano.Ledger.UTxO (EraUTxO (getConsumedValue), UTxO)
import Cardano.Ledger.Val ((<->))

-- | Evaluate the difference between the value currently being consumed by a transaction
-- and the total value being produced. This value will be zero for a valid transaction.
--
-- In case when full `Cardano.Ledger.CertState` is available then this can be simplified to:
--
-- > let lookupRefund = lookupDepositDState (certDState dpState)
-- > let isRegPoolId = (`Map.member` psStakePoolParams (certPState dpState))
-- > evalBalanceTxBody pp lookupRefund isRegPoolId utxo txBody
evalBalanceTxBody ::
  (EraUTxO era, ShelleyEraTxBody era) =>
  -- | Current protocol parameters
  PParams era ->
  -- | Lookup current deposit amount for a registered stake credential delegation. This
  -- function must produce valid answer for all of the stake credentials present in any of
  -- the `DeRegKey` delegation certificates in the supplied `TxBody`. In other words,
  -- there is no requirement to know about all of the delegation certificates in the
  -- ledger state, just the ones this transaction cares about.
  (StakeCredential (EraCrypto era) -> Maybe Coin) ->
  -- | Check whether a pool with a supplied PoolStakeId is already registered. There is no
  -- requirement to answer this question for all stake pool credentials, just for the ones
  -- that have the registration certificates included in the supplied `TxBody`
  (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
  -- | The UTxO relevant to the transaction.
  UTxO era ->
  -- | The transaction being evaluated for balance.
  TxBody era ->
  -- | The difference between what the transaction consumes and what it produces.
  Value era
evalBalanceTxBody pp lookupRefund isRegPoolId utxo txBody =
  getConsumedValue pp lookupRefund utxo txBody <-> getProducedValue pp isRegPoolId txBody
