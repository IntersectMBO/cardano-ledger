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
  txIdTxBody,

  -- * Shelley Era
  ShelleyEraTxBody,
  ttlTxBodyL,
  updateTxBodyL,
  certsTxBodyL,

  -- * Allegra Era
  AllegraEraTxBody,
  vldtTxBodyL,
  ValidityInterval (..),
  invalidBeforeL,
  invalidHereAfterL,

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
import Cardano.Ledger.BaseTypes (SlotNo, StrictMaybe (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.TxBody (ConwayEraTxBody (..))
import Cardano.Ledger.Core (Era (EraCrypto), EraTxBody (..), PParams, Value, txIdTxBody)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Core (MaryEraTxBody (..))
import Cardano.Ledger.Shelley.Core (ShelleyEraTxBody (..))
import Cardano.Ledger.Shelley.UTxO (getProducedValue)
import Cardano.Ledger.UTxO (EraUTxO (getConsumedValue), UTxO)
import Cardano.Ledger.Val ((<->))
import Lens.Micro (Lens', lens)

-- | Evaluate the difference between the value currently being consumed by a transaction
-- and the total value being produced. This value will be zero for a valid transaction.
--
-- In case when full `Cardano.Ledger.CertState` is available then this can be simplified to:
--
-- > let lookupRefund = lookupDepositDState (certDState dpState)
-- > let isRegPoolId = (`Map.member` psStakePoolParams (certPState dpState))
-- > evalBalanceTxBody pp lookupRefund isRegPoolId utxo txBody
evalBalanceTxBody ::
  EraUTxO era =>
  -- | Current protocol parameters
  PParams era ->
  -- | Lookup current deposit amount for a registered stake credential delegation. This
  -- function must produce valid answer for all of the stake credentials present in any of
  -- the `DeRegKey` delegation certificates in the supplied `TxBody`. In other words,
  -- there is no requirement to know about all of the delegation certificates in the
  -- ledger state, just the ones this transaction cares about.
  (Credential 'Staking (EraCrypto era) -> Maybe Coin) ->
  -- | Lookup current deposit amount for a registered DRep credential. This
  -- function must produce valid answer for all of the DRep credentials present in any of
  -- the `UnRegDRep` certificates in the supplied `TxBody`. In other words,
  -- there is no requirement to know about all of the DRep registrations in the
  -- ledger state, just the ones this transaction cares about.
  (Credential 'DRepRole (EraCrypto era) -> Maybe Coin) ->
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
evalBalanceTxBody pp lookupKeyRefund lookupDRepRefund isRegPoolId utxo txBody =
  getConsumedValue pp lookupKeyRefund lookupDRepRefund utxo txBody
    <-> getProducedValue pp isRegPoolId txBody

-- | Lens to access the 'invalidBefore' field of a 'ValidityInterval' as a 'Maybe SlotNo'.
invalidBeforeL :: Lens' ValidityInterval (Maybe SlotNo)
invalidBeforeL = lens g s
  where
    g :: ValidityInterval -> Maybe SlotNo
    g (ValidityInterval ma _) =
      case ma of
        SNothing -> Nothing
        SJust a -> Just a

    s :: ValidityInterval -> Maybe SlotNo -> ValidityInterval
    s (ValidityInterval _ b) a = ValidityInterval (maybe SNothing SJust a) b

-- | Lens to access the 'invalidHereAfter' field of a 'ValidityInterval' as a 'Maybe SlotNo'.
invalidHereAfterL :: Lens' ValidityInterval (Maybe SlotNo)
invalidHereAfterL = lens g s
  where
    g :: ValidityInterval -> Maybe SlotNo
    g (ValidityInterval _ mb) =
      case mb of
        SNothing -> Nothing
        SJust b -> Just b

    s :: ValidityInterval -> Maybe SlotNo -> ValidityInterval
    s (ValidityInterval ma _) = ValidityInterval ma . maybe SNothing SJust
