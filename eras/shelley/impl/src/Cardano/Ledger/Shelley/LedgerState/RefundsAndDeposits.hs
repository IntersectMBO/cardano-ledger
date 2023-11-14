{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.LedgerState.RefundsAndDeposits (
  totalCertsDeposits,
  totalCertsDepositsCertState,
  totalTxRefundsShelley,
  keyCertsRefunds,
  keyCertsRefundsCertState,
  totalTxDeposits,
  totalTxDepositsShelley,
)
where

import Cardano.Ledger.CertState (CertState (..), PState (..), lookupDepositDState)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (StakeCredential)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Shelley.Core (ShelleyEraTxBody (..), ShelleyEraTxCert)
import Cardano.Ledger.Shelley.TxCert (shelleyTotalDepositsTxCerts, shelleyTotalRefundsTxCerts)
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))

-- | Determine the total deposit amount needed from a TxBody.
-- The block may (legitimately) contain multiple registration certificates
-- for the same pool, where the first will be treated as a registration and
-- any subsequent ones as re-registration. As such, we must only take a
-- deposit for the first such registration. It is even possible for a single
-- transaction to have multiple pool registration for the same pool, so as
-- we process pool registrations, we must keep track of those that are already
-- registered, so we do not add a Deposit for the same pool twice.
--
-- Note that this is not an issue for key registrations since subsequent
-- registration certificates would be invalid.
totalCertsDeposits ::
  (EraPParams era, Foldable f, ShelleyEraTxCert era) =>
  PParams era ->
  -- | Check whether a pool with a supplied PoolStakeId is already registered.
  (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
  f (TxCert era) ->
  Coin
totalCertsDeposits = shelleyTotalDepositsTxCerts
{-# DEPRECATED totalCertsDeposits "In favor of `shelleyTotalDepositsTxCerts` or more general `getTotalDepositsTxCerts`" #-}

totalCertsDepositsCertState ::
  (EraPParams era, Foldable f, ShelleyEraTxCert era) =>
  PParams era ->
  CertState era ->
  f (TxCert era) ->
  Coin
totalCertsDepositsCertState pp dpstate =
  totalCertsDeposits pp (`Map.member` psStakePoolParams (certPState dpstate))
{-# DEPRECATED totalCertsDepositsCertState "In favor of `shelleyTotalDepositsTxCerts` or more general `getTotalDepositsTxCerts`" #-}

-- | Calculates the total amount of deposits needed for all pool registration and
-- stake delegation certificates to be valid.
totalTxDepositsShelley ::
  ShelleyEraTxBody era =>
  PParams era ->
  CertState era ->
  TxBody era ->
  Coin
totalTxDepositsShelley pp dpstate txb =
  totalCertsDepositsCertState pp dpstate (txb ^. certsTxBodyL)
{-# DEPRECATED totalTxDepositsShelley "In favor of `shelleyTotalDepositsTxCerts` or more general `Cardano.Ledger.CertState.certsTotalDepositsTxCerts`" #-}

{-# DEPRECATED totalTxDeposits "Use totalTxDepositsShelley or getTotalDepositsTxBody instead" #-}
totalTxDeposits ::
  ShelleyEraTxBody era =>
  PParams era ->
  CertState era ->
  TxBody era ->
  Coin
totalTxDeposits = totalTxDepositsShelley

-- | Compute the key deregistration refunds in a transaction
keyCertsRefundsCertState ::
  (EraPParams era, Foldable f, ShelleyEraTxCert era) =>
  PParams era ->
  CertState era ->
  f (TxCert era) ->
  Coin
keyCertsRefundsCertState pp dpstate = keyCertsRefunds pp (lookupDepositDState (certDState dpstate))
{-# DEPRECATED keyCertsRefundsCertState "In favor of `shelleyTotalRefundsTxCerts` or more general `getTotalRefundsTxCerts`" #-}

-- | Compute the key deregistration refunds in a transaction
keyCertsRefunds ::
  (EraPParams era, Foldable f, ShelleyEraTxCert era) =>
  PParams era ->
  -- | Function that can lookup current deposit, in case when the stake key is registered.
  (StakeCredential (EraCrypto era) -> Maybe Coin) ->
  f (TxCert era) ->
  Coin
keyCertsRefunds = shelleyTotalRefundsTxCerts
{-# DEPRECATED keyCertsRefunds "In favor of `shelleyTotalRefundsTxCerts` or more general `getTotalRefundsTxCerts`" #-}

-- | Compute the refunds attributable to unregistering Stake credentials in a TxBody
totalTxRefundsShelley ::
  ShelleyEraTxBody era =>
  PParams era ->
  CertState era ->
  TxBody era ->
  Coin
totalTxRefundsShelley pp dpstate tx = keyCertsRefundsCertState pp dpstate (tx ^. certsTxBodyL)
{-# DEPRECATED totalTxRefundsShelley "In favor of `shelleyTotalRefundsTxCerts` or more general `Cardano.Ledger.CertState.certsTotalRefundssTxCerts`" #-}
