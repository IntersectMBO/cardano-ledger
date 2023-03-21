{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.LedgerState.RefundsAndDeposits (
  totalTxDeposits,
  totalCertsDeposits,
  totalCertsDepositsDPState,
  keyTxRefunds,
  keyCertsRefunds,
  keyCertsRefundsDPState,
)
where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (StakeCredential)
import Cardano.Ledger.DPState (DPState (..), PState (..), lookupDepositDState)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert (..), isRegKey)
import Cardano.Ledger.Shelley.TxBody (
  PoolParams (..),
  ShelleyEraTxBody (..),
  pattern DeRegKey,
  pattern RegKey,
  pattern RegPool,
 )
import Cardano.Ledger.Val ((<+>), (<×>))
import Data.Foldable (foldMap', foldl')
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import qualified Data.Set as Set
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
  (EraPParams era, Foldable f) =>
  PParams era ->
  -- | Check whether a pool with a supplied PoolStakeId is already registered.
  (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
  f (DCert (EraCrypto era)) ->
  Coin
totalCertsDeposits pp isRegPool certs =
  numKeys <×> pp ^. ppKeyDepositL
    <+> numNewRegPoolCerts <×> pp ^. ppPoolDepositL
  where
    numKeys = getSum @Int $ foldMap' (\x -> if isRegKey x then 1 else 0) certs
    numNewRegPoolCerts = Set.size (foldl' addNewPoolIds Set.empty certs)
    addNewPoolIds regPoolIds = \case
      DCertPool (RegPool (PoolParams {ppId}))
        -- We don't pay a deposit on a pool that is already registered or duplicated in the certs
        | not (isRegPool ppId || Set.member ppId regPoolIds) -> Set.insert ppId regPoolIds
      _ -> regPoolIds

totalCertsDepositsDPState ::
  (EraPParams era, Foldable f) =>
  PParams era ->
  DPState (EraCrypto era) ->
  f (DCert (EraCrypto era)) ->
  Coin
totalCertsDepositsDPState pp dpstate =
  totalCertsDeposits pp (`Map.member` psStakePoolParams (dpsPState dpstate))

-- | Calculates the total amount of deposits needed for all pool registration and
-- stake delegation certificates to be valid.
totalTxDeposits ::
  ShelleyEraTxBody era =>
  PParams era ->
  DPState (EraCrypto era) ->
  TxBody era ->
  Coin
totalTxDeposits pp dpstate txb =
  totalCertsDepositsDPState pp dpstate (txb ^. certsTxBodyG)

-- | Compute the key deregistration refunds in a transaction
keyCertsRefundsDPState ::
  (EraPParams era, Foldable f) =>
  PParams era ->
  DPState (EraCrypto era) ->
  f (DCert (EraCrypto era)) ->
  Coin
keyCertsRefundsDPState pp dpstate = keyCertsRefunds pp (lookupDepositDState (dpsDState dpstate))

-- | Compute the key deregistration refunds in a transaction
keyCertsRefunds ::
  (EraPParams era, Foldable f) =>
  PParams era ->
  -- | Function that can lookup current deposit, in case when the stake key is registered.
  (StakeCredential (EraCrypto era) -> Maybe Coin) ->
  f (DCert (EraCrypto era)) ->
  Coin
keyCertsRefunds pp lookupDeposit certs = snd (foldl' accum (mempty, Coin 0) certs)
  where
    keyDeposit = pp ^. ppKeyDepositL
    accum (!regKeys, !totalRefunds) = \case
      DCertDeleg (RegKey k) ->
        -- Need to track new delegations in case that the same key is later deregistered in
        -- the same transaction.
        (Set.insert k regKeys, totalRefunds)
      DCertDeleg (DeRegKey k)
        -- We first check if there was already a registration certificate in this
        -- transaction.
        | Set.member k regKeys -> (Set.delete k regKeys, totalRefunds <+> keyDeposit)
        -- Check for the deposit left during registration in some previous
        -- transaction. This de-registration check will be matched first, despite being
        -- the last case to match, because registration is not possible without
        -- de-registration.
        | Just deposit <- lookupDeposit k -> (regKeys, totalRefunds <+> deposit)
      _ -> (regKeys, totalRefunds)

keyTxRefunds ::
  ShelleyEraTxBody era =>
  PParams era ->
  DPState (EraCrypto era) ->
  TxBody era ->
  Coin
keyTxRefunds pp dpstate tx = keyCertsRefundsDPState pp dpstate (tx ^. certsTxBodyG)
