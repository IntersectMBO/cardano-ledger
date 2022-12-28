{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.LedgerState.RefundsAndDeposits (
  totalTxDeposits,
  totalCertsDeposits,
  keyTxRefunds,
  keyCertsRefunds,
)
where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.DPState (DPState (..), DState (..), PState (..))
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert (..), isRegKey)
import Cardano.Ledger.Shelley.TxBody (
  PoolParams (..),
  ShelleyEraTxBody (..),
  pattern DeRegKey,
  pattern RegKey,
  pattern RegPool,
 )
import Cardano.Ledger.Val ((<+>), (<×>))
import Data.Foldable (foldl', toList)
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
  EraPParams era =>
  PParams era ->
  DPState c ->
  [DCert c] ->
  Coin
totalCertsDeposits pp dpstate certs =
  numKeys <×> pp ^. ppKeyDepositL
    <+> snd (foldl' accum (regpools, Coin 0) certs)
  where
    numKeys = length $ filter isRegKey certs
    regpools = psStakePoolParams (dpsPState dpstate)
    accum (!pools, !ans) (DCertPool (RegPool poolparam)) =
      if Map.member (ppId poolparam) pools -- We don't pay a deposit on a pool that is already registered
        then (pools, ans)
        else (Map.insert (ppId poolparam) poolparam pools, ans <+> pp ^. ppPoolDepositL)
    accum ans _ = ans

totalTxDeposits ::
  ShelleyEraTxBody era =>
  PParams era ->
  DPState (EraCrypto era) ->
  TxBody era ->
  Coin
totalTxDeposits pp dpstate txb = totalCertsDeposits pp dpstate (toList $ txb ^. certsTxBodyG)

-- | Compute the key deregistration refunds in a transaction
keyCertsRefunds ::
  EraPParams era =>
  PParams era ->
  DPState c ->
  [DCert c] ->
  Coin
keyCertsRefunds pp dpstate certs = snd (foldl' accum (regkeys, Coin 0) certs)
  where
    regkeys = dsDeposits (dpsDState dpstate)
    accum (keys, ans) (DCertDeleg (RegKey k)) =
      -- Deposit is added locally to the growing 'keys'
      (Map.insert k (pp ^. ppKeyDepositL) keys, ans)
    accum (keys, ans) (DCertDeleg (DeRegKey k)) =
      -- If the key is registered, lookup the deposit in the locally growing 'keys'
      -- if it is not registered, then just return ans
      case Map.lookup k keys of
        Just deposit -> (keys, ans <+> deposit)
        Nothing -> (keys, ans)
    accum ans _ = ans

keyTxRefunds ::
  ShelleyEraTxBody era =>
  PParams era ->
  DPState (EraCrypto era) ->
  TxBody era ->
  Coin
keyTxRefunds pp dpstate tx = keyCertsRefunds pp dpstate (toList $ tx ^. certsTxBodyG)
