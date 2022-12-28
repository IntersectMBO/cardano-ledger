{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : LedgerState
-- Description : Operational Rules
--
-- This module implements the operation rules for treating UTxO transactions ('Tx')
-- as state transformations on a ledger state ('LedgerState'),
-- as specified in /A Simplified Formal Specification of a UTxO Ledger/.
module Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  DPState (..),
  DState (..),
  emptyDState,
  rewards,
  delegations,
  ptrsMap,
  EpochState (..),
  UpecState (..),
  PulsingRewUpdate (..),
  FutureGenDeleg (..),
  InstantaneousRewards (..),
  KeyPairs, -- deprecated
  LedgerState (..),
  PPUPState (..),
  PState (..),
  RewardAccounts,
  RewardUpdate (..),
  RewardSnapShot (..),
  UTxOState (..),
  smartUTxOState,
  IncrementalStake (..),
  depositPoolChange,
  emptyRewardUpdate,
  pvCanFollow,
  reapRewards,
  availableAfterMIR,

  -- * Genesis State
  genesisState,

  -- * Validation
  WitHashes,
  unWitHashes,
  nullWitHashes,
  diffWitHashes,
  minfee,
  produced,
  witsFromTxWitnesses,

  -- * DelegationState
  keyTxRefunds,
  payKeyDeposit,
  payPoolDeposit,
  refundKeyDeposit,
  refundPoolDeposit,
  totalTxDeposits,
  obligationDPState,
  keyCertsRefunds,
  totalCertsDeposits,

  -- * Epoch boundary
  incrementalStakeDistr,
  updateStakeDistribution,
  applyRUpd,
  applyRUpdFiltered,
  filterAllRewards,
  FilteredRewards (..),
  createRUpd,
  completeRupd,
  startStep,
  pulseStep,
  completeStep,
  NewEpochState (NewEpochState, nesEL, nesEs, nesRu, nesPd, nesBprev, nesBcur),
  StashedAVVMAddresses,
  stashedAVVMAddresses,
  getGKeys,
  updateNES,
  circulation,

  -- * Decay
  decayFactor,

  -- * Remove Bootstrap Redeem Addresses
  returnRedeemAddrsToReserves,
  updateNonMyopic,
)
where

import Cardano.Ledger.DPState
import Cardano.Ledger.Era (EraCrypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (Witness))
import Cardano.Ledger.Shelley.LedgerState.IncrementalStake
import Cardano.Ledger.Shelley.LedgerState.NewEpochState
import Cardano.Ledger.Shelley.LedgerState.PulsingReward
import Cardano.Ledger.Shelley.LedgerState.RefundsAndDeposits
import Cardano.Ledger.Shelley.LedgerState.Types
import Cardano.Ledger.Shelley.PParams (PPUPState (..), pvCanFollow)
import Cardano.Ledger.Shelley.RewardUpdate
import Cardano.Ledger.Shelley.Tx (minfee, witsFromTxWitnesses)
import Cardano.Ledger.Shelley.UTxO (produced)
import Data.Default.Class (def)
import Data.Set (Set)
import qualified Data.Set as Set

{-# DEPRECATED emptyDState "Use `def` instead" #-}
emptyDState :: DState a
emptyDState = def

{-# DEPRECATED WitHashes "Use a set of keyhashes instead" #-}

type WitHashes era = Set (KeyHash 'Witness (EraCrypto era))

{-# DEPRECATED unWitHashes "Remove this function" #-}
unWitHashes :: WitHashes era -> Set (KeyHash 'Witness (EraCrypto era))
unWitHashes = id

{-# DEPRECATED nullWitHashes "Use set operations instead" #-}
nullWitHashes :: WitHashes era -> Bool
nullWitHashes = Set.null

{-# DEPRECATED diffWitHashes "Use set operations instead" #-}
diffWitHashes :: WitHashes era -> WitHashes era -> WitHashes era
diffWitHashes = Set.difference
