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
  -- * UTxO
  UTxO (..),
  CanGetUTxO (..),
  CanSetUTxO (..),

  -- * Others to organize
  AccountState (..),
  EraCertState (..),
  DState (..),
  VState (..),
  rewards,
  delegations,
  ptrsMap,
  EpochState (..),
  PulsingRewUpdate (..),
  FutureGenDeleg (..),
  InstantaneousRewards (..),
  LedgerState (..),
  PState (..),
  RewardAccounts,
  RewardUpdate (..),
  RewardSnapShot (..),
  UTxOState (..),
  smartUTxOState,

  -- * Validation
  consumed,
  produced,

  -- * DelegationState
  payPoolDeposit,
  refundPoolDeposit,
  totalObligation,
  allObligations,

  -- * Epoch boundary
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
  emptyRewardUpdate,
  pvCanFollow,
  availableAfterMIR,
  ShelleyGovState (..),

  -- * Lenses from Types
  nesPdL,
  nesEsL,
  unifiedL,
  nesELL,
  nesBprevL,
  nesBcurL,
  nesRuL,
  nesStashedAVVMAddressesL,
  nesEpochStateL,
  esAccountStateL,
  esSnapshotsL,
  esLStateL,
  esNonMyopicL,
  curPParamsEpochStateL,
  prevPParamsEpochStateL,
  futurePParamsEpochStateL,
  asTreasuryL,
  asReservesL,
  lsUTxOStateL,
  lsCertStateL,
  utxosUtxoL,
  utxosDepositedL,
  utxosFeesL,
  utxosGovStateL,
  utxosDonationL,
  epochStateGovStateL,
  epochStateStakeDistrL,
  epochStateUMapL,
  epochStatePoolParamsL,
  epochStateRegDrepL,
  epochStateDonationL,
  newEpochStateGovStateL,
  epochStateTreasuryL,

  -- * Lenses from CertState
  dsUnifiedL,
  dsGenDelegsL,
  dsIRewardsL,
  dsFutureGenDelegsL,
  psStakePoolParamsL,
  psFutureStakePoolParamsL,
  psRetiringL,
  psDepositsL,
  vsDRepsL,
  vsCommitteeStateL,

  -- * Lenses from SnapShot(s)
  ssStakeMarkL,
  ssStakeMarkPoolDistrL,
  ssStakeSetL,
  ssStakeGoL,
  ssFeeL,
  ssStakeL,
  ssStakeDistrL,
  ssDelegationsL,
  ssPoolParamsL,
) where

import Cardano.Ledger.CertState
import Cardano.Ledger.Shelley.LedgerState.IncrementalStake
import Cardano.Ledger.Shelley.LedgerState.NewEpochState
import Cardano.Ledger.Shelley.LedgerState.PulsingReward
import Cardano.Ledger.Shelley.LedgerState.Types
import Cardano.Ledger.Shelley.PParams (pvCanFollow)
import Cardano.Ledger.Shelley.RewardUpdate
import Cardano.Ledger.Shelley.Rules.Ppup (ShelleyGovState (..))
import Cardano.Ledger.Shelley.UTxO (consumed, produced)
import Cardano.Ledger.State
