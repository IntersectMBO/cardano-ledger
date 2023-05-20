{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Shelley.LedgerState.NewEpochState (
  availableAfterMIR,
  getGKeys,
  genesisState,
  depositPoolChange,
  reapRewards,
  updateNES,
  returnRedeemAddrsToReserves,
) where

import Cardano.Ledger.Address (isBootstrapRedeemer)
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
 )
import Cardano.Ledger.CertState (
  CertState (..),
  DState (..),
  InstantaneousRewards (..),
 )
import Cardano.Ledger.Coin (
  Coin (..),
  CompactForm (CompactCoin),
  addDeltaCoin,
 )
import Cardano.Ledger.Keys (
  GenDelegPair (..),
  GenDelegs (..),
  KeyHash (..),
  KeyRole (..),
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState.RefundsAndDeposits (keyTxRefunds, totalTxDeposits)
import Cardano.Ledger.Shelley.LedgerState.Types
import Cardano.Ledger.Shelley.TxBody (MIRPot (..))
import Cardano.Ledger.UMap (RDPair (..), UMElem (..), UMap (..))
import Cardano.Ledger.UTxO (UTxO (..), coinBalance)
import Cardano.Ledger.Val ((<+>), (<->))
import Data.Default.Class (def)
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Lens.Micro.Extras (view)

-- | This function returns the coin balance of a given pot, either the
-- reserves or the treasury, after the instantaneous rewards and pot
-- transfers are accounted for.
availableAfterMIR :: MIRPot -> AccountState -> InstantaneousRewards c -> Coin
availableAfterMIR ReservesMIR as ir =
  asReserves as `addDeltaCoin` deltaReserves ir <-> fold (iRReserves ir)
availableAfterMIR TreasuryMIR as ir =
  asTreasury as `addDeltaCoin` deltaTreasury ir <-> fold (iRTreasury ir)

-- ========================
-- Virtual selectors, which get the appropriate view from a DState from the embedded UnifiedMap

getGKeys ::
  NewEpochState era ->
  Set (KeyHash 'Genesis (EraCrypto era))
getGKeys nes = Map.keysSet genDelegs
  where
    NewEpochState _ _ _ es _ _ _ = nes
    EpochState _ _ ls _ _ _ = es
    LedgerState _ (CertState _ _ DState {dsGenDelegs = (GenDelegs genDelegs)}) = ls

-- | Creates the ledger state for an empty ledger which
--  contains the specified transaction outputs.
genesisState ::
  forall era.
  EraGovernance era =>
  Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era)) ->
  UTxO era ->
  LedgerState era
genesisState genDelegs0 utxo0 =
  LedgerState
    ( UTxOState
        utxo0
        (Coin 0)
        (Coin 0)
        emptyGovernanceState
        (IStake mempty Map.empty)
    )
    (CertState def def dState)
  where
    dState = def {dsGenDelegs = GenDelegs genDelegs0}

-- Functions for stake delegation model

-- | Calculate the change to the deposit pool for a given transaction.
depositPoolChange ::
  ShelleyEraTxBody era =>
  LedgerState era ->
  PParams era ->
  TxBody era ->
  Coin
depositPoolChange ls pp txBody = (currentPool <+> txDeposits) <-> txRefunds
  where
    -- Note that while (currentPool + txDeposits) >= txRefunds,
    -- it could be that txDeposits < txRefunds. We keep the parenthesis above
    -- to emphasize this point.

    currentPool = (utxosDeposited . lsUTxOState) ls
    txDeposits = totalTxDeposits pp (lsCertState ls) txBody
    txRefunds = keyTxRefunds pp (lsCertState ls) txBody

-- Remove the rewards from the UMap, but leave the deposits alone
reapRewards ::
  UMap c ->
  RewardAccounts c ->
  UMap c
reapRewards (UMap tmap ptrmap) withdrawals = UMap (Map.mapWithKey g tmap) ptrmap
  where
    g k (UMElem w x y z) = UMElem (fmap (removeRewards k) w) x y z
    removeRewards k v@(RDPair _ d) =
      if k `Map.member` withdrawals then RDPair (CompactCoin 0) d else v

-- A TxOut has 4 different shapes, depending on the shape of its embedded Addr.
-- Credentials are stored in only 2 of the 4 cases.
-- 1) TxOut (Addr _ _ (StakeRefBase cred)) coin   -> HERE
-- 2) TxOut (Addr _ _ (StakeRefPtr ptr)) coin     -> HERE
-- 3) TxOut (Addr _ _ StakeRefNull) coin          -> NOT HERE
-- 4) TxOut (AddrBootstrap _) coin                -> NOT HERE

-- | Update new epoch state
updateNES ::
  NewEpochState era ->
  BlocksMade (EraCrypto era) ->
  LedgerState era ->
  NewEpochState era
updateNES
  oldNes@( NewEpochState
            _eL
            _bprev
            _
            (EpochState acnt ss _ pr pp nm)
            _ru
            _pd
            _avvm
          )
  bcur
  ls =
    oldNes
      { nesBcur = bcur
      , nesEs = EpochState acnt ss ls pr pp nm
      }

returnRedeemAddrsToReserves ::
  forall era.
  EraTxOut era =>
  EpochState era ->
  EpochState era
returnRedeemAddrsToReserves es = es {esAccountState = acnt', esLState = ls'}
  where
    ls = esLState es
    us = lsUTxOState ls
    UTxO utxo = utxosUtxo us
    (redeemers, nonredeemers) =
      Map.partition (maybe False isBootstrapRedeemer . view bootAddrTxOutF) utxo
    acnt = esAccountState es
    utxoR = UTxO redeemers :: UTxO era
    acnt' =
      acnt
        { asReserves = asReserves acnt <+> coinBalance utxoR
        }
    us' = us {utxosUtxo = UTxO nonredeemers :: UTxO era}
    ls' = ls {lsUTxOState = us'}
