{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Shelley.LedgerState.NewEpochState
  ( availableAfterMIR,
    getGKeys,
    genesisState,
    depositPoolChange,
    reapRewards,
    updateNES,
    returnRedeemAddrsToReserves,
  )
where

import Cardano.Ledger.Address (isBootstrapRedeemer)
import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
  )
import Cardano.Ledger.Coin
  ( Coin (..),
    addDeltaCoin,
  )
import Cardano.Ledger.Core
import Cardano.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    KeyHash (..),
    KeyRole (..),
  )
import Cardano.Ledger.Shelley.LedgerState.DPState
  ( DPState (..),
    DState (..),
    InstantaneousRewards (..),
    PState (..),
  )
import Cardano.Ledger.Shelley.LedgerState.Types
import Cardano.Ledger.Shelley.TxBody
  ( MIRPot (..),
    ShelleyEraTxBody (..),
  )
import Cardano.Ledger.Shelley.UTxO
  ( UTxO (..),
    coinBalance,
    keyRefunds,
    totalDeposits,
  )
import Cardano.Ledger.UnifiedMap
  ( Trip (..),
    UMap (..),
    UnifiedMap,
  )
import Cardano.Ledger.Val ((<+>), (<->))
import Control.State.Transition (STS (State))
import Data.Default.Class (Default, def)
import Data.Foldable (fold, toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import GHC.Records (HasField (..))
import Lens.Micro
import Lens.Micro.Extras (view)

-- | This function returns the coin balance of a given pot, either the
-- reserves or the treasury, after the instantaneous rewards and pot
-- transfers are accounted for.
availableAfterMIR :: MIRPot -> AccountState -> InstantaneousRewards crypto -> Coin
availableAfterMIR ReservesMIR as ir =
  _reserves as `addDeltaCoin` deltaReserves ir <-> fold (iRReserves ir)
availableAfterMIR TreasuryMIR as ir =
  _treasury as `addDeltaCoin` deltaTreasury ir <-> fold (iRTreasury ir)

-- ========================
-- Virtual selectors, which get the appropriate view from a DState from the embedded UnifiedMap

getGKeys ::
  NewEpochState era ->
  Set (KeyHash 'Genesis (Crypto era))
getGKeys nes = Map.keysSet genDelegs
  where
    NewEpochState _ _ _ es _ _ _ = nes
    EpochState _ _ ls _ _ _ = es
    LedgerState _ (DPState (DState _ _ (GenDelegs genDelegs) _) _) = ls

-- | Creates the ledger state for an empty ledger which
--  contains the specified transaction outputs.
genesisState ::
  Default (State (EraRule "PPUP" era)) =>
  Map (KeyHash 'Genesis (Crypto era)) (GenDelegPair (Crypto era)) ->
  UTxO era ->
  LedgerState era
genesisState genDelegs0 utxo0 =
  LedgerState
    ( UTxOState
        utxo0
        (Coin 0)
        (Coin 0)
        def
        (IStake mempty Map.empty)
    )
    (DPState dState def)
  where
    dState = def {_genDelegs = GenDelegs genDelegs0}

-- Functions for stake delegation model

-- | Calculate the change to the deposit pool for a given transaction.
depositPoolChange ::
  ( HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin,
    ShelleyEraTxBody era
  ) =>
  LedgerState era ->
  PParams era ->
  TxBody era ->
  Coin
depositPoolChange ls pp txBody = (currentPool <+> txDeposits) <-> txRefunds
  where
    -- Note that while (currentPool + txDeposits) >= txRefunds,
    -- it could be that txDeposits < txRefunds. We keep the parenthesis above
    -- to emphasize this point.

    currentPool = (_deposited . lsUTxOState) ls
    pools = _pParams . dpsPState . lsDPState $ ls
    txDeposits =
      totalDeposits pp (`Map.notMember` pools) (toList $ txBody ^. certsTxBodyL)
    txRefunds = keyRefunds pp txBody

reapRewards ::
  UnifiedMap crypto ->
  RewardAccounts crypto ->
  UnifiedMap crypto
reapRewards (UnifiedMap tmap ptrmap) withdrawals = UnifiedMap (Map.mapWithKey g tmap) ptrmap
  where
    g k (Triple x y z) = Triple (fmap (removeRewards k) x) y z
    removeRewards k v = if k `Map.member` withdrawals then Coin 0 else v

-- A TxOut has 4 different shapes, depending on the shape of its embedded Addr.
-- Credentials are stored in only 2 of the 4 cases.
-- 1) TxOut (Addr _ _ (StakeRefBase cred)) coin   -> HERE
-- 2) TxOut (Addr _ _ (StakeRefPtr ptr)) coin     -> HERE
-- 3) TxOut (Addr _ _ StakeRefNull) coin          -> NOT HERE
-- 4) TxOut (AddrBootstrap _) coin                -> NOT HERE

-- | Update new epoch state
updateNES ::
  NewEpochState era ->
  BlocksMade (Crypto era) ->
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
      { nesBcur = bcur,
        nesEs = EpochState acnt ss ls pr pp nm
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
    UTxO utxo = _utxo us
    (redeemers, nonredeemers) =
      Map.partition (maybe False isBootstrapRedeemer . view bootAddrTxOutF) utxo
    acnt = esAccountState es
    utxoR = UTxO redeemers :: UTxO era
    acnt' =
      acnt
        { _reserves = _reserves acnt <+> coinBalance utxoR
        }
    us' = us {_utxo = UTxO nonredeemers :: UTxO era}
    ls' = ls {lsUTxOState = us'}
