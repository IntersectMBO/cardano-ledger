{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Ledger.Shelley.LedgerState.NewEpochState (
  availableAfterMIR,
  getGKeys,
  genesisState,
  updateNES,
  returnRedeemAddrsToReserves,
) where

import Cardano.Ledger.Address (isBootstrapRedeemer)
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
 )
import Cardano.Ledger.Coin (Coin (..), addDeltaCoin)
import Cardano.Ledger.Keys (GenDelegPair (..), GenDelegs (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState.Types
import Cardano.Ledger.State
import Cardano.Ledger.Val ((<+>), (<->))
import Data.Default (def)
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Extras (view)

-- | This function returns the coin balance of a given pot, either the
-- reserves or the treasury, after the instantaneous rewards and pot
-- transfers are accounted for.
availableAfterMIR :: MIRPot -> ChainAccountState -> InstantaneousRewards -> Coin
availableAfterMIR ReservesMIR as ir =
  casReserves as `addDeltaCoin` deltaReserves ir <-> fold (iRReserves ir)
availableAfterMIR TreasuryMIR as ir =
  casTreasury as `addDeltaCoin` deltaTreasury ir <-> fold (iRTreasury ir)

-- ========================
-- Virtual selectors, which get the appropriate view from a DState from the embedded UnifiedMap

getGKeys ::
  EraCertState era =>
  NewEpochState era ->
  Set (KeyHash GenesisRole)
getGKeys nes = Map.keysSet $ unGenDelegs (ls ^. lsCertStateL . certDStateL . dsGenDelegsL)
  where
    NewEpochState _ _ _ es _ _ _ = nes
    EpochState _ ls _ _ = es

-- | Creates the ledger state for an empty ledger which
--  contains the specified transaction outputs.
genesisState ::
  forall era.
  (EraGov era, EraCertState era, EraStake era) =>
  Map (KeyHash GenesisRole) GenDelegPair ->
  UTxO era ->
  LedgerState era
genesisState genDelegs0 utxo0 =
  LedgerState
    ( UTxOState
        utxo0
        (Coin 0)
        (Coin 0)
        emptyGovState
        mempty
        mempty
    )
    ( def
        & certDStateL .~ dState
    )
  where
    dState :: DState era
    dState =
      DState
        { dsAccounts = def
        , dsFutureGenDelegs = Map.empty
        , dsGenDelegs = GenDelegs genDelegs0 :: GenDelegs
        , dsIRewards = def
        }

-- Functions for stake delegation model

-- A TxOut has 4 different shapes, depending on the shape of its embedded Addr.
-- Credentials are stored in only 2 of the 4 cases.
-- 1) TxOut (Addr _ _ (StakeRefBase cred)) coin   -> HERE
-- 2) TxOut (Addr _ _ (StakeRefPtr ptr)) coin     -> HERE
-- 3) TxOut (Addr _ _ StakeRefNull) coin          -> NOT HERE
-- 4) TxOut (AddrBootstrap _) coin                -> NOT HERE

-- | Update new epoch state
updateNES ::
  EraGov era =>
  NewEpochState era ->
  BlocksMade ->
  LedgerState era ->
  NewEpochState era
updateNES
  oldNes@( NewEpochState
             _eL
             _bprev
             _
             es@(EpochState acnt _ ss nm)
             _ru
             _pd
             _avvm
           )
  bcur
  ls =
    let
      pp = es ^. curPParamsEpochStateL
      pr = es ^. prevPParamsEpochStateL
     in
      oldNes
        { nesBcur = bcur
        , nesEs =
            EpochState acnt ls ss nm
              & curPParamsEpochStateL .~ pp
              & prevPParamsEpochStateL .~ pr
        }

returnRedeemAddrsToReserves ::
  forall era.
  EraTxOut era =>
  EpochState era ->
  EpochState era
returnRedeemAddrsToReserves es = es {esChainAccountState = acnt', esLState = ls'}
  where
    ls = esLState es
    us = lsUTxOState ls
    UTxO utxo = utxosUtxo us
    (redeemers, nonredeemers) =
      Map.partition (maybe False isBootstrapRedeemer . view bootAddrTxOutF) utxo
    acnt = esChainAccountState es
    utxoR = UTxO redeemers :: UTxO era
    acnt' =
      acnt
        { casReserves = casReserves acnt <+> sumCoinUTxO utxoR
        }
    us' = us {utxosUtxo = UTxO nonredeemers :: UTxO era}
    ls' = ls {lsUTxOState = us'}
