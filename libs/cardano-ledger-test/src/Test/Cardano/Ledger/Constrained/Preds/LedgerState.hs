{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Cardano.Ledger.Constrained.Preds.LedgerState where

import Cardano.Ledger.Coin (Coin (..))
import Data.Map.Strict (Map)
import Data.Map.Strict as Map
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (OrdCond (..))
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Examples (checkForSoundness)
import Test.Cardano.Ledger.Constrained.Monad (Typed, failT, monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.CertState (dstateStage, pstateStage, vstateStage)
import Test.Cardano.Ledger.Constrained.Preds.PParams (pParamsStage)
import Test.Cardano.Ledger.Constrained.Preds.Repl (goRepl)
import Test.Cardano.Ledger.Constrained.Preds.Universes (universeStage)
import Test.Cardano.Ledger.Constrained.Rewrite (standardOrderInfo)
import Test.Cardano.Ledger.Constrained.Size (Size (..))
import Test.Cardano.Ledger.Constrained.Solver (toolChainSub)
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.PrettyCore (pcLedgerState, pcUTxOState)
import Test.Cardano.Ledger.Generic.Proof
import Test.QuickCheck

-- =========================================

ledgerStatePreds :: forall era. Reflect era => Proof era -> [Pred era]
ledgerStatePreds p =
  [ MetaSize (SzRng 20 25) utxoSize -- must be bigger than sum of (maxsize inputs 10) and (mazsize collateral 3)
  , Sized utxoSize preUtxo
  , Sized (Range 14 16) colUtxo
  , MapMember feeTxIn feeTxOut (Right preUtxo)
  , Subset (Dom preUtxo) txinUniv
  , Subset (Rng preUtxo) (txoutUniv p)
  , utxo p :<-: (Constr "mapunion" Map.union ^$ preUtxo ^$ colUtxo)
  , --
    Disjoint (Dom colUtxo) (Dom preUtxo)
  , Subset (Dom colUtxo) txinUniv
  , Subset (Rng colUtxo) (colTxoutUniv p)
  , NotMember feeTxIn (Dom colUtxo)
  , NotMember feeTxOut (Rng colUtxo)
  , --
    SumsTo (Right (Coin 1)) deposits EQL [SumMap stakeDeposits, SumMap poolDeposits]
  , -- , SumsTo (Right (Coin 1)) utxoCoin EQL [ProjMap CoinR outputCoinL (utxo p)]
    -- , SumsTo (Right (Coin 1)) totalAda EQL [One utxoCoin, One treasury, One reserves, One fees, One deposits, SumMap rewards]
    Random fees
  , Random (proposalsT p)
  , Random (futureProposalsT p)
  , ledgerState :<-: (ledgerStateT p)
  ]
  where
    colUtxo = Var (V "colUtxo" (MapR TxInR (TxOutR p)) No)
    utxoSize = Var (V "utxoSize" SizeR No)
    preUtxo = Var (V "preUtxo" (MapR TxInR (TxOutR p)) No)

ledgerStateStage ::
  (Reflect era) =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
ledgerStateStage proof subst0 = do
  let preds = ledgerStatePreds proof
  subst <- toolChainSub proof standardOrderInfo preds subst0
  (_env, status) <- pure (undefined, Nothing) -- monadTyped $ checkForSoundness preds subst
  case status of
    Nothing -> pure subst
    Just msg -> error msg

main :: IO ()
main = do
  let proof = Conway Standard
  -- Babbage Standard
  -- Alonzo Standard
  -- Mary Standard
  -- Shelley Standard
  env <-
    generate
      ( pure emptySubst
          >>= pParamsStage proof
          >>= universeStage proof
          >>= vstateStage proof
          >>= pstateStage proof
          >>= dstateStage proof
          >>= ledgerStateStage proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  lstate <- monadTyped $ runTarget env (ledgerStateT proof)
  putStrLn (show (pcLedgerState proof lstate))
  goRepl proof env ""
