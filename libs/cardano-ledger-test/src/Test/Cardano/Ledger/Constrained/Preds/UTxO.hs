{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Preds.UTxO where

import Control.Monad (when)
import Data.Default.Class (Default (def))
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad (monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.PParams (pParamsStage)
import Test.Cardano.Ledger.Constrained.Preds.Repl (ReplMode (..), modeRepl)
import Test.Cardano.Ledger.Constrained.Preds.Universes (UnivSize (..), universeStage)
import Test.Cardano.Ledger.Constrained.Rewrite (standardOrderInfo)
import Test.Cardano.Ledger.Constrained.Size (Size (..))
import Test.Cardano.Ledger.Constrained.Solver (toolChainSub)
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.PrettyCore (pcUTxO)
import Test.Cardano.Ledger.Generic.Proof
import Test.QuickCheck

-- ===========================================================

utxoPreds :: forall era. Reflect era => UnivSize -> Proof era -> [Pred era]
utxoPreds usize p =
  [ MetaSize (SzExact (usNumPreUtxo usize)) utxoSize -- must be bigger than sum of (maxsize inputs 10) and (maxsize collateral 3)
  , Sized utxoSize preUtxo
  , Sized (AtLeast 6) colUtxo
  , MapMember feeTxIn feeTxOut (Right preUtxo)
  , Subset (Dom preUtxo) txinUniv
  , Subset (Rng preUtxo) (txoutUniv p)
  , utxo p :<-: (Constr "mapunion" Map.union ^$ preUtxo ^$ colUtxo)
  , Disjoint (Dom preUtxo) (Dom colUtxo)
  , Subset (Dom colUtxo) txinUniv
  , Subset (Rng colUtxo) (colTxoutUniv p)
  , NotMember feeTxIn (Dom colUtxo)
  , NotMember feeTxOut (Rng colUtxo)
  , incrementalStake :<-: incrementalStakeT p -- Computes incrementalStake from the Term 'utxo' and the proof 'p'
  ]
  where
    colUtxo = Var (V "colUtxo" (MapR TxInR (TxOutR p)) No)
    utxoSize = Var (V "utxoSize" SizeR No)
    preUtxo = Var (V "preUtxo" (MapR TxInR (TxOutR p)) No)

utxoStage ::
  Reflect era =>
  UnivSize ->
  Proof era ->
  Subst era ->
  Gen (Subst era)
utxoStage usize proof subst0 = do
  let preds = utxoPreds usize proof
  subst <- toolChainSub proof standardOrderInfo preds subst0
  (_env, status) <- pure (undefined, Nothing) -- monadTyped $ checkForSoundness preds subst
  case status of
    Nothing -> pure subst
    Just msg -> error msg

demoUTxO :: Reflect era => Proof era -> ReplMode -> IO ()
demoUTxO proof mode = do
  env <-
    generate
      ( pure emptySubst
          >>= pParamsStage proof
          >>= universeStage def proof
          >>= utxoStage def proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  utx <- monadTyped $ runTerm env (utxo proof)
  when (mode == Interactive) $ putStrLn (show (pcUTxO proof (liftUTxO utx)))
  modeRepl mode proof env ""
