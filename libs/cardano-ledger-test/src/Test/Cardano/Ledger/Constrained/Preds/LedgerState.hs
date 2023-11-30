{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Preds.LedgerState where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (gasDeposit)
import Cardano.Ledger.DRep (drepDepositL)
import Control.Monad (when)
import Data.Default.Class (Default (def))
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (OrdCond (..))
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad (monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.CertState (dstateStage, pstateStage, vstateStage)
import Test.Cardano.Ledger.Constrained.Preds.PParams (pParamsStage)
import Test.Cardano.Ledger.Constrained.Preds.Repl (ReplMode (..), modeRepl)
import Test.Cardano.Ledger.Constrained.Preds.Universes (UnivSize (..), universeStage)
import Test.Cardano.Ledger.Constrained.Rewrite (standardOrderInfo)
import Test.Cardano.Ledger.Constrained.Size (Size (..))
import Test.Cardano.Ledger.Constrained.Solver (toolChainSub)
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Utils (testIO)
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.PrettyCore (pcLedgerState)
import Test.Cardano.Ledger.Generic.Proof
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain)

-- =========================================

prevGovActionIdsGenPreds :: Reflect era => Proof era -> [Pred era]
prevGovActionIdsGenPreds _ =
  [ Random prevPParamUpdate
  , Random prevHardFork
  , Random prevCommittee
  , Random prevConstitution
  , Random ppUpdateChildren
  , Random hardForkChildren
  , Random committeeChildren
  , Random constitutionChildren
  ]

prevGovActionIdsCheckPreds :: Proof era -> [Pred era]
prevGovActionIdsCheckPreds _ = []

enactStateGenPreds :: Reflect era => Proof era -> [Pred era]
enactStateGenPreds p =
  [ Random committeeVar
  , Random constitution
  , prevPParams p :<-: (Constr "id" id ^$ pparams p)
  , currPParams p :<-: (Constr "id" id ^$ pparams p)
  , Random enactTreasury
  , Random enactWithdrawals
  , -- PrevGovActionsIds constraints
    Random prevPParamUpdate
  , Random prevHardFork
  , Random prevCommittee
  , Random prevConstitution
  , Subset (Dom prevDRepState) voteUniv
  , Subset (Dom partialDRepDistr) drepUniv
  ]
    ++ prevGovActionIdsGenPreds p

enactStateCheckPreds :: Proof era -> [Pred era]
enactStateCheckPreds _ = []

ledgerStatePreds :: forall era. Reflect era => UnivSize -> Proof era -> [Pred era]
ledgerStatePreds usize p =
  [ Subset (Dom enactWithdrawals) credsUniv
  , Random enactTreasury
  , Random constitution
  , Random committeeVar
  , Random ppUpdateChildren
  , Random hardForkChildren
  , Random committeeChildren
  , Random constitutionChildren
  , Random prevPParamUpdate
  , Random prevHardFork
  , Random prevConstitution
  , Random prevCommittee
  , Sized (Range 0 1) currProposals
  , proposalDeposits :<-: (Constr "sumActionStateDeposits" (foldMap gasDeposit) :$ (Simple currProposals))
  , -- TODO, introduce ProjList so we can write: SumsTo (Right (Coin 1)) proposalDeposits  EQL [ProjList CoinR gasDepositL currProposals]
    MetaSize (SzRng 90 (usNumPreUtxo usize)) utxoSize -- must be bigger than sum of (maxsize inputs 10) and (mazsize collateral 3)
  , Sized utxoSize preUtxo
  , Sized (Range 15 (usNumColUtxo usize)) colUtxo
  , MapMember feeTxIn feeTxOut (Right preUtxo)
  , Subset (Dom preUtxo) txinUniv
  , Subset (Rng preUtxo) (txoutUniv p)
  , utxo p :<-: (Constr "mapunion" Map.union ^$ preUtxo ^$ colUtxo)
  , Disjoint (Dom preUtxo) (Dom colUtxo)
  , Subset (Dom colUtxo) txinUniv
  , Subset (Rng colUtxo) (colTxoutUniv p)
  , NotMember feeTxIn (Dom colUtxo)
  , NotMember feeTxOut (Rng colUtxo)
  , SumsTo
      (Right (Coin 1))
      deposits
      EQL
      [SumMap stakeDeposits, SumMap poolDeposits, One proposalDeposits, ProjMap CoinR drepDepositL currentDRepState]
  , -- Some things we might want in the future.
    -- , SumsTo (Right (Coin 1)) utxoCoin EQL [ProjMap CoinR outputCoinL (utxo p)]
    -- , SumsTo (Right (Coin 1)) totalAda EQL [One utxoCoin, One treasury, One reserves, One fees, One deposits, SumMap rewards]
    Random fees
  , ledgerState :<-: (ledgerStateT p)
  , Sized (Range 1 10) donation
  , prevPParams p :<-: (Constr "id" id ^$ (pparams p))
  , currPParams p :<-: (Constr "id" id ^$ (pparams p))
  , -- We need the poolDistr to generate a valid Pulser
    Dom regPools :=: Dom poolDistr
  , SumsTo (Left (1 % 1000)) (Lit RationalR 1) EQL [ProjMap RationalR individualPoolStakeL poolDistr]
  ]
    ++ ( case whichGovState p of
          GovStateConwayToConway -> prevPulsingPreds p -- Constraints to generate a valid Pulser
          GovStateShelleyToBabbage ->
            [ Sized (Range 0 1) (pparamProposals p)
            , Sized (Range 0 1) (futurePParamProposals p)
            ]
       )
  where
    colUtxo = Var (V "colUtxo" (MapR TxInR (TxOutR p)) No)
    utxoSize = Var (V "utxoSize" SizeR No)
    preUtxo = Var (V "preUtxo" (MapR TxInR (TxOutR p)) No)

ledgerStateStage ::
  Reflect era =>
  UnivSize ->
  Proof era ->
  Subst era ->
  Gen (Subst era)
ledgerStateStage usize proof subst0 = do
  let preds = ledgerStatePreds usize proof
  subst <- toolChainSub proof standardOrderInfo preds subst0
  (_env, status) <- pure (undefined, Nothing) -- monadTyped $ checkForSoundness preds subst
  case status of
    Nothing -> pure subst
    Just msg -> error msg

demo :: Reflect era => Proof era -> ReplMode -> IO ()
demo proof mode = do
  env <-
    generate
      ( pure emptySubst
          >>= pParamsStage proof
          >>= universeStage def proof
          >>= vstateStage proof
          >>= pstateStage proof
          >>= dstateStage proof
          >>= ledgerStateStage def proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  lstate <- monadTyped $ runTarget env (ledgerStateT proof)
  let env2 = getTarget lstate (ledgerStateT proof) emptyEnv
  when (mode == Interactive) $ putStrLn (show (pcLedgerState proof lstate))
  modeRepl mode proof env2 ""

demoTest :: TestTree
demoTest = testIO "Testing LedgerState Stage" (demo (Conway Standard) CI)

main :: IO ()
main = defaultMain $ testIO "Testing LedgerState Stage" (demo (Conway Standard) Interactive)

-- =================================
