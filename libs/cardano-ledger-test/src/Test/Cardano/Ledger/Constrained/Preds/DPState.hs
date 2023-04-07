{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Constrained.Preds.DPState where

import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Pretty (ppMap)
import GHC.Real ((%))
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Lenses (fGenDelegGenKeyHashL)
import Test.Cardano.Ledger.Constrained.Monad (monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.Universes
import Test.Cardano.Ledger.Constrained.Rewrite (standardOrderInfo)
import Test.Cardano.Ledger.Constrained.Size (OrdCond (..), Size (..))
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.PrettyCore (pcDState, pcIndividualPoolStake, pcKeyHash, pcPState)
import Test.Cardano.Ledger.Generic.Proof
import Test.QuickCheck

pstateNames :: [String]
pstateNames =
  [ "regPools"
  , "futureRegPools"
  , "retiring"
  , "poolDeposits"
  ]

pstatePreds :: Reflect era => Proof era -> [Pred era]
pstatePreds _p =
  [ Random epochNo -- Should this be defined in Universes?
  -- These Sized constraints are needd to be ensure that regPools is bigger than retiring
  , Sized (ExactSize 3) retiring
  , Sized (AtLeast 3) regPools
  , Subset (Dom regPools) poolsUniv
  , Subset (Dom futureRegPools) poolsUniv
  , Subset (Dom poolDeposits) poolsUniv
  , Subset (Dom retiring) (Dom regPools) -- Note regPools must be bigger than retiring
  , Dom regPools :=: Dom poolDeposits
  , Disjoint (Dom regPools) (Dom futureRegPools)
  , epochs :=: Elems retiring
  , Choose
      (SzExact 3)
      epochs
      [ (Constr "id" id ^$ e, [CanFollow e epochNo])
      , (Constr "(+1)" (+ 1) ^$ e, [CanFollow e epochNo])
      , (Constr "(+3)" (+ 3) ^$ e, [CanFollow e epochNo])
      , (Constr "(+5)" (+ 5) ^$ e, [CanFollow e epochNo])
      ]
  , -- poolDistr not needed in PState, but is needed in NewEpochState
    -- But since it is so intimately tied to regPools we define it here
    -- Alternately we could put this in NewEpochState, and insist that pStateStage
    -- preceed newEpochStateStage
    Dom regPools :=: Dom poolDistr
  , SumsTo (1 % 1000) (Lit RationalR 1) EQL [Project RationalR poolDistr]
  ]
  where
    e = var "e" EpochR
    epochs = var "epochs" (ListR EpochR)

pstateStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
pstateStage proof = toolChainSub proof standardOrderInfo (pstatePreds proof)

main :: IO ()
main = do
  let proof = Babbage Standard
  env <-
    generate
      ( pure []
          >>= universeStage proof
          >>= pstateStage proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  pstate <- monadTyped $ runTarget env pstateT
  pDistr <- monadTyped (findVar (unVar poolDistr) env)
  putStrLn (show (pcPState pstate))
  putStrLn "\n"
  putStrLn (show (ppMap pcKeyHash pcIndividualPoolStake pDistr))
  putStrLn "\n"
  putStrLn (unlines (otherFromEnv [] env))

-- ============================================================================

dstatePreds :: Proof era -> [Pred era]
dstatePreds _p =
  [ Sized (AtMost 8) rewards -- Small enough that it leaves some slack with credUniv (size about 30)
  , Sized (AtLeast 1) treasury --  If these have size zero, the SumsTo can't be solved
  , Sized (AtLeast 1) instanReserves
  , Random instanTreasury
  , Dom rewards :=: Dom stakeDeposits
  , Dom delegations :⊆: Dom rewards
  , Dom rewards :=: Rng ptrs
  , Dom genDelegs :⊆: Dom genesisUniv
  , Negate (deltaReserves) :=: deltaTreasury
  , SumsTo (Coin 1) instanReservesSum EQL [SumMap instanReserves]
  , SumsTo (DeltaCoin 1) (Delta instanReservesSum) LTH [One (Delta reserves), One deltaReserves]
  , SumsTo (Coin 1) instanTreasurySum EQL [SumMap instanTreasury]
  , SumsTo (DeltaCoin 1) (Delta instanTreasurySum) LTH [One (Delta treasury), One deltaTreasury]
  , ProjS fGenDelegGenKeyHashL GenHashR (Dom futureGenDelegs) :=: Dom genDelegs
  ]
  where
    instanReservesSum = Var (V "instanReservesSum" CoinR No)
    instanTreasurySum = Var (V "instanTreasurySum" CoinR No)

dstateStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
dstateStage proof = toolChainSub proof standardOrderInfo (dstatePreds proof)

mainD :: IO ()
mainD = do
  let proof = Babbage Standard
  env <-
    generate
      ( pure []
          >>= universeStage proof
          >>= dstateStage proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  dState <- monadTyped $ runTarget env dstateT
  putStrLn (show (pcDState dState))
  putStrLn "\n"
  putStrLn (unlines (otherFromEnv [] env))
